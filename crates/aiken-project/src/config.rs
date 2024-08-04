use crate::{github::repo::LatestRelease, package_name::PackageName, paths, Error};
pub use aiken_lang::plutus_version::PlutusVersion;
use aiken_lang::{
    ast::{
        Annotation, ByteArrayFormatPreference, Constant, ModuleConstant, Span, UntypedDefinition,
    },
    expr::UntypedExpr,
    parser::token::Base,
};
use miette::NamedSource;
use semver::Version;
use serde::{
    de,
    ser::{self, SerializeSeq},
    Deserialize, Serialize,
};
use std::{collections::BTreeMap, fmt::Display, fs, io, path::Path};

#[derive(Deserialize, Serialize, Clone)]
pub struct Config {
    pub name: PackageName,
    pub version: String,
    #[serde(
        deserialize_with = "deserialize_version",
        serialize_with = "serialize_version",
        default = "default_version"
    )]
    pub compiler: Version,
    #[serde(default)]
    pub plutus: PlutusVersion,
    pub license: Option<String>,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
    #[serde(default)]
    pub dependencies: Vec<Dependency>,
    #[serde(default)]
    pub config: BTreeMap<String, BTreeMap<String, SimpleExpr>>,
}

#[derive(Clone, Debug)]
pub enum SimpleExpr {
    Int(i64),
    Bool(bool),
    ByteArray(String),
    List(Vec<SimpleExpr>),
}

impl SimpleExpr {
    pub fn as_untyped_expr(&self) -> UntypedExpr {
        match self {
            SimpleExpr::Bool(b) => UntypedExpr::Var {
                location: Span::empty(),
                name: if *b { "True" } else { "False" }.to_string(),
            },
            SimpleExpr::Int(i) => UntypedExpr::UInt {
                location: Span::empty(),
                value: format!("{i}"),
                base: Base::Decimal {
                    numeric_underscore: false,
                },
            },
            SimpleExpr::ByteArray(s) => UntypedExpr::ByteArray {
                location: Span::empty(),
                bytes: s.as_bytes().to_vec(),
                preferred_format: ByteArrayFormatPreference::Utf8String,
            },
            SimpleExpr::List(es) => UntypedExpr::List {
                location: Span::empty(),
                elements: es.iter().map(|e| e.as_untyped_expr()).collect(),
                tail: None,
            },
        }
    }

    pub fn as_definition(&self, identifier: &str) -> UntypedDefinition {
        let location = Span::empty();

        let (value, annotation) = match self {
            SimpleExpr::Bool(..) => todo!("requires https://github.com/aiken-lang/aiken/pull/992"),
            SimpleExpr::Int(i) => (
                // TODO: Replace with 'self.as_untyped_expr()' after https://github.com/aiken-lang/aiken/pull/992
                Constant::Int {
                    location,
                    value: format!("{i}"),
                    base: Base::Decimal {
                        numeric_underscore: false,
                    },
                },
                Some(Annotation::int(location)),
            ),
            SimpleExpr::ByteArray(s) => (
                // TODO: Replace with 'self.as_untyped_expr()' after https://github.com/aiken-lang/aiken/pull/992
                Constant::ByteArray {
                    location,
                    bytes: s.as_bytes().to_vec(),
                    preferred_format: ByteArrayFormatPreference::Utf8String,
                },
                Some(Annotation::bytearray(location)),
            ),
            SimpleExpr::List(..) => todo!("requires https://github.com/aiken-lang/aiken/pull/992"),
        };

        UntypedDefinition::ModuleConstant(ModuleConstant {
            location: Span::empty(),
            doc: None,
            public: true,
            name: identifier.to_string(),
            annotation,
            value: Box::new(value),
            tipo: (),
        })
    }
}

impl Serialize for SimpleExpr {
    fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            SimpleExpr::Bool(b) => serializer.serialize_bool(*b),
            SimpleExpr::Int(i) => serializer.serialize_i64(*i),
            SimpleExpr::ByteArray(s) => serializer.serialize_str(s.as_str()),
            SimpleExpr::List(es) => {
                let mut seq = serializer.serialize_seq(Some(es.len()))?;
                for e in es {
                    seq.serialize_element(e)?;
                }
                seq.end()
            }
        }
    }
}

impl<'a> Deserialize<'a> for SimpleExpr {
    fn deserialize<D: de::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        struct SimpleExprVisitor;

        impl<'a> de::Visitor<'a> for SimpleExprVisitor {
            type Value = SimpleExpr;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("Int | Bool | ByteArray | List<any_of_those>")
            }

            fn visit_bool<E>(self, b: bool) -> Result<Self::Value, E> {
                Ok(SimpleExpr::Bool(b))
            }

            fn visit_i64<E>(self, i: i64) -> Result<Self::Value, E> {
                Ok(SimpleExpr::Int(i))
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E> {
                Ok(SimpleExpr::ByteArray(s.to_string()))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'a>,
            {
                let mut es = Vec::new();

                while let Some(e) = seq.next_element()? {
                    es.push(e);
                }

                Ok(SimpleExpr::List(es))
            }
        }

        deserializer.deserialize_any(SimpleExprVisitor)
    }
}

fn deserialize_version<'de, D>(deserializer: D) -> Result<Version, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?.replace('v', "");

    Version::parse(&buf).map_err(serde::de::Error::custom)
}

fn serialize_version<S>(version: &Version, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let version = format!("v{}", version);

    serializer.serialize_str(&version)
}

fn default_version() -> Version {
    Version::parse(built_info::PKG_VERSION).unwrap()
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Repository {
    pub user: String,
    pub project: String,
    pub platform: Platform,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Copy, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    Github,
    Gitlab,
    Bitbucket,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Debug)]
pub struct Dependency {
    pub name: PackageName,
    pub version: String,
    pub source: Platform,
}

impl Display for Platform {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        match *self {
            Platform::Github => f.write_str("github"),
            Platform::Gitlab => f.write_str("gitlab"),
            Platform::Bitbucket => f.write_str("bitbucket"),
        }
    }
}

impl Config {
    pub fn default(name: &PackageName) -> Self {
        Config {
            name: name.clone(),
            version: "0.0.0".to_string(),
            compiler: default_version(),
            plutus: PlutusVersion::default(),
            license: Some("Apache-2.0".to_string()),
            description: format!("Aiken contracts for project '{name}'"),
            repository: Some(Repository {
                user: name.owner.clone(),
                project: name.repo.clone(),
                platform: Platform::Github,
            }),
            dependencies: vec![Dependency {
                name: PackageName {
                    owner: "aiken-lang".to_string(),
                    repo: "stdlib".to_string(),
                },
                version: match LatestRelease::of("aiken-lang/stdlib") {
                    Ok(stdlib) => stdlib.tag_name,
                    _ => "1.5.0".to_string(),
                },
                source: Platform::Github,
            }],
            config: BTreeMap::new(),
        }
    }

    pub fn save(&self, dir: &Path) -> Result<(), io::Error> {
        let aiken_toml_path = dir.join(paths::project_config());
        let aiken_toml = toml::to_string_pretty(self).unwrap();
        fs::write(aiken_toml_path, aiken_toml)
    }

    pub fn load(dir: &Path) -> Result<Config, Error> {
        let config_path = dir.join(paths::project_config());
        let raw_config = fs::read_to_string(&config_path).map_err(|_| Error::MissingManifest {
            path: dir.to_path_buf(),
        })?;

        let result: Self = toml::from_str(&raw_config).map_err(|e| Error::TomlLoading {
            path: config_path.clone(),
            src: raw_config.clone(),
            named: NamedSource::new(config_path.display().to_string(), raw_config).into(),
            // this isn't actually a legit way to get the span
            location: e.span().map(|range| Span {
                start: range.start,
                end: range.end,
            }),
            help: e.to_string(),
        })?;

        Ok(result)
    }

    pub fn insert(mut self, dependency: &Dependency, and_replace: bool) -> Option<Self> {
        for existing in self.dependencies.iter_mut() {
            if existing.name == dependency.name {
                return if and_replace {
                    existing.version.clone_from(&dependency.version);
                    Some(self)
                } else {
                    None
                };
            }
        }
        self.dependencies.push(dependency.clone());
        Some(self)
    }
}

mod built_info {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

pub fn compiler_version(include_commit_hash: bool) -> String {
    if include_commit_hash {
        format!(
            "v{}+{}",
            built_info::PKG_VERSION,
            built_info::GIT_COMMIT_HASH_SHORT.unwrap_or("unknown")
        )
    } else {
        format!("v{}", built_info::PKG_VERSION,)
    }
}

pub fn compiler_info() -> String {
    format!(
        r#"
Operating System: {}
Architecture:     {}
Version:          {}"#,
        built_info::CFG_OS,
        built_info::CFG_TARGET_ARCH,
        compiler_version(true),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[allow(clippy::arc_with_non_send_sync)]
    fn arbitrary_simple_expr() -> impl Strategy<Value = SimpleExpr> {
        let leaf = prop_oneof![
            (any::<i64>)().prop_map(SimpleExpr::Int),
            (any::<bool>)().prop_map(SimpleExpr::Bool),
            "[a-z]*".prop_map(SimpleExpr::ByteArray)
        ];

        leaf.prop_recursive(3, 8, 3, |inner| {
            prop_oneof![
                inner.clone(),
                prop::collection::vec(inner.clone(), 0..3).prop_map(SimpleExpr::List)
            ]
        })
    }

    #[derive(Deserialize, Serialize)]
    struct TestConfig {
        expr: SimpleExpr,
    }

    proptest! {
        #[test]
        fn round_trip_simple_expr(expr in arbitrary_simple_expr()) {
            let pretty = toml::to_string_pretty(&TestConfig { expr });
            assert!(
                matches!(
                    pretty.as_ref().map(|s| toml::from_str::<TestConfig>(s.as_str())),
                    Ok(Ok(..)),
                ),
                "\ncounterexample: {}\n",
                pretty.unwrap_or_default(),
            )

        }
    }
}
