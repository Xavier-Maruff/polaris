use serde::Deserialize;
use std::collections::HashMap;
use toml;

#[allow(unused)]
#[derive(Deserialize, Debug)]
pub struct Config {
    pub package: String,
    pub version: String,

    pub path: String,

    #[serde(default)]
    pub opt: String,
    #[serde(default)]
    pub warnings_as_errors: bool,

    #[serde(default)]
    pub dependencies: HashMap<String, DepConfig>,
}

#[allow(unused)]
#[derive(Deserialize, Debug)]
pub struct DepConfig {
    pub min_version: Option<String>,
    pub max_version: Option<String>,
    pub path: String,
}

pub fn load_config(path: &str) -> Result<Config, Box<dyn std::error::Error>> {
    let path = format!("{}/polaris.toml", path);
    let config_str = std::fs::read_to_string(path)?;
    let config: Config = toml::from_str(&config_str)?;
    Ok(config)
}
