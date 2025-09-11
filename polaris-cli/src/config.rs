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
    pub profile: HashMap<String, ProfileConfig>,

    #[serde(default)]
    pub dependencies: HashMap<String, DepConfig>,
}

#[allow(unused)]
#[derive(Deserialize, Debug)]
pub struct ProfileConfig {
    pub optimise: Option<String>,
    pub debug: Option<bool>,
    pub out_dir: Option<String>,
    #[serde(default)]
    pub warnings_as_errors: bool,
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
    let mut config: Config = toml::from_str(&config_str)?;

    if config.profile.is_empty() {
        let default_profiles = default_profile_config();
        for (key, value) in default_profiles {
            if !config.profile.contains_key(&key) {
                config.profile.insert(key, value);
            }
        }
    }

    Ok(config)
}

fn default_profile_config() -> HashMap<String, ProfileConfig> {
    let mut map = HashMap::new();
    map.insert(
        "dev".to_string(),
        ProfileConfig {
            optimise: Some("none".to_string()),
            debug: Some(true),
            out_dir: Some("build/dev".to_string()),
            warnings_as_errors: false,
        },
    );
    map.insert(
        "release".to_string(),
        ProfileConfig {
            optimise: Some("speed".to_string()),
            debug: Some(false),
            out_dir: Some("build/release".to_string()),
            warnings_as_errors: true,
        },
    );
    map
}
