use std::{
    fs::File,
    io::{self, ErrorKind, Read, Write},
    path::PathBuf,
    sync::OnceLock,
};

use color_eyre::{eyre::Context, Result};
use serde::de::DeserializeOwned;
use thiserror::Error;
use xdg::BaseDirectories;

pub mod keybindings;

pub trait IntuiConfig: Sized + DeserializeOwned {
    fn app_name() -> &'static str;
    fn filename() -> &'static str;
    fn default_config() -> &'static str;

    fn should_exit_if_not_found() -> bool;
    fn message_if_not_found() -> Option<String>;

    fn path() -> &'static PathBuf {
        static PATH: OnceLock<PathBuf> = OnceLock::new();
        PATH.get_or_init(|| get_config_path(Self::filename(), Self::app_name()))
    }

    fn init() -> Result<Self> {
        match fetch_config::<Self>(Self::filename(), Self::app_name()) {
            Ok(mut config) => {
                config.post_init();
                Ok(config)
            }
            Err(e) => match e {
                ConfigFetchingError::Io(e) if e.kind() == ErrorKind::NotFound => {
                    let config = put_config::<Self>(
                        Self::default_config(),
                        Self::filename(),
                        Self::app_name(),
                    )?;

                    if let Some(message) = Self::message_if_not_found() {
                        eprintln!("{}", message);
                    }

                    if Self::should_exit_if_not_found() {
                        std::process::exit(0);
                    } else {
                        Ok(config)
                    }
                }
                ConfigFetchingError::Toml(e) => Err(e).with_context(|| {
                    format!(
                        "Failed to parse config located at {:?}",
                        get_config_path(Self::filename(), Self::app_name())
                    )
                }),
                _ => color_eyre::eyre::bail!(e),
            },
        }
    }

    fn post_init(&mut self) {}
}

fn fetch_config<T: DeserializeOwned>(
    config_name: &str,
    app_name: &'static str,
) -> Result<T, ConfigFetchingError> {
    let config_path = xdg_dirs(app_name)
        .find_config_file(config_name)
        .ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, format!("{config_name} not found"))
        })?;

    let mut config_buf = String::new();
    let mut config_file = File::open(config_path)?;
    config_file.read_to_string(&mut config_buf)?;

    Ok(toml::from_str(&config_buf)?)
}

fn put_config<T: DeserializeOwned>(
    content: &'static str,
    filename: &str,
    app_name: &'static str,
) -> Result<T, ConfigFetchingError> {
    let config_path = get_config_path(filename, app_name);
    let mut config_file = File::create(config_path)?;
    config_file.write_all(content.as_bytes())?;
    Ok(toml::from_str(content)?)
}

fn get_config_path(filename: &str, app_name: &'static str) -> PathBuf {
    xdg_dirs(app_name).place_config_file(filename).unwrap()
}

fn xdg_dirs(app_name: &'static str) -> BaseDirectories {
    xdg::BaseDirectories::with_prefix(app_name).unwrap()
}

#[derive(Error, Debug)]
pub enum ConfigFetchingError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Toml(#[from] toml::de::Error),
}
