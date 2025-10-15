mod build_cmd;
mod cli;
mod config;
mod log;

use clap::Parser;

#[tokio::main]
async fn main() {
    let cli = cli::Cli::parse();

    match cli.command {
        cli::Commands::Build(args) => {
            let _ = build_cmd::command(args).await;
        }
        cli::Commands::New(_args) => {
            //
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{build_cmd, cli::BuildArgs};

    #[tokio::test]
    async fn imports() {
        let args = BuildArgs {
            directory: Some("../test/imports".into()),
            profile: "dev".into(),
            verbosity: 0,
        };

        let result = build_cmd::command(args).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn types() {
        let args = BuildArgs {
            directory: Some("../test/type".into()),
            profile: "dev".into(),
            verbosity: 0,
        };

        let result = build_cmd::command(args).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn pattern_validation() {
        let args = BuildArgs {
            directory: Some("../test/pattern_validation".into()),
            profile: "dev".into(),
            verbosity: 0,
        };

        let result = build_cmd::command(args).await;

        assert!(result.is_ok());
    }
}
