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
            build_cmd::command(args).await;
        }
        cli::Commands::New(_args) => {
            //
        }
    }
}
