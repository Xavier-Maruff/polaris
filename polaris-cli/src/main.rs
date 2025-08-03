mod build_cmd;
mod cli;
mod compile_cmd;
mod log;

use clap::Parser;

#[tokio::main]
async fn main() {
    let cli = cli::Cli::parse();

    match cli.command {
        cli::Commands::Build(_args) => {}
        cli::Commands::Compile(args) => {
            compile_cmd::command(args).await;
        }
        cli::Commands::Test(_args) => {
            //
        }
        cli::Commands::New(_args) => {
            //
        }
    }
}
