use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, name = "polaris", propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Args)]
pub struct BuildArgs {
    pub directory: Option<String>,
    #[arg(short, long, default_value = "dev")]
    pub profile: String,
    #[arg(short, long, default_value_t = 1)]
    pub verbosity: u8,
}

#[derive(Args)]
pub struct NewArgs {
    #[arg(short, long)]
    pub name: String,
    #[arg(short, long)]
    pub bin: bool,
    #[arg(short, long)]
    pub lib: bool,
    #[arg(short, long, default_value_t = 1)]
    pub verbosity: u8,
}

#[derive(Subcommand)]
pub enum Commands {
    Build(BuildArgs),
    New(NewArgs),
}
