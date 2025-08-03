use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, name = "polaris", propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Args)]
pub struct BuildArgs {
    #[arg(short, long)]
    pub release: bool,
    #[arg(short, long)]
    pub target: Option<String>,
    #[arg(short, long)]
    pub opt: Option<i8>,
    #[arg(short, long, default_value_t = 1)]
    pub verbosity: u8,
}

#[derive(Args)]
pub struct CompileArgs {
    pub files: Vec<String>,
    #[arg(short, long)]
    pub release: bool,
    #[arg(short, long)]
    pub target: Option<String>,
    #[arg(short, long)]
    pub opt: Option<i8>,
    #[arg(short, long, default_value_t = 1)]
    pub verbosity: u8,
    #[arg(short, long)]
    pub werror: bool,
}

#[derive(Args)]
pub struct TestArgs {
    #[arg(short, long)]
    pub tests: Option<Vec<String>>,
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
    Compile(CompileArgs),
    Test(TestArgs),
    New(NewArgs),
}
