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

    macro_rules! build_test {
        ($name:ident, $path:expr) => {
            #[tokio::test]
            async fn $name() {
                let args = BuildArgs {
                    directory: Some($path.into()),
                    profile: "dev".into(),
                    verbosity: 0,
                };

                let result = build_cmd::command(args).await;

                assert!(result.is_ok());
            }
        };
    }

    build_test!(imports, "../test/imports");
    build_test!(types, "../test/type");
    build_test!(pattern_validation, "../test/pattern_validation");
    build_test!(lifetime_analysis, "../test/lifetime_analysis");
    build_test!(monomorphisation, "../test/monomorphisation");
    build_test!(literal_coercion, "../test/literal_coercion");
    build_test!(closures, "../test/closure");
    build_test!(array_sizing, "../test/array_sizing");
    build_test!(size_polymorphism, "../test/size_polymorphism");
    build_test!(wildcard_types, "../test/wildcard_types");
    build_test!(const_effect, "../test/const_valid");
}
