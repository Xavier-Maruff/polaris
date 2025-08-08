use std::{collections::HashSet, fs};

use crate::{cli::CompileArgs, log};
use polaris_core::{
    self as pl, compile::compile::CompileContext, desugar::desugar, parse::parse::ParseContext,
};

pub async fn command(args: CompileArgs) {
    let (logger, hdl) = match log::spawn_log_thread(args.verbosity, args.werror) {
        Ok(logger) => logger,
        Err(e) => {
            eprintln!("Failed to spawn log thread: {}", e);
            return;
        }
    };

    let files: HashSet<_> = args.files.into_iter().collect();
    let mut tasks: Vec<_> = Vec::new();
    let mut compile_ctx = CompileContext::new(logger.clone());

    //async parse files in parallel
    for file in files {
        let logger_clone = logger.clone();
        tasks.push(tokio::spawn(async move {
            let source = match fs::read_to_string(&file) {
                Ok(source) => source,
                Err(e) => {
                    return Err(pl::diagnostic::Diagnostic {
                        primary: pl::diagnostic::DiagnosticMsg {
                            message: format!("Failed to read file '{}': {}", file, e),
                            file: file.clone(),
                            span: pl::parse::CodeSpan { start: 0, end: 0 },
                            err_type: pl::diagnostic::DiagnosticMsgType::IoError,
                        },
                        notes: vec![],
                        hints: vec!["Are you sure this file exists?".to_string()],
                    });
                }
            };
            let mut ast = ParseContext::new(&logger_clone).parse(file.clone(), source);
            let mut ctx = pl::ast::pass::PassContext {
                logger: &logger_clone,
                file: file.clone(),
            };
            match desugar(&mut ast, &mut ctx) {
                Ok(_) => {}
                Err(_) => {
                    return Err(pl::diagnostic::Diagnostic {
                        primary: pl::diagnostic::DiagnosticMsg {
                            message: "Failed to desugar AST".to_string(),
                            file: file.clone(),
                            span: pl::parse::CodeSpan { start: 0, end: 0 },
                            err_type: pl::diagnostic::DiagnosticMsgType::InvalidAstOperation,
                        },
                        notes: vec![],
                        hints: vec!["Check the AST structure and desugar rules.".to_string()],
                    });
                }
            }

            println!("{}", ast);
            // println!("{:#?}", ast);
            logger_clone.step("Parsed", &file);
            Ok(ast)
        }));
    }

    for task in tasks {
        match task.await {
            Ok(result) => match result {
                Ok(ast) => {
                    compile_ctx.add_translation_unit(ast).await;
                }
                Err(err) => {
                    logger.diagnostic(&err);
                }
            },
            Err(e) => {
                logger.error(&format!("Task failed: {}", e));
            }
        }
    }

    let (warnings, errors) = compile_ctx.get_diagnostics().await;
    let comp_failed = !errors.is_empty() || (args.werror && !warnings.is_empty());
    let comp_warned = !warnings.is_empty() && !args.werror;

    for warning in warnings {
        logger.diagnostic(&warning);
    }
    for error in errors {
        logger.diagnostic(&error);
    }

    if comp_failed {
        logger.error("Compilation failed due to errors.");
    } else if comp_warned {
        logger.step("Finished", "Compiled with warnings");
    } else {
        logger.step("Finished", "Compiled successfully");
    }

    logger.quit();
    hdl.join().expect("Failed to join log thread");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cli::CompileArgs;

    #[tokio::test]
    async fn compile_command() {
        let args = CompileArgs {
            files: vec!["../example/sanity.pol".to_string()],
            verbosity: 3,
            werror: false,
            release: false,
            target: None,
            opt: None,
        };
        command(args).await;
    }
}
