use std::{collections::HashSet, fs};

use crate::{cli::CompileArgs, log};
use polaris_core::{self as pl, compile::compile::CompileContext, parse::parse::ParseContext};

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
                    return Err(pl::parse::diagnostic::Diagnostic {
                        primary: pl::parse::diagnostic::DiagnosticMsg {
                            message: format!("Failed to read file '{}': {}", file, e),
                            file: file.clone(),
                            span: pl::parse::parse::CodeSpan { start: 0, end: 0 },
                            err_type: pl::parse::diagnostic::DiagnosticMsgType::IoError,
                        },
                        notes: vec![],
                        hints: vec!["Are you sure this file exists?".to_string()],
                    });
                }
            };
            let ast = ParseContext::new(&logger_clone).parse(file.clone(), source);

            println!("{}", ast);
            //println!("{:#?}", ast);
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
