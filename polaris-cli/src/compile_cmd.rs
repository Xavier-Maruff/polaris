use std::{collections::HashSet, fs};

use crate::{cli::CompileArgs, log};
use polaris_core as pl;

pub async fn command(args: CompileArgs, verbosity: u8) {
    let (logger, hdl) = match log::spawn_log_thread(verbosity) {
        Ok(logger) => logger,
        Err(e) => {
            eprintln!("Failed to spawn log thread: {}", e);
            return;
        }
    };

    let files: HashSet<_> = args.files.into_iter().collect();

    let mut _compile_context = pl::compile::compile::CompileContext::new(&logger);
    let mut tasks: Vec<_> = Vec::new();

    //async parse files in parallel
    for file in files {
        let logger_clone = logger.clone();
        tasks.push(tokio::spawn(async move {
            let source = match fs::read_to_string(&file) {
                Ok(source) => source,
                Err(e) => {
                    logger_clone.diagnostic(&pl::parse::error::DiagnosticErr {
                        primary: pl::parse::error::SourceErr {
                            message: format!("Failed to read file '{}': {}", file, e),
                            file: file.clone(),
                            span: pl::parse::parse::CodeSpan { start: 0, end: 0 },
                            err_type: pl::parse::error::SourceErrType::IoError,
                        },
                        notes: vec![],
                        hints: vec!["Are you sure this file exists?".to_string()],
                    });
                    return;
                }
            };

            let file_name = file.clone();
            let ctx = pl::parse::parse::ParseContext::new(file_name, source, &logger_clone);

            match ctx.parse() {
                Ok(_ast) => {
                    logger_clone.info("Parsed successfully");
                }
                Err(errors) => {
                    for error in errors {
                        logger_clone.diagnostic(&error);
                    }
                }
            }
        }));
    }

    for task in tasks {
        match task.await {
            Ok(_) => {}
            Err(e) => {
                logger.error(&format!("Task failed: {}", e));
            }
        }
    }

    logger.quit();
    hdl.join().expect("Failed to join log thread");
}
