use std::{collections::HashSet, fs};

use crate::{cli::CompileArgs, log};
use polaris_core::compile::compile::CompileContext;

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

    for file in files {
        let mut ctx_clone = compile_ctx.clone();
        tasks.push(tokio::spawn(async move {
            let source = match fs::read_to_string(&file) {
                Ok(source) => source,
                Err(e) => {
                    ctx_clone
                        .logger
                        .critical(&format!("Failed to read file {}: {}", file, e));
                    unreachable!()
                }
            };

            match ctx_clone.ingest_source(file.clone(), source) {
                Ok(_) => ctx_clone,
                Err(_) => {
                    ctx_clone
                        .logger
                        .error(&format!("Failed to compile file: {}", file));
                    return ctx_clone;
                }
            }
        }));
    }

    for task in tasks {
        match task.await {
            Err(e) => {
                logger.error(&format!("Task failed: {}", e));
            }
            Ok(ctx) => compile_ctx.merge(ctx),
        }
    }

    let (warnings, errors) = compile_ctx.get_diagnostics();
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
