use std::{collections::HashSet, fs, io, path};

use crate::{cli::BuildArgs, config::load_config, log};
use polaris_core::{compile::CompileContext, log::Logger};

pub async fn command(args: BuildArgs) -> Result<(), ()> {
    let (logger, hdl) = match log::spawn_log_thread(args.verbosity, false) {
        Ok(logger) => logger,
        Err(e) => {
            eprintln!("Failed to spawn log thread: {}", e);
            return Err(());
        }
    };

    //todo: package resolution with build cache
    //todo: make ingestion async across package scope
    //
    let source_dir = args.directory.unwrap_or(".".to_string());

    let config = match load_config(source_dir.as_str()) {
        Ok(config) => config,
        Err(e) => {
            logger.error(&format!("Failed to load config: {}", e));
            logger.quit();
            hdl.join().expect("Failed to join log thread");
            return Err(());
        }
    };

    let profile = config.profile.get(args.profile.as_str());
    if profile.is_none() {
        logger.error(&format!("Profile '{}' not found in config", args.profile));
        logger.quit();
        hdl.join().expect("Failed to join log thread");
        return Err(());
    }
    let profile = profile.unwrap();

    let compile_config = polaris_core::compile::CompileConfig {
        warnings_as_errors: profile.warnings_as_errors,
        optimise: profile.optimise.clone().unwrap_or("none".to_string()),
        out_dir: profile.out_dir.clone().unwrap_or("build".to_string()),
    };

    let mut compile_ctx = CompileContext::new(logger.clone(), compile_config);
    let mut failed = false;

    let mut packages = vec![(
        config.package,
        path::Path::new(source_dir.as_str())
            .join(config.path.as_str())
            .to_string_lossy()
            .to_string(),
    )];
    packages.extend(
        config
            .dependencies
            .iter()
            .map(|(name, dep_conf)| {
                (
                    name.clone(),
                    path::Path::new(source_dir.as_str())
                        .join(dep_conf.path.as_str())
                        .to_string_lossy()
                        .to_string(),
                )
            })
            .collect::<Vec<(String, String)>>(),
    );

    while let Some((package_name, package_path)) = packages.pop() {
        if let Err(_) = ingest_package(&logger, &mut compile_ctx, &package_name, &package_path) {
            failed = true;
        }
    }

    if failed {
        logger.error("Build failed due to unexpected critical errors.");
        logger.quit();
        hdl.join().expect("Failed to join log thread");
        return Err(());
    }

    match compile_ctx.run_passes() {
        Ok(_) => logger.step("Passes", "All passes completed successfully"),
        Err(_) => {
            logger.error("Failed to run passes");
        }
    }

    let (warnings, errors) = compile_ctx.get_diagnostics();
    let comp_failed = !errors.is_empty() || (profile.warnings_as_errors && !warnings.is_empty());
    let comp_warned = !warnings.is_empty();

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

    if comp_failed { Err(()) } else { Ok(()) }
}

fn ingest_package(
    logger: &Logger,
    compile_ctx: &mut CompileContext,
    package_name: &String,
    package_path: &String,
) -> Result<(), ()> {
    let package_path = match fs::canonicalize(&package_path) {
        Ok(path) => path,
        Err(e) => {
            logger.error(&format!(
                "Failed to canonicalize package path '{}': {}",
                package_path, e
            ));
            return Err(());
        }
    };

    let package_files = match get_all_files_recursively(&package_path) {
        Ok(files) => files,
        Err(e) => {
            logger.error(&format!(
                "Failed to read package directory '{}': {}",
                package_path.display(),
                e
            ));
            return Err(());
        }
    };

    let mut failed = false;
    for file_path in package_files {
        let file_str = match file_path.to_str() {
            Some(s) => s.to_string(),
            None => {
                logger.error(&format!(
                    "Failed to convert file path to string: {}",
                    file_path.display()
                ));
                failed = true;
                continue;
            }
        };

        //module name is package name + path relative to package path
        let module_name = match file_path.strip_prefix(&package_path) {
            Ok(rel_path) => {
                package_name.clone() + "/" + &rel_path.with_extension("").to_string_lossy()
            }
            Err(e) => {
                logger.error(&format!(
                    "Failed to get module name from file path '{}': {}",
                    file_path.display(),
                    e
                ));
                failed = true;
                continue;
            }
        };

        let source = match fs::read_to_string(&file_path) {
            Ok(src) => src,
            Err(e) => {
                logger.error(&format!(
                    "Failed to read source file '{}': {}",
                    file_path.display(),
                    e
                ));
                failed = true;
                continue;
            }
        };

        logger.step(
            "Ingesting",
            &format!("Package: {}, Module: {}", package_name, module_name),
        );

        compile_ctx.ingest_source(package_name.clone(), module_name, file_str, source)
    }

    if failed { Err(()) } else { Ok(()) }
}

fn get_all_files_recursively(dir: &path::PathBuf) -> Result<Vec<path::PathBuf>, io::Error> {
    let mut files = Vec::new();
    let mut seen = HashSet::new();
    let mut dirs_to_visit = vec![dir.to_path_buf()];

    while let Some(current_dir) = dirs_to_visit.pop() {
        if !seen.insert(current_dir.clone()) {
            continue; // Skip already seen directories to avoid cycles
        }

        for entry in fs::read_dir(&current_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                dirs_to_visit.push(path);
            } else if path.is_file() {
                files.push(path);
            }
        }
    }

    Ok(files)
}
