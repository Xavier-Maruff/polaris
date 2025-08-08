use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
};
use crossbeam_channel::unbounded;
use polaris_core::{
    diagnostic::{self as pl_diagnostic, is_error},
    log::{self, Logger},
};
use std::{collections::HashMap, fs::read_to_string, io::Write, thread};
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

pub struct CliLogger {
    strict_warn: bool,
    files: SimpleFiles<String, String>,
    file_ids: HashMap<String, usize>,
    level: log::LogLevel,
    stream: StandardStream,
    cs_config: codespan_reporting::term::Config,
}

pub fn spawn_log_thread(
    verbosity: u8,
    strict_warn: bool,
) -> anyhow::Result<(Logger, thread::JoinHandle<()>)> {
    let (sender, receiver) = unbounded::<log::LogCommand>();

    let hdl = thread::Builder::new()
        .name("polaris_log".to_string())
        .spawn(move || {
            let mut logger = CliLogger::new(strict_warn);
            for command in receiver {
                match command {
                    log::LogCommand::Diagnostic(err) => logger.diagnostic(&err),
                    log::LogCommand::Step { step, file } => logger.step(&step, &file),
                    log::LogCommand::Critical(msg) => logger.error(&msg),
                    log::LogCommand::Error(msg) => logger.error(&msg),
                    log::LogCommand::Warning(msg) => logger.warn(&msg),
                    log::LogCommand::Info(msg) => logger.info(&msg),
                    log::LogCommand::Debug(msg) => logger.debug(&msg),
                    log::LogCommand::Trace(msg) => logger.trace(&msg),
                    log::LogCommand::SetLevel(level) => logger.set_level(level),
                    log::LogCommand::Quit => {
                        break;
                    }
                }
            }
        })?;

    let logger = Logger::new(sender);
    match verbosity {
        0 => logger.set_level(log::LogLevel::Error),
        1 => logger.set_level(log::LogLevel::Warning),
        2 => logger.set_level(log::LogLevel::Info),
        3 => logger.set_level(log::LogLevel::Debug),
        4 => logger.set_level(log::LogLevel::Trace),
        _ => logger.set_level(log::LogLevel::Trace),
    };

    Ok((logger, hdl))
}

impl CliLogger {
    pub fn new(strict_warn: bool) -> Self {
        let stream = StandardStream::stdout(termcolor::ColorChoice::Auto);

        CliLogger {
            stream,
            level: log::LogLevel::Info,
            files: SimpleFiles::new(),
            cs_config: codespan_reporting::term::Config::default(),
            file_ids: HashMap::new(),
            strict_warn,
        }
    }

    pub fn step(&self, step: &str, msg: &str) {
        let mut buf = StandardStream::stdout(termcolor::ColorChoice::Auto);
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
            .unwrap();
        write!(&mut buf, "{:>10}", step).unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, " {}", msg).unwrap();
    }

    pub fn set_level(&mut self, level: log::LogLevel) {
        self.level = level;
    }

    pub fn info(&mut self, msg: &str) {
        let mut buf = self.stream.lock();
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Green)))
            .unwrap();
        write!(&mut buf, "   info: ").unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, "{}", msg).unwrap();
    }

    pub fn warn(&mut self, msg: &str) {
        let mut buf = self.stream.lock();
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
            .unwrap();
        write!(&mut buf, "warning: ").unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, "{}", msg).unwrap();
    }

    pub fn error(&mut self, msg: &str) {
        let mut buf = self.stream.lock();
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Red)))
            .unwrap();
        write!(&mut buf, "  error: ").unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, "{}", msg).unwrap();
    }

    pub fn critical(&mut self, msg: &str) {
        let mut buf = self.stream.lock();
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
            .unwrap();
        write!(&mut buf, "critical: ").unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, "{}", msg).unwrap();
    }

    pub fn debug(&mut self, msg: &str) {
        let mut buf = self.stream.lock();
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Blue)))
            .unwrap();
        write!(&mut buf, "  debug: ").unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, "{}", msg).unwrap();
    }

    pub fn trace(&mut self, msg: &str) {
        let mut buf = self.stream.lock();
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
            .unwrap();
        write!(&mut buf, "  trace: ").unwrap();
        buf.reset().unwrap();
        writeln!(&mut buf, "{}", msg).unwrap();
    }

    pub fn diagnostic(&mut self, err: &pl_diagnostic::Diagnostic) {
        let mut diagnostic = match is_error(&err.primary.err_type, self.strict_warn) {
            true => Diagnostic::error(),
            false => Diagnostic::warning(),
        }
        .with_message(err.primary.message.clone())
        .with_code(pl_diagnostic::code(&err.primary.err_type));

        let get_file_id = |s: &mut Self, file_path: &String| {
            if let Some(file_id) = s.file_ids.get(file_path) {
                return Ok(*file_id);
            }

            let source = match read_to_string(file_path) {
                Ok(content) => content,
                Err(e) => {
                    s.error(&format!("Failed to read file '{}': {}", file_path, e));
                    return Err(());
                }
            };
            Ok(s.files.add(file_path.clone(), source))
        };

        diagnostic = diagnostic.with_message(err.primary.message.clone());

        if err.primary.span.start != 0 || err.primary.span.end != 0 {
            diagnostic = diagnostic.with_label(Label::primary(
                match get_file_id(self, &err.primary.file) {
                    Ok(file_id) => file_id,
                    Err(_) => {
                        return;
                    }
                },
                err.primary.span.start..err.primary.span.end,
            ))
        }
        diagnostic = diagnostic.with_labels(
            err.notes
                .iter()
                .map(|note| {
                    Ok(Label::secondary(
                        get_file_id(self, &note.file)?,
                        note.span.start..note.span.end,
                    )
                    .with_message(note.message.clone()))
                })
                .filter(|l| l.is_ok())
                .map(|l: Result<Label<usize>, ()>| l.unwrap())
                .collect::<Vec<_>>(),
        );

        for hint in &err.hints {
            diagnostic = diagnostic.with_note("💡 Hint: ".to_string() + hint);
        }

        match term::emit(
            &mut self.stream.lock(),
            &self.cs_config,
            &self.files,
            &diagnostic,
        ) {
            Ok(_) => {}
            Err(e) => {
                println!("Failed to emit diagnostic: {}", e);
            }
        }
    }
}
