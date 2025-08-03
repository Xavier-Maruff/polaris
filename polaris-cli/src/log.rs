use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
};
use crossbeam_channel::unbounded;
use polaris_core::{
    log::{self, Logger},
    parse::{self, diagnostic::is_error},
};
use std::{collections::HashMap, fs, io::Write, thread};
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

pub struct CliLogger {
    strict_warn: bool,
    files: SimpleFiles<String, String>,
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
            strict_warn,
        }
    }

    pub fn step(&self, step: &str, msg: &str) {
        let mut buf = StandardStream::stdout(termcolor::ColorChoice::Auto);
        buf.set_color(ColorSpec::new().set_fg(Some(Color::Magenta)))
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

    pub fn diagnostic(&mut self, err: &parse::diagnostic::Diagnostic) {
        let mut file_ids = HashMap::<String, usize>::new();
        let mut file_refs = Vec::with_capacity(err.notes.len() + 1);

        file_refs.push(err.primary.file.clone());

        for note in err.notes.iter() {
            file_refs.push(note.file.clone());
        }

        for f in file_refs {
            if file_ids.contains_key(&f) {
                continue;
            }

            file_ids.insert(
                f.clone(),
                self.files.add(
                    f.clone(),
                    match fs::read_to_string(&f) {
                        Ok(content) => content,
                        Err(e) => {
                            self.critical(&format!("Failed to read file '{}': {}", f, e));
                            return;
                        }
                    },
                ),
            );
        }

        let mut diagnostic = match is_error(&err.primary.err_type, self.strict_warn) {
            true => Diagnostic::error(),
            false => Diagnostic::warning(),
        }
        .with_message(err.primary.message.clone())
        .with_code(parse::diagnostic::code(&err.primary.err_type));
        if err.primary.span.start != 0 || err.primary.span.end != 0 {
            diagnostic = diagnostic.with_label(
                Label::primary(
                    file_ids[&err.primary.file],
                    err.primary.span.start..err.primary.span.end,
                )
                .with_message(err.primary.message.clone()),
            )
        }
        diagnostic = diagnostic.with_labels(
            err.notes
                .iter()
                .map(|note| {
                    Label::secondary(file_ids[&note.file], note.span.start..note.span.end)
                        .with_message(note.message.clone())
                })
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
