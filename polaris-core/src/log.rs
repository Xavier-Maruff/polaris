use crossbeam_channel::Sender;

use crate::diagnostic::Diagnostic;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum LogLevel {
    Critical,
    Error,
    Warning,
    Info,
    Debug,
    Trace,
}

pub enum LogCommand {
    SetLevel(LogLevel),
    Step { step: String, file: String },
    Diagnostic(Diagnostic),
    Critical(String),
    Error(String),
    Warning(String),
    Info(String),
    Debug(String),
    Trace(String),
    Quit,
}

#[derive(Debug, Clone)]
pub struct Logger {
    sender: Sender<LogCommand>,
}

impl Logger {
    pub fn new(sender: Sender<LogCommand>) -> Self {
        Logger { sender }
    }

    pub fn dummy() -> Self {
        let (sender, _) = crossbeam_channel::unbounded();
        Logger { sender }
    }

    pub fn quit(&self) {
        self.sender
            .send(LogCommand::Quit)
            .expect("Failed to send quit command");
    }

    pub fn set_level(&self, level: LogLevel) {
        self.sender
            .send(LogCommand::SetLevel(level))
            .expect("Failed to send log level change");
    }

    pub fn diagnostic(&self, err: &Diagnostic) {
        self.sender
            .send(LogCommand::Diagnostic(err.clone()))
            .expect("Failed to send diagnostic");
    }

    pub fn step(&self, step: &str, file: &str) {
        self.sender
            .send(LogCommand::Step {
                step: step.to_string(),
                file: file.to_string(),
            })
            .expect("Failed to send step");
    }

    pub fn critical(&self, msg: &str) {
        self.sender
            .send(LogCommand::Critical(msg.to_string()))
            .expect("Failed to send critical message");
    }

    pub fn error(&self, msg: &str) {
        self.sender
            .send(LogCommand::Error(msg.to_string()))
            .expect("Failed to send error message");
    }

    pub fn warn(&self, msg: &str) {
        self.sender
            .send(LogCommand::Warning(msg.to_string()))
            .expect("Failed to send warning message");
    }

    pub fn info(&self, msg: &str) {
        self.sender
            .send(LogCommand::Info(msg.to_string()))
            .expect("Failed to send info message");
    }

    pub fn debug(&self, msg: &str) {
        self.sender
            .send(LogCommand::Info(msg.to_string()))
            .expect("Failed to send debug message");
    }

    pub fn trace(&self, msg: &str) {
        self.sender
            .send(LogCommand::Info(msg.to_string()))
            .expect("Failed to send trace message");
    }

    pub fn log(&self, level: LogLevel, msg: &str) {
        match level {
            LogLevel::Critical => self.critical(msg),
            LogLevel::Error => self.error(msg),
            LogLevel::Warning => self.warn(msg),
            LogLevel::Info => self.info(msg),
            LogLevel::Debug => self.debug(msg),
            LogLevel::Trace => self.trace(msg),
        }
    }
}

pub struct DefaultLogger {
    queue: crossbeam_channel::Receiver<LogCommand>,
    level: LogLevel,
}

impl DefaultLogger {
    pub fn new(level: LogLevel) -> (Self, Logger) {
        let (sender, queue) = crossbeam_channel::unbounded::<LogCommand>();
        let logger = Logger::new(sender);
        (DefaultLogger { queue, level }, logger)
    }

    pub fn start_thread(level: LogLevel) -> (Logger, std::thread::JoinHandle<()>) {
        let (mut writer, logger) = Self::new(level);
        let handle = std::thread::Builder::new()
            .name("default_logger".to_string())
            .spawn(move || {
                writer.run();
            })
            .expect("Failed to spawn logger thread");
        (logger, handle)
    }

    pub fn run(&mut self) {
        while let Ok(command) = self.queue.recv() {
            match command {
                LogCommand::SetLevel(level) => self.level = level,
                LogCommand::Step { step, file } => {
                    println!("{}: {}", step, file);
                }
                LogCommand::Diagnostic(diagnostic) => {
                    println!("Diagnostic: {}", diagnostic);
                }
                LogCommand::Critical(msg) => eprintln!("Critical: {}", msg),
                LogCommand::Error(msg) => eprintln!("Error: {}", msg),
                LogCommand::Warning(msg) => eprintln!("Warning: {}", msg),
                LogCommand::Info(msg) => println!("Info: {}", msg),
                LogCommand::Debug(msg) => println!("Debug: {}", msg),
                LogCommand::Trace(msg) => println!("Trace: {}", msg),
                LogCommand::Quit => break,
            }
        }
    }
}
