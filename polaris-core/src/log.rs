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
