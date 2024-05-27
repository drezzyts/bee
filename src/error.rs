use crate::{expressions::LiteralValue, position::Position, token::Token};

pub struct BeeError {
    pub position: Position,
    pub message: String,
    pub location: String,
    pub object: Option<LiteralValue>
}

impl BeeError {
    pub fn error(position: &Position, message: &str, source: String) -> Self {
        BeeError::report(position, message, "", source)
    }

    pub fn return_v(location: &str, position: &Position, value: Option<LiteralValue>) -> Self {
        Self  {
            location: location.to_string(),
            message: "".to_string(),
            position: position.clone(),
            object: value
        }
    }

    pub fn report(position: &Position, message: &str, location: &str, source: String) -> Self {
        let parsed = format!(
            "(error) ~{} {} --> {}\n{}",
            location,
            position,
            message,
            BeeError::error_line(source, position)
        );

        Self {
            position: position.clone(),
            message: parsed,
            location: location.to_string(),
            object: None,
        }
    }

    fn error_line(source: String, position: &Position) -> String {
        let lines: Vec<&str> = source.as_str().split('\n').collect();
        let line = lines[position.line-1];

        let mut pointer = String::new();
        pointer += " ".repeat(position.cstart).as_str();
        pointer += "^";

        format!("-->\t{}\n    \t{}", line, pointer)
    }
}
