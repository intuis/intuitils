use std::error::Error;

#[derive(Debug)]
pub struct ErrorMessage {
    pub title: String,
    pub description: String,
    pub source: Box<dyn Error + Send + Sync>,
}

impl ErrorMessage {
    pub fn new(
        title: impl Into<String>,
        message: impl Into<String>,
        error: Box<dyn Error + Send + Sync>,
    ) -> Self {
        Self {
            title: title.into(),
            description: message.into(),
            source: error,
        }
    }
}
