//! Serialized diagnostic output, for tooling and screenreaders, etc.

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Severity {
    Bug,
    Error,
    Warning,
    Note,
    Help,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Diagnostic {
    pub message: String,
    pub severity: Severity,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Label {
    pub file: String,
    pub start: Location,
    pub end: Location,
    pub style: LabelStyle,
    pub message: String,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub byte: usize,
}
