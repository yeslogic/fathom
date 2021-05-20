//! This stuff would probably be better off in `codespan_reporting`...

use codespan_reporting::diagnostic::{Diagnostic, LabelStyle, Severity};
use codespan_reporting::files::{self, Files};

pub mod api;

#[derive(Debug, Copy, Clone)]
pub enum Style {
    Human,
    Json,
}

pub fn to_json_diagnostic<'files, F: Files<'files>>(
    files: &'files F,
    diagnostic: &Diagnostic<F::FileId>,
) -> Result<api::Diagnostic, files::Error> {
    let labels = diagnostic
        .labels
        .iter()
        .map(|label| {
            let start = files.location(label.file_id, label.range.start)?;
            let end = files.location(label.file_id, label.range.end)?;

            Ok(api::Label {
                file: files.name(label.file_id)?.to_string(),
                start: api::Location {
                    line: start.line_number,
                    column: start.column_number,
                    byte: label.range.start,
                },
                end: api::Location {
                    line: end.line_number,
                    column: end.column_number,
                    byte: label.range.end,
                },
                style: match label.style {
                    LabelStyle::Primary => api::LabelStyle::Primary,
                    LabelStyle::Secondary => api::LabelStyle::Secondary,
                },
                message: label.message.clone(),
            })
        })
        .collect::<Result<_, files::Error>>()?;

    Ok(api::Diagnostic {
        message: diagnostic.message.clone(),
        severity: match diagnostic.severity {
            Severity::Bug => api::Severity::Bug,
            Severity::Error => api::Severity::Error,
            Severity::Warning => api::Severity::Warning,
            Severity::Note => api::Severity::Note,
            Severity::Help => api::Severity::Help,
        },
        labels,
        notes: diagnostic.notes.clone(),
    })
}
