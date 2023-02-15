use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams};
use spl_frontend::{ast::GlobalDeclaration, ToRange};
use tokio::sync::mpsc::Sender;

pub(crate) async fn fold(
    doctx: Sender<DocumentRequest>,
    params: FoldingRangeParams,
) -> Result<Vec<FoldingRange>> {
    let doc_params = params.text_document;
    if let Some(doc_info) = super::get_doc_info(doc_params.uri, doctx).await? {
        let folding_ranges = doc_info
            .ast
            .global_declarations
            .iter()
            .filter_map(|gd| match gd {
                GlobalDeclaration::Procedure(p) => Some(p),
                _ => None,
            })
            .map(|p| {
                let range = convert_range(&p.to_range(), &doc_info.text);
                FoldingRange {
                    start_line: range.start.line,
                    end_line: range.end.line,
                    kind: Some(FoldingRangeKind::Region),
                    ..Default::default()
                }
            })
            .collect();
        Ok(folding_ranges)
    } else {
        Ok(Vec::new())
    }
}
