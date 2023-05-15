use crate::document::{convert_range, DocumentRequest};
use color_eyre::eyre::Result;
use lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams};
use spl_frontend::{
    ast::GlobalDeclaration,
    token::{Token, TokenType},
    Shiftable, ToRange,
};
use tokio::sync::mpsc::Sender;

pub async fn fold(
    doctx: Sender<DocumentRequest>,
    params: FoldingRangeParams,
) -> Result<Vec<FoldingRange>> {
    let doc_params = params.text_document;
    if let Some(doc_info) = super::get_doc_info(doc_params.uri, doctx).await? {
        let folding_ranges = doc_info
            .ast
            .global_declarations
            .iter()
            .filter_map(|gd| match gd.as_ref() {
                GlobalDeclaration::Procedure(p) => Some((p, gd.offset)),
                _ => None,
            })
            .map(|(p, offset)| {
                let proc_tokens = &doc_info.tokens[p.to_range().shift(offset)];
                let tokens = skip_leading_comments(proc_tokens);
                let text_range = if let (Some(first), Some(last)) = (tokens.first(), tokens.last())
                {
                    first.range.start..last.range.end
                } else {
                    0..0
                };
                let range = convert_range(&text_range, &doc_info.text);
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

fn skip_leading_comments(tokens: &[Token]) -> &[Token] {
    if let Some((
        Token {
            token_type: TokenType::Comment(_),
            ..
        },
        rest,
    )) = tokens.split_first()
    {
        skip_leading_comments(rest)
    } else {
        tokens
    }
}
