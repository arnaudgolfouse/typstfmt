use crate::Ctx;
use tracing::instrument;
use typst_syntax::{
    LinkedNode,
    SyntaxKind::{LeftBracket, Markup, RightBracket, Space},
};

#[instrument(skip_all)]
pub(crate) fn format_content_blocks(
    parent: &LinkedNode,
    children: &[String],
    ctx: &mut Ctx,
) -> String {
    // if children.iter().any(|c| c.contains('\n')) {
    //     debug!("tight");
    //     return format_block_content_breaking(parent, children, ctx);
    // }
    format_block_content_tight(parent, children, ctx)
}

/// doesn't change anything
pub(crate) fn format_block_content_tight(
    parent: &LinkedNode,
    children: &[String],
    ctx: &mut Ctx,
) -> String {
    let mut res = String::new();
    for (s, _) in children.iter().zip(parent.children()) {
        ctx.push_raw_in(s, &mut res);
    }
    res
}

#[instrument(skip_all)]
pub(crate) fn format_block_content_breaking(
    parent: &LinkedNode,
    children: &[String],
    ctx: &mut Ctx,
) -> String {
    let mut res = String::new();
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            LeftBracket => {
                res.push_str(&format!("{s}\n{}", ctx.get_indent()));
                ctx.just_spaced = true;
            }
            RightBracket => {
                ctx.push_raw_in("\n", &mut res);
                res.push_str(s);
            }
            Markup => ctx.push_raw_indent(s.trim_start_matches([' ', '\n']), &mut res),
            Space => {}
            _ => {
                ctx.push_raw_indent(s, &mut res);
            }
        }
    }
    res
}
