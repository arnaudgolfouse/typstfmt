#![doc = include_str!("../../README.md")]
#![warn(clippy::all)]

use itertools::Itertools as _;
use tracing::{debug, instrument, warn};
use typst_syntax::{
    parse, LinkedNode,
    SyntaxKind::{
        Args, Array, Binary, BlockComment, CodeBlock, Colon, ContentBlock, Destructuring, Dict, Eq,
        Keyed, LetBinding, LineComment, Named, Params, Parenthesized, Raw, Set, Show, Space,
    },
};

mod config;
pub use config::Config;
mod context;
use context::Ctx;

mod utils;

mod args;
mod binary;
mod code_blocks;
mod content_blocks;

#[must_use]
pub fn format(s: &str, config: Config) -> String {
    let init = parse(s);
    let mut context = Ctx::from_config(config);
    let root = LinkedNode::new(&init);
    visit(&root, &mut context)
}

/// This is recursively called on the AST, the formatting is bottom up,
/// nodes will decide based on the size of their children and the max line length
/// how they will be formatted.
///
/// One assumed rule is that no kind should be formatting with surrounded space
#[instrument(skip_all,name = "V", fields(kind = format!("{:?}",node.kind())))]
fn visit(node: &LinkedNode, ctx: &mut Ctx) -> String {
    let mut res: Vec<String> = vec![];
    for child in node.children() {
        let child_fmt = visit(&child, ctx);
        res.push(child_fmt);
    }
    let res = match node.kind() {
        Binary => binary::format_bin_left_assoc(node, &res, ctx),
        Named | Keyed => format_named_args(node, &res, ctx),
        CodeBlock => code_blocks::format_code_blocks(node, &res, ctx),
        ContentBlock => content_blocks::format_content_blocks(node, &res, ctx),
        Args | Params | Dict | Array | Destructuring | Parenthesized => {
            args::format_args(node, &res, ctx)
        }
        LetBinding => format_let_binding(node, &res, ctx),
        Raw | BlockComment | LineComment => {
            ctx.lost_context();
            node.text().to_string()
        }
        _ => format_default(node, &res, ctx),
    };
    if node.children().count() == 0 {
        debug!("visiting token {:?}", node.kind());
    } else {
        debug!("visiting parent: {:?}", node.kind());
    }
    res
}

/// formats a node for which no specific function was found. Last resort.
/// For the text of the node:
/// Trim spaces for Space nodes if they contain a linebreak.
/// avoids:
/// - putting two consecutive spaces.
/// - putting more than two consecutive newlines.
///
/// For the already formatted children, change nothing.
#[instrument(skip_all, ret)]
fn format_default(node: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    debug!("::format_default: {:?}", node.kind());
    debug!(
        "with children: {:?}",
        node.children().map(|c| c.kind()).collect_vec()
    );

    let mut res = String::new();
    ctx.push_in(node.text(), &mut res);
    for s in children {
        ctx.push_raw_in(s, &mut res);
    }
    res
}

#[instrument(skip_all, ret)]
pub(crate) fn format_named_args(parent: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    let mut res = String::new();
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            Show | Set => {
                ctx.push_raw_in(s, &mut res);
                ctx.push_in(" ", &mut res);
            }
            Colon => res.push_str(": "),
            Space => {}
            LineComment | BlockComment => ctx.push_raw_in(s, &mut res),
            _ => {
                ctx.push_raw_in(s, &mut res);
            }
        }
    }
    res
}

#[instrument(skip_all, ret)]
pub(crate) fn format_let_binding(
    parent: &LinkedNode,
    children: &[String],
    ctx: &mut Ctx,
) -> String {
    let mut res = String::new();
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            Eq => {
                ctx.push_in(" ", &mut res);
                ctx.push_in(s, &mut res);
                ctx.push_in(" ", &mut res);
            }
            Space => ctx.push_in(s, &mut res),
            _ => {
                ctx.push_raw_in(s, &mut res);
            }
        }
    }
    res
}

#[cfg(test)]
mod tests;
