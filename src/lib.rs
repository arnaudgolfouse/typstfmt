#![doc = include_str!("../README.md")]
#![warn(
    clippy::all,
    clippy::print_stdout,
    clippy::print_stderr,
    clippy::dbg_macro
)]

use itertools::Itertools;
use pretty_print::Format;
use pretty_print::PrettyPrinter;
use tracing::debug;
use tracing::instrument;
use tracing::warn;
use typst_syntax::ast::BinOp;
use typst_syntax::SyntaxKind;
use typst_syntax::SyntaxKind::*;
use typst_syntax::SyntaxNode;
use typst_syntax::{parse, LinkedNode};
use Option::None;

mod config;

pub use config::Config;

mod context;

use context::Ctx;

mod utils;

mod binary;
mod code_blocks;
mod markup;
mod math;
mod params;

mod pretty_print;

#[must_use]
pub fn format(s: &str, config: Config) -> String {
    //replace tabs
    // let s = &s.replace('\t', &" ".repeat(config.indent_space));

    let mut context = Ctx::from_config(config);
    // let init = parse(s);
    // let root = LinkedNode::new(&init);
    // let s = visit(&root, &mut context);
    let root = typst_syntax::parse(s);
    let Some(format) = visit_bis(&root, &mut context, false) else {
        return "".into();
    };
    PrettyPrinter {
        format: &format,
        max_width: config.max_line_length as _,
    }
    .to_string()
}

/// Returns [`None`] if the node is empty, or should be ignored (in code mode).
fn visit_bis(node: &SyntaxNode, ctx: &mut Ctx, in_code: bool) -> Option<Format> {
    if ctx.off {
        let mut result = Format::text(node.text().clone());
        if node.kind() == SyntaxKind::LineComment && node.text() == "// typstfmt::on" {
            ctx.off = false;
            return Some(result);
        }
        for child in node.children() {
            if let Some(format) = visit_bis(child, ctx, in_code) {
                result &= format;
            }
        }
        return Some(result);
    }
    if node.is_empty() {
        return None;
    }
    ctx.just_spaced = false;
    match node.kind() {
        SyntaxKind::Markup => markup::format_markup_bis(node, ctx),
        SyntaxKind::Space => {
            // FIXME: need a mode 'math', where the only significant whitespace are the ones right before/after the dollar signs.
            if in_code {
                None
            } else if node.text().contains('\n') {
                ctx.just_spaced = true;
                Some(Format::newline())
            } else {
                ctx.just_spaced = true;
                Some(Format::space())
            }
        }
        // FIXME: Add a 'soft' newline right after the linebreak
        SyntaxKind::Linebreak => Some(Format::text(node.text().clone())),
        SyntaxKind::Parbreak => Some(Format::newline() & Format::newline()),
        SyntaxKind::Strong | SyntaxKind::Emph => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, false));
            let open = children.next().unwrap();
            let markup = children.next().unwrap();
            let close = children.next().unwrap();
            Some(open & markup & close)
        }
        SyntaxKind::Heading => {
            let mut children = node.children();
            let marker = children.next().unwrap();
            let mut heading = Format::text(marker.text().clone());
            for c in children {
                let Some(n) = visit_bis(c, ctx, false) else {
                    continue;
                };
                heading &= n;
            }
            Some(heading.flat())
        }
        SyntaxKind::ListItem | SyntaxKind::EnumItem => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, false));
            let mut item = children.next().unwrap();
            for c in children {
                item &= c.indent();
            }
            Some(item)
        }
        SyntaxKind::TermItem => todo!("{node:#?}"),
        SyntaxKind::Equation => {
            // FIXME: handle '&' alignement. Look at the existing code !
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, false));
            let mut equation = children.next().unwrap();
            for c in children {
                equation &= c;
            }
            Some(equation)
        }
        SyntaxKind::Math => markup::format_markup_bis(node, ctx),
        SyntaxKind::MathIdent => {
            // TODO: auto convert to UTF8 ? Like pi => π
            Some(Format::text(node.text().clone()))
        }
        SyntaxKind::MathDelimited => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, false));
            let mut delim = children.next().unwrap();
            for c in children {
                delim &= c;
            }
            Some(delim)
        }
        SyntaxKind::Code => unreachable!("already handled in CodeBlock"),
        SyntaxKind::CodeBlock => {
            let mut children = node.children().filter(|c| c.kind() != SyntaxKind::Space);
            let left_brace = Format::text(children.next().unwrap().text().clone());
            let code = children.next().unwrap();
            let Some(right_brace) = children.next() else {
                // `code` is in fact the closing "}"
                return Some(left_brace & Format::text(code.text().clone()));
            };
            let right_brace = Format::text(right_brace.text().clone());
            let mut multiple_lines = false;
            let mut result_code = None;
            for c in code.children().filter_map(|c| {
                if c.kind() == SyntaxKind::Semicolon {
                    None
                } else {
                    visit_bis(c, ctx, true)
                }
            }) {
                multiple_lines = result_code.is_some();
                result_code = Some(match result_code {
                    Some(res) => res & Format::newline() & c,
                    None => c,
                });
            }
            let result_code = result_code.unwrap();
            if multiple_lines {
                Some(
                    (left_brace
                        & (Format::newline() & result_code).indent()
                        & Format::newline()
                        & right_brace)
                        .unflat(),
                )
            } else {
                Some(
                    (left_brace.clone()
                        & Format::space()
                        & result_code.clone().flat()
                        & Format::space()
                        & right_brace.clone())
                        | (left_brace
                            & (Format::newline() & result_code).indent()
                            & Format::newline()
                            & right_brace),
                )
            }
        }
        SyntaxKind::ContentBlock => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, false));
            let mut block = children.next().unwrap();
            for c in children {
                block &= c;
            }
            Some(block)
        }
        SyntaxKind::Parenthesized => {
            // TODO: merge this with SyntaxKind::Array
            let mut children = node
                .children()
                .filter_map(|c| visit_bis(c, ctx, true).map(|f| (f, c.kind())));
            let open_paren = children.next().unwrap().0;
            let mut expr = None;
            let mut close_paren = None;
            let trivia1: Option<Format> = (&mut children)
                .take_while(|(c, kind)| {
                    if !kind.is_trivia() {
                        expr = Some(c.clone());
                        false
                    } else {
                        true
                    }
                })
                .map(|(c, _)| c)
                .collect();
            let trivia2: Option<Format> = children
                .take_while(|(c, kind)| {
                    if !kind.is_trivia() {
                        close_paren = Some(c.clone());
                        false
                    } else {
                        true
                    }
                })
                .map(|(c, _)| c)
                .collect();
            let force_multiline = match (&trivia1, &trivia2) {
                (_, Some(f)) | (Some(f), _) => f.has_newline(),
                _ => false,
            };
            let expr = expr.unwrap();
            let close_paren = close_paren.unwrap();
            let mut inner_single = expr.clone();
            let mut inner_multi = expr;
            if let Some(trivia) = trivia1 {
                inner_single = trivia.clone() & inner_single;
                if trivia.has_newline() {
                    inner_multi = (Format::newline() & trivia & inner_multi).indent();
                } else {
                    inner_multi = (trivia & Format::newline() & inner_multi).indent();
                }
            } else {
                inner_multi = (Format::newline() & inner_multi).indent();
            }
            if let Some(trivia) = trivia2 {
                let has_newline = trivia.has_newline();
                inner_single &= trivia.clone();
                inner_multi &= Format::space() & trivia;
                if !has_newline {
                    inner_multi &= Format::newline();
                }
            } else {
                inner_multi &= Format::newline();
            }
            Some(if force_multiline {
                (open_paren & inner_multi & close_paren).unflat()
            } else {
                (open_paren.clone() & inner_single.flat() & close_paren.clone())
                    | (open_paren & inner_multi & close_paren)
            })
        }
        SyntaxKind::Array
        | SyntaxKind::Dict
        | SyntaxKind::Destructuring
        | SyntaxKind::Args
        | SyntaxKind::Params => {
            let mut children = node.children();
            let open_paren = children
                .find_map(|c| {
                    if c.kind() == SyntaxKind::LeftParen {
                        Some(Format::text(c.text().clone()))
                    } else {
                        None
                    }
                })
                .unwrap();
            let mut single_line = open_paren.clone();
            let mut multi_line = Format::newline();

            let mut first = true;
            let mut has_comment = false;
            for c in &mut children {
                let Some(format) = visit_bis(c, ctx, true) else {
                    continue;
                };
                let kind = c.kind();
                if kind == SyntaxKind::RightParen {
                    single_line &= format.clone();
                    multi_line =
                        open_paren & multi_line.indent() & Format::newline() & format.clone();
                    break;
                }
                if !kind.is_trivia() && kind != SyntaxKind::Comma {
                    if first {
                        first = false;
                    } else {
                        single_line &= Format::text(", ".into());
                        multi_line &= Format::newline();
                    }
                    single_line &= format.clone();
                    multi_line &= format & Format::text(",".into());
                } else if kind.is_trivia() {
                    has_comment = true;
                    multi_line &= Format::space() & Format::text(c.text().clone());
                }
            }
            // trailing comments
            for c in children {
                let Some(format) = visit_bis(c, ctx, in_code) else {
                    continue;
                };
                single_line &= format.clone();
                multi_line &= format;
            }
            if first && !has_comment {
                // no args, no comments: "()"
                Some(single_line)
            } else if has_comment {
                // A comment forces line breaking
                Some(multi_line)
            } else {
                Some(single_line | multi_line)
            }
        }
        SyntaxKind::Named | SyntaxKind::Keyed => {
            // FIXME: handle comments
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let name = children.next().unwrap();
            let colon = children.next().unwrap();
            let expr = children.next().unwrap();
            Some(name & colon & Format::space() & expr)
        }
        SyntaxKind::Unary => {
            // FIXME: handle comments
            let mut children = node.children();
            let op = children.next().unwrap();
            let kind = op.kind();
            let op = Format::text(op.text().clone());
            let op = match kind {
                SyntaxKind::Not => op & Format::space(),
                _ => op,
            };
            let expr = children
                .filter_map(|c| visit_bis(c, ctx, true))
                .next()
                .unwrap();
            Some(op & expr)
        }
        SyntaxKind::Binary => {
            // FIXME: handle comments
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let left = children.next().unwrap();
            let op = children.next().unwrap();
            let right = children.next().unwrap();
            let single_line = left & Format::space() & op & Format::space() & right;
            Some(single_line)
        }
        SyntaxKind::Spread => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let dotdot = children.next().unwrap();
            let arg = children.next().unwrap();
            Some(
                dotdot & arg, // | (dotdot & (Format::newline() & arg).indent()),
            )
        }
        SyntaxKind::Closure => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let params = children.next().unwrap();
            let arrow = children.next().unwrap();
            let body = children.next().unwrap();
            if let Some(new_body) = children.next() {
                // This is a named function
                let name = params;
                let params = arrow;
                let arrow = body;
                Some(name & params & Format::space() & arrow & Format::space() & new_body)
            } else {
                Some(params & Format::space() & arrow & Format::space() & body)
            }
        }
        SyntaxKind::LetBinding => {
            if node
                .children()
                .filter(|c| c.kind() != SyntaxKind::Space)
                .nth(1)
                .unwrap()
                .kind()
                == SyntaxKind::Closure
            {
                // This is a function definition
                let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
                let let_kw = children.next().unwrap();
                let closure = children.next().unwrap();
                return Some(let_kw & Format::space() & closure);
            }
            let mut children = node
                .children()
                .filter_map(|c| visit_bis(c, ctx, true).map(|f| (f, c.kind())));
            let let_kw = children.next().unwrap().0 & Format::space();
            let pattern = (&mut children)
                .take_while(|(_, kind)| {
                    !matches!(
                        kind,
                        SyntaxKind::Eq
                            | SyntaxKind::PlusEq
                            | SyntaxKind::HyphEq
                            | SyntaxKind::StarEq
                            | SyntaxKind::SlashEq
                    )
                })
                .next()
                .unwrap()
                .0;
            let equal = Format::space() & children.next().unwrap().0;
            let expr = children.next().unwrap().0;

            Some(let_kw & pattern & equal & Format::space() & expr)
        }
        SyntaxKind::SetRule => todo!("{node:#?}"),
        SyntaxKind::ShowRule => todo!("{node:#?}"),
        SyntaxKind::Conditional => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let if_kw = children.next().unwrap();
            let condition = children.next().unwrap();
            let then_body = children.next().unwrap();
            let mut multi_line = if_kw & Format::space() & condition & Format::space() & then_body;
            if let Some(else_kw) = children.next() {
                let else_body = children.next().unwrap();
                multi_line &= Format::space() & else_kw & Format::space() & else_body;
            }
            let single_line = multi_line.clone().flat();
            Some(single_line | multi_line)
        }
        SyntaxKind::WhileLoop => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let while_kw = children.next().unwrap();
            let condition = children.next().unwrap();
            let body = children.next().unwrap();
            let multi_line = while_kw & Format::space() & condition & Format::space() & body;
            let single_line = multi_line.clone().flat();
            Some(single_line | multi_line)
        }
        SyntaxKind::ForLoop => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let for_kw = children.next().unwrap();
            let pattern = children.next().unwrap();
            let in_kw = children.next().unwrap();
            let expr = children.next().unwrap();
            let body = children.next().unwrap();
            let multi_line = for_kw
                & Format::space()
                & pattern
                & Format::space()
                & in_kw
                & Format::space()
                & expr
                & Format::space()
                & body;
            let single_line = multi_line.clone().flat();
            Some(single_line | multi_line)
        }
        SyntaxKind::LoopBreak => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            children.next()
        }
        SyntaxKind::LoopContinue => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            children.next()
        }
        SyntaxKind::ModuleImport => todo!("{node:#?}"),
        SyntaxKind::ImportItems => todo!("{node:#?}"),
        SyntaxKind::RenamedImportItem => todo!("{node:#?}"),
        SyntaxKind::ModuleInclude => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let include_kw = children.next().unwrap();
            let path = children.next().unwrap();
            Some(include_kw & Format::space() & path)
        }
        SyntaxKind::FuncReturn => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let return_kw = children.next().unwrap();
            match children.next() {
                Some(expr) => Some(return_kw & Format::space() & expr),
                None => Some(return_kw),
            }
        }
        SyntaxKind::DestructAssignment => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, true));
            let pattern = children.next().unwrap();
            let equal = children.next().unwrap();
            let expr = children.next().unwrap();
            Some(pattern & Format::space() & equal & Format::space() & expr)
        }
        SyntaxKind::LineComment => {
            if node.text() == "// typstfmt::off" {
                ctx.off = true;
                Some(Format::text(node.text().clone()))
            } else {
                Some(Format::text(node.text().clone()) & Format::newline())
            }
        }
        SyntaxKind::Raw => {
            let text = node.text().clone();
            if text.starts_with("```") {
                Some(Format::raw_block(text))
            } else {
                Some(Format::text(text))
            }
        }
        SyntaxKind::Error => todo!("{node:#?}"),
        // Default behaviour:
        // - tokens are rendered as-is
        // - node are the flattening of their children
        _ => {
            let mut children = node.children().filter_map(|c| visit_bis(c, ctx, in_code));
            if let Some(mut item) = children.next() {
                for c in children {
                    item &= c.indent();
                }
                Some(item)
            } else {
                debug_assert!(children.next().is_none(), "This is not a token !");
                Some(Format::text(node.text().clone()))
            }
        }
    }
}

/// This is recursively called on the AST, the formatting is bottom up,
/// nodes will decide based on the size of their children and the max line length
/// how they will be formatted.
///
/// One assumed rule is that no kind should be formatting with surrounded space
#[instrument(skip_all, name = "V", fields(kind = format!("{:?}",node.kind())))]
fn visit(node: &LinkedNode, ctx: &mut Ctx) -> String {
    let mut res: Vec<String> = vec![];
    for child in node.children() {
        let child_fmt = visit(&child, ctx);
        res.push(child_fmt);
    }
    let res = match node.kind() {
        LineComment => format_comment_handling_disable(node, &res, ctx),
        _ if ctx.off => no_format(node, &res, ctx),
        Binary => binary::format_bin_left_assoc(node, &res, ctx),
        Named | Keyed => format_named_args(node, &res, ctx),
        ListItem | EnumItem | TermItem => format_list_enum(node, &res, ctx),
        CodeBlock => code_blocks::format_code_blocks(node, &res, ctx),
        Markup => markup::format_markup(node, &res, ctx),
        ContentBlock => markup::format_content_blocks(node, &res, ctx),
        Args | Params | Dict | Array | Destructuring | Parenthesized => {
            params::format_args(node, &res, ctx)
        }
        LetBinding => format_let_binding(node, &res, ctx),
        Conditional => conditional_format(node, &res, ctx),
        Raw | BlockComment => {
            ctx.lost_context();
            node.text().to_string()
        }
        Equation => math::format_equation(node, &res, ctx),
        Math => math::format_math(node, &res, ctx),
        Str => no_format(node, &res, ctx),
        _ => format_default(node, &res, ctx),
    };
    if node.children().count() == 0 {
        debug!("TOKEN : {:?}", node.kind());
    } else {
        debug!("PARENT: {:?}", node.kind());
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
    let mut res = String::new();
    ctx.push_in(node.text(), &mut res);
    for s in children {
        ctx.push_raw_in(s, &mut res);
    }
    res
}

fn no_format(parent: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    let mut res = String::new();
    ctx.push_raw_in(parent.text(), &mut res);
    for s in children {
        ctx.push_raw_in(s, &mut res);
    }
    res
}

fn deep_no_format(parent: &LinkedNode) -> String {
    let mut res: Vec<String> = vec![];
    for child in parent.children() {
        let child_fmt = deep_no_format(&child);
        res.push(child_fmt);
    }
    no_format(parent, &res, &mut Ctx::default())
}

fn conditional_format(parent: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    let mut res = String::new();
    ctx.push_raw_in(parent.text(), &mut res);
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            _ if ctx.off => res.push_str(node.text()),
            Space => {}
            If => {
                ctx.push_raw_in(s, &mut res);
                ctx.push_raw_in(" ", &mut res);
            }
            CodeBlock | ContentBlock => {
                ctx.push_raw_in(" ", &mut res);
                ctx.push_raw_in(s, &mut res);
            }
            Else => {
                ctx.push_raw_in(" ", &mut res);
                ctx.push_raw_in(s, &mut res);
                if node.next_sibling_kind() == Some(Conditional) {
                    ctx.push_raw_in(" ", &mut res);
                }
            }
            _ => ctx.push_raw_in(s, &mut res),
        }
    }
    res
}

#[instrument(skip_all, ret)]
pub(crate) fn format_named_args(parent: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    let mut res = String::new();
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            _ if ctx.off => res.push_str(node.text()),
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
            _ if ctx.off => res.push_str(node.text()),
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

fn format_comment_handling_disable(parent: &LinkedNode, _: &[String], ctx: &mut Ctx) -> String {
    ctx.lost_context();
    if parent.text().contains("typstfmt::off") {
        ctx.off = true;
    } else if parent.text().contains("typstfmt::on") {
        ctx.off = false;
    } else if parent.text().contains("typstfmt::") {
        warn!("your comment contains `typstfmt::` not followed by `on` or `off`, did you make a typo?");
    }
    parent.text().to_string()
}

fn format_list_enum(parent: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    let mut res = String::new();
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            _ if ctx.off => res.push_str(node.text()),
            EnumMarker | ListMarker | TermMarker => {
                ctx.push_raw_in(node.text(), &mut res);
            }
            _ => {
                ctx.push_raw_indent(s, &mut res);
            }
        }
    }
    res
}

#[cfg(test)]
mod tests;
