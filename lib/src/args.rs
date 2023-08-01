use crate::{
    utils::{self, get_next_ignoring, next_is_ignoring},
    Ctx,
};
use tracing::instrument;
use typst_syntax::{
    LinkedNode,
    SyntaxKind::{
        Array, BlockComment, Comma, LeftParen, LineComment, Markup, Parbreak, Parenthesized,
        RightParen, Space,
    },
};

#[instrument(skip_all)]
/// format args using [format_args_tight] or [format_args_breaking] depending on the context.
pub(crate) fn format_args(parent: &LinkedNode, children: &[String], ctx: &mut Ctx) -> String {
    // check if any children is markup and contains a linebreak, if so, breaking
    let mut res = vec![];
    utils::find_children(&mut res, parent, &|c| {
        c.parent_kind() == Some(Markup)
            && (c.kind() == Parbreak || (c.kind() == Space) && c.text().contains('\n'))
    });

    if !res.is_empty() {
        return format_args_breaking(parent, children, ctx);
    }

    if parent.children().any(|c| c.kind() == LineComment) {
        return format_args_breaking(parent, children, ctx);
    }

    let number_of_args = parent
        .children()
        .filter_map(|node| {
            if [
                Comma,
                Space,
                LeftParen,
                RightParen,
                LineComment,
                BlockComment,
            ]
            .contains(&node.kind())
            {
                None
            } else {
                Some(node)
            }
        })
        .count();

    if number_of_args <= 1 {
        return format_args_tight(parent, children, ctx);
    }

    let res = format_args_tight(parent, children, ctx);
    if utils::max_line_length(&res) >= ctx.config.max_line_length {
        return format_args_breaking(parent, children, ctx);
    }
    res
}

pub(crate) fn format_args_tight(
    parent: &LinkedNode<'_>,
    children: &[String],
    ctx: &mut Ctx,
) -> String {
    let mut res = String::new();
    for (s, node) in children.iter().zip(parent.children()) {
        match node.kind() {
            Space => {}
            Comma => {
                if next_is_ignoring(&node, RightParen, &[Space]) {
                    // not putting the comma in would result in a parenthesized expression, not an array
                    // "(a,) != (a)"
                    if node.parent_kind() == Some(Array) {
                        ctx.push_raw_in(",", &mut res);
                    }
                    // don't print
                } else {
                    ctx.push_raw_in(s, &mut res);
                    ctx.push_in(" ", &mut res);
                }
            }
            _ => {
                ctx.push_raw_in(s, &mut res);
            }
        }
    }
    res
}

/// this breaks line and adds indentation for items
/// breakline if line max length is over the line or if items on one line >= 3
pub(crate) fn format_args_breaking(
    parent: &LinkedNode<'_>,
    children: &[String],
    ctx: &mut Ctx,
) -> String {
    let mut res = String::new();
    let mut missing_trailing = !(parent.kind() == Parenthesized);
    let mut consecutive_items = 0;

    for (s, node) in children.iter().zip(parent.children()) {
        let is_last = next_is_ignoring(&node, RightParen, &[Space, LineComment, BlockComment]);
        match node.kind() {
            LeftParen => {
                res.push_str(s);
                res.push('\n');
                res.push_str(&ctx.get_indent());
            }
            RightParen => {
                if parent.kind() == Parenthesized {
                    // no trailing comma we don't have a newline!
                    ctx.push_in("\n", &mut res);
                }
                res.push_str(s);
            }
            LineComment | BlockComment => {
                consecutive_items = 0;
                if utils::prev_is_ignoring(&node, LineComment, &[Space])
                    || utils::prev_is_ignoring(&node, BlockComment, &[Space])
                {
                    ctx.push_raw_in(s, &mut res);
                    ctx.push_in("\n", &mut res);
                } else {
                    let prev = node.prev_sibling().unwrap();
                    let mark = res.rfind(|x| x != ' ' && x != '\n').unwrap() + 1;
                    let prev_maybe_space = get_next_ignoring(&prev, &[]);
                    res = res[..mark].to_string();
                    match prev_maybe_space {
                        Some(space) if space.kind() == Space && space.text().contains('\n') => {
                            res.push('\n');
                            res.push_str(&ctx.get_indent());
                            res.push_str(s);
                        }
                        _ => {
                            res.push(' ');
                            res.push_str(s);
                            res.push('\n');
                            ctx.consec_new_line = 2;
                        } // same line I need to regen jump
                    }
                }

                if !next_is_ignoring(&node, RightParen, &[Space]) {
                    ctx.push_raw_in(&ctx.get_indent(), &mut res);
                    ctx.just_spaced = true;
                }
            }
            Space => {}
            // handles trailing comma
            // handles Line comment
            Comma => {
                missing_trailing = false;
                let is_last_comma = utils::find_next(&node, &|x| x.kind() == Comma).is_none();
                let is_trailing =
                    next_is_ignoring(&node, RightParen, &[Space, LineComment, BlockComment]);

                if is_last_comma && is_trailing {
                    // no indent
                    ctx.push_raw_in(s, &mut res);
                    ctx.push_raw_in("\n", &mut res);
                } else {
                    if is_last_comma && !is_trailing {
                        missing_trailing = true;
                    }
                    if !ctx.config.experimental_args_breaking_consecutive
                        || consecutive_items >= 3
                        || s.contains('\n')
                        || res
                            .lines()
                            .last()
                            .is_some_and(|line| utils::max_line_length(&format!("{line}, ")) >= 10)
                    {
                        res.push_str(s);
                        res.push('\n');
                        res.push_str(&ctx.get_indent());
                        consecutive_items = 0;
                    } else {
                        consecutive_items += 1;
                        res.push_str(s);
                        res.push(' ');
                    }
                }
            }
            _ => {
                ctx.push_raw_indent(s, &mut res);
                if is_last && missing_trailing {
                    ctx.push_raw_in(",\n", &mut res);
                }
            }
        }
    }
    res
}
