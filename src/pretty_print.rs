//! An implementation of Wadler's [A prettier printer](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf), adapted for typst.
//!
//! **Note on naming**: the paper calls the main manipulated object a `Document`. I
//! think this is not ideal for us, since document already has a separate meaning, so I
//! called it `Format`.
//!
//! It allows for a nice declarative design for formatting: the main function will
//! return an object of type [`Format`]. Formats can be combined with a few operators:
//! - **concat** (`&`) concatenates two formats, printing them one after the other.
//! - **choice** (`|`) prompts the printer for a choice between two formats, based on
//! whether or not we want to break a line.
//! - **ident** adds an identation level to the format.
//!
//! Now if we encounter, say a function `f` called with two expressions `e1` and `e2`,
//! we will write something like
//! ```rust,ignore
//! let doc_f = get_doc(f);
//! let doc_e1 = get_doc(e1);
//! let doc_e2 = get_doc(e2);
//! return (doc_f & "(" & e1 & ", " & e2 & ")") // If this fits on one line
//!     | (doc_f & "(" & "\n" & (e1 & ", \n" & e2 & ", \n").ident() & ")") // If it does not
//! ```

mod format;

pub(crate) use self::format::Format;
use self::format::FormatInner;

use std::fmt::Display;

/// _The_ Pretty Printer !
///
/// This structure is used to display a [`Format`].
///
/// Use its [`Display`] implementation to actually print it.
pub(crate) struct PrettyPrinter<'a> {
    /// Maximum line width that we'll try to stay within
    pub(crate) max_width: u64,
    /// The format to print.
    pub(crate) format: &'a Format,
}

/// The actual structure responsible for printing.
///
/// Created while [`Display`]ing [`PrettyPrinter`].
struct PrettyPrinterInner<'a> {
    /// Maximum line width that we'll try to stay within
    max_width: u64,
    /// Current column position
    col: u64,
    /// A stack of chunks to print. The _top_ of the stack is the
    /// _end_ of the vector, which represents the _earliest_ part
    /// of the document to print.
    chunks: Vec<Chunk<'a>>,
}

/// Structure used to print a [`Format`].
#[derive(Debug, Clone, Copy)]
struct Chunk<'a> {
    /// The format to print.
    format: &'a Format,
    /// Indentation of the current chunk.
    indent: u64,
    /// Are we inside a [flat](`FormatInner::Flat`) format?
    flat: bool,
}

impl<'a> Chunk<'a> {
    /// Set a new format for the chunk.
    fn with_format(self, format: &'a Format) -> Chunk<'a> {
        Chunk {
            format,
            indent: self.indent,
            flat: self.flat,
        }
    }

    /// Add indentation to the chunk.
    fn indent(mut self) -> Self {
        self.indent += 1;
        self
    }

    /// Set the chunk to flat.
    fn flat(mut self) -> Self {
        self.flat = true;
        self
    }
}

impl<'a> PrettyPrinterInner<'a> {
    /// Returns `false` if `chunk` cannot be inserted on the current line without
    /// overflowing the maximum width.
    fn fits(&self, chunk: Chunk<'a>) -> bool {
        let mut remaining = if self.col <= self.max_width {
            self.max_width - self.col
        } else {
            return false;
        };
        let mut stack = vec![chunk];
        let mut chunks = &self.chunks as &[Chunk];

        loop {
            let chunk = match stack.pop() {
                Some(chunk) => chunk,
                None => match chunks.split_last() {
                    None => return true,
                    Some((chunk, more_chunks)) => {
                        chunks = more_chunks;
                        *chunk
                    }
                },
            };

            match &chunk.format.0 {
                FormatInner::Newline => return true,
                // FIXME: maybe needs something more involved here: read the other cases to understand what is going on.
                FormatInner::RawBlock(_) => return false,
                FormatInner::Space => {
                    if 1 <= remaining {
                        remaining -= 1;
                    } else {
                        return false;
                    }
                }
                FormatInner::Text(_, text_width) => {
                    if *text_width <= remaining {
                        remaining -= *text_width;
                    } else {
                        return false;
                    }
                }
                FormatInner::Flat(f) => stack.push(chunk.with_format(f).flat()),
                FormatInner::UnFlat(_) => return false,
                FormatInner::Indent(f) => stack.push(chunk.with_format(f).indent()),
                FormatInner::Concat(f1, f2) => {
                    stack.push(chunk.with_format(f2));
                    stack.push(chunk.with_format(f1));
                }
                FormatInner::Choice(f1, f2) => {
                    if chunk.flat {
                        stack.push(chunk.with_format(f1));
                    } else {
                        // Relies on the rule that for every choice
                        // `x | y`, the first line of `y` is no longer
                        // than the first line of `x`.
                        stack.push(chunk.with_format(f2));
                    }
                }
            }
        }
    }
}

impl<'a> Display for PrettyPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut printer = PrettyPrinterInner {
            max_width: self.max_width,
            col: 0,
            chunks: vec![Chunk {
                format: self.format,
                indent: 0,
                flat: false,
            }],
        };
        while let Some(chunk) = printer.chunks.pop() {
            let format = &chunk.format.0;
            match format {
                FormatInner::Text(text, width) => {
                    f.write_str(text)?;
                    printer.col += width;
                }
                FormatInner::Newline => {
                    f.write_str("\n")?;
                    for _ in 0..chunk.indent {
                        // FIXME: add custom indentation
                        f.write_str("  ")?;
                    }
                    printer.col = 2 * chunk.indent;
                }
                FormatInner::Space => {
                    f.write_str(" ")?;
                    printer.col += 1;
                }
                FormatInner::Flat(f) => printer.chunks.push(chunk.with_format(f).flat()),
                FormatInner::UnFlat(f) => printer.chunks.push(chunk.with_format(f)),
                FormatInner::Indent(f) => printer.chunks.push(chunk.with_format(f).indent()),
                FormatInner::Concat(f1, f2) => {
                    printer.chunks.push(chunk.with_format(f2));
                    printer.chunks.push(chunk.with_format(f1));
                }
                FormatInner::Choice(f1, f2) => {
                    if chunk.flat || printer.fits(chunk.with_format(f1)) {
                        printer.chunks.push(chunk.with_format(f1));
                    } else {
                        printer.chunks.push(chunk.with_format(f2));
                    }
                }
                FormatInner::RawBlock(text) => {
                    // FIXME: isn't it a bit too hacky ?
                    debug_assert!(text.starts_with("```"));
                    debug_assert!(text.ends_with("```"));
                    let start = text.find(|c: char| c.is_whitespace()).unwrap();
                    let (add_indent, remove_ident) = {
                        let mut leading_space = u64::MAX;
                        for mut line in text.lines().skip(1) {
                            let mut line_leading_space = 0;
                            while line.starts_with(' ') {
                                line = &line[1..];
                                line_leading_space += 1;
                            }
                            if line_leading_space < leading_space {
                                leading_space = line_leading_space;
                            }
                        }
                        (
                            chunk.indent.saturating_sub(leading_space),
                            leading_space.saturating_sub(chunk.indent),
                        )
                    };

                    f.write_str(&text[..start])?;
                    let text = &text[start + 1..];
                    for line in text.lines() {
                        f.write_str("\n")?;
                        if remove_ident > 0 {
                            f.write_str(&line[remove_ident as usize..])?;
                        } else {
                            for _ in 0..add_indent {
                                // FIXME: add custom indentation
                                f.write_str("  ")?;
                            }
                            f.write_str(line)?;
                        }
                    }
                    printer.col = 2 * chunk.indent + 3;
                }
            }
        }
        Ok(())
    }
}
