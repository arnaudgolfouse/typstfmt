use ecow::EcoString;
use std::{
    ops::{BitAnd, BitAndAssign, BitOr},
    rc::Rc,
};

/// The main type of Wadler's algorithm. This contains all the formatting choices we made.
#[derive(Clone, Debug)]
pub(crate) struct Format(pub(super) FormatInner);

// Note that the `Rc` (rather than `Box`) is vital here: without it, the number of
// `Format` objects could grow exponentially big.
#[derive(Clone, Debug)]
pub(super) enum FormatInner {
    /// See [`Format::newline`]
    Newline,
    /// See [`Format::space`]
    Space,
    /// See [`Format::text`]
    Text(EcoString, u64),
    /// See [`Format::indent`]
    Indent(Rc<Format>),
    /// See [`Format::flat`]
    Flat(Rc<Format>),
    /// See [`Format::unflat`]
    UnFlat(Rc<Format>),
    /// Concatenation of two formats, will be printed side-by-side.
    Concat(Rc<Format>, Rc<Format>),
    /// Choice between two formats, only one will be printed.
    Choice(Rc<Format>, Rc<Format>),

    /// See [`Format::raw_block`]
    RawBlock(EcoString),
}

impl Format {
    /// A piece of text to print as-is. Contains the length in characters of the text,
    /// in order to compute when to break a line.
    pub(crate) fn text(string: EcoString) -> Self {
        let width = unicode_width::UnicodeWidthStr::width(&string as &str) as u64;
        Self(FormatInner::Text(string, width))
    }

    /// A newline. It **will** appear in the final document.
    ///
    /// # Tips
    ///
    /// Take care that newline elements are always inserted on the right-hand of a choice.
    // FIXME: there need to be a way to say "this format contains newlines unconditionnaly, and thus its parent(s) should not be flat."
    pub(crate) fn newline() -> Self {
        Self(FormatInner::Newline)
    }

    /// Returns `true` if the format recursively contains a [`newline`](Self::newline).
    ///
    /// Text containing the newline character `\n` is **also** considered.
    ///
    /// When encountering a choice, the format contains a newline if *both* choices contain a newline.
    pub(crate) fn has_newline(&self) -> bool {
        match &self.0 {
            FormatInner::Newline => true,
            FormatInner::Space => false,
            FormatInner::Text(s, _) | FormatInner::RawBlock(s) => s.contains('\n'),
            FormatInner::Indent(f) | FormatInner::Flat(f) | FormatInner::UnFlat(f) => {
                f.has_newline()
            }
            FormatInner::Concat(f1, f2) => f1.has_newline() || f2.has_newline(),
            FormatInner::Choice(f1, f2) => f1.has_newline() && f2.has_newline(),
        }
    }

    /// A single space.
    pub(crate) fn space() -> Self {
        Self(FormatInner::Space)
    }

    /// Returns `true` if `self` is a single space, created with [`Self::space`].
    pub(crate) fn is_space(&self) -> bool {
        matches!(self.0, FormatInner::Space)
    }

    /// The format used for raw blocks, so that we correctly handle indentation.
    pub(crate) fn raw_block(string: EcoString) -> Self {
        Self(FormatInner::RawBlock(string))
    }

    /// A format indented once.
    ///
    /// The actual number of spaces in an identation is determined by the configuration.
    pub(crate) fn indent(self) -> Self {
        Self(FormatInner::Indent(Rc::new(self)))
    }

    /// 'Flatten' the given format. That is, always make the left-most choice.
    pub(crate) fn flat(self) -> Self {
        Self(FormatInner::Flat(Rc::new(self)))
    }

    /// 'Unflatten' the given format.
    ///
    /// This forces this format's parent to use their right-most option.
    pub(crate) fn unflat(self) -> Self {
        Self(FormatInner::UnFlat(Rc::new(self)))
    }
}

impl BitAnd for Format {
    type Output = Self;

    /// Display both formats. The first character of the right
    /// format immediately follows the last character of the
    /// left format.
    fn bitand(self, other: Self) -> Self {
        Self(FormatInner::Concat(Rc::new(self), Rc::new(other)))
    }
}

impl BitAndAssign for Format {
    fn bitand_assign(&mut self, other: Self) {
        let this = std::mem::replace(self, Self::newline());
        *self = Self(FormatInner::Concat(Rc::new(this), Rc::new(other)))
    }
}

impl BitOr for Format {
    type Output = Self;

    /// If inside a `flat`, _or_ the first line of the left format
    /// fits within the required width, then display the left
    /// format. Otherwise, display the right format.
    fn bitor(self, other: Self) -> Self {
        Self(FormatInner::Choice(Rc::new(self), Rc::new(other)))
    }
}

impl FromIterator<Format> for Option<Format> {
    fn from_iter<T: IntoIterator<Item = Format>>(iter: T) -> Self {
        let mut result: Option<Format> = None;
        for f in iter {
            result = Some(match result {
                Some(r) => r & f,
                None => f,
            });
        }
        result
    }
}
