//! # Background
//! Most work on pretty printing in the literature focuses on variations the
//! [Wadler-style pretty printer], which was itself a successor to the
//! [Hughes-Peyton Jones style pretty printer].
//!
//! Though expressive, these pretty printers are inefficient for two reasons:
//! - They rely on the lazy evaluation semantics of the Haskell language, and if
//!   naively translated to a strict language, have exponential runtime
//!   complexity!
//! - They construct an intermediate `Doc` abstract syntax tree and then
//!   immediately traverse it to print the document. This is both slower and
//!   requires heap allocation
//!
//! There are some older, imperative algorithms that avoid these problems.
//!
//! The [Oppen algorithm] is used by the [rustc AST pretty printer],
//! [`prettyplease`] (a third party pretty printer for the Rust AST),
//! and [`swift-format`]. Its runtime complexity is `O(n)` (where n is the
//! size of the document), and space complexity is `O(m)` (where m is the
//! maximum line length).
//! The seemingly obscure [Pugh & Sinofsky algorithm] is equivalent to the Oppen
//! algorithm, but significantly simpler.
//!
//! [Wadler-style pretty printer]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
//! [Hughes-Peyton Jones style pretty printer]: http://belle.sourceforge.net/doc/hughes95design.pdf
//! [Oppen algorithm]: https://dl.acm.org/doi/pdf/10.1145/357114.357115
//! [Pugh & Sinofsky algorithm]: https://ecommons.cornell.edu/handle/1813/6648
//! [rustc AST pretty printer]: https://github.com/rust-lang/rust/blob/master/compiler/rustc_ast_pretty/src/pp.rs
//! [`prettyplease`]: https://github.com/dtolnay/prettyplease
//! [`swift-format`]: https://github.com/apple/swift-format/blob/main/Documentation/PrettyPrinter.md
// TODO: what algorithm does rustfmt use?

use std::collections::VecDeque;

// TODO: Wadler-style combinators on top of imperative backend
// TODO: output to `io::Write`?

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Char(char),
    // TODO: use `&str`?
    String(String),
    Indent {
        width: usize,
    },
    Outdent {
        width: usize,
    },
    /// Unconditional line break
    Hardline,
    /// Connected conditional line break
    Break {
        mode: BreakMode,
        group_level: isize,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum BreakMode {
    Consistent,
    Inconsistent,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Break {
    /// number of tokens to the left of the break in the entire
    /// string
    tokens_pushed: usize,
    /// number of open grouping tokens minus number of closing group tokens
    /// preceding the break
    group_level: isize,
    mode: BreakMode,
}

pub struct Printer {
    out: String,
    group_level: isize,
    break_level: isize,
    tokens: VecDeque<Token>,
    breaks: VecDeque<Break>,
    total_tokens_pushed: usize,
    total_tokens_flushed: usize,

    pending_chars: usize,
    pending_indent: bool,
    left_margin: usize,
    target_width: usize,
}

/// Public API
impl Printer {
    pub fn new(target_width: usize) -> Self {
        Self {
            out: String::new(),
            group_level: 0,
            break_level: -1,
            tokens: VecDeque::new(),
            breaks: VecDeque::new(),
            total_tokens_pushed: 0,
            total_tokens_flushed: 0,

            pending_chars: 0,
            pending_indent: true,
            left_margin: 0,
            target_width,
        }
    }

    pub fn flush(&mut self) -> String {
        self.print_buffer(self.tokens.len());
        let out = std::mem::take(&mut self.out);

        self.group_level = 0;
        self.break_level = -1;
        self.tokens.clear();
        self.breaks.clear();
        self.total_tokens_pushed = 0;
        self.total_tokens_flushed = 0;
        self.pending_chars = 0;
        self.pending_indent = true;
        self.left_margin = 0;

        out
    }

    pub fn char(&mut self, c: char) {
        if self.needs_split(1) {
            self.try_split()
        }

        self.push_token(Token::Char(c));
        self.pending_chars += 1;
    }

    pub fn string(&mut self, s: &str) {
        let num_chars = s.chars().count();
        if self.needs_split(num_chars) {
            self.try_split()
        }

        self.push_token(Token::String(s.into()));
        self.pending_chars += num_chars;
    }

    pub fn open_group(&mut self) {
        self.group_level += 1
    }

    pub fn close_group(&mut self) {
        self.group_level -= 1;
        self.break_level = std::cmp::min(self.break_level, self.group_level);
    }

    pub fn indent(&mut self, width: usize) {
        self.push_token(Token::Indent { width });
    }

    pub fn outdent(&mut self, width: usize) {
        self.push_token(Token::Outdent { width });
    }

    pub fn hardline(&mut self) {
        self.breaks.clear();
        self.break_level = self.group_level;
        self.push_token(Token::Hardline);
        self.print_buffer(self.tokens.len());
    }

    pub fn softline(&mut self) {
        while let Some(r#break) = self.breaks.front() {
            if r#break.group_level > self.group_level
                || r#break.mode == BreakMode::Inconsistent
                    && r#break.group_level == self.group_level
            {
                self.breaks.pop_front();
            } else {
                break;
            }
        }

        self.breaks.push_front(Break {
            tokens_pushed: self.total_tokens_pushed,
            group_level: self.group_level,
            mode: BreakMode::Inconsistent,
        })
    }

    pub fn consistent_line(&mut self) {
        if self.break_level < self.group_level {
            while let Some(r#break) = self.breaks.front() {
                if r#break.group_level >= self.group_level {
                    self.breaks.pop_front();
                } else {
                    break;
                }
            }
            self.push_token(Token::Break {
                group_level: self.group_level,
                mode: BreakMode::Consistent,
            });
            self.breaks.push_front(Break {
                tokens_pushed: self.total_tokens_pushed,
                group_level: self.group_level,
                mode: BreakMode::Consistent,
            });
        } else {
            // take an immediate line break
            self.hardline();
        }
    }
}

/// Private implementation
impl Printer {
    fn push_token(&mut self, token: Token) {
        self.tokens.push_back(token);
        self.total_tokens_pushed += 1;
    }

    fn needs_split(&self, num_chars: usize) -> bool {
        self.left_margin + self.pending_chars + num_chars > self.target_width
    }

    fn try_split(&mut self) {
        // need to split line
        if let Some(r#break) = self.breaks.pop_back() {
            // split line at a break
            self.break_level = r#break.group_level;
            self.print_buffer(r#break.tokens_pushed - self.total_tokens_flushed);
            if r#break.mode == BreakMode::Inconsistent {
                self.write_newline()
            }
            self.break_level = std::cmp::min(self.break_level, self.group_level);
        } else {
            // there are no breaks to take :(
        }
    }

    /// Write `c` to the output
    fn write_char(&mut self, c: char) {
        if self.pending_indent {
            self.write_indentation();
            self.pending_indent = false;
        }
        self.pending_chars -= 1;
        self.out.push(c);
    }

    /// Write `s` to the output
    fn write_str(&mut self, s: &str) {
        if self.pending_indent {
            self.write_indentation();
            self.pending_indent = false;
        }
        self.pending_chars -= s.chars().count();
        self.out.push_str(s);
    }

    /// Write `\n` to the output
    fn write_newline(&mut self) {
        self.pending_indent = true;
        self.out.push('\n');
    }

    /// Write `self.left_margin` spaces to the output
    fn write_indentation(&mut self) {
        (self.out).extend(std::iter::repeat(' ').take(self.left_margin))
    }

    fn print_buffer(&mut self, k: usize) {
        let mut buffer = std::mem::take(&mut self.tokens);
        for token in buffer.drain(..k) {
            self.total_tokens_flushed += 1;
            match token {
                Token::String(s) => self.write_str(&s),
                Token::Char(c) => self.write_char(c),
                Token::Indent { width } => self.left_margin += width,
                Token::Outdent { width } => self.left_margin -= width,
                Token::Hardline => self.write_newline(),
                Token::Break { mode, group_level } => {
                    if mode == BreakMode::Consistent && group_level <= self.break_level {
                        self.write_newline();
                    } else {
                        // TODO: make space optional
                        self.out.push(' ');
                    }
                }
            }
        }
        self.tokens = buffer;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        fn fmt(_p: &mut Printer) {}

        let mut printer = Printer::new(20);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "");
    }

    #[test]
    fn char() {
        fn fmt(p: &mut Printer) {
            p.char('a');
        }

        let mut printer = Printer::new(20);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "a");
    }

    #[test]
    fn string() {
        fn fmt(p: &mut Printer) {
            p.string("abc");
        }

        let mut printer = Printer::new(20);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "abc");
    }

    #[test]
    fn hardline() {
        fn fmt(p: &mut Printer) {
            p.string("abc");
            p.hardline();
            p.string("def");
        }

        let mut printer = Printer::new(20);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "abc\ndef");
    }

    #[test]
    fn softline() {
        fn fmt(p: &mut Printer) {
            p.string("abc");
            p.softline();
            p.string("def");
        }

        let mut printer = Printer::new(20);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "abcdef");

        let mut printer = Printer::new(10);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "abcdef");

        let mut printer = Printer::new(5);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "abc\ndef");
    }

    #[test]
    fn example1() {
        fn fmt(p: &mut Printer) {
            p.open_group();
            p.string("if ");
            p.indent(2);
            p.open_group();
            p.string("x ");
            p.softline();
            p.string("< 0");
            p.close_group();
            p.outdent(2);
            p.string(" then ");
            p.softline();
            p.indent(2);
            p.open_group();
            p.string("x :=");
            p.softline();
            p.string(" -x");
            p.close_group();
            p.outdent(2);
            p.close_group();
        }

        let mut printer = Printer::new(25);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "if x < 0 then x := -x");

        let mut printer = Printer::new(15);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "if x < 0 then \n  x := -x");

        let mut printer = Printer::new(10);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "if x \n  < 0 then \n  x := -x");

        let mut printer = Printer::new(5);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "if x \n  < 0 then \n  x :=\n   -x");
    }

    #[test]
    fn connected_lines() {
        fn fmt(p: &mut Printer) {
            p.open_group();
            p.string("f(");
            p.indent(2);
            p.consistent_line();
            p.string("arg1,");
            p.consistent_line();
            p.string("arg2,");
            p.consistent_line();
            p.string("arg3,");
            p.consistent_line();
            p.string(")");
            p.outdent(2);
            p.close_group();
        }

        let mut printer = Printer::new(30);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "f( arg1, arg2, arg3, )");

        let mut printer = Printer::new(10);
        fmt(&mut printer);
        assert_eq!(printer.flush(), "f(\n  arg1,\n  arg2,\n  arg3,\n  )");
    }
}
