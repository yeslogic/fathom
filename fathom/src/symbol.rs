use std::fmt;
use std::ops::Range;
use std::sync::RwLock;

use once_cell::sync::Lazy;

struct StringInterner {
    strings: lasso::ThreadedRodeo,
    tuple_labels: Vec<Symbol>,
    alphabetic_names: Vec<Symbol>,
}

static INTERNER: Lazy<RwLock<StringInterner>> = Lazy::new(|| {
    RwLock::new(StringInterner {
        strings: lasso::ThreadedRodeo::new(),
        tuple_labels: Vec::new(),
        alphabetic_names: Vec::new(),
    })
});

impl StringInterner {
    /// Allocate and intern all alphabetic names up-to and including `max_index`
    /// if they are not already present.
    pub fn reserve_alphabetic_names(&mut self, max_index: usize) {
        fill_vec(&mut self.alphabetic_names, max_index, |index| {
            Symbol(self.strings.get_or_intern(alphabetic_name(index)))
        })
    }

    /// Retrieve an alphabetic name based on a numeric count. This is useful for
    /// producing human-readable names for unnamed binders.
    ///
    /// ## Example
    ///
    /// ```rust
    /// use fathom::symbol::Symbol;
    ///
    /// assert_eq!(Symbol::get_alphabetic_name(0), Symbol::intern("a"));
    /// // ...
    /// assert_eq!(Symbol::get_alphabetic_name(25), Symbol::intern("z"));
    /// assert_eq!(Symbol::get_alphabetic_name(26), Symbol::intern("a1"));
    /// // ...
    /// assert_eq!(Symbol::get_alphabetic_name(51), Symbol::intern("z1"));
    /// assert_eq!(Symbol::get_alphabetic_name(52), Symbol::intern("a2"));
    /// // ...
    /// ```
    pub fn get_alphabetic_name(&mut self, index: usize) -> Symbol {
        self.reserve_alphabetic_names(index);
        self.alphabetic_names[index]
    }

    /// Allocate and intern all tuple labels up-to and including `max_index`
    /// if they are not already present.
    pub fn reserve_tuple_labels(&mut self, max_index: usize) {
        fill_vec(&mut self.tuple_labels, max_index, |index| {
            Symbol(self.strings.get_or_intern(format!("_{index}")))
        })
    }

    /// Get or intern a string in the form `_{index}`.
    ///
    /// ## Example
    ///
    /// ```rust
    /// use fathom::symbol::Symbol;
    ///
    /// assert_eq!(Symbol::get_tuple_label(0), Symbol::intern("_0"));
    /// assert_eq!(Symbol::get_tuple_label(1), Symbol::intern("_1"));
    /// ```
    pub fn get_tuple_label(&mut self, index: usize) -> Symbol {
        self.reserve_tuple_labels(index);
        self.tuple_labels[index]
    }

    /// Get or intern a slice of strings in the form `_{index}` for each index
    /// in `range`.
    pub fn get_tuple_labels(&mut self, range: Range<usize>) -> &[Symbol] {
        self.reserve_tuple_labels(range.end.saturating_sub(1));
        &self.tuple_labels[range]
    }

    /// Returns true if `label` refers to a string in the form `_{index}`.
    pub fn is_tuple_label(&mut self, index: usize, label: Symbol) -> bool {
        label == self.get_tuple_label(index)
    }

    /// Returns true if `labels` is a sequence of tuple labels: `_0`, `_1`, ...
    pub fn is_tuple_labels(&mut self, labels: &[Symbol]) -> bool {
        labels == self.get_tuple_labels(0..labels.len())
    }
}

fn alphabetic_name(index: usize) -> String {
    let base = index / 26;
    let letter = index % 26;
    let letter = (letter as u8 + b'a') as char;
    if base == 0 {
        format!("{letter}")
    } else {
        format!("{letter}{base}")
    }
}

fn fill_vec<T>(vec: &mut Vec<T>, max_index: usize, f: impl FnMut(usize) -> T) {
    vec.extend((vec.len()..=max_index).map(f))
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(lasso::Spur);

impl Symbol {
    pub fn intern(sym: impl AsRef<str>) -> Self {
        Self(INTERNER.write().unwrap().strings.get_or_intern(sym))
    }

    pub fn intern_static(sym: &'static str) -> Self {
        Self(INTERNER.write().unwrap().strings.get_or_intern_static(sym))
    }

    pub fn resolve<'a>(&'a self) -> &'a str {
        let interner = INTERNER.write().unwrap();
        let symbol = interner.strings.resolve(&self.0);

        // SAFETY: The lifetime is a bit of a lie: it is really tied to the lifetime of
        // `INTERNER`. But `INTERNER` is never dropped (since it is static), so it is
        // safe to truncate the lifetime to the shorter lifetime of `'a`.
        // See also: https://github.com/rust-lang/rust/blob/e4dd9edb76a34ecbca539967f9662b8c0cc9c7fb/compiler/rustc_span/src/symbol.rs#L1845
        unsafe { std::mem::transmute::<&str, &'a str>(symbol) }
    }

    pub fn get_alphabetic_name(index: usize) -> Symbol {
        let mut interner = INTERNER.write().unwrap();
        interner.get_alphabetic_name(index)
    }

    pub fn get_tuple_label(index: usize) -> Symbol {
        let mut interner = INTERNER.write().unwrap();
        interner.get_tuple_label(index)
    }

    pub fn get_tuple_labels(range: Range<usize>) -> &'static [Symbol] {
        let mut interner = INTERNER.write().unwrap();

        // SAFETY: `INTERNER` is static, so references into it are `'static`
        unsafe { std::mem::transmute::<&[Symbol], &[Symbol]>(interner.get_tuple_labels(range)) }
    }

    /// Returns true if `label` refers to a string in the form `_{index}`.
    pub fn is_tuple_label(index: usize, label: Symbol) -> bool {
        let mut interner = INTERNER.write().unwrap();
        interner.is_tuple_label(index, label)
    }

    /// Returns true if `labels` is a sequence of tuple labels: `_0`, `_1`, ...
    pub fn is_tuple_labels(labels: &[Symbol]) -> bool {
        let mut interner = INTERNER.write().unwrap();
        interner.is_tuple_labels(labels)
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.resolve()
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.resolve())
    }
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        self.resolve().to_string()
    }
}
