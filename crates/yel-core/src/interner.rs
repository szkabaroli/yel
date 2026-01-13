//! String interner for efficient symbol storage.
//!
//! The [`Interner`] deduplicates strings and assigns each unique string
//! a [`Name`] (an index). This reduces memory usage and enables fast
//! equality comparisons via integer comparison.

use serde::Serialize;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

use parking_lot::Mutex;

/// An interned string identifier.
///
/// This is a lightweight handle (just a `usize`) that can be used to
/// retrieve the original string from an [`Interner`].
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Serialize)]
pub struct Name(pub usize);

/// A reference-counted string wrapper.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArcStr(Arc<String>);

impl fmt::Display for ArcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl fmt::Debug for ArcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl ArcStr {
    fn new(value: String) -> ArcStr {
        ArcStr(Arc::new(value))
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        &self.0[..]
    }
}

impl Deref for ArcStr {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

/// Internal interner state.
#[derive(Debug)]
struct Internal {
    map: HashMap<ArcStr, Name>,
    vec: Vec<ArcStr>,
}

/// A thread-safe string interner.
///
/// Interns strings to produce [`Name`] handles that can be compared
/// cheaply and used to retrieve the original string.
#[derive(Debug)]
pub struct Interner {
    data: Mutex<Internal>,
}

impl Interner {
    /// Create a new empty interner.
    pub fn new() -> Interner {
        Interner {
            data: Mutex::new(Internal {
                map: HashMap::new(),
                vec: Vec::new(),
            }),
        }
    }

    /// Intern a string, returning its unique [`Name`].
    ///
    /// If the string was already interned, returns the existing name.
    pub fn intern(&self, name: &str) -> Name {
        let mut data = self.data.lock();

        if let Some(&val) = data.map.get(name) {
            return val;
        }

        let key = ArcStr::new(String::from(name));
        let value = Name(data.vec.len());

        data.vec.push(key.clone());
        data.map.insert(key, value);

        value
    }

    /// Get the string for a previously interned [`Name`].
    pub fn str(&self, name: Name) -> ArcStr {
        let data = self.data.lock();
        data.vec[name.0].clone()
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner() {
        let interner = Interner::new();

        assert_eq!(Name(0), interner.intern("hello"));
        assert_eq!(Name(0), interner.intern("hello"));

        assert_eq!(Name(1), interner.intern("world"));
        assert_eq!(Name(1), interner.intern("world"));

        assert_eq!("hello", *interner.str(Name(0)));
        assert_eq!("world", *interner.str(Name(1)));

        assert_eq!(Name(2), interner.intern("keyword"));
        assert_eq!(Name(2), interner.intern("keyword"));

        assert_eq!("keyword", *interner.str(Name(2)));
    }
}
