//! String interner for efficient symbol storage.
//!
//! The [`Interner`] deduplicates strings and assigns each unique string
//! a [`Name`] (an index). This reduces memory usage and enables fast
//! equality comparisons via integer comparison.

use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

use parking_lot::Mutex;

/// An interned string identifier.
///
/// This is a lightweight handle (a `u32` index) that can be used to
/// retrieve the original string from an [`Interner`].
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize)]
pub struct Name(pub u32);

/// A reference-counted string wrapper.
///
/// Backed by `Arc<str>` (a single heap allocation, fat pointer) rather than
/// `Arc<String>` (which adds a second indirection and allocation per string).
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArcStr(Arc<str>);

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
    /// Build from a string slice in a single allocation. `Arc::<str>::from(&str)`
    /// allocates once and copies the bytes; going through an owned `String`
    /// first would allocate twice.
    fn new(value: &str) -> ArcStr {
        ArcStr(Arc::from(value))
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Deref for ArcStr {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl PartialEq<str> for ArcStr {
    fn eq(&self, other: &str) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<&str> for ArcStr {
    fn eq(&self, other: &&str) -> bool {
        &*self.0 == *other
    }
}

impl PartialEq<String> for ArcStr {
    fn eq(&self, other: &String) -> bool {
        &*self.0 == other.as_str()
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
                map: HashMap::default(),
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

        let key = ArcStr::new(name);
        let value = Name(u32::try_from(data.vec.len()).expect("interner exceeded u32::MAX names"));

        data.vec.push(key.clone());
        data.map.insert(key, value);

        value
    }

    /// Get the string for a previously interned [`Name`].
    pub fn str(&self, name: Name) -> ArcStr {
        let data = self.data.lock();
        data.vec[name.0 as usize].clone()
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

        assert_eq!("hello", &*interner.str(Name(0)));
        assert_eq!("world", &*interner.str(Name(1)));

        assert_eq!(Name(2), interner.intern("keyword"));
        assert_eq!(Name(2), interner.intern("keyword"));

        assert_eq!("keyword", &*interner.str(Name(2)));
    }
}
