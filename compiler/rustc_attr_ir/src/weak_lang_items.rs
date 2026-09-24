//! Validity checking for weak lang items

use rustc_span::{Symbol, sym};

use crate::LangItem;

macro_rules! weak_lang_items {
    ($($item:ident, $sym:ident;)*) => {
        pub static WEAK_LANG_ITEMS: &[LangItem] = &[$(LangItem::$item,)*];

        impl LangItem {
            pub fn is_weak(self) -> bool {
                matches!(self, $(LangItem::$item)|*)
            }

            pub fn link_name(self) -> Option<Symbol> {
                match self {
                    $( LangItem::$item => Some(sym::$sym),)*
                    _ => None,
                }
            }
        }
    }
}

weak_lang_items! {
    PanicImpl,          rust_begin_unwind;
    // ROG: a distinct name keeps the runtime's personality from colliding
    // with the one in any other Rust staticlib linked into the program.
    EhPersonality,      rog_rust_eh_personality;
}
