//@ check-pass
#![feature(const_trait_impl, effects)] //~ WARN the feature `effects` is incomplete

#[const_trait]
trait Trait {
    // FIXME(effects): `~const` bounds in trait associated types (excluding associated type bounds)
    // don't look super useful. Should we forbid them again?
    type Assoc<T: ~const Bound>;
}

impl const Trait for () {
    type Assoc<T: ~const Bound> = T;
}

#[const_trait]
trait Bound {}

fn main() {}
