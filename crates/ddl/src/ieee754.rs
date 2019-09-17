//! Operations related to IEEE 754 floating point numbers.

pub trait Ieee754: Copy {
    type Bits: PartialEq;

    fn is_nan(self) -> bool;
    fn to_bits(self) -> Self::Bits;
}

impl Ieee754 for f32 {
    type Bits = u32;

    fn is_nan(self) -> bool {
        self.is_nan()
    }

    fn to_bits(self) -> u32 {
        self.to_bits()
    }
}

impl Ieee754 for f64 {
    type Bits = u64;

    fn is_nan(self) -> bool {
        self.is_nan()
    }

    fn to_bits(self) -> u64 {
        self.to_bits()
    }
}

/// Use bitwise equality, combined with a NaN check to provide a
/// logically consistent equality comparison of floating point
/// numbers. This means that the following weirdness (from an
/// IEEE-754 perspective) happens at the type level:
///
/// - 0.0 != -0.0
/// - NaN == NaN
/// - NaN == -NaN
///
/// # References
///
/// - https://github.com/idris-lang/Idris-dev/issues/2609
/// - https://github.com/dhall-lang/dhall-lang/issues/425
/// - https://github.com/agda/agda/issues/2169
/// - https://agda.readthedocs.io/en/v2.5.4.2/language/built-ins.html#floats
pub fn logical_eq<T: Ieee754>(value0: T, value1: T) -> bool {
    value0.to_bits() == value1.to_bits() || value0.is_nan() && value1.is_nan()
}

#[cfg(test)]
mod tests {
    use proptest::num::{f32, f64};
    use proptest::{prop_assert, proptest};

    use super::*;

    proptest! {
        #[test]
        fn f32_reflexive(value in f32::ANY) {
            prop_assert!(logical_eq(value, value));
        }

        #[test]
        fn f32_neg_zero_not_equiv(value in f32::ZERO) {
            prop_assert!(!logical_eq(-value, value));
            prop_assert!(!logical_eq(value, -value));
        }

        #[test]
        fn f32_nan_equiv(
            value0 in f32::QUIET_NAN | f32::SIGNALING_NAN,
            value1 in f32::QUIET_NAN | f32::SIGNALING_NAN,
        ) {
            prop_assert!(logical_eq(value0, value1));
        }

        #[test]
        fn f64_reflexive(value in f64::ANY) {
            prop_assert!(logical_eq(value, value));
        }

        #[test]
        fn f64_neg_zero_not_equiv(value in f64::ZERO) {
            prop_assert!(!logical_eq(-value, value));
            prop_assert!(!logical_eq(value, -value));
        }

        #[test]
        fn f64_nan_equiv(
            value0 in f64::QUIET_NAN | f64::SIGNALING_NAN,
            value1 in f64::QUIET_NAN | f64::SIGNALING_NAN,
        ) {
            prop_assert!(logical_eq(value0, value1));
        }
    }
}
