//! IEEE 754 Floating Point Numbers
//!
//! https://en.wikipedia.org/wiki/IEEE_754

// FIXME: endianness?
// https://en.wikipedia.org/wiki/Endianness#Floating_point

/// Half precision
///
/// https://en.wikipedia.org/wiki/Half-precision_floating-point_format
binary16 = {
    sign: b1,
    exponent: [b1; 5],
    significand: [b1; 10],
};

/// Single precision
///
/// https://en.wikipedia.org/wiki/Single-precision_floating-point_format
binary32 = {
    sign: b1,
    exponent: [b1; 8],
    significand: [b1; 23],
};

/// Double precision
///
/// https://en.wikipedia.org/wiki/Double-precision_floating-point_format
binary64 = {
    sign: b1,
    exponent: [b1; 11],
    significand: [b1; 52],
};

/// Quadruple precision
///
/// https://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format
binary128 = {
    sign: b1,
    exponent: [b1; 15],
    significand: [b1; 112],
};

/// Octuple precision
///
/// https://en.wikipedia.org/wiki/Octuple-precision_floating-point_format
binary256 = {
    sign: b1,
    exponent: [b1; 19],
    significand: [b1; 236],
};

/// https://en.wikipedia.org/wiki/Decimal32_floating-point_format
decimal32 = {
    sign: b1,
    combination: [b1; 5],
    exponent_cont: [b1; 6],
    coefficient_cont: [b1; 20],
};

/// https://en.wikipedia.org/wiki/Decimal64_floating-point_format
decimal64 = {
    sign: b1,
    combination: [b1; 5],
    exponent_cont: [b1; 8],
    coefficient_cont: [b1; 50],
};

/// https://en.wikipedia.org/wiki/Decimal128_floating-point_format
decimal128 = {
    sign: b1,
    combination: [b1; 17],
    exponent_cont: [b1; 110],
};
