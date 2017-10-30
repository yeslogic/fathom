//! IEEE 754 Floating Point Numbers
//!
//! https://en.wikipedia.org/wiki/IEEE_754

// FIXME: endianness?
// https://en.wikipedia.org/wiki/Endianness#Floating_point

/// Half precision
///
/// https://en.wikipedia.org/wiki/Half-precision_floating-point_format
binary16 = struct {
    sign: bit,
    exponent: [bit; 5],
    significand: [bit; 10],
};

/// Single precision
///
/// https://en.wikipedia.org/wiki/Single-precision_floating-point_format
binary32 = struct {
    sign: bit,
    exponent: [bit; 8],
    significand: [bit; 23],
};

/// Double precision
///
/// https://en.wikipedia.org/wiki/Double-precision_floating-point_format
binary64 = struct {
    sign: bit,
    exponent: [bit; 11],
    significand: [bit; 52],
};

/// Quadruple precision
///
/// https://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format
binary128 = struct {
    sign: bit,
    exponent: [bit; 15],
    significand: [bit; 112],
};

/// Octuple precision
///
/// https://en.wikipedia.org/wiki/Octuple-precision_floating-point_format
binary256 = struct {
    sign: bit,
    exponent: [bit; 19],
    significand: [bit; 236],
};

/// https://en.wikipedia.org/wiki/Decimal32_floating-point_format
decimal32 = struct {
    sign: bit,
    combination: [bit; 5],
    exponent_cont: [bit; 6],
    coefficient_cont: [bit; 20],
};

/// https://en.wikipedia.org/wiki/Decimal64_floating-point_format
decimal64 = struct {
    sign: bit,
    combination: [bit; 5],
    exponent_cont: [bit; 8],
    coefficient_cont: [bit; 50],
};

/// https://en.wikipedia.org/wiki/Decimal128_floating-point_format
decimal128 = struct {
    sign: bit,
    combination: [bit; 17],
    exponent_cont: [bit; 110],
};
