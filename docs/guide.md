# Fathom: Language Guide

Binary formats exist in many forms in computing. Examples of these include:

- Image formats: JPEG, GIF, TIFF, etc.
- Archive formats: ZIP, RAR, GZIP, etc.
- Fonts: OTF, TTF
- &hellip;and many more!

Fathom is a domain-specific language that can be used to describe these various
binary data types in a declarative way that is both human and machine readable.

## A first binary format

```
struct Pixel {
    red : U8,
    green : U8,
    blue : U8,
}

struct Picture {
    width : U16Be,
    height : U16Be,
    data : FormatArray (width * height) Pixel,
}
```

## Differences from Protobufs, ASN.1, etc.

TODO

## Editor support

Not yet implemented!

## Compilers

- Rust Compiler
- Documentation Compiler
