# Fathom

[![Actions Status][actions-badge]][actions-url]
[![Matrix][matrix-badge]][matrix-lobby]
[![License][license-badge]][license-url]
[![GitHub forks][forks-badge]][github-url]

[actions-badge]: https://github.com/yeslogic/fathom/workflows/ci/badge.svg
[actions-url]: https://github.com/yeslogic/fathom/actions
[matrix-badge]: https://img.shields.io/matrix/fathom-lang:matrix.org?label=%23fathom-lang%3Amatrix.org
[matrix-lobby]: https://app.element.io/#/room/#fathom-lang:matrix.org
[license-badge]: https://img.shields.io/github/license/yeslogic/fathom
[license-url]: https://github.com/yeslogic/fathom/blob/master/LICENSE
[forks-badge]: https://img.shields.io/github/forks/yeslogic/fathom?style=social
[github-url]: https://github.com/yeslogic/fathom

> **NOTE**: This documentation is still a work in progress!

Fathom is a domain-specific language that can be used to describe existing binary formats declaratively,
in a way that is both human and machine readable.

Binary formats exist in many forms in computing. Examples of these include:

- Image formats: JPEG, GIF, TIFF, etc.
- Archive formats: ZIP, RAR, GZIP, etc.
- Fonts: OTF, TTF
- &hellip;and many more!

Implementing binary format parsers by hand in general purpose programming languages is tedious and error-prone,
especially when the implementations are derived from natural language specifications that leave room for ambiguity.
Fathom aims to streamline this process for new and existing binary formats.

## Fathom at a glance

Here is a very simple example of a binary format specified using Fathom:

```fathom
RgbPixel : Format = {
    red : U8,
    green : U8,
    blue : U8,
};

Image : Format = {
    width : U16Be,
    height : U16Be,
    data : FormatArray (width * height) RgbPixel,
};
```

For more information, read on in the [Fathom Language Guide](./guide.md)!
