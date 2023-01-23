# Fathom

[![Actions Status][actions-badge]][actions-url]
[![Matrix][matrix-badge]][matrix-lobby]
[![License][license-badge]][license-url]

[actions-badge]: https://github.com/yeslogic/fathom/workflows/ci/badge.svg
[actions-url]: https://github.com/yeslogic/fathom/actions
[matrix-badge]: https://img.shields.io/badge/chat-%23fathom--lang%3Amatrix.org-brightgreen
[matrix-lobby]: https://matrix.to/#/#fathom-lang:matrix.org
[license-badge]: https://img.shields.io/github/license/yeslogic/fathom
[license-url]: ./LICENSE

A language for specifying data-dependent binary formats.

## Example

```fathom
def pixel = {
    red <- u8,
    green <- u8,
    blue <- u8,
};

def main = {
    width <- u16le,
    height <- u16le,
    pixels <- repeat_len16 (width * height) pixel,
};
```

More examples can be found in the [formats](./formats) directory.

## Code of Conduct

Please note that this project is released with a [Code of Conduct](./CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

Fathom is licensed under the terms of the Apache License (Version 2.0).

See [LICENSE](./LICENSE) or <http://www.apache.org/licenses/LICENSE-2.0> for details.
