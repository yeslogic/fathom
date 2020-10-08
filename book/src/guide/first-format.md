# A simple binary format

Binary format specifications are often described using natural language.
For example, consider the following specification of a simple bitmap format:

> ## A simple bitmap image format
>
> All numeric data is in big-endian.
>
> **Image Table**
>
> Top level image data.
>
> | Field | Type | Description |
> | ----- | ---- | ----------- |
> | width | u16  | The width of the image, in pixels |
> | height | u16  | The width of the image, in pixels |
> | data | Pixel[] | The uncompressed pixel data (width * height pixels) |
>
> **Pixel**
>
> RGB pixel in the image
>
> | Field | Type | Description |
> | ----- | ---- | ----------- |
> | red | u8 | Red channel |
> | green | u8 | Green channel |
> | blue | u8 | Blue channel |
>

The above natural language specification can be described using the following description in Fathom:

```fathom
//! A simple bitmap image format

/// RGB pixel in the image
RgbPixel : Format = {
    /// Red channel
    red : U8,
    /// Green channel
    green : U8,
    /// Blue channel
    blue : U8,
};

/// Top level image data
Image : Format = {
    /// The width of the image, in pixels
    width : U16Be,
    /// The height of the image, in pixels
    height : U16Be,
    /// The uncompressed pixel data
    data : FormatArray (width * height) RgbPixel,
};
```

We use the built-in format descriptions, `U8`, `U16Be`, and `FormatArray` to create the composite format descriptions, `RgbPixel` and `Image`.

Note in particular how the `data` field of `Format` depends on `width` and `height`.
These data-dependencies are very important when parsing binary formats!

From this description we can use the Fathom tooling to generate HTML documentation for out format,
and compile to an efficient Rust parser.

> **TODO**: demonstrate integer overflow by using `U64Be` for the image width and height
