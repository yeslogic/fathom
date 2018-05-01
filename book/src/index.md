# DDL (placeholder name)

A declarative data definition language for formally specifying binary
data formats.

## Example

```plain
Bitmap = struct {
    width: u16le,
    height: u16le,
    data: [u8; width * height],
};
```
