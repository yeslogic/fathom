# `fathom data`

## Help information

Short help can be printed with `-h`

```console
$ fathom data -h
fathom-data 
Manipulate binary data based on a Fathom format

USAGE:
    fathom data [OPTIONS] <BINARY_FILE>

ARGS:
    <BINARY_FILE>    Path to the binary data to read from

OPTIONS:
        --module <MODULE_FILE>    Path to a module to load when reading
        --format <FORMAT>         Format used when reading the binary data [default: main]
        --allow-errors            Continue even if errors were encountered
    -h, --help                    Print help information

Examples:

  $ fathom data --format '{ magic <- u32be where u32_eq magic "icns" }' AppIcon.icns
  $ fathom data --module formats/opentype.fathom Monaco.ttf
  $ fathom data --module formats/icns.fathom --format header AppIcon.icns

```

Long help can be printed with `--help`

```console
$ fathom data --help
fathom-data 
Manipulate binary data based on a Fathom format

USAGE:
    fathom data [OPTIONS] <BINARY_FILE>

ARGS:
    <BINARY_FILE>
            Path to the binary data to read from

OPTIONS:
        --module <MODULE_FILE>
            Path to a module to load when reading

        --format <FORMAT>
            Format used when reading the binary data
            
            The term provided by `FORMAT` must be of type `Format`.
            
            Required unless `--module` is present.
            
            [default: main]

        --allow-errors
            Continue even if errors were encountered

    -h, --help
            Print help information

Binary data can be read using a term supplied by the `--format` option:

  $ fathom data --format '{ magic <- u32be where u32_eq magic "icns" }' AppIcon.icns

Alternatively data can be read using a module:

  $ fathom data --module formats/opentype.fathom Monaco.ttf
  $ fathom data --module formats/stl-binary.fathom cube.stl

When a module is specified the binary data is read assuming that it contains a
`main` definition, but this can be overridden using the `--format` option:

  $ fathom data --module formats/icns.fathom --format header AppIcon.icns

```

## Usage examples

### Reading data with adhoc formats

Binary data can be read using `--format`

```console
$ fathom data --format "{ magic <- u64le where u64_eq magic 0x00ffffffffffff00 }"
>             formats/data/edid/dell-P2415Q.edid
0 = [ { magic = 72057594037927680 } ]

```

### Reading data with a module

Binary data can be read using a module supplied with `--module`

```console
$ fathom data --module formats/edid.fathom formats/data/edid/dell-P2415Q.edid
0 = [
    {
        header = {
            magic = 72057594037927680,
            manufacturer_id = 44048,
            product_code = 41150,
            serial = 810372684,
            manufacturer_week = 10,
            manufacturer_year_mod = 29,
            edid_version_major = 1,
            edid_version_minor = 4,
        },
        display_parameters = {
            video_input_parameters = 165,
            screen_size_h = 53,
            screen_size_v = 30,
            gamma_mod = 120,
            supported_features = 58,
        },
        chromacity_coordinates = {
            red_green_lsb = 226,
            blue_white_lsb = 69,
            red_x_msb = 168,
            red_y_msb = 85,
            green_x_msb = 77,
            green_y_msb = 163,
            blue_x_msb = 38,
            blue_y_msb = 11,
            white_x_msb = 80,
            white_y_msb = 84,
        },
        established_timing = { mode_bitmap = [ 165, 75, 0 ] },
        standard_timing_information = {},
    },
]

```

### Overriding the default entrypoint

An explicit entrypoint can be supplied with `--format`

```console
$ fathom data --module formats/edid.fathom --format header
>             formats/data/edid/dell-P2415Q.edid
0 = [
    {
        magic = 72057594037927680,
        manufacturer_id = 44048,
        product_code = 41150,
        serial = 810372684,
        manufacturer_week = 10,
        manufacturer_year_mod = 29,
        edid_version_major = 1,
        edid_version_minor = 4,
    },
]

```

Offsets can be used to more look more deeply into binary files

```console
$ fathom data --module formats/opentype.fathom
>             --format "{ start <- stream_pos, link <- link (pos_add_u32 start 4396) (cmap_subtable 3) }"
>             formats/data/opentype/aots/cmap0_font1.otf
0 = [ { start = 0, link = 4396 } ]
4396 = [
    {
        table_start = 4396,
        format = 0,
        data = {
            length = 262,
            language = 0,
            glyph_id_array = [
                0,
                0,
                0,
                0,
...

```

## Error cases

### Argument conflicts

Arguments must be provided to `fathom data`

```console
$ fathom data
? failed
error: The following required arguments were not provided:
    <BINARY_FILE>

USAGE:
    fathom data [OPTIONS] <BINARY_FILE>

For more information try --help

```

The `--format` option must be present when `--module` is not supplied

```console
$ fathom data formats/data/edid/dell-P2415Q.edid
? failed
error: The following required arguments were not provided:
    --format <FORMAT>

USAGE:
    fathom data --format <FORMAT> <BINARY_FILE>

For more information try --help

```

### Missing files

The path to the binary file must exist

```console
$ fathom data --format "{ magic <- u64le where u64_eq magic 0x00ffffffffffff00 }" does/not/exist
? failed
error: couldn't read `does/not/exist`: No such file or directory (os error 2)


```

The path supplied with `--module` must exist

```console
$ fathom data --module woopsie.fathom formats/data/edid/dell-P2415Q.edid
? failed
error: couldn't read `woopsie.fathom`: No such file or directory (os error 2)


```

### Malformed binary data

Unexpected data in the binary file will result in an error

```console
$ fathom data --module formats/edid.fathom formats/data/edid/invalid/wrong-magic.edid
? failed
error: conditional format failed
   ┌─ formats/edid.fathom:18:26
   │
18 │     magic <- u64le where u64_eq magic 0x00ffffffffffff00,
   │                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   │
   = The predicate on a conditional format did not succeed.


```

### Type errors

Fathom ensures that the term supplied with `--format` is of type `Format`

```console
$ fathom data --format "{ x : U64 }" formats/data/edid/dell-P2415Q.edid
? failed
error: mismatched types
  ┌─ <FORMAT>:1:1
  │
1 │ { x : U64 }
  │ ^^^^^^^^^^^ type mismatch, expected `Type`, found `Format`
  │
  = expected `Type`
       found `Format`


```

The term supplied `--format` is also checked when `--module` is present

```console
$ fathom data --module formats/opentype.fathom --format "offset16"
>             formats/data/opentype/aots/cmap0_font1.otf
? failed
error: mismatched types
  ┌─ <FORMAT>:1:1
  │
1 │ offset16
  │ ^^^^^^^^ type mismatch, expected `Pos -> Format -> Format`, found `Format`
  │
  = expected `Pos -> Format -> Format`
       found `Format`


```
