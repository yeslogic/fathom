import prelude (..);

Header = struct {
    /// Fixed header pattern
    magic: u64le where magic => magic == 0x00ffffffffffff00u64,
    /// Manufacturer ID
    mfg_bytes: u16le,
    /// Manufacturer product code
    product_code: u16le,
    /// Serial number
    serial: u32le,
    /// Week of manufacture. Week numbering is not consistent between manufacturers.
    mfg_week: u8,
    /// Year of manufacture, less 1990. (1990–2245). If week=255, it is the model year instead.
    mfg_year_mod: u8,
    /// Year of manufacture, derived from `mfg_year_mod`
    mfg_year: compute u16 from mfg_year_mod as u16 + 1990u16,
    /// EDID version, usually 1 (for 1.3)
    edid_version_major: u8,
    /// EDID revision, usually 3 (for 1.3)
    edid_version_minor: u8,
};

DisplayParams = struct {
    /// Video input parameters bitmap
    input_flags: u8,
    /// Maximum horizontal image size, in centimetres (max 292 cm/115 in at
    /// 16:9 aspect ratio)
    screen_size_h: u8,
    /// Maximum vertical image size, in centimetres. If either byte is 0,
    /// undefined (e.g. projector)
    screen_size_v: u8,
    /// Display gamma data, factory default (range 1.00–3.54), where
    /// `gamma_mod = (gamma×100)-100 = (gamma−1)×100` If 225, gamma is defined
    /// by DI-EXT block.
    gamma_mod: u8,
    /// Display gamma, derived from `gamma_mod`
    gamma: compute f32 from (gamma_mod as f32 + 100.0f32) / 100.0f32,
    /// Supported features bitmap
    features_flags: u8,
};

/// https://en.wikipedia.org/wiki/Extended_Display_Identification_Data
Edid = struct {
    /// Header information
    header: Header,
    /// Basic display parameters
    display_params: DisplayParams,
    // TODO: Chromaticity coordinates.
    // TODO: Established timing bitmap.
    // TODO: Standard timing information.
};
