//! endian: le

/// https://en.wikipedia.org/wiki/Extended_Display_Identification_Data
Edid = {
    /// Header information
    header: Header,
    /// Basic display parameters
    display_params: DisplayParams,
    // TODO: Chromaticity coordinates.
    // TODO: Established timing bitmap.
    // TODO: Standard timing information.
};

Header = {
    /// Fixed header pattern
    magic: [u8; 8] = [0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00],
    /// Manufacturer ID
    mfg_bytes: u16,
    /// Manufacturer product code
    product_code: u16,
    /// Serial number
    serial: u32,
    /// Week of manufacture. Week numbering is not consistent between manufacturers.
    mfg_week: u8,
    /// Year of manufacture, less 1990. (1990–2245). If week=255, it is the model year instead.
    mfg_year_mod: u8,
    /// EDID version, usually 1 (for 1.3)
    edid_version_major: u8,
    /// EDID revision, usually 3 (for 1.3)
    edid_version_minor: u8,
};

DisplayParams = {
    /// Video input parameters bitmap
    input_flags: u8,
    /// Maximum horizontal image size, in centimetres (max 292 cm/115 in at 16:9 aspect ratio)
    screen_size_h: u8,
    /// Maximum vertical image size, in centimetres. If either byte is 0, undefined (e.g. projector)
    screen_size_v: u8,
    /// Display gamma, datavalue = (gamma*100)-100 (range 1.00–3.54)
    gamma_mod: u8,
    /// Supported features bitmap
    features_flags: u8,
};
