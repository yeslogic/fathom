module opentype;


struct Unknown {};

// TODO: Missing primitives:

struct VArray (A : Type) {};

struct U24Be {
    value : Array 3 U8,
};


// -----------------------------------------------------------------------------
// Data types
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#data-types>
// -----------------------------------------------------------------------------

/// 32-bit signed fixed-point number (16.16)
struct Fixed {
    value : U32Be,
};

/// `S16Be` that describes a quantity in font design units.
struct FWord {
    value : S16Be,
};

/// `U16Be` that describes a quantity in font design units.
struct UfWord {
    value : U16Be,
};

/// Date represented in number of seconds since 12:00 midnight, January 1, 1904.
/// The value is represented as a signed 64-bit integer.
struct LongDateTime {
    value : S64Be,
};

/// Array of four uint8s (length = 32 bits) used to identify a script, language
/// system, feature, or baseline
struct Tag {
    value: Array 4 U8,
};



// =============================================================================
//
// OpenType Top Level Organization
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
//
// =============================================================================

// TODO:
// union OpenType (file_start : Pos) {
//     OffsetTable file_start,
//     TtcHeader file_start,
// };


// -----------------------------------------------------------------------------
// Offset Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
// -----------------------------------------------------------------------------

struct OffsetTableRecord (file_start : Pos) {
    /// Table identifier
    tag : Tag,
    /// CheckSum for this table
    ///
    /// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#calculating-checksums>
    checksum : U32Be,
    /// Offset from beginning of TrueType font file
    offset : Offset32Be file_start Unknown,
    /// Length of this table
    length : U32Be,
};

struct OffsetTable (file_start : Pos) {
    /// 0x00010000 or 0x4F54544F ('OTTO')
    ///
    /// Apple allows 'true' and 'typ1' ?
    sfnt_version : U32Be,
    /// Number of tables
    num_tables : U16Be,
    /// (Maximum power of 2 <= numTables) x 16
    search_range : U16Be,
    /// Log2(maximum power of 2 <= numTables)
    entry_selector : U16Be,
    /// NumTables x 16-searchRange
    range_shift : U16Be,
    /// FIXME: sorted in ascending order by tag
    table_records : Array num_tables (OffsetTableRecord file_start),
};


// -----------------------------------------------------------------------------
// Font Collections
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
// -----------------------------------------------------------------------------

// TODO:
// /// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#ttc-header>
// union TtcHeader (file_start : Pos) {
//     TtcHeader1 file_start,
//     TtcHeader2 file_start,
// };

struct TtcHeader1 (file_start : Pos) {
    /// Font Collection ID
    ttc_tag : Tag,
    /// Major version of the TTC Header, = 1
    major_version : U16Be,
    /// Minor version of the TTC Header, = 0
    minor_version : U16Be,
    /// Number of fonts in TTC
    num_fonts : U32Be,
    /// Array of offsets to the OffsetTable for each font from the beginning of the file
    offset_tables : Array num_fonts (Offset32Be file_start (OffsetTable file_start)),
};

struct TtcHeader2 (file_start : Pos) {
    /// Font Collection ID
    ttc_tag : Tag,
    /// Major version of the TTC Header, = 2
    major_version : U16Be,
    /// Minor version of the TTC Header, = 0
    minor_version : U16Be,
    /// Number of fonts in TTC
    num_fonts : U32Be,
    /// Array of offsets to the OffsetTable for each font from the beginning of the file
    offset_tables : Array num_fonts (Offset32Be file_start (OffsetTable file_start)),
    /// Tag indicating that a DSIG table exists, 0x44534947 ('DSIG') (null if no signature)
    dsig_tag : U32Be, // FIXME: Tag?
    /// The length (in bytes) of the DSIG table (null if no signature)
    dsig_length : U32Be,
    /// The offset (in bytes) of the DSIG table from the beginning of the TTC file (null if no signature)
    // FIXME: dsig_offset : Offset32Be file_start Dsig,
    dsig_offset : Offset32Be file_start Unknown,
};



// =============================================================================
//
// OpenType Layout Common Table Formats
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2>
//
// =============================================================================


// -----------------------------------------------------------------------------
// Scripts and Languages
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#scripts-and-languages>
// -----------------------------------------------------------------------------

/// Script Record
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#slTbl_sRec>
struct ScriptRecord (script_list_start : Pos) {
    /// 4-byte script tag identifier
    script_tag : Tag,
    /// Offset to Script table, from beginning of ScriptList
    script_offset : Offset16Be script_list_start Script,
};

/// Script List Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#slTbl_sRec>
struct ScriptList {
    table_start : Pos,
    /// Number of ScriptRecords
    script_count : U16Be,
    /// Array of ScriptRecords, listed alphabetically by script tag
    script_records : Array script_count (ScriptRecord table_start),
};


/// Language System Record
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#script-table-and-language-system-record>
struct LangSysRecord (script_start : Pos) {
    /// 4-byte LangSysTag identifier
    lang_sys_tag : Tag,
    /// Offset to LangSys table, from beginning of Script table
    lang_sys_offset : Offset16Be script_start LangSys,
};

/// Script Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#script-table-and-language-system-record>
struct Script {
    table_start : Pos,
    /// Offset to default LangSys table, from beginning of Script table — may be NULL
    // FIXME: default_lang_sys : Offset16Be table_start LangSys,
    default_lang_sys : Offset16Be table_start LangSys,
    /// Number of LangSysRecords for this script — excluding the default LangSys
    lang_sys_count : U16Be,
    /// Array of LangSysRecords, listed alphabetically by LangSys tag
    lang_sys_records : Array lang_sys_count (LangSysRecord table_start),
};


/// Language System Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#language-system-table>
struct LangSys {
    /// = NULL (reserved for an offset to a reordering table)
    // lookup_order : Offset16Be _ Unknown,
    lookup_order : U16Be, // TODO: Mark as private?
    /// Index of a feature required for this language system; if no required features = 0xFFFF
    required_feature_index : U16Be,
    /// Number of feature index values for this language system — excludes the required feature
    feature_index_count : U16Be,
    /// Array of indices into the FeatureList, in arbitrary order
    feature_indices : Array feature_index_count U16Be,
};


// -----------------------------------------------------------------------------
// Features and Lookups
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#features-and-lookups>
// -----------------------------------------------------------------------------

/// Feature Record
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#flTbl>
struct FeatureRecord (feature_list_start : Pos) {
    /// 4-byte feature identification tag
    feature_tag : Tag,
    /// Offset to Feature table, from beginning of FeatureList
    feature_offset : Offset16Be feature_list_start Feature,
};

/// Feature List table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#flTbl>
struct FeatureList {
    table_start : Pos,
    /// Number of FeatureRecords in this table
    feature_count : U16Be,
    /// Array of FeatureRecords — zero-based (first feature has FeatureIndex = 0), listed alphabetically by feature tag
    feature_records : Array feature_count (FeatureRecord table_start),
};


/// Feature Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-table>
struct Feature {
    /// = NULL (reserved for offset to FeatureParams)
    // feature_params : Offset16Be _ Unknown,
    feature_params : U16Be, // TODO: Mark as private?
    /// Number of LookupList indices for this feature
    lookup_index_count : U16Be,
    /// Array of indices into the LookupList — zero-based (first lookup is LookupListIndex = 0)
    lookup_list_indices : Array lookup_index_count U16Be,
};


/// Lookup List Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookup-list-table>
struct LookupList {
    table_start : Pos,
    /// Number of lookups in this table
    lookup_count : U16Be,
    /// Array of offsets to Lookup tables, from beginning of LookupList — zero based (first lookup is Lookup index = 0)
    lookups : Array lookup_count (Offset16Be table_start Lookup),
};


// FIXME: enumerations
// /// LookupFlag bit enumeration
// ///
// /// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookupTbl>
// enum LookupFlag : u16 {
//     /// This bit relates only to the correct processing of the cursive attachment lookup type (GPOS lookup type 3). When this bit is set, the last glyph in a given sequence to which the cursive attachment lookup is applied, will be positioned on the baseline.
//     /// Note: Setting of this bit is not intended to be used by operating systems or applications to determine text direction.
//     right_to_left = 0x0001,
//     /// If set, skips over base glyphs
//     ignore_base_glyphs = 0x0002,
//     /// If set, skips over ligatures
//     ignore_ligatures = 0x0004,
//     /// If set, skips over all combining marks
//     ignore_marks = 0x0008,
//     /// If set, indicates that the lookup table structure is followed by a MarkFilteringSet field. The layout engine skips over all mark glyphs not in the mark filtering set indicated.
//     use_mark_filtering_set = 0x0010,
//     /// For future use (Set to zero)
//     reserved = 0x00E0,
//     /// If not zero, skips over all marks of attachment type different from specified.
//     mark_attachment_type = 0xFF00,
// };

/// Lookup Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookupTbl>
struct Lookup {
    table_start : Pos,
    /// Different enumerations for GSUB and GPOS
    lookup_type : U16Be,
    /// Lookup qualifiers
    lookup_flag : U16Be,
    // FIXME: lookup_flag : LookupFlag,
    /// Number of subtables for this lookup
    sub_table_count : U16Be,
    /// Array of offsets to lookup subtables, from beginning of Lookup table
    subtable_offsets : Array sub_table_count (Offset16Be table_start Unknown),
    /// Index (base 0) into GDEF mark glyph sets structure. This field is only present if bit useMarkFilteringSet of lookup flags is set.
    mark_filtering_set : U16Be,
};

/// Coverage Format 1 table: Individual glyph indices
struct CoverageFormat1 {
    /// Format identifier — format = 1
    coverage_format : U16Be,
    /// Number of glyphs in the glyph array
    glyph_count : U16Be,
    /// Array of glyph IDs — in numerical order
    glyph_array : Array glyph_count U16Be,
};

/// Range Record
struct RangeRecord {
    /// First glyph ID in the range
    start_glyph_id : U16Be,
    /// Last glyph ID in the range
    end_glyph_id : U16Be,
    /// Coverage Index of first glyph ID in range
    start_coverage_index : U16Be,
};

/// Coverage Format 2 table: Range of glyphs
struct CoverageFormat2 {
    /// Format identifier — format = 2
    coverage_format : U16Be,
    /// Number of RangeRecords
    range_count : U16Be,
    /// Array of glyph ranges — ordered by startGlyphID.
    range_records : Array range_count RangeRecord,
};

/// Coverage Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-table>
struct Coverage {
    /// Format identifier
    coverage_format : U16Be,
    body : match coverage_format {
        // FIXME: 1 => CoverageFormat1,
        // FIXME: 2 => CoverageFormat2,
        _ => Unknown,
    },
};

// TODO: Class Definition Table

// TODO: Device and VariationIndex Tables

// TODO: FeatureVariations Table

// TODO: ConditionSet Table

// TODO: FeatureTableSubstitution Table



// =============================================================================
//
// cmap - Character To Glyph Index Mapping Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap>
//
// =============================================================================


// -----------------------------------------------------------------------------
//
// CMap Header
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap>
//
// -----------------------------------------------------------------------------

/// Character To Glyph Index Mapping Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#cmap-header>
struct CMap {
    table_start : Pos,
    /// Table version number (0)
    version : U16Be,
    /// Number of encoding records that follow
    num_tables : U16Be,
    /// A list of encoding records
    encoding_records : Array num_tables (EncodingRecord table_start),
};

/// Specifies a particular encoding and the offset to the corresponding subtable
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#encoding-records-and-encodings>
struct EncodingRecord (cmap_start : Pos) {
    /// Platform ID.
    platform_id : U16Be,
    /// Platform-specific encoding ID.
    encoding_id : U16Be,
    /// Byte offset from beginning of table to the subtable for this encoding.
    subtable_offset : Offset32Be cmap_start CMapSubtable,
};

/// CMap Subtable
struct CMapSubtable {
    format : U16Be,
    body : match format {
        // TODO: 0 => CMapSubtable0,
        // TODO: 2 => CMapSubtable2,
        // TODO: 4 => CMapSubtable4,
        // TODO: 6 => CMapSubtable6,
        // TODO: 8 => CMapSubtable8,
        // TODO: 10 => CmapSubtable10,
        // TODO: 12 => CMapSubtable12,
        // TODO: 13 => CMapSubtable13,
        // TODO: 14 => CMapSubtable14,
        _ => Unknown,
    },
};


// -----------------------------------------------------------------------------
// FORMAT 0
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-0-byte-encoding-table>
// -----------------------------------------------------------------------------

/// Format 0: Byte encoding table
struct CMapSubtable0 {
    /// This is the length in bytes of the subtable.
    length : U16Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U16Be,
    /// An array that maps character codes to glyph index values.
    glyph_id_array : Array 256 U8,
};


// -----------------------------------------------------------------------------
// FORMAT 2
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-2-high-byte-mapping-through-table>
// -----------------------------------------------------------------------------

/// Format 2: High-byte mapping through table
struct CMapSubtable2 {
    /// This is the length in bytes of the subtable.
    length : U16Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U16Be,
    /// Array that maps high bytes to subHeaders: value is subHeader index * 8.
    sub_header_keys : Array 256 U16Be,
    /// Variable-length array of `CMapSubtable2SubHeader` records.
    subHeaders : VArray CMapSubtable2SubHeader,
    /// Variable-length array containing subarrays used for mapping the low byte
    /// of 2-byte characters.
    glyphIndexArray : VArray U16Be,
};

struct CMapSubtable2SubHeader {
    /// First valid low byte for this SubHeader.
    first_code : U16Be,
    /// Number of valid low bytes for this SubHeader.
    entry_count : U16Be,
    /// See text below.
    id_delta : S16Be,
    /// See text below.
    id_range_offset : U16Be,
};


// -----------------------------------------------------------------------------
// FORMAT 4
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values>
// -----------------------------------------------------------------------------

/// Format 4: Segment mapping to delta values
struct CMapSubtable4 {
    /// This is the length in bytes of the subtable.
    length : U16Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U16Be,
    /// `2 x seg_count.`
    seg_count_x2 : U16Be,
    /// `2 x (2**floor(log2(seg_count)))`
    search_range : U16Be,
    /// `log2(search_range/2)`
    entry_selector : U16Be,
    /// `2 x seg_count - search_range`
    range_shift : U16Be,
    // TODO:
    // /// End characterCode for each segment, `last = 0xFFFF`.
    // end_count : Array (seg_count_x2 / 2) U16Be,
    // /// Set to `0`.
    // reserved_pad : U16Be,
    // /// Start character code for each segment.
    // start_count : Array (seg_count_x2 / 2) U16Be,
    // /// Delta for all character codes in segment.
    // id_delta : Array (seg_count_x2 / 2) S16Be,
    // /// Offsets into `glyph_id_array` or 0
    // id_range_offset : Array (seg_count_x2 / 2) U16Be,
    // /// Glyph index array (arbitrary length)
    // glyph_id_array : Array ((length / 2 - 8) - (2 * seg_count_x2)) U16Be,
};


// -----------------------------------------------------------------------------
// FORMAT 6
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-6-trimmed-table-mapping>
// -----------------------------------------------------------------------------

/// Format 6: Trimmed table mapping
struct CMapSubtable6 {
    /// This is the length in bytes of the subtable.
    length : U16Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U16Be,
    /// First character code of subrange.
    first_code : U16Be,
    /// Number of character codes in subrange.
    entry_count : U16Be,
    /// Array of glyph index values for character codes in the range.
    glyph_id_array : Array entry_count U16Be,
};


// -----------------------------------------------------------------------------
// FORMAT 8
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-8-mixed-16-bit-and-32-bit-coverage>
// -----------------------------------------------------------------------------

/// Format 8: mixed 16-bit and 32-bit coverage
struct CMapSubtable8 {
    /// Reserved; set to 0
    reserved : U16Be,
    /// Byte length of this subtable (including the header)
    length : U32Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U32Be,
    /// Tightly packed array of bits (8K bytes total) indicating whether the
    /// particular 16-bit (index) value is the start of a 32-bit character code
    is32 : Array 8192 U8,
    /// Number of groupings which follow
    num_groups : U32Be,
    /// Array of SequentialMapGroup records.
    groups : Array num_groups CMapSubtable8SequentialMapGroup,
};

struct CMapSubtable8SequentialMapGroup {
    /// First character code in this group; note that if this group is for one
    /// or more 16-bit character codes (which is determined from the is32
    /// array), this 32-bit value will have the high 16-bits set to zero
    start_char_code : U32Be,
    /// Last character code in this group; same condition as listed above for
    /// the `start_char_code`
    end_char_code : U32Be,
    /// Glyph index corresponding to the starting character code
    start_glyph_id : U32Be,
};


// -----------------------------------------------------------------------------
// FORMAT 10
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-10-trimmed-array>
// -----------------------------------------------------------------------------

/// Format 10: Trimmed array
struct CmapSubtable10 {
    /// Reserved; set to 0
    reserved : U16Be,
    /// Byte length of this subtable (including the header)
    length : U32Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U32Be,
    /// First character code covered
    start_char_code : U32Be,
    /// Number of character codes covered
    num_chars : U32Be,
    /// Array of glyph indices for the character codes covered
    glyphs : VArray U16Be,
};


// -----------------------------------------------------------------------------
// FORMAT 12
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-12-segmented-coverage>
// -----------------------------------------------------------------------------

/// Format 12: Segmented coverage
struct CMapSubtable12 {
    /// Reserved; set to 0
    reserved : U16Be,
    /// Byte length of this subtable (including the header)
    length : U32Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U32Be,
    /// Number of groupings which follow
    num_groups : U32Be,
    /// Array of `CMapSubtable12SequentialMapGroup` records.
    groups : Array num_groups CMapSubtable12SequentialMapGroup,
};

struct CMapSubtable12SequentialMapGroup {
    /// First character code in this group
    start_char_code : U32Be,
    /// Last character code in this group
    end_char_code : U32Be,
    /// Glyph index corresponding to the starting character code
    start_glyph_id : U32Be,
};


// -----------------------------------------------------------------------------
// FORMAT 13
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-13-many-to-one-range-mappings>
// -----------------------------------------------------------------------------

/// Format 13: Many-to-one range mappings
struct CMapSubtable13 {
    /// Reserved; set to 0
    reserved : U16Be,
    /// Byte length of this subtable (including the header)
    length : U32Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U32Be,
    /// Number of groupings which follow
    num_groups : U32Be,
    /// Array of `CMapSubtable13ConstantMapGroup` records.
    groups : Array num_groups CMapSubtable13ConstantMapGroup,
};

struct CMapSubtable13ConstantMapGroup {
    /// First character code in this group
    start_char_code : U32Be,
    /// Last character code in this group
    end_char_code : U32Be,
    /// Glyph index to be used for all the characters in the group's range.
    start_glyph_id : U32Be,
};


// -----------------------------------------------------------------------------
// FORMAT 14
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-14-unicode-variation-sequences>
// -----------------------------------------------------------------------------

/// Format 14: Unicode Variation Sequences
struct CMapSubtable14 {
    table_start : Pos,
    /// Byte length of this subtable (including this header)
    length : U32Be,
    /// Number of variation Selector Records
    num_var_selector_records : U32Be,
    /// Array of `CMapSubtable14VariationSelector` records.
    var_selector : Array num_var_selector_records (CMapSubtable14VariationSelector table_start),
};

struct CMapSubtable14VariationSelector (subtable_start : Pos) {
    /// Variation selector
    var_selector : U24Be,
    /// Offset from the start of the format 14 subtable to Default UVS Table. May be 0.
    default_uvs_offset : Offset32Be subtable_start DefaultUvs,
    /// Offset from the start of the format 14 subtable to Non-Default UVS Table. May be 0.
    non_default_uvs_offset : Offset32Be subtable_start NonDefaultUvs,
};

/// Default UVS table
struct DefaultUvs {
    /// Number of Unicode character ranges.
    num_unicode_value_ranges : U32Be,
    /// Array of UnicodeRange records.
    ranges : Array num_unicode_value_ranges UnicodeRange,
};

struct UnicodeRange {
    /// First value in this range
    start_unicode_value : U24Be,
    /// Number of additional values in this range
    additional_count : U8,
};

/// Non-Default UVS Table
struct NonDefaultUvs {
    /// Number of UVS Mappings that follow
    num_uvs_mappings : U32Be,
    /// Array of `UvsMapping` records.
    uvs_mappings : Array num_uvs_mappings UvsMapping,
};

struct UvsMapping {
    /// Base Unicode value of the UVS
    unicode_value : U24Be,
    /// Glyph ID of the UVS
    glyph_id : U16Be,
};



// =============================================================================
//
// head — Font Header Table
//
// <https://www.microsoft.com/typography/otspec/head.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6head.html>
//
// =============================================================================

/// Font header table
///
/// <https://www.microsoft.com/typography/otspec/head.htm>
struct FontHeaderTable {
    /// Major version number of the font header table — set to `1`.
    major_version : U16Be,
    /// Minor version number of the font header table — set to `0`.
    minor_version : U16Be,
    /// Set by font manufacturer.
    font_revision : Fixed,
    /// To compute: set it to `0`, sum the entire font as `U32Be`, then store
    /// `0xB1B0AFBA - sum`. If the font is used as a component in a font
    /// collection file, the value of this field will be invalidated by changes
    /// to the file structure and font table directory, and must be ignored.
    check_sum_adjustment : U32Be,
    /// Set to `0x5F0F3CF5`.
    magic_number : U32Be,
    // TODO: Docs
    flags : U16Be,
    /// Set to a value from `16` to `16384`. Any value in this range is valid.
    /// In fonts that have TrueType outlines, a power of 2 is recommended as
    /// this allows performance optimizations in some rasterizers.
    units_per_em : U16Be,
    /// Number of seconds since 12:00 midnight that started January 1st 1904 in
    /// GMT/UTC time zone. 64-bit integer
    created : LongDateTime,
    /// Number of seconds since 12:00 midnight that started January 1st 1904 in
    /// GMT/UTC time zone. 64-bit integer
    modified : LongDateTime,
    /// For all glyph bounding boxes.
    x_min : S16Be,
    /// For all glyph bounding boxes.
    y_min : S16Be,
    /// For all glyph bounding boxes.
    x_max : S16Be,
    /// For all glyph bounding boxes.
    y_max : S16Be,
    // TODO: Docs
    mac_style : U16Be,
    /// Smallest readable size in pixels.
    lowest_rec_ppem : U16Be,
    /// Deprecated (Set to 2).
    ///
    /// * `0`: Fully mixed directional glyphs
    /// * `1`: Only strongly left to right
    /// * `2`: Like `1` but also contains neutrals
    /// * `-1`: Only strongly right to left
    /// * `-2`: Like `-1` but also contains neutrals
    font_direction_hint : S16Be,
    /// `0` for short offsets (`Offset16`), `1` for long (`Offset32`).
    index_to_loc_format : S16Be,
    /// `0` for current format.
    glyph_data_format : S16Be,
};



// =============================================================================
//
// hhea — Horizontal Header Table
//
// <https://www.microsoft.com/typography/otspec/hhea.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html>
//
// =============================================================================

/// Horizontal Header Table
///
/// <https://www.microsoft.com/typography/otspec/hhea.htm>
struct HorizontalHeader {
    /// Minor version number of the horizontal header table — set to 0.
    minor_version : U16Be,
    /// Major version number of the horizontal header table — set to 1.
    major_version : U16Be,
    /// Typographic ascent (Distance from baseline of highest ascender).
    ascender : FWord,
    /// Typographic descent (Distance from baseline of lowest descender).
    descender : FWord,
    /// Typographic line gap.
    ///
    /// Negative `line_gap` values are treated as zero in Windows 3.1, and in
    /// Mac OS System 6 and System 7.
    line_gap : FWord,
    /// Maximum advance width value in 'hmtx' table.
    advance_width_max : UfWord,
    /// Minimum left sidebearing value in 'hmtx' table.
    min_left_side_bearing : FWord,
    /// Minimum right sidebearing value; calculated as `min(aw - lsb - (x_max - x_min))`.
    min_right_side_bearing : FWord,
    /// `max(lsb + (x_max - x_min))`.
    x_max_extent : FWord,
    /// Used to calculate the slope of the cursor (rise/run); 1 for vertical.
    caret_slope_rise : S16Be,
    /// 0 for vertical.
    caret_slope_run : S16Be,
    /// The amount by which a slanted highlight on a glyph needs to be shifted
    /// to produce the best appearance. Set to 0 for non-slanted fonts
    caret_offset : S16Be,
    /// (reserved) set to 0
    reserved0 : S16Be,
    /// (reserved) set to 0
    reserved1 : S16Be,
    /// (reserved) set to 0
    reserved2 : S16Be,
    /// (reserved) set to 0
    reserved3 : S16Be,
    /// 0 for current format.
    metric_data_format : S16Be,
    /// Number of `h_metric` entries in 'hmtx' table
    number_of_h_metrics : U16Be,
};



// =============================================================================
//
// hmtx - Horizontal Metrics
//
// <https://www.microsoft.com/typography/otspec/hmtx.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hmtx.html>
//
// =============================================================================

/// <https://www.microsoft.com/typography/otspec/hmtx.htm#hmtxHeader>
struct HorizontalMetrics (num_glyphs : U16) (number_of_h_metrics : U16) {
    /// Paired advance width and left side bearing values for each glyph.
    /// Records are indexed by glyph ID.
    h_metrics : Array number_of_h_metrics LongHorMetric,
    // TODO:
    // /// Left side bearings for glyph IDs greater than or equal to `number_of_h_metrics`.
    // left_side_bearings : Array (num_glyphs - number_of_h_metrics) S16Be,
};

/// <https://www.microsoft.com/typography/otspec/hmtx.htm#lhm>
struct LongHorMetric {
    /// Advance width, in font design units.
    advance_width : U16Be,
    /// Glyph left side bearing, in font design units.
    lsb : S16Be,
};



// =============================================================================
//
// maxp - Maximum Profile
//
// <https://www.microsoft.com/typography/otspec/hmtx.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6maxp.html>
//
// =============================================================================

/// Maximum Profile
///
/// <https://www.microsoft.com/typography/otspec/maxp.htm>
///
/// Establishes the memory requirements for this font.
struct MaximumProfile {
    version : Fixed,
    data : match version.value {
        // TODO: 0x00005000u32 => Version_0_5,
        // TODO: 0x00010000u32 => Version_1_0,
        _ => Unknown,
    },
};

/// Version 0.5
///
/// Fonts with CFF data must use Version 0.5 of this table, specifying only the
/// `num_glyphs` field
struct Version_0_5 {
    /// The number of glyphs in the font
    num_glyphs : U16Be,
};

/// Version 1.0
///
/// Fonts with TrueType outlines must use Version 1.0 of this table, where all
/// data is required
struct Version_1_0 {
    /// The number of glyphs in the font
    num_glyphs : U16Be,
    /// Maximum points in a non-composite glyph.
    max_points : U16Be,
    /// Maximum contours in a non-composite glyph.
    max_contours : U16Be,
    /// Maximum points in a composite glyph.
    max_composite_points : U16Be,
    /// Maximum contours in a composite glyph.
    max_composite_contours : U16Be,
    /// 1 if instructions do not use the twilight zone (Z0), or 2 if
    /// instructions do use Z0; should be set to 2 in most cases.
    max_zones : U16Be,
    /// Maximum points used in Z0.
    max_twilight_points : U16Be,
    /// Number of Storage Area locations.
    max_storage : U16Be,
    /// Number of FDEFs, equal to the highest function number + 1.
    max_function_defs : U16Be,
    /// Number of IDEFs.
    max_instruction_defs : U16Be,
    /// Maximum stack depth2.
    max_stack_elements : U16Be,
    /// Maximum byte count for glyph instructions.
    max_size_of_instructions : U16Be,
    /// Maximum number of components referenced at “top level” for any composite glyph.
    max_component_elements : U16Be,
    /// Maximum levels of recursion; 1 for simple components.
    max_component_depth : U16Be,
};



// =============================================================================
//
// name — Naming Table
//
// <https://www.microsoft.com/typography/otspec/name.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6name.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// OS/2 — OS/2 and Windows Metrics Table
//
// <https://www.microsoft.com/typography/otspec/os2.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6OS2.html>
//
// =============================================================================

/// OS/2 and Windows Metrics Table
///
/// <https://www.microsoft.com/typography/otspec/os2.htm>
struct Os2 {
    version : U16Be,

    // FIXME: Proper version switching
    // TODO: Documentation

    // Version 0
    //
    // <https://www.microsoft.com/typography/otspec/os2ver0.htm>

    x_avg_char_width : S16Be,
    us_weight_class : U16Be,
    us_width_class : U16Be,
    fs_type : U16Be,
    y_subscript_x_size : S16Be,
    y_subscript_y_size : S16Be,
    y_subscript_x_offset : S16Be,
    y_subscript_y_offset : S16Be,
    y_superscript_x_size : S16Be,
    y_superscript_y_size : S16Be,
    y_superscript_x_offset : S16Be,
    y_superscript_y_offset : S16Be,
    y_strikeout_size : S16Be,
    y_strikeout_position : S16Be,
    s_family_class : S16Be,
    panose : Array 10 U8,
    /// Bits 0–31
    ul_unicode_range1 : U32Be,
    /// Bits 32–63
    ul_unicode_range2 : U32Be,
    /// Bits 64–95
    ul_unicode_range3 : U32Be,
    /// Bits 96–127
    ul_unicode_range4 : U32Be,
    ach_vend_id : Tag,
    fs_selection : U16Be,
    us_first_char_index : U16Be,
    us_last_char_index : U16Be,

    // Version 1
    //
    // <https://www.microsoft.com/typography/otspec/os2ver1.htm>

    s_typo_ascender : S16Be,
    s_typo_descender : S16Be,
    s_typo_line_gap : S16Be,
    us_win_ascent : U16Be,
    us_win_descent : U16Be,
    /// Bits 0–31
    ul_code_page_range1 : U32Be,
    /// Bits 32–63
    ul_code_page_range2 : U32Be,

    // Version 2
    //
    // <https://www.microsoft.com/typography/otspec/os2ver2.htm>

    sx_height : S16Be,
    s_cap_height : S16Be,
    us_default_char : U16Be,
    us_break_char : U16Be,
    us_max_context : U16Be,

    // Version 5
    //
    // <https://www.microsoft.com/typography/otspec/os2.htm>

    us_lower_optical_point_size : U16Be,
    us_upper_optical_point_size : U16Be,
};



// =============================================================================
//
// post — PostScript Table
//
// <https://www.microsoft.com/typography/otspec/post.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6post.html>
//
// =============================================================================

/// PostScript Table
struct Post {
    /// Version number.
    ///
    /// * `0x00010000` - for version 1.0
    /// * `0x00020000` - for version 2.0
    /// * `0x00025000` - for version 2.5 (deprecated)
    /// * `0x00030000` - for version 3.0
    version : Fixed,
    /// Italic angle in counter-clockwise degrees from the vertical. Zero for
    /// upright text, negative for text that leans to the right (forward).
    italic_angle : Fixed,
    /// This is the suggested distance of the top of the underline from the
    /// baseline (negative values indicate below baseline).
    ///
    /// The PostScript definition of this FontInfo dictionary key (the y
    /// coordinate of the center of the stroke) is not used for historical
    /// reasons. The value of the PostScript key may be calculated by
    /// subtracting half the `underline_thickness` from the value of this field.
    underline_position : FWord,
    /// Suggested values for the underline thickness.
    underline_thickness : FWord,
    /// Set to 0 if the font is proportionally spaced, non-zero if the font is
    /// not proportionally spaced (i.e. monospaced).
    is_fixed_pitch : U32Be,
    /// Minimum memory usage when an OpenType font is downloaded.
    min_mem_type42 : U32Be,
    /// Maximum memory usage when an OpenType font is downloaded.
    max_mem_type42 : U32Be,
    /// Minimum memory usage when an OpenType font is downloaded as a Type 1 font.
    min_mem_type1 : U32Be,
    /// Maximum memory usage when an OpenType font is downloaded as a Type 1 font.
    max_mem_type1 : U32Be,

    // Version 2.0

    /// Number of glyphs (this should be the same as numGlyphs in 'maxp' table).
    num_glyphs : U16Be,
    /// This is not an offset, but is the ordinal number of the glyph in 'post'
    /// string tables.
    glyph_name_index : Array num_glyphs U16Be,
    // FIXME: num_new_glyphs ???
    // /// Glyph names with length bytes [variable] (a Pascal string).
    // names : Array num_new_glyphs i8,


    // TODO: other versions!
};




// =============================================================================
//
// cvt — Control Value Table
//
// <https://www.microsoft.com/typography/otspec/cvt.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cvt.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// fpgm - Font Program
//
// <https://www.microsoft.com/typography/otspec/fpgm.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6fpgm.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// glyf - Glyf Data
//
// <https://www.microsoft.com/typography/otspec/glyf.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// loca - Index to Location
//
// <https://www.microsoft.com/typography/otspec/loca.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6loca.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// prep - Control Value Program
//
// <https://www.microsoft.com/typography/otspec/prep.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6prep.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// gasp — Grid-fitting And Scan-conversion Procedure Table
//
// <https://www.microsoft.com/typography/otspec/gasp.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6gasp.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// CFF - Compact Font Format table
//
// <https://www.microsoft.com/typography/otspec/cff.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// CFF - Compact Font Format table
//
// <https://www.microsoft.com/typography/otspec/cff2.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// VORG - Vertical Origin Table
//
// <https://www.microsoft.com/typography/otspec/vorg.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// SVG - The SVG (Scalable Vector Graphics) table
//
// <https://www.microsoft.com/typography/otspec/svg.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// EBDT - Embedded Bitmap Data Table
//
// <https://www.microsoft.com/typography/otspec/ebdt.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// EBLC - Embedded Bitmap Location Table
//
// <https://www.microsoft.com/typography/otspec/eblc.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// EBSC - Embedded Bitmap Scaling Table
//
// <https://www.microsoft.com/typography/otspec/ebsc.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6EBSC.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// CBDT - Color Bitmap Data Table
//
// <https://www.microsoft.com/typography/otspec/cbdt.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// CBLC - Color Bitmap Location Table
//
// <https://www.microsoft.com/typography/otspec/cblc.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// sbix — Standard Bitmap Graphics Table
//
// <https://www.microsoft.com/typography/otspec/sbix.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6sbix.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// BASE - Baseline Table
//
// <https://www.microsoft.com/typography/otspec/base.htm>
//
// =============================================================================

// TODO



// TODO: Rest of OpenType!
