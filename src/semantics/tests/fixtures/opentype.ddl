module opentype;


struct Unknown {};

// TODO: Missing primitives:

struct VArray (A : Type) {};
struct U24Be {};


// -----------------------------------------------------------------------------
// Data types
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#data-types>
// -----------------------------------------------------------------------------

Tag = Array 4 U8;



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


// TODO: Rest of OpenType!
