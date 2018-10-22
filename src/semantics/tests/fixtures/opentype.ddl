module opentype;


struct Unknown {};

// TODO: Missing primitives:

struct VArray (A : Type) {};

struct U24Be {
    value : Array 3 U8,
};

// TODO: Nullable offsets?


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

/// Array of four `U8`s (length = 32 bits) used to identify a script, language
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

struct OffsetTableRecord (file_start : Pos) {
    /// Table identifier
    tag : Tag,
    /// CheckSum for this table
    ///
    /// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#calculating-checksums>
    checksum : U32Be,
    /// Offset from beginning of TrueType font file
    offset : Offset32Be file_start (FontTable tag),
    /// Length of this table
    length : U32Be,
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
    dsig_offset : Offset32Be file_start DigitalSignature,
};


// -----------------------------------------------------------------------------
// Font Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#font-tables>
// -----------------------------------------------------------------------------

/// A mapping from a tag to the corresponding font table type
FontTable (tag : Tag) = match tag.value {
    // Required Tables
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#required-tables
    "cmap" => CharMap,          // Character to glyph mapping
    "head" => FontHeader,       // Font header
    "hhea" => HorizontalHeader, // Horizontal header
    "hmtx" => Unknown,          // Horizontal metrics // TODO: Depends on "hhea"
    "maxp" => Unknown,          // Maximum profile
    "name" => Unknown,          // Naming table
    "OS/2" => Os2,              // OS/2 and Windows specific metrics
    "post" => PostScript,       // PostScript information

    // Tables Related to TrueType Outlines
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-truetype-outlines

    "cvt " => Unknown,          // Control Value Table (optional table)
    "fpgm" => Unknown,          // Font program (optional table)
    "glyf" => Unknown,          // Glyph data
    "loca" => Unknown,          // Index to location
    "prep" => Unknown,          // CVT Program (optional table)
    "gasp" => Unknown,          // Grid-fitting/Scan-conversion (optional table)

    // Tables Related to CFF Outlines
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-cff-outlines
    "CFF " => Unknown,          // Compact Font Format 1.0
    "CFF2" => Unknown,          // Compact Font Format 2.0
    "VORG" => Unknown,          // Vertical Origin (optional table)

    // Table Related to SVG Outlines
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#table-related-to-svg-outlines
    "SVG " => Svg,              // The SVG (Scalable Vector Graphics) table

    // Tables Related to Bitmap Glyphs
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-bitmap-glyphs
    "EBDT" => Unknown,          // Embedded bitmap data
    "EBLC" => Unknown,          // Embedded bitmap location data
    "EBSC" => Unknown,          // Embedded bitmap scaling data
    "CBDT" => Unknown,          // Color bitmap data
    "CBLC" => Unknown,          // Color bitmap location data
    "sbix" => Unknown,          // Standard bitmap graphics

    // Advanced Typographic Tables
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#advanced-typographic-tables
    "BASE" => Unknown,          // Baseline data
    "GDEF" => GlyphDef,         // Glyph definition data
    "GPOS" => GlyphPos,         // Glyph positioning data
    "GSUB" => GlyphSub,         // Glyph substitution data
    "JSTF" => Unknown,          // Justification data
    "MATH" => Unknown,          // Math layout data

    // Tables used for OpenType Font Variations
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-used-for-opentype-font-variations
    "avar" => Unknown,          // Axis variations
    "cvar" => Unknown,          // CVT variations (TrueType outlines only)
    "fvar" => Unknown,          // Font variations
    "gvar" => Unknown,          // Glyph variations (TrueType outlines only)
    "HVAR" => Unknown,          // Horizontal metrics variations
    "MVAR" => Unknown,          // Metrics variations
    // "STAT" => StyleAttributes,  // Style attributes (required for variable fonts, optional for non-variable fonts)
    "VVAR" => Unknown,          // Vertical metrics variations

    // Tables Related to Color Fonts
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-color-fonts
    "COLR" => Unknown,          // Color table
    "CPAL" => Unknown,          // Color palette table
    // "CBDT" => Unknown,          // Color bitmap data
    // "CBLC" => Unknown,          // Color bitmap location data
    // "sbix" => Unknown,          // Standard bitmap graphics
    // "SVG " => Svg,              // The SVG (Scalable Vector Graphics) table

    // Other OpenType Tables
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#other-opentype-tables
    "DSIG" => DigitalSignature, // Digital signature
    "hdmx" => Unknown,          // Horizontal device metrics // TODO: Depends on "maxp"
    "kern" => Unknown,          // Kerning
    "LTSH" => Unknown,          // Linear threshold data
    "MERG" => Unknown,          // Merge
    "meta" => Metadata,         // Metadata
    "STAT" => StyleAttributes,  // Style attributes
    "PCLT" => Unknown,          // PCL 5 data
    "VDMX" => Unknown,          // Vertical device metrics
    "vhea" => Unknown,          // Vertical Metrics header
    "vmtx" => Unknown,          // Vertical Metrics

    _ => Unknown,
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
    start : Pos,
    /// Number of ScriptRecords
    script_count : U16Be,
    /// Array of ScriptRecords, listed alphabetically by script tag
    script_records : Array script_count (ScriptRecord start),
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
    start : Pos,
    /// Offset to default LangSys table, from beginning of Script table — may be NULL
    // FIXME: default_lang_sys : Offset16Be start LangSys,
    default_lang_sys : Offset16Be start LangSys,
    /// Number of LangSysRecords for this script — excluding the default LangSys
    lang_sys_count : U16Be,
    /// Array of LangSysRecords, listed alphabetically by LangSys tag
    lang_sys_records : Array lang_sys_count (LangSysRecord start),
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
    start : Pos,
    /// Number of FeatureRecords in this table
    feature_count : U16Be,
    /// Array of FeatureRecords — zero-based (first feature has FeatureIndex = 0), listed alphabetically by feature tag
    feature_records : Array feature_count (FeatureRecord start),
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
    start : Pos,
    /// Number of lookups in this table
    lookup_count : U16Be,
    /// Array of offsets to Lookup tables, from beginning of LookupList — zero based (first lookup is Lookup index = 0)
    lookups : Array lookup_count (Offset16Be start Lookup),
};


// TODO: enumerations
// /// LookupFlag bit enumeration
// ///
// /// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookupTbl>
// enum LookupFlag : u16 {
//     /// This bit relates only to the correct processing of the cursive attachment lookup type (GPOS lookup type 3). When this bit is set, the last glyph in a given sequence to which the cursive attachment lookup is applied, will be positioned on the baseline.
//     /// Note: Setting of this bit is not intended to be used by operating systems or applications to determine text direction.
//     RIGHT_TO_LEFT = 0x0001,
//     /// If set, skips over base glyphs
//     IGNORE_BASE_GLYPHS = 0x0002,
//     /// If set, skips over ligatures
//     IGNORE_LIGATURES = 0x0004,
//     /// If set, skips over all combining marks
//     IGNORE_MARKS = 0x0008,
//     /// If set, indicates that the lookup table structure is followed by a MarkFilteringSet field. The layout engine skips over all mark glyphs not in the mark filtering set indicated.
//     USE_MARK_FILTERING_SET = 0x0010,
//     /// For future use (Set to zero)
//     RESERVED = 0x00E0,
//     /// If not zero, skips over all marks of attachment type different from specified.
//     MARK_ATTACHMENT_TYPE = 0xFF00,
// };

/// Lookup Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookupTbl>
struct Lookup {
    start : Pos,
    /// Different enumerations for GSUB and GPOS
    lookup_type : U16Be,
    /// Lookup qualifiers
    lookup_flag : U16Be,
    // FIXME: lookup_flag : LookupFlag,
    /// Number of subtables for this lookup
    sub_table_count : U16Be,
    /// Array of offsets to lookup subtables, from beginning of Lookup table
    subtable_offsets : Array sub_table_count (Offset16Be start Unknown),
    /// Index (base 0) into GDEF mark glyph sets structure. This field is only present if bit useMarkFilteringSet of lookup flags is set.
    mark_filtering_set : U16Be,
};

// TODO:
// /// Coverage Table
// ///
// /// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-table>
// union Coverage {
//     CoverageFormat1,
//     CoverageFormat2,
// };

/// Coverage Format 1 table: Individual glyph indices
struct CoverageFormat1 {
    /// Format identifier — format = 1
    coverage_format : U16Be,
    /// Number of glyphs in the glyph array
    glyph_count : U16Be,
    /// Array of glyph IDs — in numerical order
    glyph_array : Array glyph_count U16Be,
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

/// Range Record
struct RangeRecord {
    /// First glyph ID in the range
    start_glyph_id : U16Be,
    /// Last glyph ID in the range
    end_glyph_id : U16Be,
    /// Coverage Index of first glyph ID in range
    start_coverage_index : U16Be,
};


// -----------------------------------------------------------------------------
// Class Definition Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table>
// -----------------------------------------------------------------------------

// TODO



// -----------------------------------------------------------------------------
// Device and VariationIndex Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#device-and-variationindex-tables>
// -----------------------------------------------------------------------------

// TODO: enumerations
// enum DeltaFormat {
//     /// Signed 2-bit value, 8 values per `U16`
//     LOCAL_2_BIT_DELTAS = 0x0001,
//     /// Signed 4-bit value, 4 values per `U16`
//     LOCAL_4_BIT_DELTAS = 0x0002,
//     /// Signed 8-bit value, 2 values per `U16`
//     LOCAL_8_BIT_DELTAS = 0x0003,
//     /// VariationIndex table, contains a delta-set index pair.
//     VARIATION_INDEX = 0x8000,
//     /// For future use — set to 0
//     Reserved = 0x7FFC,
// }

struct Device {
    /// Smallest size to correct, in ppem
    start_size : U16Be,
    /// Largest size to correct, in ppem
    end_size : U16Be,
    /// Format of deltaValue array data: 0x0001, 0x0002, or 0x0003
    // TODO: delta_format : DeltaFormat,
    delta_format : U16Be,
    /// Array of compressed data
    delta_value : VArray U16Be, // TODO: what length?
};

struct VariationIndex {
    /// A delta-set outer index — used to select an item variation data subtable within the item variation store.
    delta_set_outer_index : U16Be,
    /// A delta-set inner index — used to select a delta-set row within an item variation data subtable.
    delta_set_inner_index : U16Be,
    /// Format, = 0x8000
    // TODO: delta_format : DeltaFormat,
    delta_format : U16Be,
};


// -----------------------------------------------------------------------------
// FeatureVariations Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#featurevariations-table>
// -----------------------------------------------------------------------------

struct FeatureVariations {
    // TODO
};



// -----------------------------------------------------------------------------
// ConditionSet Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#conditionset-table>
// -----------------------------------------------------------------------------

// TODO



// -----------------------------------------------------------------------------
// FeatureTableSubstitution Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#featuretablesubstitution-table>
// -----------------------------------------------------------------------------

// TODO



// =============================================================================
//
// OpenType Font Variations Common Table Formats
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats>
//
// =============================================================================

// TODO



// =============================================================================
//
// cmap - Character To Glyph Index Mapping Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap>
//
// =============================================================================


// -----------------------------------------------------------------------------
//
// cmap Header
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap>
//
// -----------------------------------------------------------------------------

/// Character To Glyph Index Mapping Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#cmap-header>
struct CharMap {
    start : Pos,
    /// Table version number (0)
    version : U16Be,
    /// Number of encoding records that follow
    num_tables : U16Be,
    /// A list of encoding records
    encoding_records : Array num_tables (EncodingRecord start),
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
    // TODO: subtable_offset : Offset32Be cmap_start CharMapSubtable,
    subtable_offset : Offset32Be cmap_start Unknown,
};

// TODO:
// /// CharMap Subtable
// union CharMapSubtable {
//     CharMapSubtable0,
//     CharMapSubtable2,
//     CharMapSubtable4,
//     CharMapSubtable6,
//     CharMapSubtable8,
//     CharMapSubtable10,
//     CharMapSubtable12,
//     CharMapSubtable13,
//     CharMapSubtable14,
// };


// -----------------------------------------------------------------------------
// FORMAT 0
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-0-byte-encoding-table>
// -----------------------------------------------------------------------------

/// Format 0: Byte encoding table
struct CharMapSubtable0 {
    /// Format number is set to 0
    format : U16Be,
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
struct CharMapSubtable2 {
    /// Format number is set to 2
    format : U16Be,
    /// This is the length in bytes of the subtable.
    length : U16Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U16Be,
    /// Array that maps high bytes to subHeaders: value is subHeader index * 8.
    sub_header_keys : Array 256 U16Be,
    /// Variable-length array of `CharMapSubtable2SubHeader` records.
    subHeaders : VArray CharMapSubtable2SubHeader, // TODO
    /// Variable-length array containing subarrays used for mapping the low byte
    /// of 2-byte characters.
    glyphIndexArray : VArray U16Be, // TODO
};

struct CharMapSubtable2SubHeader {
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
struct CharMapSubtable4 {
    /// Format number is set to 4
    format : U16Be,
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
struct CharMapSubtable6 {
    /// Format number is set to 6
    format : U16Be,
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
struct CharMapSubtable8 {
    /// Format number is set to 8
    format : U16Be,
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
    groups : Array num_groups CharMapSubtable8SequentialMapGroup,
};

struct CharMapSubtable8SequentialMapGroup {
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
struct CharMapSubtable10 {
    /// Format number is set to 10
    format : U16Be,
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
    glyphs : VArray U16Be, // TODO
};


// -----------------------------------------------------------------------------
// FORMAT 12
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-12-segmented-coverage>
// -----------------------------------------------------------------------------

/// Format 12: Segmented coverage
struct CharMapSubtable12 {
    /// Format number is set to 12
    format : U16Be,
    /// Reserved; set to 0
    reserved : U16Be,
    /// Byte length of this subtable (including the header)
    length : U32Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U32Be,
    /// Number of groupings which follow
    num_groups : U32Be,
    /// Array of `CharMapSubtable12SequentialMapGroup` records.
    groups : Array num_groups CharMapSubtable12SequentialMapGroup,
};

struct CharMapSubtable12SequentialMapGroup {
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
struct CharMapSubtable13 {
    /// Format number is set to 13
    format : U16Be,
    /// Reserved; set to 0
    reserved : U16Be,
    /// Byte length of this subtable (including the header)
    length : U32Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U32Be,
    /// Number of groupings which follow
    num_groups : U32Be,
    /// Array of `CharMapSubtable13ConstantMapGroup` records.
    groups : Array num_groups CharMapSubtable13ConstantMapGroup,
};

struct CharMapSubtable13ConstantMapGroup {
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
struct CharMapSubtable14 {
    start : Pos,
    /// Format number is set to 14
    format : U16Be,
    /// Byte length of this subtable (including this header)
    length : U32Be,
    /// Number of variation Selector Records
    num_var_selector_records : U32Be,
    /// Array of `CharMapSubtable14VariationSelector` records.
    var_selector : Array num_var_selector_records (CharMapSubtable14VariationSelector start),
};

struct CharMapSubtable14VariationSelector (substart : Pos) {
    /// Variation selector
    var_selector : U24Be,
    /// Offset from the start of the format 14 subtable to Default UVS Table. May be 0.
    default_uvs_offset : Offset32Be substart DefaultUvs,
    /// Offset from the start of the format 14 subtable to Non-Default UVS Table. May be 0.
    non_default_uvs_offset : Offset32Be substart NonDefaultUvs,
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
struct FontHeader {
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

// TODO:
// /// Maximum Profile
// ///
// /// <https://www.microsoft.com/typography/otspec/maxp.htm>
// ///
// /// Establishes the memory requirements for this font.
// union MaximumProfile {
//     MaximumProfileVersion_0_5,
//     MaximumProfileVersion_1_0,
// };

/// Version 0.5
///
/// Fonts with CFF data must use Version 0.5 of this table, specifying only the
/// `num_glyphs` field
struct MaximumProfileVersion_0_5 {
    /// 0x00005000 for version 0.5
    version : Fixed,
    /// The number of glyphs in the font
    num_glyphs : U16Be,
};

/// Version 1.0
///
/// Fonts with TrueType outlines must use Version 1.0 of this table, where all
/// data is required
struct MaximumProfileVersion_1_0 {
    /// 0x00010000 for version 1.0.
    version : Fixed,
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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6name.html>
//
// =============================================================================

// TODO
// union Naming {
//     NamingFormat0,
//     NamingFormat1,
// };

// -----------------------------------------------------------------------------
// Naming table format 0
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name#naming-table-format-0>
// -----------------------------------------------------------------------------

struct NamingFormat0 {
    start : Pos,
    /// Format selector (=0).
    format : U16Be,
    /// Number of name records.
    count : U16Be,
    /// Offset to start of string storage (from start of table).
    string_offset : Offset16Be start Unknown, // TODO
    // TODO: name_record : Array count (NameRecord string_offset),
    name_record : Array count Unknown,
    // TODO:
    // /// Storage for the actual string data.
    // // (Variable),
};


// -----------------------------------------------------------------------------
// Naming table format 1
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name#naming-table-format-1>
// -----------------------------------------------------------------------------

struct NamingFormat1 {
    start : Pos,
    /// Format selector (=1).
    format : U16Be,
    /// Number of name records.
    count : U16Be,
    /// Offset to start of string storage (from start of table).
    string_offset : Offset16Be start Unknown, // TODO
    /// The name records where count is the number of records.
    // TODO: name_record : Array count (NameRecord string_offset),
    name_record : Array count Unknown,
    /// Number of language-tag records.
    lang_tag_count : U16Be,
    /// The language-tag records where langTagCount is the number of records.
    // TODO: lang_tag_record : Array lang_tag_count (LangTagRecord string_offset),
    lang_tag_record : Array lang_tag_count Unknown,
    // TODO:
    // /// Storage for the actual string data.
    // // (Variable),
};

struct LangTagRecord (storage_start : Pos) {
    /// Language-tag string length (in bytes)
    length : U16Be,
    // /// Language-tag string offset from start of storage area (in bytes).
    offset : Offset16Be storage_start (Array length U8), // TODO: String?
};


// -----------------------------------------------------------------------------
// Name Records
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name#name-records>
// -----------------------------------------------------------------------------

struct NameRecord (storage_start : Pos) {
    /// Platform ID.
    platform_id : U16Be,
    /// Platform-specific encoding ID.
    encoding_id : U16Be,
    /// Language ID.
    language_id : U16Be,
    /// Name ID.
    name_id : U16Be,
    /// String length (in bytes).
    length : U16Be,
    /// String offset from start of storage area (in bytes).
    offset : Offset16Be storage_start (Array length U8), // TODO: String?
};


// -----------------------------------------------------------------------------
// Platform IDs
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name#platform-ids>
// -----------------------------------------------------------------------------

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
struct PostScript {
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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/svg>
//
// =============================================================================


// -----------------------------------------------------------------------------
// SVG Table Header
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/svg#svg-table-header>
// -----------------------------------------------------------------------------

struct Svg {
    start : Pos,
    /// Table version (starting at 0). Set to 0.
    version : U16Be,
    /// Offset to the SVG Document List, from the start of the SVG table. Must be non-zero.
    offset_to_svg_document_list : Offset32Be start SvgDocumentList,
    /// Set to 0.
    reserved : U32Be, // TODO: Private?
};


// -----------------------------------------------------------------------------
// SVG Document List
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/svg#svg-document-list>
// -----------------------------------------------------------------------------

struct SvgDocumentList {
    start : Pos,
    /// Number of SVG document records. Must be non-zero.
    num_entries : U16Be,
    /// Array of SVG document records.
    document_records : Array num_entries (SvgDocumentRecord start),
};

struct SvgDocumentRecord (svg_document_list_start : Pos) {
    /// The first glyph ID for the range covered by this record.
    start_glyph_id : U16Be,
    /// The last glyph ID for the range covered by this record.
    end_glyph_id : U16Be,
    /// Offset from the beginning of the SVGDocumentList to an SVG document. Must be non-zero.
    svg_doc_offset : Offset32Be svg_document_list_start Unknown, // TODO
    /// Length of the SVG document data. Must be non-zero.
    svg_doc_length : U32Be,
};



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



// =============================================================================
//
// GDEF — Glyph Definition Table
//
// <https://www.microsoft.com/typography/otspec/gdef.htm>
//
// =============================================================================


// -----------------------------------------------------------------------------
//
// GDEF Header
//
// <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader>
//
// -----------------------------------------------------------------------------

/// GDEF Header
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader>
struct GlyphDef {
    start : Pos,
    /// Major version of the GDEF table, = 1
    major_version : U16Be,
    /// Minor version of the GDEF table, = 3
    minor_version : U16Be,

    // FIXME: Proper version switching

    // GDEF Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_10>

    /// Offset to class definition table for glyph type, from beginning of GDEF header (may be NULL)
    glyph_class_def_offset : Offset16Be start Unknown, // TODO
    /// Offset to attachment point list table, from beginning of GDEF header (may be NULL)
    attach_list_offset : Offset16Be start Unknown, // TODO
    /// Offset to ligature caret list table, from beginning of GDEF header (may be NULL)
    lig_caret_list_offset : Offset16Be start LigCaretList,
    /// Offset to class definition table for mark attachment type, from beginning of GDEF header (may be NULL)
    mark_attach_class_def_offset : Offset16Be start Unknown, // TODO

    // GDEF Header, Version 1.2
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_12>

    /// Offset to the table of mark glyph set definitions, from beginning of GDEF header (may be NULL)
    mark_glyph_sets_def_offset : Offset16Be start Unknown, // TODO

    // GDEF Header, Version 1.3
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_12>

    /// Offset to the Item Variation Store table, from beginning of GDEF header (may be NULL)
    item_var_store_offset : Offset32Be start Unknown, // TODO
};


// -----------------------------------------------------------------------------
// Glyph Class Definition Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#glyphClassDefTbl>
// -----------------------------------------------------------------------------

// TODO: GlyphClassDef = ClassDef(GlyphClassDefEnum);


// -----------------------------------------------------------------------------
// Attachment Point List Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#attachmentPointListTbl>
// -----------------------------------------------------------------------------

/// AttachList table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#attachListTable>
struct AttachList {
    start : Pos,
    /// Offset to Coverage table - from beginning of AttachList table
    // TODO: coverage_offset : Offset16Be start Coverage,
    coverage_offset : Offset16Be start Unknown,
    /// Number of glyphs with attachment points
    glyph_count : U16Be,
    /// Array of offsets to AttachPoint tables-from beginning of AttachList
    /// table-in Coverage Index order
    attach_point_offsets : Array glyph_count (Offset16Be start AttachPoint),
};

/// AttachPoint table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#attachPointTable>
struct AttachPoint {
    /// Number of attachment points on this glyph
    point_count : U16Be,
    /// Array of contour point indices -in increasing numerical order
    point_indices : Array point_count U16Be,
};


// -----------------------------------------------------------------------------
// Ligature Caret List Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#ligatureCaretListTbl>
// -----------------------------------------------------------------------------

/// Ligature Caret List Table
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#ligatureCaretListTbl>
struct LigCaretList {
    start : Pos,
    /// Offset to Coverage table - from beginning of LigCaretList table
    // TODO: coverage_offset : Offset16Be start Coverage,
    coverage_offset : Offset16Be start Unknown,
    /// Number of ligature glyphs
    lig_glyph_count : U16Be,
    /// Array of offsets to LigGlyph tables, from beginning of LigCaretList
    /// table —in Coverage Index order
    lig_glyph_offsets : Array lig_glyph_count (Offset16Be start LigGlyph),
};

/// LigGlyph table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#ligGlyphTable>
struct LigGlyph {
    start : Pos,
    /// Number of CaretValue tables for this ligature (components - 1)
    caret_count : U16Be,
    /// Array of offsets to CaretValue tables, from beginning of LigGlyph
    /// table — in increasing coordinate order
    // TODO: caret_value_offsets : Array caret_count (Offset16Be start CaretValue),
    caret_value_offsets : Array caret_count (Offset16Be start Unknown),
};

// TODO:
// /// Caret Value Tables
// ///
// /// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTbls>
// union CaretValue {
//     CaretValueFormat1,
//     CaretValueFormat2,
//     CaretValueFormat3,
// };

/// CaretValue Format 1: Design units only
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTbl1>
struct CaretValueFormat1 {
    /// Format identifier: format = 1
    caret_value_format : U16Be,
    /// X or Y value, in design units
    coordinate : S16Be,
};

/// CaretValue Format 2: Contour point
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTbl2>
struct CaretValueFormat2 {
    /// Format identifier: format = 2
    caret_value_format : U16Be,
    /// Contour point index on glyph
    caret_value_point_index : U16Be,
};

/// Caret Value Format 3: Design units plus Device or VariationIndex table
///
/// <https://www.microsoft.com/typography/otspec/gdef.htm#caretValueTable_3>
struct CaretValueFormat3 {
    start : Pos,
    /// Format identifier: format = 3
    caret_value_format : U16Be,
    /// X or Y value, in design units
    coordinate : S16Be,
    /// Offset to Device table (non-variable font) / Variation Index table
    /// (variable font) for X or Y value-from beginning of CaretValue table
    device_offset : Offset16Be start Device,
};


// -----------------------------------------------------------------------------
// Mark Attachment Class Definition Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm#ligatureCaretListTbl>
// -----------------------------------------------------------------------------

// TODO



// =============================================================================
//
// GPOS — Glyph Positioning Table
//
// <https://www.microsoft.com/typography/otspec/gpos.htm>
//
// =============================================================================


// -----------------------------------------------------------------------------
//
// GPOS Header
//
// <https://www.microsoft.com/typography/otspec/gpos.htm>
//
// -----------------------------------------------------------------------------

/// GPOS Header
///
/// <https://www.microsoft.com/typography/otspec/gpos.htm#header>
struct GlyphPos {
    start : Pos,
    /// Major version of the GPOS table
    major_version : U16Be,
    /// Minor version of the GPOS table
    minor_version : U16Be,

    // FIXME: Proper version switching

    // GPOS Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gpos.htm#gposHeader_10>

    /// Offset to `ScriptList` table, from beginning of GPOS table
    script_list_offset : Offset16Be start ScriptList,
    /// Offset to `FeatureList` table, from beginning of GPOS table
    feature_list_offset : Offset16Be start FeatureList,
    /// Offset to `LookupList` table, from beginning of GPOS table
    lookup_list_offset : Offset16Be start LookupList,

    // GPOS Header, Version 1.1
    // <https://www.microsoft.com/typography/otspec/gpos.htm#gposHeader_11>

    /// Offset to `FeatureVariations` table, from beginning of GPOS table (may be NULL)
    feature_variations_offset : Offset32Be start FeatureVariations,
};



// =============================================================================
//
// GSUB — Glyph Substitution Table
//
// <https://www.microsoft.com/typography/otspec/gsub.htm>
//
// =============================================================================

/// GSUB Header
///
/// <https://www.microsoft.com/typography/otspec/gsub.htm#header>
struct GlyphSub {
    start : Pos,
    /// Major version of the GSUB table
    major_version : U16Be,
    /// Minor version of the GSUB table
    minor_version : U16Be,

    // FIXME: Proper version switching

    // GSUB Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gsub.htm#gsubHeader_10>

    /// Offset to `ScriptList` table, from beginning of GSUB table
    script_list_offset : Offset16Be start ScriptList,
    /// Offset to `FeatureList` table, from beginning of GSUB table
    feature_list_offset : Offset16Be start FeatureList,
    /// Offset to `LookupList` table, from beginning of GSUB table
    lookup_list_offset : Offset16Be start LookupList,

    // GSUB Header, Version 1.1
    // <https://www.microsoft.com/typography/otspec/gsub.htm#gsubHeader_11>

    /// Offset to `FeatureVariations` table, from beginning of GSUB table (may be NULL)
    feature_variations_offset : Offset32Be start FeatureVariations,
};

// TODO



// =============================================================================
//
// JSTF — Justification Table
//
// <https://www.microsoft.com/typography/otspec/jstf.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// MATH - The mathematical typesetting table
//
// <https://www.microsoft.com/typography/otspec/math.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// avar — Axis Variations Table
//
// <https://www.microsoft.com/typography/otspec/avar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6avar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// cvar — CVT Variations Table
//
// <https://www.microsoft.com/typography/otspec/cvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cvar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// fvar — Font Variations Table
//
// <https://www.microsoft.com/typography/otspec/fvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6fvar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// gvar — Glyph Variations Table
//
// <https://www.microsoft.com/typography/otspec/gvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6gvar.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// HVAR — Horizontal Metrics Variations Table
//
// <https://www.microsoft.com/typography/otspec/hvar.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// MVAR — Metrics Variations Table
//
// <https://www.microsoft.com/typography/otspec/mvar.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// STAT — Style Attributes Table
//
// <https://www.microsoft.com/typography/otspec/stat.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// VVAR — Vertical Metrics Variations Table
//
// <https://www.microsoft.com/typography/otspec/vvar.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// COLR - Color Table
//
// <https://www.microsoft.com/typography/otspec/colr.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// CPAL - Color Palette Table
//
// <https://www.microsoft.com/typography/otspec/cpal.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// DSIG - Digital Signature Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/dsig>
//
// =============================================================================

/// DSIG — Digital Signature Table
struct DigitalSignature {
    start : Pos,
    /// Version number of the DSIG table (0x00000001)
    version : U32Be,
    /// Number of signatures in the table
    num_signatures : U16Be,
    /// permission flags Bit 0: cannot be resigned Bits 1-7: Reserved (Set to 0)
    flags : U16Be,
    /// Array of signature records
    signature_records : Array num_signatures (SignatureRecord start),
};

struct SignatureRecord (digital_signature_start : Pos) {
    /// Format of the signature
    format : U32Be,
    /// Length of signature in bytes
    length : U32Be,
    /// Offset to the signature block from the beginning of the table
    // TODO: offset : Offset32Be digital_signature_start SignatureBlock,
    offset : Offset32Be digital_signature_start Unknown,
};

// TODO:
// union SignatureBlock {
//     SignatureBlockFormat1,
// }

struct SignatureBlockFormat1 {
    /// Reserved for future use; set to zero.
    reserved1 : U16Be, // TODO: private?
    /// Reserved for future use; set to zero.
    reserved2 : U16Be, // TODO: private?
    /// Length (in bytes) of the PKCS#7 packet in the signature field.
    signature_length : U32Be,
    /// PKCS#7 packet
    signature : Array signature_length U8,
};



// =============================================================================
//
// hdmx - Horizontal Device Metrics
//
// <https://www.microsoft.com/typography/otspec/hdmx.htm>
//
// =============================================================================

/// Horizontal Device Metrics Table
struct HorizontalDeviceMetrics (num_glyphs : U16) {
    /// Table version number (0)
    version : U16Be,
    /// Number of device records.
    num_records : S16Be,
    /// Size of a device record, 32-bit aligned.
    size_device_record : S32Be,
    /// Array of device records.
    // FIXME: num_records is signed!
    // records : Array num_records (DeviceRecord num_glyphs),
    records : Unknown,
};

/// Device Record
struct DeviceRecord (num_glyphs : U16) {
    /// Pixel size for following widths (as ppem).
    pixel_size : U8,
    /// Maximum width.e
    max_width : U8,
    /// Array of widths (numGlyphs is from the 'maxp' table).
    widths : Array num_glyphs U8,
};



// =============================================================================
//
// kern - Kerning
//
// <https://www.microsoft.com/typography/otspec/kern.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// LTSH - Linear Threshold
//
// <https://www.microsoft.com/typography/otspec/ltsh.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// MERG — Merge Table
//
// <https://www.microsoft.com/typography/otspec/merg.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// meta — Metadata Table
//
// <https://www.microsoft.com/typography/otspec/meta.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6meta.html>
//
// =============================================================================

/// Metadata Table
struct Metadata {
    start : Pos,
    /// Version number of the metadata table — set to 1.
    version : U32Be,
    /// Flags — currently unused; set to 0.
    flags : U32Be,
    /// Not used; should be set to 0.
    reserved : U32Be,
    /// The number of data maps in the table.
    data_maps_count : U32Be,
    /// Array of data map records.
    data_maps : Array data_maps_count (DataMap start),
};

/// DataMap record
struct DataMap (metadata_start : Pos) {
    /// A tag indicating the type of metadata.
    tag : Tag,
    /// Offset in bytes from the beginning of the metadata table to the data for this tag.
    data_offset : Offset32Be metadata_start (MetadataInfo tag), // TODO
    /// Length of the data, in bytes. The data is not required to be padded to any byte boundary.
    data_length : U32Be,
};

/// Metadata information
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/meta#metadata-tags>
MetadataInfo (tag : Tag) = match tag.value {
    "appl" => Unknown, // TODO
    "bild" => Unknown, // TODO
    "dlng" => Unknown, // TODO
    "slng" => Unknown, // TODO
    _ => Unknown,
};

// TODO



// =============================================================================
//
// STAT — Style Attributes Table
//
// <https://www.microsoft.com/typography/otspec/stat.htm>
//
// =============================================================================

struct StyleAttributes {
    start : Pos,
    /// Major version number of the style attributes table — set to 1.
    major_version: U16Be,
    /// Minor version number of the style attributes table — set to 2.
    minor_version: U16Be,

    // FIXME: Proper version switching

    // Version 1.0 (deprecated)

    /// The size in bytes of each axis record.
    design_axis_size : U16Be,
    /// The number of design axis records. In a font with an 'fvar' table, this
    /// value must be greater than or equal to the axisCount value in the 'fvar'
    /// table. In all fonts, must be greater than zero if axisValueCount is
    /// greater than zero.
    design_axis_count : U16Be,
    /// Offset in bytes from the beginning of the STAT table to the start of the
    /// design axes array. If designAxisCount is zero, set to zero; if
    /// designAxisCount is greater than zero, must be greater than zero.
    design_axes_offset : Offset32Be start (Array design_axis_count AxisRecord),
    /// The number of axis value tables.
    axis_value_count : U16Be,
    /// Offset in bytes from the beginning of the STAT table to the start of the
    /// design axes value offsets array. If axisValueCount is zero, set to zero;
    /// if axisValueCount is greater than zero, must be greater than zero.
    offset_to_axis_value_offsets : Offset32Be start (Array axis_value_count Unknown),

    // Version 1.1

    /// Name ID used as fallback when projection of names into a particular font
    /// model produces a subfamily name containing only elidable elements.
    elided_fallback_name_id: U16Be,
};


// -----------------------------------------------------------------------------
// Axis Records
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-records>
// -----------------------------------------------------------------------------

/// https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-records
struct AxisRecord {
    /// A tag identifying the axis of design variation.
    axis_tag : Tag,
    /// The name ID for entries in the 'name' table that provide a display string for this axis.
    axis_name_id : U16Be,
    /// A value that applications can use to determine primary sorting of face names, or for ordering of descriptors when composing family or face names.
    axis_ordering : U16Be,
};


// -----------------------------------------------------------------------------
// Axis Value Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-tables>
// -----------------------------------------------------------------------------

// TODO:
// union AxisValueFormat (major_version : U16Be) (minor_version : U16Be) {
//     AxisValueFormat1,
//     AxisValueFormat2,
//     AxisValueFormat3,
//     AxisValueFormat4, // TODO: If Version 1.2
// }

struct AxisValueFormat1 {
    /// Format identifier — set to 1.
    format : U16Be,
    /// Zero-base index into the axis record array identifying the axis of design variation to which the axis value record applies. Must be less than designAxisCount.
    axis_index : U16Be,
    /// Flags — see below for details.
    flags : U16Be,
    /// The name ID for entries in the 'name' table that provide a display string for this attribute value.
    value_name_id : U16Be,
    /// A numeric value for this attribute value.
    value : Fixed,
};

struct AxisValueFormat2 {
    /// Format identifier — set to 2.
    format : U16Be,
    /// Zero-base index into the axis record array identifying the axis of design variation to which the axis value record applies. Must be less than designAxisCount.
    axis_index : U16Be,
    /// Flags — see below for details.
    flags : U16Be,
    /// The name ID for entries in the 'name' table that provide a display string for this attribute value.
    value_name_id : U16Be,
    /// A nominal numeric value for this attribute value.
    nominal_value : Fixed,
    /// The minimum value for a range associated with the specified name ID.
    range_min_value : Fixed,
    /// The maximum value for a range associated with the specified name ID.
    range_max_value : Fixed,
};

struct AxisValueFormat3 {
    /// Format identifier — set to 3.
    format : U16Be,
    /// Zero-base index into the axis record array identifying the axis of design variation to which the axis value record applies. Must be less than designAxisCount.
    axis_index : U16Be,
    /// Flags — see below for details.
    flags : U16Be,
    /// The name ID for entries in the 'name' table that provide a display string for this attribute value.
    value_name_id : U16Be,
    /// A numeric value for this attribute value.
    value : Fixed,
    /// The numeric value for a style-linked mapping from this value.
    linked_value : Fixed,
};

struct AxisValueFormat4 {
    /// Format identifier — set to 4.
    format : U16Be,
    /// The total number of axes contributing to this axis-values combination.
    axis_count : U16Be,
    /// Flags — see below for details.
    flags : U16Be,
    /// The name ID for entries in the 'name' table that provide a display string for this combination of axis values.
    value_name_id : U16Be,
    /// Array of AxisValue records that provide the combination of axis values, one for each contributing axis.
    axis_values : Array axis_count AxisValue,
};

struct AxisValue {
    /// Zero-base index into the axis record array identifying the axis to which this value applies. Must be less than designAxisCount.
    axis_index : U16Be,
    /// A numeric value for this attribute value.
    value : Fixed,
};



// =============================================================================
//
// PCLT - PCL 5 Table
//
// <https://www.microsoft.com/typography/otspec/pclt.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// VDMX - Vertical Device Metrics
//
// <https://www.microsoft.com/typography/otspec/vdmx.htm>
//
// =============================================================================

// TODO



// =============================================================================
//
// vhea — Vertical Header Table
//
// <https://www.microsoft.com/typography/otspec/vhea.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vhea.html>
//
// =============================================================================

// TODO



// =============================================================================
//
// vmtx - Vertical Metrics Table
//
// <https://www.microsoft.com/typography/otspec/vmtx.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vmtx.html>
//
// =============================================================================

// TODO
