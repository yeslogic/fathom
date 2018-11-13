module opentype;


struct Unknown {};

// TODO: Missing primitives:

struct VArray (A : Type) {};

struct U24Be {
    value : Array 3 U8,
};

// FIXME: A tad hacky - add operators and proper bounds handling?
nat_eq : int {0 ..} -> int {0 ..} -> Bool;
nat_eq = extern "int-eq";

// FIXME: A tad hacky - add operators and proper bounds handling?
nat_add : int {0 ..} -> int {0 ..} -> int {0 ..};
nat_add = extern "int-add";

// FIXME: A tad hacky - add operators and proper bounds handling?
nat_sub : int {0 ..} -> int {0 ..} -> int {0 ..};
nat_sub = extern "int-sub";

// FIXME: A tad hacky - add operators and proper bounds handling?
nat_mul : int {0 ..} -> int {0 ..} -> int {0 ..};
nat_mul = extern "int-mul";

// FIXME: A tad hacky - add operators and proper bounds handling?
nat_div : int {0 ..} -> int {0 ..} -> int {0 ..};
nat_div = extern "int-div";

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

/// 16-bit signed fixed number with the low 14 bits of fraction (2.14).
struct F2Dot14 {
    value: S16Be,
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

struct File {
    start : Pos,
    open_type : OpenType start,
};

union OpenType (file_start : Pos) {
    OffsetTable file_start,
    TtcHeader file_start,
};


// -----------------------------------------------------------------------------
// Offset Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
// -----------------------------------------------------------------------------

struct OffsetTable (file_start : Pos) {
    /// 0x00010000 or 0x4F54544F ('OTTO')
    ///
    /// Apple allows 'true' and 'typ1' ?
    sfnt_version : U32Be, // TODO: constrain version
    /// Number of tables
    num_tables : U16Be,
    /// (Maximum power of 2 <= num_tables) x 16
    search_range : U16Be,
    /// Log2(maximum power of 2 <= num_tables)
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
    offset : U32Be,
    /// Length of this table
    length : U32Be,
    /// The computed position of this table
    pos : OffsetPos file_start offset (FontTable tag length)
};


// -----------------------------------------------------------------------------
// Font Collections
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#organization-of-an-opentype-font>
// -----------------------------------------------------------------------------

/// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#ttc-header>
union TtcHeader (file_start : Pos) {
    TtcHeader1 file_start,
    TtcHeader2 file_start,
};

struct TtcHeader1 (file_start : Pos) {
    /// Font Collection ID
    ttc_tag : Tag,
    /// Major version of the TTC Header, = 1
    major_version : { version : U16Be | nat_eq version 1 },
    /// Minor version of the TTC Header, = 0
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Number of fonts in TTC
    num_fonts : U32Be,
    /// Array of offsets to the OffsetTable for each font from the beginning of the file
    offset_tables : Array num_fonts (Offset32Be file_start (OffsetTable file_start)),
};

struct TtcHeader2 (file_start : Pos) {
    /// Font Collection ID
    ttc_tag : Tag,
    /// Major version of the TTC Header, = 2
    major_version : { version : U16Be | nat_eq version 2 },
    /// Minor version of the TTC Header, = 0
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Number of fonts in TTC
    num_fonts : U32Be,
    /// Array of offsets to the OffsetTable for each font from the beginning of the file
    offset_tables : Array num_fonts (Offset32Be file_start (OffsetTable file_start)),
    /// Tag indicating that a DSIG table exists, 0x44534947 ('DSIG') (null if no signature)
    dsig_tag : U32Be, // FIXME: Tag?
    /// The length (in bytes) of the DSIG table (null if no signature)
    dsig_length : U32Be,
    /// The offset (in bytes) of the DSIG table from the beginning of the TTC file (null if no signature)
    dsig_offset : Offset32Be file_start (DigitalSignature dsig_length),
};


// -----------------------------------------------------------------------------
// Font Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/otff#font-tables>
// -----------------------------------------------------------------------------

/// A mapping from a tag to the corresponding font table type
FontTable (tag : Tag) (length : U32) = match tag.value {
    // Required Tables
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#required-tables
    "cmap" => CharMap,                      // Character to glyph mapping
    "head" => FontHeader,                   // Font header
    "hhea" => HorizontalHeader,             // Horizontal header
    "hmtx" => Unknown,                      // Horizontal metrics // TODO: Depends on `num_glyphs` and `number_of_h_metrics` from "hhea"
    "maxp" => MaximumProfile,               // Maximum profile
    "name" => Naming,                       // Naming table
    "OS/2" => Os2,                          // OS/2 and Windows specific metrics
    "post" => PostScript,                   // PostScript information

    // Tables Related to TrueType Outlines
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-truetype-outlines

    "cvt " => ControlValue length,          // Control Value Table (optional table)
    "fpgm" => FontProgram length,           // Font program (optional table)
    "glyf" => Unknown,                      // Glyph data // TODO: Depends on `num_glyphs` from "maxp"
    "loca" => Unknown,                      // Index to location // TODO: Depends on `num_glyphs` from "maxp", offset to "glyph", `index_to_loc_format` from "head"
    "prep" => ControlValueProgram length,   // CVT Program (optional table)
    "gasp" => GridFittingScanConversion,    // Grid-fitting/Scan-conversion (optional table)

    // Tables Related to CFF Outlines
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-cff-outlines
    "CFF " => Unknown,                      // Compact Font Format 1.0
    "CFF2" => Unknown,                      // Compact Font Format 2.0
    "VORG" => VerticalOrigin,               // Vertical Origin (optional table)

    // Table Related to SVG Outlines
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#table-related-to-svg-outlines
    "SVG " => Svg,                          // The SVG (Scalable Vector Graphics) table

    // Tables Related to Bitmap Glyphs
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-bitmap-glyphs
    "EBDT" => EmbeddedBitmapData,           // Embedded bitmap data // TODO: Depends on "EBLC" table
    "EBLC" => EmbeddedBitmapLocationData,   // Embedded bitmap location data // TODO: Depends on "EBDT" table start position
    "EBSC" => EmbeddedBitmapScalingData,    // Embedded bitmap scaling data
    "CBDT" => Unknown,                      // Color bitmap data
    "CBLC" => Unknown,                      // Color bitmap location data
    "sbix" => Unknown,                      // Standard bitmap graphics

    // Advanced Typographic Tables
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#advanced-typographic-tables
    "BASE" => BaselineData,                 // Baseline data
    "GDEF" => GlyphDefinitionData,          // Glyph definition data
    "GPOS" => GlyphPositioningData,         // Glyph positioning data
    "GSUB" => GlyphSubstitutionData,        // Glyph substitution data
    "JSTF" => JustificationData,            // Justification data
    "MATH" => MathLayoutData,               // Math layout data

    // Tables used for OpenType Font Variations
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-used-for-opentype-font-variations
    "avar" => AxisVariations,               // Axis variations
    "cvar" => ControlValueVariations,       // CVT variations (TrueType outlines only)
    "fvar" => FontVariations,               // Font variations
    "gvar" => Unknown,                      // Glyph variations (TrueType outlines only)
    "HVAR" => Unknown,                      // Horizontal metrics variations
    "MVAR" => Unknown,                      // Metrics variations
    // "STAT" => StyleAttributes,              // Style attributes (required for variable fonts, optional for non-variable fonts)
    "VVAR" => Unknown,                      // Vertical metrics variations

    // Tables Related to Color Fonts
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tables-related-to-color-fonts
    "COLR" => Unknown,                      // Color table
    "CPAL" => Unknown,                      // Color palette table
    // "CBDT" => Unknown,                      // Color bitmap data
    // "CBLC" => Unknown,                      // Color bitmap location data
    // "sbix" => Unknown,                      // Standard bitmap graphics
    // "SVG " => Svg,                          // The SVG (Scalable Vector Graphics) table

    // Other OpenType Tables
    // https://docs.microsoft.com/en-us/typography/opentype/spec/otff#other-opentype-tables
    "DSIG" => DigitalSignature length,      // Digital signature
    "hdmx" => Unknown,                      // Horizontal device metrics // TODO: Depends on `num_glyphs` from "maxp"
    "kern" => Kerning,                      // Kerning
    "LTSH" => LinearThreshold,              // Linear threshold data // TODO: Depends on `num_glyphs` from "maxp"
    "MERG" => Merge,                        // Merge
    "meta" => Metadata,                     // Metadata
    "STAT" => StyleAttributes,              // Style attributes
    "PCLT" => Pcl5,                         // PCL 5 data
    "VDMX" => VerticalDeviceMetrics,        // Vertical device metrics
    "vhea" => Unknown,                      // Vertical Metrics header
    "vmtx" => Unknown,                      // Vertical Metrics

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
    // TODO: lookup_order : Reserved U16Be 0,
    lookup_order : Reserved U16Be,
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
    // TODO: feature_params : Reserved U16Be 0,
    feature_params : Reserved U16Be,
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

/// Coverage Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-table>
union Coverage {
    CoverageFormat1,
    CoverageFormat2,
};

/// Coverage Format 1 table: Individual glyph indices
struct CoverageFormat1 {
    /// Format identifier — format = 1
    coverage_format : { format : U16Be | nat_eq format 1 },
    /// Number of glyphs in the glyph array
    glyph_count : U16Be,
    /// Array of glyph IDs — in numerical order
    glyph_array : Array glyph_count U16Be,
};

/// Coverage Format 2 table: Range of glyphs
struct CoverageFormat2 {
    /// Format identifier — format = 2
    coverage_format : { format : U16Be | nat_eq format 2 },
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

// FIXME: Class type must be U16Be ???

union ClassDef (Class : Type) {
    ClassDefFormat1 Class,
    ClassDefFormat2 Class,
};

/// ClassDefFormat1 table: Class array
struct ClassDefFormat1 (Class : Type) {
    /// Format identifier — format = 1
    class_format : { format : U16Be | nat_eq format 1 },
    /// First glyph ID of the `class_value_array`
    start_glyph_id : U16Be,
    /// Size of the `class_value_array`
    glyph_count : U16Be,
    /// Array of Class Values — one per glyph ID
    class_value_array : Array glyph_count Class,
};

/// ClassDefFormat2 table: Class ranges
struct ClassDefFormat2 (Class : Type) {
    /// Format identifier — format = 2
    class_format : { format : U16Be | nat_eq format 2 },
    /// Number of ClassRangeRecords
    class_range_count : U16Be,
    /// Array of ClassRangeRecords — ordered by `start_glyph_id`
    class_range_records : Array class_range_count (ClassRangeRecord Class),
};

struct ClassRangeRecord (Class : Type) {
    /// First glyph ID in the range
    start_glyph_id : U16Be,
    /// Last glyph ID in the range
    end_glyph_id : U16Be,
    /// Applied to all glyphs in the range
    class : Class,
};



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
    start : Pos,
    /// Major version of the FeatureVariations table — set to 1.
    major_version : { version : U16Be | nat_eq version 1 },
    /// Minor version of the FeatureVariations table — set to 0.
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Number of feature variation records.
    feature_variation_record_count : U32Be,
    /// Array of feature variation records.
    feature_variation_records : Array feature_variation_record_count (FeatureVariationRecord start),
};

struct FeatureVariationRecord (feature_variations_start : Pos) {
    /// Offset to a condition set table, from beginning of FeatureVariations table.
    condition_set_offset : Offset32Be feature_variations_start ConditionSet,
    /// Offset to a feature table substitution table, from beginning of the FeatureVariations table.
    feature_table_substitution_offset : Offset32Be feature_variations_start FeatureTableSubstitution,
};


// -----------------------------------------------------------------------------
// ConditionSet Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#conditionset-table>
// -----------------------------------------------------------------------------

struct ConditionSet {
    start : Pos,
    /// Number of conditions for this condition set.
    condition_count : U16Be,
    /// Array of offsets to condition tables, from beginning of the ConditionSet table.
    conditions : Array condition_count (Offset32Be start ConditionTableFormat),
};


// -----------------------------------------------------------------------------
// Condition Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#condition-table>
// -----------------------------------------------------------------------------

union ConditionTableFormat {
    ConditionTableFormat1,
};

struct ConditionTableFormat1 {
    /// Format, = 1
    format : { format : U16Be | nat_eq format 1 },
    /// Index (zero-based) for the variation axis within the 'fvar' table.
    axis_index : U16Be,
    /// Minimum value of the font variation instances that satisfy this condition.
    filter_range_min_value : F2Dot14,
    /// Maximum value of the font variation instances that satisfy this condition.
    filter_range_max_value : F2Dot14,
};


// -----------------------------------------------------------------------------
// FeatureTableSubstitution Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#featuretablesubstitution-table>
// -----------------------------------------------------------------------------

struct FeatureTableSubstitution {
    start : Pos,
    /// Major version of the feature table substitution table — set to 1
    major_version : { version : U16Be | nat_eq version 1 },
    /// Minor version of the feature table substitution table — set to 0.
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Number of feature table substitution records.
    substitution_count : U16Be,
    /// Array of feature table substitution records.
    substitutions : Array substitution_count (FeatureTableSubstitutionRecord start),
};

struct FeatureTableSubstitutionRecord (feature_table_substitution_start : Pos) {
    /// The feature table index to match.
    feature_index : U16Be,
    /// Offset to an alternate feature table, from start of the FeatureTableSubstitution table.
    alternate_feature_table : Offset32Be feature_table_substitution_start Unknown, // TODO
};



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
    version : { version : U16Be | nat_eq version 0 },
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
    subtable_offset : Offset32Be cmap_start CharMapSubtable,
};

/// CharMap Subtable
union CharMapSubtable {
    CharMapSubtable0,
    CharMapSubtable2,
    CharMapSubtable4,
    CharMapSubtable6,
    CharMapSubtable8,
    CharMapSubtable10,
    CharMapSubtable12,
    CharMapSubtable13,
    CharMapSubtable14,
};


// -----------------------------------------------------------------------------
// FORMAT 0
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-0-byte-encoding-table>
// -----------------------------------------------------------------------------

/// Format 0: Byte encoding table
struct CharMapSubtable0 {
    /// Format number is set to 0
    format : { format : U16Be | nat_eq format 0 },
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
    format : { format : U16Be | nat_eq format 2 },
    /// This is the length in bytes of the subtable.
    length : U16Be,
    /// Please see "[Note on the language field in 'cmap' subtables]
    /// (https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#language)".
    language : U16Be,
    /// Array that maps high bytes to sub_headers: value is sub_header index * 8.
    sub_header_keys : Array 256 U16Be,
    /// Variable-length array of `CharMapSubtable2SubHeader` records.
    sub_headers : VArray CharMapSubtable2SubHeader, // TODO
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
    format : { format : U16Be | nat_eq format 4 },
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
    /// End characterCode for each segment, `last = 0xFFFF`.
    end_count : Array (nat_div seg_count_x2 2) U16Be,
    /// Set to `0`.
    // TODO: reserved_pad : Reserved U16Be 0,
    reserved_pad : Reserved U16Be,
    /// Start character code for each segment.
    start_count : Array (nat_div seg_count_x2 2) U16Be,
    /// Delta for all character codes in segment.
    id_delta : Array (nat_div seg_count_x2 2) S16Be,
    /// Offsets into `glyph_id_array` or 0
    id_range_offset : Array (nat_div seg_count_x2 2) U16Be,
    /// Glyph index array (arbitrary length)
    glyph_id_array : Array (nat_sub (nat_sub (nat_div length 2) 8) (nat_mul 2 seg_count_x2)) U16Be,
};


// -----------------------------------------------------------------------------
// FORMAT 6
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-6-trimmed-table-mapping>
// -----------------------------------------------------------------------------

/// Format 6: Trimmed table mapping
struct CharMapSubtable6 {
    /// Format number is set to 6
    format : { format : U16Be | nat_eq format 6 },
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
    format : { format : U16Be | nat_eq format 8 },
    /// Reserved; set to 0
    // TODO: reserved : Reserved U16Be 0,
    reserved : Reserved U16Be,
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
    format : { format : U16Be | nat_eq format 10 },
    /// Reserved; set to 0
    // TODO: reserved : Reserved U16Be 0,
    reserved : Reserved U16Be,
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
    format : { format : U16Be | nat_eq format 12 },
    /// Reserved; set to 0
    // TODO: reserved : Reserved U16Be 0,
    reserved : Reserved U16Be,
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
    format : { format : U16Be | nat_eq format 13 },
    /// Reserved; set to 0
    // TODO: reserved : Reserved U16Be 0,
    reserved : Reserved U16Be,
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
    format : { format : U16Be | nat_eq format 14 },
    /// Byte length of this subtable (including this header)
    length : U32Be,
    /// Number of variation Selector Records
    num_var_selector_records : U32Be,
    /// Array of `CharMapSubtable14VariationSelector` records.
    var_selector : Array num_var_selector_records (CharMapSubtable14VariationSelector start),
};

struct CharMapSubtable14VariationSelector (subtable_start : Pos) {
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
struct FontHeader {
    /// Major version number of the font header table — set to `1`.
    major_version : { version : U16Be | nat_eq version 1 },
    /// Minor version number of the font header table — set to `0`.
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Set by font manufacturer.
    font_revision : Fixed,
    /// To compute: set it to `0`, sum the entire font as `U32Be`, then store
    /// `0xB1B0AFBA - sum`. If the font is used as a component in a font
    /// collection file, the value of this field will be invalidated by changes
    /// to the file structure and font table directory, and must be ignored.
    check_sum_adjustment : U32Be,
    /// Set to `0x5F0F3CF5`.
    magic_number : { magic : U32Be | nat_eq magic 0x5F0F3CF5 },
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
    /// Major version number of the horizontal header table — set to 1.
    major_version : { version : U16Be | nat_eq version 1 },
    /// Minor version number of the horizontal header table — set to 0.
    minor_version : { version : U16Be | nat_eq version 0 },
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
    // TODO: reserved0 : Reserved S16Be 0,
    reserved0 : Reserved S16Be,
    /// (reserved) set to 0
    // TODO: reserved1 : Reserved S16Be 0,
    reserved1 : Reserved S16Be,
    /// (reserved) set to 0
    // TODO: reserved2 : Reserved S16Be 0,
    reserved2 : Reserved S16Be,
    /// (reserved) set to 0
    // TODO: reserved3 : Reserved S16Be 0,
    reserved3 : Reserved S16Be,
    /// 0 for current format.
    metric_data_format : S16Be,
    /// Number of `h_metric` entries in 'hmtx' table
    number_of_h_metrics : U16Be,
};



// =============================================================================
//
// hmtx - Horizontal Metrics
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hmtx.html>
//
// =============================================================================

/// <https://www.microsoft.com/typography/otspec/hmtx.htm#hmtxHeader>
struct HorizontalMetrics (num_glyphs : U16) (number_of_h_metrics : U16) {
    /// Paired advance width and left side bearing values for each glyph.
    /// Records are indexed by glyph ID.
    h_metrics : Array number_of_h_metrics LongHorMetric,
    /// Left side bearings for glyph IDs greater than or equal to `number_of_h_metrics`.
    left_side_bearings : Array (nat_sub num_glyphs number_of_h_metrics) S16Be,
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
union MaximumProfile {
    MaximumProfileVersion_0_5,
    MaximumProfileVersion_1_0,
};

/// Version 0.5
///
/// Fonts with CFF data must use Version 0.5 of this table, specifying only the
/// `num_glyphs` field
struct MaximumProfileVersion_0_5 {
    /// 0x00005000 for version 0.5
    version : { version : Fixed | nat_eq version.value 0x00005000 },
    /// The number of glyphs in the font
    num_glyphs : U16Be,
};

/// Version 1.0
///
/// Fonts with TrueType outlines must use Version 1.0 of this table, where all
/// data is required
struct MaximumProfileVersion_1_0 {
    /// 0x00010000 for version 1.0.
    version : { version : Fixed | nat_eq version.value 0x00010000 },
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

union Naming {
    NamingFormat0,
    NamingFormat1,
};

// -----------------------------------------------------------------------------
// Naming table format 0
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name#naming-table-format-0>
// -----------------------------------------------------------------------------

struct NamingFormat0 {
    start : Pos,
    /// Format selector (=0).
    format : { format : U16Be | nat_eq format 0 },
    /// Number of name records.
    count : U16Be,
    /// Offset to start of string storage (from start of table).
    string_offset : U16Be,
    /// The name records where count is the number of records.
    name_record : Array count (NameRecord start string_offset),
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
    format : { format : U16Be | nat_eq format 1 },
    /// Number of name records.
    count : U16Be,
    /// Offset to start of string storage (from start of table).
    string_offset : U16Be,
    /// The name records where count is the number of records.
    name_record : Array count (NameRecord start string_offset),
    /// Number of language-tag records.
    lang_tag_count : U16Be,
    /// The language-tag records where langTagCount is the number of records.
    // TODO: lang_tag_record : Array lang_tag_count (LangTagRecord string_offset),
    lang_tag_record : Array lang_tag_count Unknown,
    // TODO:
    // /// Storage for the actual string data.
    // // (Variable),
};

struct LangTagRecord (naming_start : Pos) (storage_offset : U16) {
    /// Language-tag string length (in bytes)
    length : U16Be,
    /// Language-tag string offset from start of storage area (in bytes).
    offset : U16Be,
    /// Language-tag string
    // TODO: Array->String
    // TODO: Operators
    name_pos : OffsetPos naming_start (nat_add storage_offset offset) (Array length U8),
};


// -----------------------------------------------------------------------------
// Name Records
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/name#name-records>
// -----------------------------------------------------------------------------

struct NameRecord (naming_start : Pos) (storage_offset : U16) {
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
    offset : U16Be,
    /// The computed position of the name
    // TODO: Array->String
    // TODO: Operators
    name_pos : OffsetPos naming_start (nat_add storage_offset offset) (Array length U8),
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
    version : U16Be, // TODO: constrain version!

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
    version : Fixed,  // TODO: constrain version!
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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/cvt>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cvt.html>
//
// =============================================================================

struct ControlValue (length : U32) {
    /// List of values referenceable by instructions.
    values : Array (nat_div length 2) FWord, // TODO: repeat to length?
};



// =============================================================================
//
// fpgm - Font Program
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/fpgm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6fpgm.html>
//
// =============================================================================

struct FontProgram (length : U32) {
    instructions : Array length U8, // TODO: repeat to length?
};



// =============================================================================
//
// glyf - Glyf Data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/glyf>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html>
//
// =============================================================================

struct Glyphs (num_glyphs : U16) {
    glyphs : Array num_glyphs Glyph,
};

struct Glyph {
    /// If the number of contours is greater than or equal to zero, this is a
    /// simple glyph. If negative, this is a composite glyph — the value -1
    /// should be used for composite glyphs.
    number_of_contours : S16Be,
    /// Minimum x for coordinate data.
    x_min : S16Be,
    /// Minimum y for coordinate data.
    y_min : S16Be,
    /// Maximum x for coordinate data.
    x_max : S16Be,
    /// Maximum y for coordinate data.
    y_max : S16Be,
    // TODO: SimpleGlyph or CompositeGlyph depending on `number_of_contours`
    // description : if number_of_contours >= 0 {
    //     SimpleGlyph (floor 0 number_of_contours)
    // } else {
    //     CompositeGlyph
    // },
};


// -----------------------------------------------------------------------------
// Simple Glyph Description
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#simple-glyph-description>
// -----------------------------------------------------------------------------

struct SimpleGlyph (number_of_contours : U16) {
    /// Array of point indices for the last point of each contour, in increasing
    /// numeric order.
    end_pts_of_contours : Array number_of_contours U16Be,
    /// Total number of bytes for instructions. If instruction_length is zero, no
    /// instructions are present for this glyph, and this field is followed
    /// directly by the flags field.
    instruction_length : U16Be,
    /// Array of instruction byte code for the glyph.
    instructions : Array instruction_length U8,
    /// Array of flag elements. See below for details regarding the number of
    /// flag array elements.
    flags : VArray U8,
    // TODO: depends on `flags`
    // /// Contour point x-coordinates. See below for details regarding the number
    // /// of coordinate array elements. Coordinate for the first point is relative
    // /// to (0,0); others are relative to previous point.
    // x_coordinates : VArray (U8 or S16Be),
    // /// Contour point y-coordinates. See below for details regarding the number
    // /// of coordinate array elements. Coordinate for the first point is relative
    // /// to (0,0); others are relative to previous point.
    // y_coordinates : VArray (U8 or S16Be),
};

// enum SimpleGlyphFlags : U8 {
//     /// Bit 0: If set, the point is on the curve; otherwise, it is off the curve.
//     ON_CURVE_POINT = 0x01,
//     /// Bit 1: If set, the corresponding x-coordinate is 1 byte long. If not set,
//     /// it is two bytes long. For the sign of this value, see the description of
//     /// the X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR flag.
//     X_SHORT_VECTOR = 0x02,
//     /// Bit 2: If set, the corresponding y-coordinate is 1 byte long. If not set,
//     /// it is two bytes long. For the sign of this value, see the description of
//     /// the Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR flag.
//     Y_SHORT_VECTOR = 0x04,
//     /// Bit 3: If set, the next byte (read as unsigned) specifies the number of
//     /// additional times this flag byte is to be repeated in the logical flags
//     /// array — that is, the number of additional logical flag entries inserted
//     /// after this entry. (In the expanded logical array, this bit is ignored.)
//     /// In this way, the number of flags listed can be smaller than the number
//     /// of points in the glyph description.
//     REPEAT_FLAG = 0x08,
//     /// Bit 4: This flag has two meanings, depending on how the X_SHORT_VECTOR
//     /// flag is set. If X_SHORT_VECTOR is set, this bit describes the sign of
//     /// the value, with 1 equalling positive and 0 negative. If X_SHORT_VECTOR
//     /// is not set and this bit is set, then the current x-coordinate is the
//     /// same as the previous x-coordinate. If X_SHORT_VECTOR is not set and this
//     /// bit is also not set, the current x-coordinate is a signed 16-bit delta
//     /// vector.
//     X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR = 0x10,
//     /// Bit 5: This flag has two meanings, depending on how the Y_SHORT_VECTOR
//     /// flag is set. If Y_SHORT_VECTOR is set, this bit describes the sign of
//     /// the value, with 1 equalling positive and 0 negative. If Y_SHORT_VECTOR
//     /// is not set and this bit is set, then the current y-coordinate is the
//     /// same as the previous y-coordinate. If Y_SHORT_VECTOR is not set and this
//     /// bit is also not set, the current y-coordinate is a signed 16-bit delta
//     /// vector.
//     Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR = 0x20,
//     /// Bit 6: If set, contours in the glyph description may overlap. Use of
//     /// this flag is not required in OpenType — that is, it is valid to have
//     /// contours overlap without having this flag set. It may affect behaviors
//     /// in some platforms, however. (See the discussion of “Overlapping
//     /// contours” in Apple’s specification for details regarding behavior in
//     /// Apple platforms.) When used, it must be set on the first flag byte for
//     /// the glyph. See additional details below.
//     OVERLAP_SIMPLE = 0x40,
//     /// Bit 7 is reserved: set to zero.
//     Reserved = 0x80,
// };


// -----------------------------------------------------------------------------
// Composite Glyph Description
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#composite-glyph-description>
// -----------------------------------------------------------------------------

struct CompositeGlyph {
    /// component flag
    flags : U16Be,
    /// glyph index of component
    glyph_index : U16Be,
    // TODO: depends on `flags`
    // /// x-offset for component or point number; type depends on bits 0 and 1 in component flags
    // argument1 : U8, S8, U16Be or S16Be,
    // /// y-offset for component or point number; type depends on bits 0 and 1 in component flags
    // argument2 : U8, S8, U16Be or S16Be,
};

// TODO:
// enum CompositeGlyphFlags : U16 {
//     /// Bit 0: If this is set, the arguments are 16-bit (uint16 or int16);
//     /// otherwise, they are bytes (uint8 or int8).
//     ARG_1_AND_2_ARE_WORDS = 0x0001,
//     /// Bit 1: If this is set, the arguments are signed xy values; otherwise,
//     /// they are unsigned point numbers.
//     ARGS_ARE_XY_VALUES = 0x0002,
//     /// Bit 2: For the xy values if the preceding is true.
//     ROUND_XY_TO_GRID = 0x0004,
//     /// Bit 3: This indicates that there is a simple scale for the component.
//     /// Otherwise, scale = 1.0.
//     WE_HAVE_A_SCALE = 0x0008,
//     /// Bit 5: Indicates at least one more glyph after this one.
//     MORE_COMPONENTS = 0x0020,
//     /// Bit 6: The x direction will use a different scale from the y direction.
//     WE_HAVE_AN_X_AND_Y_SCALE = 0x0040,
//     /// Bit 7: There is a 2 by 2 transformation that will be used to scale the
//     /// component.
//     WE_HAVE_A_TWO_BY_TWO = 0x0080,
//     /// Bit 8: Following the last component are instructions for the composite
//     /// character.
//     WE_HAVE_INSTRUCTIONS = 0x0100,
//     /// Bit 9: If set, this forces the aw and lsb (and rsb) for the composite to
//     /// be equal to those from this original glyph. This works for hinted and
//     /// unhinted characters.
//     USE_MY_METRICS = 0x0200,
//     /// Bit 10: If set, the components of the compound glyph overlap. Use of
//     /// this flag is not required in OpenType — that is, it is valid to have
//     /// components overlap without having this flag set. It may affect behaviors
//     /// in some platforms, however. (See Apple’s specification for details
//     /// regarding behavior in Apple platforms.) When used, it must be set on the
//     /// flag word for the first component. See additional remarks, above, for
//     /// the similar OVERLAP_SIMPLE flag used in simple-glyph descriptions.
//     OVERLAP_COMPOUND = 0x0400,
//     /// Bit 11: The composite is designed to have the component offset scaled.
//     SCALED_COMPONENT_OFFSET = 0x0800,
//     /// Bit 12: The composite is designed not to have the component offset
//     /// scaled.
//     UNSCALED_COMPONENT_OFFSET = 0x1000,
//     /// Bits 4, 13, 14 and 15 are reserved: set to 0.
//     Reserved = 0xE010,
// };



// =============================================================================
//
// loca - Index to Location
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/loca>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6loca.html>
//
// =============================================================================

struct IndexToLocation (num_glyphs : U32) (glyph_data_start : Pos) {
    // TODO
};



// =============================================================================
//
// prep - Control Value Program
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/prep>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6prep.html>
//
// =============================================================================

struct ControlValueProgram (length : U32) {
    /// Set of instructions executed whenever point size or font or transformation change.
    // TODO: repeat to length?
    // TODO: constrain U8 by ControlValue table?
    instructions : Array length U8,
};



// =============================================================================
//
// gasp — Grid-fitting And Scan-conversion Procedure Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gasp>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6gasp.html>
//
// =============================================================================

struct GridFittingScanConversion {
    /// Version number (set to 1)
    version : { version : U16Be | nat_eq version 1 },
    /// Number of records to follow
    num_ranges : U16Be,
    /// Sorted by ppem
    gasp_ranges : Array num_ranges GridFittingScanConversionRange,
};

struct GridFittingScanConversionRange {
    /// Upper limit of range, in PPEM
    range_max_ppem : U16Be,
    /// Flags describing desired rasterizer behavior.
    range_gasp_behavior : U16Be,
};

// TODO:
// enum RangeGridFittingScanConversionBehavior {
//     /// Use gridfitting
//     GASP_GRIDFIT = 0x0001,
//     /// Use grayscale rendering
//     GASP_DOGRAY = 0x0002,
//     /// Use gridfitting with ClearType symmetric smoothing
//     /// Only supported in version 1 'gasp'
//     GASP_SYMMETRIC_GRIDFIT = 0x0004,
//     ///  smoothing along multiple axes with ClearType®
//     /// Only supported in version 1 'gasp'
//     GASP_SYMMETRIC_SMOOTHING = 0x0008,
//     /// Reserved flags — set to 0
//     Reserved = 0xFFF0,
// };



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

struct VerticalOrigin {
    /// Major version (starting at 1). Set to 1.
    major_version : U16Be,
    /// Minor version (starting at 0). Set to 0.
    minor_version : U16Be,
    /// The y coordinate of a glyph’s vertical origin, in the font’s design
    /// coordinate system, to be used if no entry is present for the glyph in
    /// the `vert_origin_y_metrics` array.
    default_vert_origin_y : S16Be,
    /// Number of elements in the `vert_origin_y_metrics` array.
    num_vert_origin_y_metrics : U16Be,
    /// Vertical origin Y mertics data
    vert_origin_y_metrics : Array num_vert_origin_y_metrics VerticalOriginYMetric,
};

struct VerticalOriginYMetric {
    /// Glyph index.
    glyph_index : U16Be,
    /// Y coordinate, in the font’s design coordinate system, of the vertical
    /// origin of glyph with index glyph_index.
    vert_origin_y : S16Be,
};



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
    version : { version : U16Be | nat_eq version 0 },
    /// Offset to the SVG Document List, from the start of the SVG table. Must be non-zero.
    offset_to_svg_document_list : Offset32Be start SvgDocumentList,
    /// Set to 0.
    // TODO: reserved : Reserved U32Be 0,
    reserved : Reserved U32Be,
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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt>
//
// =============================================================================

struct EmbeddedBitmapData {
    /// Major version of the EBDT table, = 2.
    major_version : { version : U16Be | nat_eq version 2 },
    /// Minor version of the EBDT table, = 0.
    minor_version : { version : U16Be | nat_eq version 0 },
    // TODO: array of GlyphBitmapDataFormat[1-9], based off "EBLC" table
};

struct BigGlyphMetrics {
    height : U8,
    width : U8,
    horizontal_bearing_x : S8,
    horizontal_bearing_y : S8,
    horizontal_advance : U8,
    vertical_bearing_x : S8,
    vertical_bearing_y : S8,
    vertical_advance : U8,
};

// Note: bearing direction depends on the `flags` field in the `BitmapSize` tables
// within the EBLC table
struct SmallGlyphMetrics {
    height : U8,
    width : U8,
    bearing_x : S8,
    bearing_y : S8,
    advance : U8,
};


// -----------------------------------------------------------------------------
// Format 1: small metrics, byte-aligned data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-1-small-metrics-byte-aligned-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat1 {
    /// Metrics information for the glyph
    small_metrics : SmallGlyphMetrics,
    /// Byte-aligned bitmap data
    image_data : VArray U8, // TODO
};


// -----------------------------------------------------------------------------
// Format 2: small metrics, bit-aligned data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-2-small-metrics-bit-aligned-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat2 {
    /// Metrics information for the glyph
    small_metrics : SmallGlyphMetrics,
    /// Bit-aligned bitmap data
    image_data : VArray U8, // TODO
};


// -----------------------------------------------------------------------------
// Format 3: (obsolete)
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-3-obsolete>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat3 {};


// -----------------------------------------------------------------------------
// Format 4: (not supported) metrics in EBLC, compressed data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-4-not-supported-metrics-in-eblc-compressed-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat4 {};


// -----------------------------------------------------------------------------
// Format 5: metrics in EBLC, bit-aligned image data only
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-5-metrics-in-eblc-bit-aligned-image-data-only>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat5 {
    /// Bit-aligned bitmap data
    image_data : VArray U8, // TODO
};


// -----------------------------------------------------------------------------
// Format 6: big metrics, byte-aligned data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-6-big-metrics-byte-aligned-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat6 {
    /// Metrics information for the glyph
    big_metrics : BigGlyphMetrics,
    /// Byte-aligned bitmap data
    image_data : VArray U8, // TODO
};


// -----------------------------------------------------------------------------
// Format 7: big metrics, bit-aligned data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format7-big-metrics-bit-aligned-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat7 {
    /// Metrics information for the glyph
    big_metrics : BigGlyphMetrics,
    /// Bit-aligned bitmap data
    image_data : VArray U8, // TODO
};


// -----------------------------------------------------------------------------
// EbdtComponent Record
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#ebdtcomponent-record>
// -----------------------------------------------------------------------------

struct EmbeddedBitmapDataComponent {
    /// Component glyph ID
    glyph_id : U16Be,
    /// Position of component left
    x_offset : S8,
    /// Position of component top
    y_offset : S8,
};


// -----------------------------------------------------------------------------
// Format 8: small metrics, component data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-8-small-metrics-component-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat8 {
    /// Metrics information for the glyph
    small_metrics : SmallGlyphMetrics,
    /// Pad to 16-bit boundary
    pad : Reserved U8,
    /// Number of components
    num_components : U16Be,
    /// Array of EmbeddedBitmapDataComponent records
    components : Array num_components EmbeddedBitmapDataComponent,
};


// -----------------------------------------------------------------------------
// Format 9: big metrics, component data
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebdt#format-9-big-metrics-component-data>
// -----------------------------------------------------------------------------

struct GlyphBitmapDataFormat9 {
    /// Metrics information for the glyph
    big_metrics : BigGlyphMetrics,
    /// Number of components
    num_components : U16Be,
    /// Array of EmbeddedBitmapDataComponent records
    components : Array num_components EmbeddedBitmapDataComponent,
};



// =============================================================================
//
// EBLC - Embedded Bitmap Location Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc>
//
// =============================================================================

struct EmbeddedBitmapLocationData {
    start : Pos,
    /// Major version of the EBLC table, = 2.
    major_version : { version : U16Be | nat_eq version 2 },
    /// Minor version of the EBLC table, = 0.
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Number of `BitmapSize` tables.
    num_sizes : U32Be,
    /// Array of `BitmapSize` tables.
    bitmap_sizes : Array num_sizes (BitmapSize start),
};


// -----------------------------------------------------------------------------
// BitmapSize Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#bitmapsize-table>
// -----------------------------------------------------------------------------

struct BitmapSize (embedded_bitmap_location_start : Pos) {
    start : Pos,
    /// Offset to IndexSubtableArray, from beginning of EBLC.
    index_sub_table_array_offset : Offset32Be embedded_bitmap_location_start (IndexSubtableArray start),
    /// Number of bytes in corresponding index subtables and array.
    index_tables_size : U32Be,
    /// There is an IndexSubtable for each range or format change.
    number_of_index_sub_tables : U32Be,
    /// Not used; set to 0.
    color_ref : U32Be,
    /// Line metrics for text rendered horizontally.
    hori : ScalarBitmapLineMetrics,
    /// Line metrics for text rendered vertically.
    vert : ScalarBitmapLineMetrics,
    /// Lowest glyph index for this size.
    start_glyph_index : U16Be,
    /// Highest glyph index for this size.
    end_glyph_index : U16Be,
    /// Horizontal pixels per em.
    ppem_x : U8,
    /// Vertical pixels per em.
    ppem_y : U8,
    /// The Microsoft rasterizer v.1.7 or greater supports the following
    /// `bit_depth` values, as described below: 1, 2, 4, and 8.
    // TODO: bit_depth : BitDepth,
    bit_depth : U8,
    /// Vertical or horizontal (see Bitmap Flags, below).
    // TODO: flags : BitmapFlags,
    flags : S8,
};


// -----------------------------------------------------------------------------
// SbitLineMetrics
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#bitmapsize-table>
// -----------------------------------------------------------------------------

struct ScalarBitmapLineMetrics {
    ascender : S8,
    descender : S8,
    width_max : U8,
    caret_slope_numerator : S8,
    caret_slope_denominator : S8,
    caret_offset : S8,
    min_origin_sb : S8,
    min_advance_sb : S8,
    max_before_bl : S8,
    min_after_bl : S8,
    pad1 : Reserved S8,
    pad2 : Reserved S8,
};


// -----------------------------------------------------------------------------
// Bit Depth
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#bit-depth>
// -----------------------------------------------------------------------------

// TODO:
// enum BitDepth : U8 {
//     /// black/white
//     BLACK_AND_WHITE = 1,
//     /// 4 levels of gray
//     GRAYSCALE_4 = 2,
//     /// 16 levels of gray
//     GRAYSCALE_16 = 4,
//     /// 256 levels of gray
//     GRAYSCALE_256 = 8,
// };


// -----------------------------------------------------------------------------
// Bitmap Flags
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#bitmap-flags>
// -----------------------------------------------------------------------------

// TODO:
// enum BitmapFlags : S8 {
//     /// Horizontal
//     HORIZONTAL_METRICS = 0x01,
//     /// Vertical
//     VERTICAL_METRICS = 0x02,
//     /// For future use — set to 0.
//     Reserved = 0xFC,
// };


// -----------------------------------------------------------------------------
// BigGlyphMetrics
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#bigglyphmetrics>
// -----------------------------------------------------------------------------

// NOTE: Already defined with EBDT definitions
// struct BigGlyphMetrics {
//     height : U8,
//     width : U8,
//     horizontal_bearing_x : S8,
//     horizontal_bearing_y : S8,
//     horizontal_advance : U8,
//     vertical_bearing_x : S8,
//     vertical_bearing_y : S8,
//     vertical_advance : U8,
// };


// -----------------------------------------------------------------------------
// SmallGlyphMetrics
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#smallglyphmetrics>
// -----------------------------------------------------------------------------

// NOTE: Already defined with EBDT definitions
// struct SmallGlyphMetrics {
//     height : U8,
//     width : U8,
//     bearing_x : S8,
//     bearing_y : S8,
//     advance : U8,
// };


// -----------------------------------------------------------------------------
// IndexSubTableArray
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtablearray>
// -----------------------------------------------------------------------------

struct IndexSubtableArray (bitmap_size_start : Pos) {
    /// First glyph ID of this range.
    first_glyph_index : U16Be,
    /// Last glyph ID of this range (inclusive).
    last_glyph_index : U16Be,
    /// Add to `index_sub_table_array_offset` to get offset from beginning of EBLC.
    // TODO: additional_offset_to_index_subtable : Offset32Be bitmap_size_start IndexSubTable,
    additional_offset_to_index_subtable : Offset32Be bitmap_size_start Unknown,
};


// -----------------------------------------------------------------------------
// IndexSubHeader
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubheader>
// -----------------------------------------------------------------------------

struct IndexSubHeader (embedded_bitmap_data_start : Pos) {
    /// Format of this IndexSubTable.
    index_format : U16Be,
    /// Format of EBDT image data.
    image_format : U16Be,
    /// Offset to image data in EBDT table.
    image_data_offset : Offset32Be embedded_bitmap_data_start Unknown, // TODO
};


// -----------------------------------------------------------------------------
// IndexSubTables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtables>
// -----------------------------------------------------------------------------

union IndexSubTable (embedded_bitmap_data_start : Pos) {
    IndexSubTable1 embedded_bitmap_data_start,
    IndexSubTable2 embedded_bitmap_data_start,
    IndexSubTable3 embedded_bitmap_data_start,
    IndexSubTable4 embedded_bitmap_data_start,
    IndexSubTable5 embedded_bitmap_data_start,
};

/// IndexSubTable1: variable-metrics glyphs with 4-byte offsets
struct IndexSubTable1 (embedded_bitmap_data_start : Pos) {
    /// Header info.
    header : IndexSubHeader embedded_bitmap_data_start,
    /// offsetArray[glyphIndex] + imageDataOffset = glyphData sizeOfArray = (lastGlyph - firstGlyph + 1) + 1 + 1 pad if needed
    offset_array : VArray U32Be, // TODO: Offset and length?
};

/// IndexSubTable2: all glyphs have identical metrics
struct IndexSubTable2 (embedded_bitmap_data_start : Pos) {
    /// Header info.
    header : IndexSubHeader embedded_bitmap_data_start,
    /// All the glyphs are of the same size.
    image_size : U32Be,
    /// All glyphs have the same metrics; glyph data may be compressed, byte-aligned, or bit-aligned.
    big_metrics : BigGlyphMetrics,
};

/// IndexSubTable3: variable-metrics glyphs with 2-byte offsets
struct IndexSubTable3 (embedded_bitmap_data_start : Pos) {
    /// Header info.
    header : IndexSubHeader embedded_bitmap_data_start,
    /// offsetArray[glyphIndex] + imageDataOffset = glyphData sizeOfArray = (lastGlyph - firstGlyph + 1) + 1 + 1 pad if needed
    offset_array : VArray U16Be, // TODO: Offset and length?
};

/// IndexSubTable4: variable-metrics glyphs with sparse glyph codes
struct IndexSubTable4 (embedded_bitmap_data_start : Pos) {
    /// Header info.
    header : IndexSubHeader embedded_bitmap_data_start,
    /// Array length.
    num_glyphs : U32Be,
    /// One per glyph.
    glyph_array : Array (nat_add num_glyphs 1) (GlyphIdOffsetPair embedded_bitmap_data_start),
};

struct GlyphIdOffsetPair (embedded_bitmap_data_start : Pos) {
    /// Glyph ID of glyph present.
    glyph_id : U16Be,
    /// Location in EBDT.
    offset : Offset16Be embedded_bitmap_data_start Unknown, // TODO
};

/// IndexSubTable5: constant-metrics glyphs with sparse glyph codes
struct IndexSubTable5 (embedded_bitmap_data_start : Pos) {
    /// Header info.
    header : IndexSubHeader embedded_bitmap_data_start,
    /// All glyphs have the same data size.
    image_size : U32Be,
    /// All glyphs have the same metrics.
    big_metrics : BigGlyphMetrics,
    /// Array length.
    num_glyphs : U32Be,
    /// One per glyph, sorted by glyph ID.
    glyph_id_array : Array num_glyphs U16Be,
};



// =============================================================================
//
// EBSC - Embedded Bitmap Scaling Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebsc>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6EBSC.html>
//
// =============================================================================

struct EmbeddedBitmapScalingData {
    /// Major version of the EBSC table, = 2.
    major_version : { version : U16Be | nat_eq version 2 },
    /// Minor version of the EBSC table, = 0.
    minor_version : { version : U16Be | nat_eq version 0 },
    /// Number of `BitmapScale` tables
    num_sizes : U32Be,
    /// `BitmapScale` table array
    scales : Array num_sizes BitmapScale,
};


// -----------------------------------------------------------------------------
// BitmapScale Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ebsc#bitmapscale-table>
// -----------------------------------------------------------------------------

struct BitmapScale {
    /// line metrics
    horizontal : ScalarBitmapLineMetrics,
    /// line metrics
    vertical : ScalarBitmapLineMetrics,
    /// target horizontal pixels per Em
    ppem_x : U8,
    /// target vertical pixels per Em
    ppem_y : U8,
    /// use bitmaps of this size
    substitute_ppem_x : U8,
    /// use bitmaps of this size
    substitute_ppem_y : U8,
};



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

struct BaselineData {
    start : Pos,
    /// Major version of the BASE table, = 1
    major_version : { version : U16Be | nat_eq version 1 },
    /// Minor version of the BASE table, = 0
    minor_version : { version : U16Be | nat_eq version 0 },

    // FIXME: Proper version switching

    // BASE Header, Version 1.0

    /// Offset to horizontal Axis table, from beginning of BASE table (may be NULL)
    horizontal_axis_offset : Offset16Be start Axis,
    /// Offset to vertical Axis table, from beginning of BASE table (may be NULL)
    vertical_axis_offset : Offset16Be start Axis,

    // BASE Header, Version 1.1

    /// Offset to Item Variation Store table, from beginning of BASE table (may be null)
    item_var_store_offset : Offset32Be start Unknown, // TODO: font variation common formats
};


// -----------------------------------------------------------------------------
//
// Axis Tables: HorizAxis and VertAxis
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#axis-tables-horizaxis-and-vertaxis>
//
// -----------------------------------------------------------------------------

struct Axis {
    start : Pos,
    /// Offset to BaseTagList table, from beginning of Axis table (may be NULL)
    base_tag_list_offset : Offset16Be start BaseTagList,
    /// Offset to BaseScriptList table, from beginning of Axis table
    base_script_list_offset : Offset16Be start BaseScriptList,
};


// -----------------------------------------------------------------------------
// BaseTagList Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#basetaglist-table>
// -----------------------------------------------------------------------------

struct BaseTagList {
    /// Number of baseline identification tags in this text direction — may be zero (0)
    base_tag_count : U16Be,
    /// Array of 4-byte baseline identification tags — must be in alphabetical order
    baseline_tags : Array base_tag_count Tag, // TODO: alphabetical order
};


// -----------------------------------------------------------------------------
// BaseScriptList Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#basescriptlist-table>
// -----------------------------------------------------------------------------

struct BaseScriptList {
    start : Pos,
    /// 4-byte script identification tag
    base_script_tag : Tag,
    /// Offset to BaseScript table, from beginning of BaseScriptList
    base_script_offset : Offset16Be start BaseScript,
};


// -----------------------------------------------------------------------------
// BaseScript Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#basescript-table>
// -----------------------------------------------------------------------------

struct BaseScript {
    start : Pos,
    /// Offset to BaseValues table, from beginning of BaseScript table (may be NULL)
    base_values_offset : Offset16Be start BaseValues,
    /// Offset to MinMax table, from beginning of BaseScript table (may be NULL)
    default_min_max_offset : Offset16Be start MinMax,
    /// Number of BaseLangSysRecords defined — may be zero (0)
    base_lang_sys_count : U16Be,
    /// Array of BaseLangSysRecords, in alphabetical order by BaseLangSysTag
    base_lang_sys_records : Array base_lang_sys_count (BaseLangSysRecord start), // TODO: alphabetical order
};


// -----------------------------------------------------------------------------
// BaseLangSysRecord
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#baselangsysrecord>
// -----------------------------------------------------------------------------

struct BaseLangSysRecord (base_script_start : Pos) {
    /// 4-byte language system identification tag
    base_lang_sys_tag : Tag,
    /// Offset to MinMax table, from beginning of BaseScript table
    min_max_offset : Offset16Be base_script_start MinMax,
};


// -----------------------------------------------------------------------------
// BaseValues Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#basevalues-table>
// -----------------------------------------------------------------------------

struct BaseValues {
    start : Pos,
    /// Index number of default baseline for this script — equals index position
    /// of baseline tag in baselineTags array of the BaseTagList
    default_baseline_index : U16Be,
    /// Number of BaseCoord tables defined — should equal baseTagCount in the BaseTagList
    base_coord_count : U16Be,
    /// Array of offsets to BaseCoord tables, from beginning of BaseValues table
    /// — order matches `baseline_tags` array in the BaseTagList
    base_coords : Array base_coord_count (Offset16Be start BaseCoord), // TODO: order matches `baseline_tags` array
};


// -----------------------------------------------------------------------------
// MinMax table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#minmax-table>
// -----------------------------------------------------------------------------

struct MinMax {
    start : Pos,
    /// Offset to BaseCoord table that defines the minimum extent value, from
    /// the beginning of MinMax table (may be NULL)
    min_coord : Offset16Be start BaseCoord,
    /// Offset to BaseCoord table that defines maximum extent value, from the
    /// beginning of MinMax table (may be NULL)
    max_coord : Offset16Be start BaseCoord,
    /// Number of FeatMinMaxRecords — may be zero (0)
    feat_min_max_count : U16Be,
    /// Array of FeatMinMaxRecords, in alphabetical order by featureTableTag
    feat_min_max_records : Array feat_min_max_count (FeatMinMaxRecord start), // TODO: alphabetical order
};


// -----------------------------------------------------------------------------
// FeatMinMaxRecord
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#featminmaxrecord>
// -----------------------------------------------------------------------------

struct FeatMinMaxRecord (min_max_start : Pos) {
    /// 4-byte feature identification tag — must match feature tag in FeatureList
    feature_table_tag : Tag,
    /// Offset to BaseCoord table that defines the minimum extent value, from
    /// beginning of MinMax table (may be NULL)
    min_coord : Offset16Be min_max_start BaseCoord,
    /// Offset to BaseCoord table that defines the maximum extent value, from
    /// beginning of MinMax table (may be NULL)
    max_coord : Offset16Be min_max_start BaseCoord,
};


// -----------------------------------------------------------------------------
//
// BaseCoord Tables
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/base#basecoord-tables>
//
// -----------------------------------------------------------------------------

union BaseCoord {
    BaseCoordFormat1,
    BaseCoordFormat2,
    BaseCoordFormat3,
};

struct BaseCoordFormat1 {
    /// Format identifier — format = 1
    base_coord_format : { format : U16Be | nat_eq format 1 },
    /// X or Y value, in design units
    coordinate : S16Be,
};

struct BaseCoordFormat2 {
    /// Format identifier — format = 2
    base_coord_format : { format : U16Be | nat_eq format 2 },
    /// X or Y value, in design units
    coordinate : S16Be,
    /// Glyph ID of control glyph
    reference_glyph : U16Be,
    /// Index of contour point on the reference glyph
    base_coord_point : U16Be,
};

struct BaseCoordFormat3 {
    start : Pos,
    /// Format identifier — format = 3
    base_coord_format : { format : U16Be | nat_eq format 3 },
    /// X or Y value, in design units
    coordinate : S16Be,
    /// Offset to Device table (non-variable font) / Variation Index table
    /// (variable font) for X or Y value, from beginning of BaseCoord table
    /// (may be NULL).
    device_table : Offset16Be start Device,
};



// =============================================================================
//
// GDEF — Glyph Definition Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef>
//
// =============================================================================


// -----------------------------------------------------------------------------
//
// GDEF Header
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#gdef-header>
//
// -----------------------------------------------------------------------------

/// GDEF Header
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#gdef-header>
struct GlyphDefinitionData {
    start : Pos,
    /// Major version of the GDEF table, = 1
    major_version : U16Be, // TODO: constrain version
    /// Minor version of the GDEF table, = 3
    minor_version : U16Be, // TODO: constrain version

    // FIXME: Proper version switching

    // GDEF Header, Version 1.0
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_10>

    /// Offset to class definition table for glyph type, from beginning of GDEF header (may be NULL)
    glyph_class_def_offset : Offset16Be start GlyphClassDef,
    /// Offset to attachment point list table, from beginning of GDEF header (may be NULL)
    attach_list_offset : Offset16Be start AttachList,
    /// Offset to ligature caret list table, from beginning of GDEF header (may be NULL)
    lig_caret_list_offset : Offset16Be start LigCaretList,
    /// Offset to class definition table for mark attachment type, from beginning of GDEF header (may be NULL)
    mark_attach_class_def_offset : Offset16Be start MarkAttachClassDef,

    // GDEF Header, Version 1.2
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_12>

    /// Offset to the table of mark glyph set definitions, from beginning of GDEF header (may be NULL)
    mark_glyph_sets_def_offset : Offset16Be start MarkGlyphSets,

    // GDEF Header, Version 1.3
    // <https://www.microsoft.com/typography/otspec/gdef.htm#gdefHeader_12>

    /// Offset to the Item Variation Store table, from beginning of GDEF header (may be NULL)
    item_var_store_offset : Offset32Be start Unknown, // TODO
};


// -----------------------------------------------------------------------------
// Glyph Class Definition Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#glyph-class-definition-table>
// -----------------------------------------------------------------------------

GlyphClassDefEnum = U16Be; // TODO: Enumerations

// enum GlyphClassDefEnum : U16Be {
//     /// Base glyph (single character, spacing glyph)
//     BASE_GLYPH = 1,
//     /// Ligature glyph (multiple character, spacing glyph)
//     LIGATURE_GLYPH = 2,
//     /// Mark glyph (non-spacing combining glyph)
//     MARK_GLYPH = 3,
//     /// Component glyph (part of single character, spacing glyph)
//     COMPONENT_GLYPH = 4,
// };

GlyphClassDef = ClassDef GlyphClassDefEnum;


// -----------------------------------------------------------------------------
// Attachment Point List Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#attachment-point-list-table>
// -----------------------------------------------------------------------------

struct AttachList {
    start : Pos,
    /// Offset to Coverage table - from beginning of AttachList table
    coverage_offset : Offset16Be start Coverage,
    /// Number of glyphs with attachment points
    glyph_count : U16Be,
    /// Array of offsets to AttachPoint tables-from beginning of AttachList
    /// table-in Coverage Index order
    attach_point_offsets : Array glyph_count (Offset16Be start AttachPoint),
};

struct AttachPoint {
    /// Number of attachment points on this glyph
    point_count : U16Be,
    /// Array of contour point indices -in increasing numerical order
    point_indices : Array point_count U16Be,
};


// -----------------------------------------------------------------------------
// Ligature Caret List Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#ligature-caret-list-table>
// -----------------------------------------------------------------------------

struct LigCaretList {
    start : Pos,
    /// Offset to Coverage table - from beginning of LigCaretList table
    coverage_offset : Offset16Be start Coverage,
    /// Number of ligature glyphs
    lig_glyph_count : U16Be,
    /// Array of offsets to LigGlyph tables, from beginning of LigCaretList
    /// table —in Coverage Index order
    lig_glyph_offsets : Array lig_glyph_count (Offset16Be start LigGlyph),
};

struct LigGlyph {
    start : Pos,
    /// Number of CaretValue tables for this ligature (components - 1)
    caret_count : U16Be,
    /// Array of offsets to CaretValue tables, from beginning of LigGlyph
    /// table — in increasing coordinate order
    caret_value_offsets : Array caret_count (Offset16Be start CaretValue),
};

/// Caret Value Tables
union CaretValue {
    CaretValueFormat1,
    CaretValueFormat2,
    CaretValueFormat3,
};

/// CaretValue Format 1: Design units only
struct CaretValueFormat1 {
    /// Format identifier: format = 1
    caret_value_format : { format : U16Be | nat_eq format 1 },
    /// X or Y value, in design units
    coordinate : S16Be,
};

/// CaretValue Format 2: Contour point
struct CaretValueFormat2 {
    /// Format identifier: format = 2
    caret_value_format : { format : U16Be | nat_eq format 2 },
    /// Contour point index on glyph
    caret_value_point_index : U16Be,
};

/// Caret Value Format 3: Design units plus Device or VariationIndex table
struct CaretValueFormat3 {
    start : Pos,
    /// Format identifier: format = 3
    caret_value_format : { format : U16Be | nat_eq format 3 },
    /// X or Y value, in design units
    coordinate : S16Be,
    /// Offset to Device table (non-variable font) / Variation Index table
    /// (variable font) for X or Y value-from beginning of CaretValue table
    device_offset : Offset16Be start Device,
};


// -----------------------------------------------------------------------------
// Mark Attachment Class Definition Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#mark-attachment-class-definition-table>
// -----------------------------------------------------------------------------

MarkAttachClassDef = ClassDef U16Be; // TODO: Figure out what the class format is


// -----------------------------------------------------------------------------
// Mark Glyph Sets Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#mark-glyph-sets-table>
// -----------------------------------------------------------------------------

union MarkGlyphSets {
    MarkGlyphSetsFormat1,
};

struct MarkGlyphSetsFormat1 {
    start : Pos,
    /// Format identifier == 1
    mark_glyph_set_table_format : { format : U16Be | nat_eq format 1 },
    /// Number of mark glyph sets defined
    mark_glyph_set_count : U16Be,
    /// Array of offsets to mark glyph set coverage tables.
    // FIXME: offset start is not defined in the spec, guessing here!
    coverage_offsets : Array mark_glyph_set_count (Offset32Be start Coverage),
};


// -----------------------------------------------------------------------------
// Item Variation Store Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#item-variation-store-table>
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
struct GlyphPositioningData {
    start : Pos,
    /// Major version of the GPOS table
    major_version : U16Be, // TODO: constrain version
    /// Minor version of the GPOS table
    minor_version : U16Be, // TODO: constrain version

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
struct GlyphSubstitutionData {
    start : Pos,
    /// Major version of the GSUB table
    major_version : U16Be, // TODO: constrain version
    /// Minor version of the GSUB table
    minor_version : U16Be, // TODO: constrain version

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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/jstf>
//
// =============================================================================

struct JustificationData {
    start : Pos,
    /// Major version of the JSTF table, = 1
    major_version : U16Be, // TODO: constrain version
    /// Minor version of the JSTF table, = 0
    minor_version : U16Be, // TODO: constrain version
    /// Number of JstfScriptRecords in this table
    justification_script_count : U16Be,
    /// Array of JustificationScriptRecords, in alphabetical order by
    /// `justification_script_tag`
    justification_script_records : Array justification_script_count (JustificationScriptRecord start), // TODO: alphabetical order
};

struct JustificationScriptRecord (justification_data_start : Pos) {
    /// 4-byte JustificationScript identification
    justification_script_tag : Tag,
    /// Offset to JustificationScript table, from beginning of JSTF Header
    justification_script_offset : Offset16Be justification_data_start JustificationScript,
};

struct JustificationScript {
    start : Pos,
    /// Offset to ExtenderGlyph table, from beginning of JustificationScript
    /// table (may be NULL)
    extender_glyph_offset : Offset16Be start ExtenderGlyph,
    /// Offset to default JustificationLangSys table, from beginning of
    /// JustificationScript table (may be NULL)
    def_justification_lang_sys_offset : Offset16Be start JustificationLangSys,
    /// Number of JustificationLangSysRecords in this table - may be zero (0)
    justification_lang_sys_count : U16Be,
    /// Array of JustificationLangSysRecords, in alphabetical order by
    /// JustificationLangSysTag
    justification_lang_sys_records : Array justification_lang_sys_count (JustificationLangSysRecord start), // TODO: alphabetical order
};

struct JustificationLangSysRecord (justification_script_start : Pos) {
    /// 4-byte JustificationLangSys identifier
    justification_lang_sys_tag : Tag,
    /// Offset to JustificationLangSys table, from beginning of
    /// JustificationScript table
    justification_lang_sys_offset : Offset16Be justification_script_start JustificationLangSys,
};

struct ExtenderGlyph {
    /// Number of extender glyphs in this script
    glyph_count : U16Be,
    /// Extender glyph IDs — in increasing numerical order
    extender_glyphs : Array glyph_count U16Be, // TODO: numerical order
};

struct JustificationLangSys {
    start : Pos,
    /// Number of JustificationPriority tables
    justification_priority_count : U16Be,
    /// Array of offsets to JustificationPriority tables, from beginning of
    /// JustificationLangSys table, in priority order
    justification_priority_offsets : Array justification_priority_count (Offset16Be start JustificationPriority), // TODO: priority order
};

struct JustificationPriority {
    start : Pos,
    /// Offset to shrinkage-enable JustificationGlyphPositionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    shrinkage_enable_gsub : Offset16Be start JustificationGlyphPositionModList,
    /// Offset to shrinkage-disable JustificationGlyphPositionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    shrinkage_disable_gsub : Offset16Be start JustificationGlyphPositionModList,
    /// Offset to shrinkage-enable JustificationGlyphSubstitutionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    shrinkage_enable_gpos : Offset16Be start JustificationGlyphSubstitutionModList,
    /// Offset to shrinkage-disable JustificationGlyphSubstitutionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    shrinkage_disable_gpos : Offset16Be start JustificationGlyphSubstitutionModList,
    /// Offset to shrinkage JustificationMax table, from beginning of
    /// JustificationPriority table (may be NULL)
    shrinkage_justification_max : Offset16Be start JustificationMax,
    /// Offset to extension-enable JustificationGlyphPositionModList table,
    /// from beginnning of JustificationPriority table (may be NULL)
    extension_enable_gsub : Offset16Be start JustificationGlyphPositionModList,
    /// Offset to extension-disable JustificationGlyphPositionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    extension_disable_gsub : Offset16Be start JustificationGlyphPositionModList,
    /// Offset to extension-enable JustificationGlyphSubstitutionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    extension_enable_gpos : Offset16Be start JustificationGlyphSubstitutionModList,
    /// Offset to extension-disable JustificationGlyphSubstitutionModList table,
    /// from beginning of JustificationPriority table (may be NULL)
    extension_disable_gpos : Offset16Be start JustificationGlyphSubstitutionModList,
    /// Offset to extension JustificationMax table, from beginning of
    /// JustificationPriority table (may be NULL)
    extension_justification_max : Offset16Be start JustificationMax,
};

struct JustificationGlyphSubstitutionModList {
    /// Number of lookups for this modification
    lookup_count : U16Be,
    /// Array of Lookup indices into the GlyphSubstitution LookupList, in increasing
    /// numerical order
    gsub_lookup_indices : Array lookup_count U16Be, // TODO: numerical order
};

struct JustificationGlyphPositionModList {
    /// Number of lookups for this modification
    lookup_count : U16Be,
    /// Array of Lookup indices into the GlyphPosition LookupList, in increasing
    /// numerical order
    gpos_lookup_indices : Array lookup_count U16Be, // TODO: numerical order
};

struct JustificationMax {
    start : Pos,
    /// Number of lookup Indices for this modification
    lookup_count : U16Be,
    /// Array of offsets to GPOS-type lookup tables, from beginning of
    /// JustificationMax table, in design order
    lookup_offsets : Array lookup_count (Offset16Be start Unknown), // TODO: design order, "GPOS-type lookup tables"???
};



// =============================================================================
//
// MATH - The mathematical typesetting table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/math>
//
// =============================================================================

struct MathLayoutData {
    start : Pos,
    /// Major version of the MATH table, = 1.
    major_version : U16Be, // TODO: constrain version
    /// Minor version of the MATH table, = 0.
    minor_version : U16Be, // TODO: constrain version
    /// Offset to MathConstants table - from the beginning of MATH table.
    math_constants_offset : Offset16Be start MathConstants,
    /// Offset to MathGlyphInfo table - from the beginning of MATH table.
    math_glyph_info_offset : Offset16Be start MathGlyphInfo,
    /// Offset to MathVariants table - from the beginning of MATH table.
    math_variants_offset : Offset16Be start MathVariants,
};

struct MathValueRecord (parent_start : Pos) {
    /// The X or Y value in design units
    value : S16Be,
    /// Offset to the device table – from the beginning of parent table. May be
    /// NULL. Suggested format for device table is 1.
    device_table_offset : Offset16Be parent_start Device,
};

struct MathConstants {
    start : Pos,
    /// Percentage of scaling down for level 1 superscripts and subscripts.
    /// Suggested value: 80%.
    script_percent_scale_down : S16Be,
    /// Percentage of scaling down for level 2 (scriptScript) superscripts and
    /// subscripts. Suggested value: 60%.
    script_script_percent_scale_down : S16Be,
    /// Minimum height required for a delimited expression (contained within
    /// parentheses, etc.) to be treated as a sub-formula. Suggested value:
    /// normal line height × 1.5.
    delimited_sub_formula_min_height : U16Be,
    /// Minimum height of n-ary operators (such as integral and summation) for
    /// formulas in display mode (that is, appearing as standalone page
    /// elements, not embedded inline within text).
    display_operator_min_height : U16Be,
    /// White space to be left between math formulas to ensure proper line
    /// spacing. For example, for applications that treat line gap as a part of
    /// line ascender, formulas with ink going above (os2.sTypoAscender +
    /// os2.sTypoLineGap - MathLeading) or with ink going below
    /// os2.sTypoDescender will result in increasing line height.
    math_leading : MathValueRecord start,
    /// Axis height of the font.
    ///
    /// In math typesetting, the term axis refers to a horizontal reference line
    /// used for positioning elements in a formula. The math axis is similar to
    /// but distinct from the baseline for regular text layout. For example, in
    /// a simple equation, a minus symbol or fraction rule would be on the axis,
    /// but a string for a variable name would be set on a baseline that is
    /// offset from the axis. The axisHeight value determines the amount of that
    /// offset.
    axis_height : MathValueRecord start,
    /// Maximum (ink) height of accent base that does not require raising the
    /// accents. Suggested: x‑height of the font (os2.sxHeight) plus any
    /// possible overshots.
    accent_base_height : MathValueRecord start,
    /// Maximum (ink) height of accent base that does not require flattening the
    /// accents. Suggested: cap height of the font (os2.sCapHeight).
    flattened_accent_base_height : MathValueRecord start,
    /// The standard shift down applied to subscript elements. Positive for
    /// moving in the downward direction. Suggested: os2.ySubscriptYOffset.
    subscript_shift_down : MathValueRecord start,
    /// Maximum allowed height of the (ink) top of subscripts that does not
    /// require moving subscripts further down. Suggested: 4/5 x- height.
    subscript_top_max : MathValueRecord start,
    /// Minimum allowed drop of the baseline of subscripts relative to the (ink)
    /// bottom of the base. Checked for bases that are treated as a box or
    /// extended shape. Positive for subscript baseline dropped below the base
    /// bottom.
    subscript_baseline_drop_min : MathValueRecord start,
    /// Standard shift up applied to superscript elements.
    /// Suggested: os2.ySuperscriptYOffset.
    superscript_shift_up : MathValueRecord start,
    /// Standard shift of superscripts relative to the base, in cramped style.
    superscript_shift_up_cramped : MathValueRecord start,
    /// Minimum allowed height of the (ink) bottom of superscripts that does not
    /// require moving subscripts further up. Suggested: ¼ x-height.
    superscript_bottom_min : MathValueRecord start,
    /// Maximum allowed drop of the baseline of superscripts relative to the
    /// (ink) top of the base. Checked for bases that are treated as a box or
    /// extended shape. Positive for superscript baseline below the base top.
    superscript_baseline_drop_max : MathValueRecord start,
    /// Minimum gap between the superscript and subscript ink. Suggested:
    /// 4 × default rule thickness.
    sub_superscript_gap_min : MathValueRecord start,
    /// The maximum level to which the (ink) bottom of superscript can be
    /// pushed to increase the gap between superscript and subscript, before
    /// subscript starts being moved down. Suggested: 4/5 x-height.
    superscript_bottom_max_with_subscript : MathValueRecord start,
    /// Extra white space to be added after each subscript and superscript.
    /// Suggested: 0.5 pt for a 12 pt font.
    space_after_script : MathValueRecord start,
    /// Minimum gap between the (ink) bottom of the upper limit, and the (ink)
    /// top of the base operator.
    upper_limit_gap_min : MathValueRecord start,
    /// Minimum distance between baseline of upper limit and (ink) top of the
    /// base operator.
    upper_limit_baseline_rise_min : MathValueRecord start,
    /// Minimum gap between (ink) top of the lower limit, and (ink) bottom of
    /// the base operator.
    lower_limit_gap_min : MathValueRecord start,
    /// Minimum distance between baseline of the lower limit and (ink) bottom of
    /// the base operator.
    lower_limit_baseline_drop_min : MathValueRecord start,
    /// Standard shift up applied to the top element of a stack.
    stack_top_shift_up : MathValueRecord start,
    /// Standard shift up applied to the top element of a stack in display
    /// style.
    stack_top_display_style_shift_up : MathValueRecord start,
    /// Standard shift down applied to the bottom element of a stack. Positive
    /// for moving in the downward direction.
    stack_bottom_shift_down : MathValueRecord start,
    /// Standard shift down applied to the bottom element of a stack in display
    /// style. Positive for moving in the downward direction.
    stack_bottom_display_style_shift_down : MathValueRecord start,
    /// Minimum gap between (ink) bottom of the top element of a stack, and the
    /// (ink) top of the bottom element. Suggested: 3 × default rule thickness.
    stack_gap_min : MathValueRecord start,
    /// Minimum gap between (ink) bottom of the top element of a stack, and the
    /// (ink) top of the bottom element in display style. Suggested: 7 × default
    /// rule thickness.
    stack_display_style_gap_min : MathValueRecord start,
    /// Standard shift up applied to the top element of the stretch stack.
    stretch_stack_top_shift_up : MathValueRecord start,
    /// Standard shift down applied to the bottom element of the stretch stack.
    /// Positive for moving in the downward direction.
    stretch_stack_bottom_shift_down : MathValueRecord start,
    /// Minimum gap between the ink of the stretched element, and the (ink)
    /// bottom of the element above. Suggested: same value as upperLimitGapMin.
    stretch_stack_gap_above_min : MathValueRecord start,
    /// Minimum gap between the ink of the stretched element, and the (ink)
    /// top of the element below. Suggested: same value as lowerLimitGapMin.
    stretch_stack_gap_below_min : MathValueRecord start,
    /// Standard shift up applied to the numerator.
    fraction_numerator_shift_up : MathValueRecord start,
    /// Standard shift up applied to the numerator in display style.
    /// Suggested: same value as stackTopDisplayStyleShiftUp.
    fraction_numerator_display_style_shift_up : MathValueRecord start,
    /// Standard shift down applied to the denominator. Positive for moving in
    /// the downward direction.
    fraction_denominator_shift_down : MathValueRecord start,
    /// Standard shift down applied to the denominator in display style.
    /// Positive for moving in the downward direction. Suggested: same value as
    /// stackBottomDisplayStyleShiftDown.
    fraction_denominator_display_style_shift_down : MathValueRecord start,
    /// Minimum tolerated gap between the (ink) bottom of the numerator and the
    /// ink of the fraction bar. Suggested: default rule thickness.
    fraction_numerator_gap_min : MathValueRecord start,
    /// Minimum tolerated gap between the (ink) bottom of the numerator and the
    /// ink of the fraction bar in display style. Suggested: 3 × default rule
    /// thickness.
    fraction_num_display_style_gap_min : MathValueRecord start,
    /// Thickness of the fraction bar. Suggested: default rule thickness.
    fraction_rule_thickness : MathValueRecord start,
    /// Minimum tolerated gap between the (ink) top of the denominator and the
    /// ink of the fraction bar. Suggested: default rule thickness.
    fraction_denominator_gap_min : MathValueRecord start,
    /// Minimum tolerated gap between the (ink) top of the denominator and the
    /// ink of the fraction bar in display style. Suggested: 3 × default rule
    /// thickness.
    fraction_denom_display_style_gap_min : MathValueRecord start,
    /// Horizontal distance between the top and bottom elements of a skewed
    /// fraction.
    skewed_fraction_horizontal_gap : MathValueRecord start,
    /// Vertical distance between the ink of the top and bottom elements of a
    /// skewed fraction.
    skewed_fraction_vertical_gap : MathValueRecord start,
    /// Distance between the overbar and the (ink) top of he base.
    /// Suggested: 3 × default rule thickness.
    overbar_vertical_gap : MathValueRecord start,
    /// Thickness of overbar. Suggested: default rule thickness.
    overbar_rule_thickness : MathValueRecord start,
    /// Extra white space reserved above the overbar. Suggested: default rule
    /// thickness.
    overbar_extra_ascender : MathValueRecord start,
    /// Distance between underbar and (ink) bottom of the base.
    /// Suggested: 3 × default rule thickness.
    underbar_vertical_gap : MathValueRecord start,
    /// Thickness of underbar. Suggested: default rule thickness.
    underbar_rule_thickness : MathValueRecord start,
    /// Extra white space reserved below the underbar. Always positive.
    /// Suggested: default rule thickness.
    underbar_extra_descender : MathValueRecord start,
    /// Space between the (ink) top of the expression and the bar over it.
    /// Suggested: 1¼ default rule thickness.
    radical_vertical_gap : MathValueRecord start,
    /// Space between the (ink) top of the expression and the bar over it.
    /// Suggested: default rule thickness + ¼ x-height.
    radical_display_style_vertical_gap : MathValueRecord start,
    /// Thickness of the radical rule. This is the thickness of the rule in
    /// designed or constructed radical signs. Suggested: default rule thickness.
    radical_rule_thickness : MathValueRecord start,
    /// Extra white space reserved above the radical. Suggested: same value as
    /// radicalRuleThickness.
    radical_extra_ascender : MathValueRecord start,
    /// Extra horizontal kern before the degree of a radical, if such is
    /// present.
    radical_kern_before_degree : MathValueRecord start,
    /// Negative kern after the degree of a radical, if such is present.
    /// Suggested: −10/18 of em.
    radical_kern_after_degree : MathValueRecord start,
    /// Height of the bottom of the radical degree, if such is present, in
    /// proportion to the ascender of the radical sign. Suggested: 60%.
    radical_degree_bottom_raise_percent : S16Be,
};

struct MathGlyphInfo {
    start : Pos,
    /// Offset to MathItalicsCorrectionInfo table, from the beginning of the
    /// MathGlyphInfo table.
    math_italics_correction_info_offset : Offset16Be start MathItalicsCorrectionInfo,
    /// Offset to MathTopAccentAttachment table, from the beginning of the
    /// MathGlyphInfo table.
    math_top_accent_attachment_offset : Offset16Be start MathTopAccentAttachment,
    /// Offset to ExtendedShapes coverage table, from the beginning of the
    /// MathGlyphInfo table. When the glyph to the left or right of a box is an
    /// extended shape variant, the (ink) box should be used for vertical
    /// positioning purposes, not the default position defined by values in
    /// MathConstants table. May be NULL.
    extended_shape_coverage_offset : Offset16Be start Unknown, // TODO: ExtendedShapes coverage table
    /// Offset to MathKernInfo table, from the beginning of the MathGlyphInfo
    /// table.
    math_kern_info_offset : Offset16Be start MathKernInfo,
};

struct MathItalicsCorrectionInfo {
    start : Pos,
    /// Offset to Coverage table - from the beginning of MathItalicsCorrectionInfo table.
    italics_correction_coverage_offset : Offset16Be start Coverage,
    /// Number of italics correction values. Should coincide with the number of covered glyphs.
    italics_correction_count : U16Be,
    /// Array of MathValueRecords defining italics correction values for each covered glyph.
    italics_correction : Array italics_correction_count (MathValueRecord start),
};

struct MathTopAccentAttachment {
    start : Pos,
    /// Offset to Coverage table, from the beginning of the
    /// MathTopAccentAttachment table.
    top_accent_coverage_offset : Offset16Be start Coverage,
    /// Number of top accent attachment point values. Must be the same as the
    /// number of glyph IDs referenced in the Coverage table.
    top_accent_attachment_count : U16Be,
    /// Array of MathValueRecords defining top accent attachment points for each
    /// covered glyph.
    top_accent_attachment : Array top_accent_attachment_count (MathValueRecord start),
};

struct MathKernInfo {
    start : Pos,
    /// Offset to Coverage table, from the beginning of the MathKernInfo table.
    math_kern_coverage_offset : Offset16Be start Coverage,
    /// Number of MathKernInfoRecords. Must be the same as the number of glyph
    /// IDs referenced in the Coverage table.
    math_kern_count : U16Be,
    /// Array of MathKernInfoRecords, one for each covered glyph.
    math_kern_info_records : Array math_kern_count (MathKernInfoRecord start),
};

struct MathKernInfoRecord (math_kern_info_start : Pos) {
    /// Offset to MathKern table for top right corner, from the beginning of the
    /// MathKernInfo table. May be NULL.
    top_right_math_kern_offset : Offset16Be math_kern_info_start MathKern,
    /// Offset to MathKern table for the top left corner, from the beginning of
    /// the MathKernInfo table. May be NULL.
    top_left_math_kern_offset : Offset16Be math_kern_info_start MathKern,
    /// Offset to MathKern table for bottom right corner, from the beginning of
    /// the MathKernInfo table. May be NULL.
    bottom_right_math_kern_offset : Offset16Be math_kern_info_start MathKern,
    /// Offset to MathKern table for bottom left corner, from the beginning of
    /// the MathKernInfo table. May be NULL.
    bottom_left_math_kern_offset : Offset16Be math_kern_info_start MathKern,
};

struct MathKern {
    start : Pos,
    /// Number of heights at which the kern value changes.
    height_count : U16Be,
    /// Array of correction heights, in design units, sorted from lowest to
    /// highest.
    correction_height : Array height_count (MathValueRecord start),
    /// Array of kerning values for different height ranges. Negative values are
    /// used to move glyphs closer to each other.
    kern_values : Array (nat_add height_count 1) (MathValueRecord start),
};

struct MathVariants {
    start : Pos,
    /// Minimum overlap of connecting glyphs during glyph construction, in
    /// design units.
    min_connector_overlap : U16Be,
    /// Offset to Coverage table, from the beginning of the MathVariants table.
    vertical_glyph_coverage_offset : Offset16Be start Coverage,
    /// Offset to Coverage table, from the beginning of the MathVariants table.
    horizontal_glyph_coverage_offset : Offset16Be start Coverage,
    /// Number of glyphs for which information is provided for vertically
    /// growing variants. Must be the same as the number of glyph IDs referenced
    /// in the vertical Coverage table.
    vertical_glyph_count : U16Be,
    /// Number of glyphs for which information is provided for horizontally
    /// growing variants. Must be the same as the number of glyph IDs referenced
    /// in the horizontal Coverage table.
    horizontal_glyph_count : U16Be,
    /// Array of offsets to MathGlyphConstruction tables, from the beginning of
    /// the MathVariants table, for shapes growing in the vertical direction.
    vertical_glyph_construction_offsets : Array vertical_glyph_count (Offset16Be start MathGlyphConstruction),
    /// Array of offsets to MathGlyphConstruction tables, from the beginning of
    /// the MathVariants table, for shapes growing in the horizontal direction.
    horizontal_glyph_construction_offsets : Array horizontal_glyph_count (Offset16Be start MathGlyphConstruction),
};

struct MathGlyphConstruction {
    start : Pos,
    /// Offset to the GlyphAssembly table for this shape, from the beginning of
    /// the MathGlyphConstruction table. May be NULL.
    glyph_assembly_offset : Offset16Be start GlyphAssembly,
    /// Count of glyph growing variants for this glyph.
    variant_count : U16Be,
    /// MathGlyphVariantRecords for alternative variants of the glyphs.
    math_glyph_variant_record : Array variant_count MathGlyphVariantRecord,
};

struct MathGlyphVariantRecord {
    /// Glyph ID for the variant.
    variant_glyph : U16Be,
    /// Advance width/height, in design units, of the variant, in the direction
    /// of requested glyph extension.
    advance_measurement : U16Be,
};

struct GlyphAssembly {
    start : Pos,
    /// Italics correction of this GlyphAssembly. Should not depend on the
    /// assembly size.
    italics_correction : MathValueRecord start,
    /// Number of parts in this assembly.
    part_count : U16Be,
    /// Array of part records, from left to right (for assemblies that extend
    /// horizontally) or bottom to top (for assemblies that extend vertically).
    part_records : Array part_count GlyphPartRecord,
};

struct GlyphPartRecord {
    /// Glyph ID for the part.
    glyph_id : U16Be,
    /// Advance width/ height, in design units, of the straight bar connector
    /// material at the start of the glyph in the direction of the extension
    /// (the left end for horizontal extension, the bottom end for vertical
    /// extension).
    start_connector_length : U16Be,
    /// Advance width/ height, in design units, of the straight bar connector
    /// material at the end of the glyph in the direction of the extension
    /// (the right end for horizontal extension, the top end for vertical
    /// extension).
    end_connector_length : U16Be,
    /// Full advance width/height for this part in the direction of the
    /// extension, in design units.
    full_advance : U16Be,
    /// Part qualifiers. PartFlags enumeration currently uses only one bit:
    ///
    /// - 0x0001 fExtender If set, the part can be skipped or repeated.
    /// - 0xFFFE Reserved.
    part_flags : U16Be,
};



// =============================================================================
//
// avar — Axis Variations Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/avar>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6avar.html>
//
// =============================================================================

struct AxisVariations {
    /// Major version number of the axis variations table — set to 1.
    major_version : U16Be, // TODO: constrain version
    /// Minor version number of the axis variations table — set to 0.
    minor_version : U16Be, // TODO: constrain version
    /// Permanently reserved; set to zero.
    reserved : Reserved U16Be,
    /// The number of variation axes for this font. This must be the same number
    /// as axisCount in the 'fvar' table.
    axis_count : U16Be,
    /// The segment maps array — one segment map for each axis, in the order of
    /// axes specified in the 'fvar' table.
    axis_segment_maps : Array axis_count SegmentMaps,
};

struct SegmentMaps {
    /// The number of correspondence pairs for this axis.
    position_map_count : U16Be,
    /// The array of axis value map records for this axis.
    axis_value_maps : Array position_map_count AxisValueMap,
};

struct AxisValueMap {
    /// A normalized coordinate value obtained using default normalization.
    from_coordinate : F2Dot14,
    /// The modified, normalized coordinate value.
    to_coordinate : F2Dot14,
};



// =============================================================================
//
// cvar — CVT Variations Table
//
// <https://www.microsoft.com/typography/otspec/cvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6cvar.html>
//
// =============================================================================

struct ControlValueVariations {
    start : Pos,
    /// Major version number of the CVT variations table — set to 1.
    major_version : U16Be, // TODO: constrain version
    /// Minor version number of the CVT variations table — set to 0.
    minor_version : U16Be, // TODO: constrain version
    /// A packed field. The high 4 bits are flags, and the low 12 bits are the number of tuple-variation data tables. The count can be any number between 1 and 4095.
    tuple_variation_count : U16Be,
    /// Offset from the start of the 'cvar' table to the serialized data.
    data_offset : Offset16Be start Unknown, // TODO: "serialized data"?
    /// Array of tuple variation headers.
    tuple_variation_headers : Array tuple_variation_count Unknown, // TODO: TupleVariationHeader
};



// =============================================================================
//
// fvar — Font Variations Table
//
// <https://www.microsoft.com/typography/otspec/fvar.htm>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6fvar.html>
//
// =============================================================================

struct FontVariations {
    start : Pos,
    /// Major version number of the font variations table — set to 1.
    major_version : U16Be, // TODO: constrain version
    /// Minor version number of the font variations table — set to 0.
    minor_version : U16Be, // TODO: constrain version
    /// Offset in bytes from the beginning of the table to the start of the
    /// VariationAxisRecord array.
    axes_array_offset : Offset16Be start VariationAxisRecord, // TODO: avoid reparsing?
    /// This field is permanently reserved. Set to 2.
    reserved : Reserved U16Be,
    /// The number of variation axes in the font (the number of records in the
    /// axes array).
    axis_count : U16Be,
    /// The size in bytes of each VariationAxisRecord — set to 20 (0x0014) for
    /// this version.
    axis_size : U16Be,
    /// The number of named instances defined in the font (the number of records
    /// in the instances array).
    instance_count : U16Be,
    /// The size in bytes of each InstanceRecord — set to either
    /// axisCount * sizeof(Fixed) + 4, or to axisCount * sizeof(Fixed) + 6.
    instance_size : U16Be,
    /// The variation axis array.
    axes : Array axis_count VariationAxisRecord,
    /// The named instance array.
    instances : Array instance_count (InstanceRecord axis_count),
};

struct VariationAxisRecord {
    /// Tag identifying the design variation for the axis.
    axis_tag : Tag,
    /// The minimum coordinate value for the axis.
    min_value : Fixed,
    /// The default coordinate value for the axis.
    default_value : Fixed,
    /// The maximum coordinate value for the axis.
    max_value : Fixed,
    /// Axis qualifiers — see details below.
    flags : U16Be, // TODO: VariationAxisFlags
    /// The name ID for entries in the 'name' table that provide a display name
    /// for this axis.
    axis_name_id : U16Be,
};

// TODO:
// enum VariationAxisFlags : U16 {
//     /// The axis should not be exposed directly in user interfaces.
//     HIDDEN_AXIS = 0x0001,
//     /// Reserved for future use — set to 0.
//     Reserved = 0xFFFE,
// };

struct InstanceTuple (axis_count : U16) {
    /// Coordinate array specifying a position within the font’s variation space.
    coordinates : Array axis_count Fixed,
};

struct InstanceRecord (axis_count : U16) {
    /// The name ID for entries in the 'name' table that provide subfamily names
    /// for this instance.
    subfamily_name_id : U16Be,
    /// Reserved for future use — set to 0.
    flags : Reserved U16Be,
    /// The coordinates array for this instance.
    coordinates : InstanceTuple axis_count,
    /// Optional. The name ID for entries in the 'name' table that provide
    /// PostScript names for this instance.
    post_script_name_id : U16Be,
};



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
struct DigitalSignature (length : U32) {
    start : Pos,
    /// Version number of the DSIG table (0x00000001)
    version : U32Be,
    /// Number of signatures in the table
    num_signatures : U16Be,
    /// permission flags Bit 0: cannot be resigned Bits 1-7: Reserved (Set to 0)
    // TODO: flags : Reserved U16Be 0
    flags : Reserved U16Be,
    /// Array of signature records
    signature_records : Array num_signatures (SignatureRecord start),
};

struct SignatureRecord (digital_signature_start : Pos) {
    /// Format of the signature
    format : U32Be,
    /// Length of signature in bytes
    length : U32Be,
    /// Offset to the signature block from the beginning of the table
    offset : Offset32Be digital_signature_start SignatureBlock,
};

union SignatureBlock {
    SignatureBlockFormat1,
};

struct SignatureBlockFormat1 {
    /// Reserved for future use; set to zero.
    // TODO: reserved1 : Reserved U16Be 0,
    reserved1 : Reserved U16Be,
    /// Reserved for future use; set to zero.
    // TODO: reserved2 : Reserved U16Be 0,
    reserved2 : Reserved U16Be,
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
    version : U16Be, // TODO: constrain version
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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/kern>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html>
//
// =============================================================================

struct Kerning {
    /// Table version number (0)
    version : U16Be,
    /// Number of subtables in the kerning table.
    n_tables : U16Be,
};

union KerningSubtable {
    KerningSubtableFormat0,
    KerningSubtableFormat2,
};

struct KerningSubtableFormat0 {
    /// Kern subtable version number
    version : { version : U16Be | nat_eq version 0 },
    /// Length of the subtable, in bytes (including this header).
    length : U16Be,
    /// What type of information is contained in this table.
    coverage : U16Be, // TODO: Coverage

    /// This gives the number of kerning pairs in the table.
    n_pairs : U16Be,
    /// The largest power of two less than or equal to the value of `n_pairs`,
    /// multiplied by the size in bytes of an entry in the table.
    search_range : U16Be,
    /// This is calculated as log2 of the largest power of two less than or
    /// equal to the value of `n_pairs`. This value indicates how many
    /// iterations of the search loop will have to be made. (For example, in a
    /// list of eight items, there would have to be three iterations of the
    /// loop).
    entry_selector : U16Be,
    /// The value of `n_pairs` minus the largest power of two less than or equal
    /// to `n_pairs`, and then multiplied by the size in bytes of an entry in
    /// the table.
    range_shift : U16Be,
    /// Sorted list of kerning pairs and values
    kerning_pair_values : Array n_pairs KerningPair, // TODO: Sorted
};

struct KerningPair {
    /// The glyph index for the left-hand glyph in the kerning pair.
    left : U16Be,
    /// The glyph index for the right-hand glyph in the kerning pair.
    right : U16Be,
    /// The kerning value for the above pair, in FUnits. If this value is
    /// greater than zero, the characters will be moved apart. If this value is
    /// less than zero, the character will be moved closer together.
    value : FWord,
};

struct KerningSubtableFormat2 {
    start : Pos,
    /// Kern subtable version number
    version : { version : U16Be | nat_eq version 2 },
    /// Length of the subtable, in bytes (including this header).
    length : U16Be,
    /// What type of information is contained in this table.
    coverage : U16Be, // TODO: Coverage

    /// The width, in bytes, of a row in the table.
    row_width : U16Be,
    /// Offset from beginning of this subtable to left-hand class table.
    left_class_table : Offset16Be start KerningClassTable,
    /// Offset from beginning of this subtable to right-hand class table.
    right_class_table : Offset16Be start KerningClassTable,
    /// Offset from beginning of this subtable to the start of the kerning array.
    array : Offset16Be start KerningClassTable,
};

struct KerningClassTable {
    /// First glyph in class range.
    first_glyph : U16Be,
    /// Number of glyph in class range.
    n_glyphs : U16Be,
    /// Class values
    ///
    /// A left by right array of kerning values, which are FWords, where left is
    /// the number of left-hand classes and R is the number of right-hand
    /// classes. The array is stored by row.
    values : Array n_glyphs FWord,
};



// =============================================================================
//
// LTSH - Linear Threshold
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/ltsh>
//
// =============================================================================

struct LinearThreshold {
    /// Version number (starts at 0).
    version : U16Be,
    /// Number of glyphs (from `num_glyphs` in 'maxp' table).
    num_glyphs : U16Be, // TODO: constrained by `num_glyphs` in `maxp` table
    /// The vertical pel height at which the glyph can be assumed to scale linearly. On a per glyph basis.
    y_pels : Array num_glyphs U8, // TODO: Sorted
};



// =============================================================================
//
// MERG — Merge Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/merg>
//
// =============================================================================

struct Merge {
    start : Pos,
    /// Version number of the merge table — set to 0.
    version : U16Be,
    /// The number of merge classes.
    merge_class_count : U16Be,
    /// Offset to the array of merge-entry data.
    merge_data_offset : Offset16Be start (MergeEntry merge_class_count),
    /// The number of class definition tables.
    class_def_count : U16Be,
    /// Offset to an array of offsets to class definition tables — in bytes from the start of the MERG table.
    offset_to_class_def_offsets : Offset16Be start (Array class_def_count Unknown),
};

struct MergeEntry (merge_class_count : U16) {
    /// Array of merge-entry rows.
    merge_entry_rows : Array merge_class_count (MergeEntryRow merge_class_count),
};

struct MergeEntryRow (merge_class_count : U16) {
    /// Array of merge entries.
    merge_entries : Array merge_class_count U8, // TODO: enumerations
};



// =============================================================================
//
// meta — Metadata Table
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/meta>
// <https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6meta.html>
//
// =============================================================================

/// Metadata Table
struct Metadata {
    start : Pos,
    /// Version number of the metadata table — set to 1.
    version : U32Be, // TODO: constrain version
    /// Flags — currently unused; set to 0.
    // TODO: flags : Reserved U32Be 0,
    flags : Reserved U32Be,
    /// Not used; should be set to 0.
    // TODO: reserved : Reserved U32Be 0,
    reserved : Reserved U32Be,
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
    data_offset : U32Be,
    /// Length of the data, in bytes. The data is not required to be padded to any byte boundary.
    data_length : U32Be,
    /// The metadata information for this tag
    data_pos : OffsetPos metadata_start data_offset (MetadataInfo tag data_length),
};

/// Metadata information
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/meta#metadata-tags>
MetadataInfo (tag : Tag) (length : U32) = match tag.value {
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
    major_version: U16Be, // TODO: constrain version
    /// Minor version number of the style attributes table — set to 2.
    minor_version: U16Be, // TODO: constrain version

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
    offset_to_axis_value_offsets : Offset32Be start (Array axis_value_count (AxisValueFormat major_version minor_version)),

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

union AxisValueFormat (major_version : U16) (minor_version : U16) {
    AxisValueFormat1,
    AxisValueFormat2,
    AxisValueFormat3,
    AxisValueFormat4, // TODO: If Version 1.2
};

struct AxisValueFormat1 {
    /// Format identifier — set to 1.
    format : { format : U16Be | nat_eq format 1 },
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
    format : { format : U16Be | nat_eq format 2 },
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
    format : { format : U16Be | nat_eq format 3 },
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
    format : { format : U16Be | nat_eq format 4 },
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
// <https://docs.microsoft.com/en-us/typography/opentype/spec/pclt>
//
// =============================================================================

struct Pcl5 {
    major_version : U16Be, // TODO: 1
    minor_version : U16Be, // TODO: 0
    font_number : U32Be, // TODO: Enumeration
    /// The width of the space in FUnits (FUnits are described by the
    /// `units_per_em` field of the 'head' table). Monospace fonts derive the
    /// width of all characters from this field.
    pitch : U16Be,
    /// The height of the optical line describing the height of the lowercase x
    /// in FUnits. This might not be the same as the measured height of the
    /// lowercase x.
    x_height : U16Be,
    style : U16Be, // TODO: Enumeration
    type_family : U16Be, // TODO: Enumeration
    /// The height of the optical line describing the top of the uppercase H in
    /// FUnits. This might not be the same as the measured height of the
    /// uppercase H.
    cap_height : U16Be,
    symbol_set : U16Be, // TODO: Enumeration
    typeface : Array 16 S8, // TODO: Enumeration
    character_complement : Array 8 S8, // TODO: Enumeration
    file_name : Array 6 S8,
    stroke_weight : S8, // TODO: Enumeration
    width_type : S8, // TODO: Enumeration
    serif_style : U8, // TODO: Enumeration
    padding : Reserved U8,
};

// TODO: Enumerations



// =============================================================================
//
// VDMX - Vertical Device Metrics
//
// <https://docs.microsoft.com/en-us/typography/opentype/spec/vdmx>
//
// =============================================================================

struct VerticalDeviceMetrics {
    start : Pos,
    /// Version number (0 or 1).
    version : U16Be, // TODO: Constrain value
    /// Number of VDMX groups present
    num_recs : U16Be, // FIXME: not used?
    /// Number of aspect ratio groupings
    num_ratios : U16Be,
    /// Ratio record array.
    rat_range : Array num_ratios (RatioRange version),
    /// Offset from start of this table to the `VerticalDeviceMetricsGroup`
    /// table for a corresponding RatioRange record.
    groups : Array num_ratios (Offset16Be start VerticalDeviceMetricsGroup),
    // groups : repeat num_recs VerticalDeviceMetricsGroup,
};

struct RatioRange (version : U16) {
    /// Character set (see below).
    b_char_set : U8, // TODO: Enumeration (depends on `version`)
    /// Value to use for x-Ratio
    x_ratio : U8,
    /// Starting y-Ratio value.
    y_start_ratio : U8,
    /// Ending y-Ratio value.
    y_end_ratio : U8,
};

struct VerticalDeviceMetricsGroup {
    /// Number of height records in this group
    recs : U16Be,
    /// Starting `y_pel_height`
    startsz : U8,
    /// Ending `y_pel_height`
    endsz : U8,
    /// The VDMX records
    entry : Array recs VerticalDeviceMetricsRecord, // TODO: sorted by `y_pel_height`
};

struct VerticalDeviceMetricsRecord {
    /// `y_pel_height` to which values apply.
    y_pel_height : U16Be,
    /// Maximum value (in pels) for this `y_pel_height`.
    y_max : S16Be,
    /// Minimum value (in pels) for this `y_pel_height`.
    y_min : S16Be,
};



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
