module opentype;


struct Unknown {};


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
    // FIXME: script_offset : Offset16Be Script,
    script_offset : Offset16Be script_list_start Unknown,
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
    // FIXME: lang_sys_offset : Offset16Be LangSys,
    lang_sys_offset : Offset16Be script_start Unknown,
};

/// Script Table
///
/// <https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#script-table-and-language-system-record>
struct Script {
    table_start : Pos,
    /// Offset to default LangSys table, from beginning of Script table — may be NULL
    // FIXME: default_lang_sys : Offset16Be table_start LangSys,
    default_lang_sys : Offset16Be table_start Unknown,
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
    // FIXME: feature_offset : Offset16Be Feature,
    feature_offset : Offset16Be feature_list_start Unknown,
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
    // FIXME: lookups : Array lookup_count (Offset16Be table_start Lookup),
    lookups : Array lookup_count (Offset16Be table_start Unknown),
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


// TODO: Rest of OpenType!
