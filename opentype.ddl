
// @pragma big_endian;

// FIXME tables must (should?) be 4-byte aligned
// FIXME padding bytes between tables must (should?) be zero

// @root
OpenType: union {
    OffsetTable,
    TTCHeader
}

OffsetTable: struct {
    sfnt_version: uint32 => 0x00010000,
    num_tables: uint16,
    search_range: uint16, // (Maximum power of 2 <= numTables) x 16
    entry_selector: uint16, // Log2(maximum power of 2 <= numTables)
    range_shift: uint16, // NumTables x 16-searchRange
    table_records: struct[num_tables] { // sorted by tag
	tag: uint32,
	checksum: uint32,
	offset: uint32 => offset(Table), // from @root (OffsetTable or TTCHeader)
	length: uint32
    }
}

TTCHeader: union {
    TTCHeader1,
    TTCHeader2
}

TTCHeader1: struct {
    ttc_tag: uint32 => 'ttcf',
    version: uint32 => 0x00010000,
    num_fonts: uint32,
    offset_tables: uint32[num_fonts] => offset(OffsetTable) // from TTCHeader1
}

TTCHeader2: struct {
    ttc_tag: uint32 => 'ttcf',
    version: uint32 => 0x00020000,
    num_fonts: uint32,
    offset_tables: uint32[num_fonts] => offset(OffsetTable), // from TTCHeader2
    dsig_tag: uint32 => 0 | 'DSIG',
    dsig_length: uint32 => 0 | *,
    dsig_offset: uint32 => 0 | offset(DSIG) // from TTCHeader2
}

// Common structs for OpenType Layout

ScriptList: struct {
    script_count: uint16,
    script_records: struct[script_count] { // sorted by script_tag
	script_tag: uint32,
	script: uint16 => offset(Script) // from ScriptList
    }
}

Script: struct {
    default_lang_sys: uint16 => 0 | offset(LangSys), // from Script
    lang_sys_count: uint16,
    lang_sys_records: struct[lang_sys_count] { // sorted by lang_sys_tag
	lang_sys_tag: uint32,
	lang_sys: uint16 => offset(LangSys) // from Script
    }
}

LangSys: struct {
    lookup_order: uint16 => 0,
    req_feature_index: uint16, // feature index or 0xFFFF
    feature_count: uint16,
    feature_index: uint16[feature_count] // index into FeatureList
}

FeatureList: struct {
    feature_count: uint16,
    feature_records: struct[feature_count] { // sorted by feature_tag
	feature_tag: uint32,
	feature: uint16 => offset(Feature) // from FeatureList
    }
}

Feature: struct {
    feature_params: uint16 => 0,
    lookup_count: uint16,
    lookup_list_index: uint16[lookup_count] // index into LookupList
}

LookupList: struct {
    lookup_count: uint16,
    lookup: uint16[lookup_count] => offset(Lookup) // from LookupList
}

Lookup: struct {
    lookup_type: uint16,
    lookup_flag: uint16, // bitfield
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] => offset(SubTable), // from Lookup
    //FIXME mark_filtering_set: uint16 // index into GDEF, only present if UseMarkFilteringSet
}

Coverage: union {
    CoverageFormat1,
    CoverageFormat2
}

CoverageFormat1: struct {
    coverage_format: uint16 => 1,
    glyph_count: uint16,
    glyph_array: uint16[glyph_count] // sorted numerically
}

CoverageFormat2: struct {
    coverage_format: uint16 => 2,
    range_count: uint16,
    range_record: struct[range_count] {
	start: uint16,
	end: uint16,
	start_coverage_index: uint16
    }
}

// GSUB table

GSUB: struct {
    version: uint32 => 0x00010000,
    script_list: uint16 => offset(ScriptList), // from GSUB
    feature_list: uint16 => offset(FeatureList), // from GSUB
    lookup_list: uint16 => offset(LookupList) // from GSUB
}

SingleSubst: union {
    SingleSubstFormat1,
    SingleSubstFormat2
}

SingleSubstFormat1: struct {
    subst_format: uint16 => 1,
    coverage: uint16 => offset(Coverage), // from SingleSubstFormat1
    delta_glyph_id: int16
}

SingleSubstFormat2: struct {
    subst_format: uint16 => 2,
    coverage: uint16 => offset(Coverage), // from SingleSubstFormat2
    glyph_count: uint16,
    substitute: uint16[glyph_count]
}

