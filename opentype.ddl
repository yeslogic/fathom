
// @pragma big_endian;

// FIXME tables must (should?) be 4-byte aligned
// FIXME padding bytes between tables must (should?) be zero

// @root
OpenType: union {
    OffsetTable,
    TTCHeader
}

OffsetTable: struct {
    sfnt_version: uint32 = 0x00010000 | 'OTTO', // FIXME Apple allows 'true' and 'typ1' ?
    num_tables: uint16,
    search_range: uint16, // (Maximum power of 2 <= numTables) x 16
    entry_selector: uint16, // Log2(maximum power of 2 <= numTables)
    range_shift: uint16, // NumTables x 16-searchRange
    table_records[num_tables]: struct { // FIXME sorted by tag
	tag: uint32,
	checksum: uint32,
	offset: uint32 @offset(@root) => tag_magic(tag), // FIXME byte[length]
	length: uint32
    }
}

TTCHeader: union {
    TTCHeader1,
    TTCHeader2
}

TTCHeader1: struct {
    ttc_tag: uint32 = 'ttcf',
    version: uint32 = 0x00010000,
    num_fonts: uint32,
    offset_tables[num_fonts]: uint32 @offset => OffsetTable
}

TTCHeader2: struct {
    ttc_tag: uint32 = 'ttcf',
    version: uint32 = 0x00020000,
    num_fonts: uint32,
    offset_tables[num_fonts]: uint32 @offset => OffsetTable,
    dsig_tag: uint32 = 0 | 'DSIG',
    dsig_length: uint32,
    dsig_offset: uint32 = 0 | @offset => DSIG // FIXME byte[dsig_length]
    // FIXME if dsig_tag = 0 -> dsig_length = 0
    // FIXME if dsig_tag = 0 -> dsig_offset = 0
}

head: struct {
    version: uint32 = 0x00010000,
    font_revision: uint32, // FIXME 16.16 fixed point
    check_sum_adjustment: uint32, // FIXME see spec
    magic_number: uint32 = 0x5F0F3CF5,
    flags: uint16, // FIXME see spec
    units_per_em: uint16, // FIXME see spec
    created: int64, // FIXME longdatetime
    modified: int64, // FIXME longdatetime
    x_min: int16,
    y_min: int16,
    x_max: int16,
    y_max: int16,
    mac_style: uint16, // FIXME see spec
    lowest_rec_ppem: uint16, // FIXME see spec
    font_direction_hint: int16, // deprecated
    index_to_loc_format: int16 = 0 | 1,
    glyph_data_format: int16 = 0
}

hhea: struct {
    version: uint32 = 0x00010000,
    ascender: int16,
    descender: int16,
    line_gap: int16,
    advance_width_max: uint16,
    min_left_side_bearing: int16,
    min_right_side_bearing: int16,
    x_max_extent: int16,
    caret_slope_rise: int16,
    caret_slope_run: int16,
    caret_offset: int16,
    reserved1: int16 = 0,
    reserved2: int16 = 0,
    reserved3: int16 = 0,
    reserved4: int16 = 0,
    metric_data_format: int16 = 0,
    number_of_h_metrics: uint16
}

// Common structs for OpenType Layout

ScriptList: struct {
    script_count: uint16,
    script_records[script_count]: struct { // FIXME sorted by script_tag
	script_tag: uint32,
	script: uint16 @offset(ScriptList) => Script
    }
}

Script: struct {
    default_lang_sys: uint16 = 0 | @offset => LangSys,
    lang_sys_count: uint16,
    lang_sys_records[lang_sys_count]: struct { // FIXME sorted by lang_sys_tag
	lang_sys_tag: uint32,
	lang_sys: uint16 @offset(Script) => LangSys
    }
}

LangSys: struct {
    lookup_order: uint16 = 0,
    req_feature_index: uint16, // FIXME feature index or 0xFFFF
    feature_count: uint16,
    feature_index[feature_count]: uint16 // FIXME index into FeatureList
}

FeatureList: struct {
    feature_count: uint16,
    feature_records[feature_count]: struct { // FIXME sorted by feature_tag
	feature_tag: uint32,
	feature: uint16 @offset(FeatureList) => Feature
    }
}

Feature: struct {
    feature_params: uint16 = 0,
    lookup_count: uint16,
    lookup_list_index[lookup_count]: uint16 // FIXME index into LookupList
}

// FIXME duplicated below for GSUB and GPOS
LookupList: struct {
    lookup_count: uint16,
    lookup[lookup_count]: uint16 @offset => Lookup
}

// FIXME duplicated below for GSUB and GPOS
Lookup: struct {
    lookup_type: uint16,
    lookup_flag: uint16, // bitfield
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 // FIXME @offset => SubTable
    //FIXME mark_filtering_set: uint16 // index into GDEF, only present if UseMarkFilteringSet
}

Coverage: union {
    CoverageFormat1,
    CoverageFormat2
}

CoverageFormat1: struct {
    coverage_format: uint16 = 1,
    glyph_count: uint16,
    glyph_array[glyph_count]: uint16 // FIXME sorted numerically
}

CoverageFormat2: struct {
    coverage_format: uint16 = 2,
    range_count: uint16,
    range_record[range_count]: struct {
	// FIXME start =< end
	start: uint16,
	end: uint16,
	start_coverage_index: uint16
    }
}

ClassDef: union {
    ClassDefFormat1,
    ClassDefFormat2
}

ClassDefFormat1: struct {
    class_format: uint16 = 1,
    start_glyph: uint16,
    glyph_count: uint16,
    class_value_array[glyph_count]: uint16
}

ClassDefFormat2: struct {
    class_format: uint16 = 2,
    class_range_count: uint16,
    class_range_record[class_range_count]: struct {
	// FIXME start =< end
	start: uint16,
	end: uint16,
	class: uint16
    }
}

// GSUB table

GSUB: struct {
    version: uint32 = 0x00010000,
    script_list: uint16 @offset => ScriptList,
    feature_list: uint16 @offset => FeatureList,
    lookup_list: uint16 @offset => LookupListGSUB
}

LookupListGSUB: struct {
    lookup_count: uint16,
    lookup[lookup_count]: uint16 @offset => LookupGSUB
}

LookupGSUB: union {
    LookupGSUB1,
    LookupGSUB2,
    LookupGSUB3,
    LookupGSUB4,
    LookupGSUB5,
    LookupGSUB6,
    LookupGSUB7,
    LookupGSUB8
}

LookupGSUB1: struct {
    lookup_type: uint16 = 1,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => SingleSubst
}

LookupGSUB2: struct {
    lookup_type: uint16 = 2,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => MultipleSubst
}

LookupGSUB3: struct {
    lookup_type: uint16 = 3,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => AlternateSubst
}

LookupGSUB4: struct {
    lookup_type: uint16 = 4,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => LigatureSubst
}

LookupGSUB5: struct {
    lookup_type: uint16 = 5,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ContextSubst
}

LookupGSUB6: struct {
    lookup_type: uint16 = 6,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ChainContextSubst
}

LookupGSUB7: struct {
    lookup_type: uint16 = 7,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ExtensionSubst
}

LookupGSUB8: struct {
    lookup_type: uint16 = 8,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ReverseChainSingleSubst
}

SingleSubst: union {
    SingleSubstFormat1,
    SingleSubstFormat2
}

SingleSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    delta_glyph_id: int16
}

SingleSubstFormat2: struct {
    subst_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    glyph_count: uint16,
    substitute[glyph_count]: uint16
}

MultipleSubst: union {
    MultipleSubstFormat1
}

MultipleSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    sequence_count: uint16,
    sequence[sequence_count]: uint16 @offset => Sequence
}

Sequence: struct {
    glyph_count: uint16, // FIXME > 0
    substitute[glyph_count]: uint16
}

AlternateSubst: union {
    AlternateSubstFormat1
}

AlternateSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    alternate_set_count: uint16,
    alternate_set[alternate_set_count]: uint16 @offset => AlternateSet
}

AlternateSet: struct {
    glyph_count: uint16,
    alternate[glyph_count]: uint16
}

LigatureSubst: union {
    LigatureSubstFormat1
}

LigatureSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    lig_set_count: uint16,
    ligature_set[lig_set_count]: uint16 @offset => LigatureSet
}

LigatureSet: struct {
    ligature_count: uint16,
    ligature[ligature_count]: uint16 @offset => Ligature
}

Ligature: struct {
    lig_glyph: uint16,
    comp_count: uint16, // FIXME > 0
    component[comp_count - 1]: uint16
}

SubstLookupRecord: struct {
    sequence_index: uint16,
    lookup_list_index: uint16 // FIXME index into LookupList
}

ContextSubst: union {
    ContextSubstFormat1,
    ContextSubstFormat2,
    ContextSubstFormat3
}

ContextSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    sub_rule_set_count: uint16, // FIXME must equal glyph_count in Coverage
    sub_rule_set[sub_rule_set_count]: uint16 @offset => SubRuleSet
}

SubRuleSet: struct {
    sub_rule_count: uint16,
    sub_rule[sub_rule_count]: uint16 @offset => SubRule
}

SubRule: struct {
    glyph_count: uint16, // FIXME > 0
    subst_count: uint16,
    input[glyph_count - 1]: uint16,
    substs[subst_count]: SubstLookupRecord
}

ContextSubstFormat2: struct {
    subst_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    class_def: uint16 @offset => ClassDef,
    sub_class_count: uint16,
    sub_class_set[sub_class_count]: uint16 @offset => SubClassSet
}

SubClassSet: struct {
    sub_class_rule_count: uint16,
    sub_class_rule[sub_class_rule_count]: uint16 = 0 | @offset => SubClassRule
}

SubClassRule: struct {
    glyph_count: uint16, // FIXME > 0
    subst_count: uint16,
    class[glyph_count - 1]: uint16,
    substs[subst_count]: SubstLookupRecord
}

ContextSubstFormat3: struct {
    subst_format: uint16 = 3,
    glyph_count: uint16,
    subst_count: uint16,
    coverage[glyph_count]: uint16 @offset => Coverage,
    substs[subst_count]: SubstLookupRecord
}

ChainContextSubst: union {
    ChainContextSubstFormat1,
    ChainContextSubstFormat2,
    ChainContextSubstFormat3
}

ChainContextSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    chain_sub_rule_set_count: uint16,
    chain_sub_rule_set[chain_sub_rule_set_count]: uint16 @offset => ChainSubRuleSet
}

ChainSubRuleSet: struct {
    chain_sub_rule_count: uint16,
    chain_sub_rule[chain_sub_rule_count]: uint16 @offset => ChainSubRule
}

ChainSubRule: struct {
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16,
    input_glyph_count: uint16,
    input[input_glyph_count]: uint16,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16,
    subst_count: uint16,
    substs[subst_count]: SubstLookupRecord
}

ChainContextSubstFormat2: struct {
    subst_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    backtrack_class_def: uint16 @offset => ClassDef,
    input_class_def: uint16 @offset => ClassDef,
    lookahead_class_def: uint16 @offset => ClassDef,
    chain_sub_class_set_count: uint16,
    chain_sub_class_set[chain_sub_class_set_count]: uint16 = 0 | @offset => ChainSubClassSet
}

ChainSubClassSet: struct {
    chain_sub_class_rule_count: uint16,
    chain_sub_class_rule[chain_sub_class_rule_count]: uint16 @offset => ChainSubClassRule
}

ChainSubClassRule: struct{
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16,
    input_glyph_count: uint16, // FIXME > 0
    input[input_glyph_count - 1]: uint16,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16,
    subst_count: uint16,
    substs[subst_count]: SubstLookupRecord
}

ChainContextSubstFormat3: struct {
    subst_format: uint16 = 3,
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16 @offset => Coverage,
    input_glyph_count: uint16,
    input[input_glyph_count]: uint16 @offset => Coverage,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16 @offset => Coverage,
    subst_count: uint16,
    substs[subst_count]: SubstLookupRecord
}

ExtensionSubst: union {
    ExtensionSubstFormat1
}

ExtensionSubstFormat1: struct {
    subst_format: uint16 = 1,
    extension_lookup_type: uint16, // FIXME any GSUB lookup type except 7
    extension_offset: uint32 // FIXME @offset => SubTable
}

ReverseChainSingleSubst: union {
    ReverseChainSingleSubstFormat1
}

ReverseChainSingleSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16 @offset => Coverage,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16 @offset => Coverage,
    glyph_count: uint16,
    substitute[glyph_count]: uint16
}

// GPOS table

GPOS: struct {
    version: uint32 = 0x00010000,
    script_list: uint16 @offset => ScriptList,
    feature_list: uint16 @offset => FeatureList,
    lookup_list: uint16 @offset => LookupListGPOS
}

LookupListGPOS: struct {
    lookup_count: uint16,
    lookup[lookup_count]: uint16 @offset => LookupGPOS
}

LookupGPOS: union {
    LookupGPOS1,
    LookupGPOS2,
    LookupGPOS3,
    LookupGPOS4,
    LookupGPOS5,
    LookupGPOS6,
    LookupGPOS7,
    LookupGPOS8,
    LookupGPOS9
}

LookupGPOS1: struct {
    lookup_type: uint16 = 1,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => SinglePos
}

LookupGPOS2: struct {
    lookup_type: uint16 = 2,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => PairPos
}

LookupGPOS3: struct {
    lookup_type: uint16 = 3,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => CursivePos
}

LookupGPOS4: struct {
    lookup_type: uint16 = 4,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => MarkBasePos
}

LookupGPOS5: struct {
    lookup_type: uint16 = 5,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => MarkLigPos
}

LookupGPOS6: struct {
    lookup_type: uint16 = 6,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => MarkMarkPos
}

LookupGPOS7: struct {
    lookup_type: uint16 = 7,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ContextPos
}

LookupGPOS8: struct {
    lookup_type: uint16 = 8,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ChainContextPos
}

LookupGPOS9: struct {
    lookup_type: uint16 = 9,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table[sub_table_count]: uint16 @offset => ExtensionPos
}

SinglePos: union {
    SinglePosFormat1,
    SinglePosFormat2
}

SinglePosFormat1: struct {
    pos_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    value_format: uint16,
    value: ValueRecord(value_format)
}

SinglePosFormat2: struct {
    pos_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    value_format: uint16,
    value_count: uint16,
    value[value_count]: ValueRecord(value_format)
}

PairPos: union {
    PairPosFormat1,
    PairPosFormat2
}

PairPosFormat1: struct {
    pos_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    value_format1: uint16,
    value_format2: uint16,
    pair_set_count: uint16,
    pair_set_offset[pair_set_count]: uint16 @offset => PairSet(value_format1, value_format2)
}

PairSet(value_format1, value_format2): struct {
    pair_value_count: uint16,
    pair_value_record[pair_value_count]: PairValueRecord(value_format1, value_format2)
}

PairValueRecord(value_format1, value_format2): struct {
    second_glyph: uint16,
    value1: ValueRecord(value_format1),
    value2: ValueRecord(value_format2)
}

PairPosFormat2: struct {
    pos_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    value_format1: uint16,
    value_format2: uint16,
    class_def1: uint16 @offset => ClassDef,
    class_def2: uint16 @offset => ClassDef,
    class1_count: uint16,
    class2_count: uint16,
    class1_record[class1_count]: struct {
	class2_record[class2_count]: struct {
	    value1: ValueRecord(value_format1),
	    value2: ValueRecord(value_format2)
	}
    }
}

CursivePos: union {
    CursivePosFormat1
}

CursivePosFormat1: struct {
    pos_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    entry_exit_count: uint16,
    entry_exit_record[entry_exit_count]: struct {
	entry_anchor: uint16 @offset => Anchor,
	exit_anchor: uint16 @offset => Anchor
    }
}

MarkBasePos: union {
    MarkBasePosFormat1
}

MarkBasePosFormat1: struct {
    pos_format: uint16 = 1,
    mark_coverage: uint16 @offset => Coverage,
    base_coverage: uint16 @offset => Coverage,
    class_count: uint16,
    mark_array: uint16 @offset => MarkArray,
    base_array: uint16 @offset => BaseArray(class_count)
}

BaseArray(class_count): struct {
    base_count: uint16,
    base_record[base_count]: struct {
	base_anchor[class_count]: uint16 = 0 | @offset => Anchor
    }
}

MarkLigPos: union {
    MarkLigPosFormat1
}

MarkLigPosFormat1: struct {
    pos_format: uint16 = 1,
    mark_coverage: uint16 @offset => Coverage,
    ligature_coverage: uint16 @offset => Coverage,
    class_count: uint16,
    mark_array: uint16 @offset => MarkArray,
    ligature_array: uint16 @offset => LigatureArray(class_count)
}

LigatureArray(class_count): struct {
    ligature_count: uint16,
    ligature_attach[ligature_count]: uint16 @offset => LigatureAttach(class_count)
}

LigatureAttach(class_count): struct {
    component_count: uint16,
    component_record[component_count]: struct {
	ligature_anchor[class_count]: uint16 = 0 | @offset => Anchor
    }
}

MarkMarkPos: union {
    MarkMarkPosFormat1
}

MarkMarkPosFormat1: struct {
    pos_format: uint16 = 1,
    mark1_coverage: uint16 @offset => Coverage,
    mark2_coverage: uint16 @offset => Coverage,
    class_count: uint16,
    mark1_array: uint16 @offset => MarkArray,
    mark2_array: uint16 @offset => Mark2Array(class_count)
}

Mark2Array(class_count): struct {
    mark2_count: uint16,
    mark2_record[mark2_count]: struct {
	mark2_anchor[class_count]: uint16 @offset => Anchor
    }
}

PosLookupRecord: struct {
    sequence_index: uint16,
    lookup_list_index: uint16 // FIXME index into LookupList
}

ContextPos: union {
    ContextPosFormat1,
    ContextPosFormat2,
    ContextPosFormat3
}

ContextPosFormat1: struct {
    pos_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    pos_rule_set_count: uint16,
    pos_rule_set[pos_rule_set_count]: uint16 @offset => PosRuleSet
}

PosRuleSet: struct {
    pos_rule_count: uint16,
    pos_rule[pos_rule_count]: uint16 @offset => PosRule
}

PosRule: struct {
    glyph_count: uint16, // FIXME > 0
    pos_count: uint16,
    input[glyph_count - 1]: uint16,
    pos_lookup_record[pos_count]: PosLookupRecord
}

ContextPosFormat2: struct {
    pos_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    class_def: uint16 @offset => ClassDef,
    pos_class_set_count: uint16,
    pos_class_set[pos_class_set_count]: uint16 = 0 | @offset => PosClassSet
}

PosClassSet: struct {
    pos_class_rule_count: uint16,
    pos_class_rule[pos_class_rule_count]: uint16 @offset => PosClassRule
}

PosClassRule: struct {
    glyph_count: uint16, // FIXME > 0
    pos_count: uint16,
    class[glyph_count - 1]: uint16,
    pos_lookup_record[pos_count]: PosLookupRecord
}

ContextPosFormat3: struct {
    pos_format: uint16 = 3,
    glyph_count: uint16,
    pos_count: uint16,
    coverage[glyph_count]: uint16 @offset => Coverage,
    pos_lookup_record[pos_count]: PosLookupRecord
}

ChainContextPos: union {
    ChainContextPosFormat1,
    ChainContextPosFormat2,
    ChainContextPosFormat3
}

ChainContextPosFormat1: struct {
    pos_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    chain_pos_rule_set_count: uint16,
    chain_pos_rule_set[chain_pos_rule_set_count]: uint16 @offset => ChainPosRuleSet
}

ChainPosRuleSet: struct {
    chain_pos_rule_count: uint16,
    chain_pos_rule[chain_pos_rule_count]: uint16 @offset => ChainPosRule
}

ChainPosRule: struct {
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16,
    input_glyph_count: uint16, // FIXME > 0
    input[input_glyph_count - 1]: uint16,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16,
    pos_count: uint16,
    pos_lookup_record[pos_count]: PosLookupRecord
}

ChainContextPosFormat2: struct {
    pos_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    backtrack_class_def: uint16 @offset => ClassDef,
    input_class_def: uint16 @offset => ClassDef,
    lookahead_class_def: uint16 @offset => ClassDef,
    chain_pos_class_set_count: uint16,
    chain_pos_class_set[chain_pos_class_set_count]: uint16 = 0 | @offset => ChainPosClassSet
}

ChainPosClassSet: struct {
    chain_pos_class_rule_count: uint16,
    chain_pos_class_rule[chain_pos_class_rule_count]: uint16 @offset => ChainPosClassRule
}

ChainPosClassRule: struct {
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16,
    input_glyph_count: uint16, // FIXME > 0
    input[input_glyph_count - 1]: uint16,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16,
    pos_count: uint16,
    pos_lookup_record[pos_count]: PosLookupRecord
}

ChainContextPosFormat3: struct {
    pos_format: uint16 = 3,
    backtrack_glyph_count: uint16,
    backtrack[backtrack_glyph_count]: uint16 @offset => Coverage,
    input_glyph_count: uint16,
    input[input_glyph_count]: uint16 @offset => Coverage,
    lookahead_glyph_count: uint16,
    lookahead[lookahead_glyph_count]: uint16 @offset => Coverage,
    pos_count: uint16,
    pos_lookup_record[pos_count]: PosLookupRecord
}

ExtensionPos: union {
    ExtensionPosFormat1
}

ExtensionPosFormat1: struct {
    pos_format: uint16 = 1,
    extension_lookup_type: uint16, // FIXME any GPOS lookup type except 9
    extension_offset: uint32 // FIXME @offset => SubTable
}

ValueRecord(value_format): struct {
    @if value_format & 0x01: x_placement: int16,
    @if value_format & 0x02: y_placement: int16,
    @if value_format & 0x04: x_advance: uint16,
    @if value_format & 0x08: y_advance: uint16,
    @if value_format & 0x10: x_pla_device: uint16 = 0 | @offset(PosTable) => Device,
    @if value_format & 0x20: y_pla_device: uint16 = 0 | @offset(PosTable) => Device,
    @if value_format & 0x40: x_adv_device: uint16 = 0 | @offset(PosTable) => Device,
    @if value_format & 0x80: y_adv_device: uint16 = 0 | @offset(PosTable) => Device
}

Anchor: union {
    AnchorFormat1,
    AnchorFormat2,
    AnchorFormat3
}

AnchorFormat1: struct {
    anchor_format: uint16 = 1,
    x_coordinate: int16,
    y_coordinate: int16
}

AnchorFormat2: struct {
    anchor_format: uint16 = 2,
    x_coordinate: int16,
    y_coordinate: int16,
    anchor_point: uint16 // FIXME index to glyph contour point
}

AnchorFormat3: struct {
    anchor_format: uint16 = 3,
    x_coordinate: int16,
    y_coordinate: int16,
    x_device_table: uint16 = 0 | @offset => Device,
    y_device_table: uint16 = 0 | @offset => Device
}

MarkArray: struct {
    mark_count: uint16,
    mark_record[mark_count]: struct {
	class: uint16,
	mark_anchor: uint16 @offset => Anchor
    }
}
