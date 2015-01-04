
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
    table_records: struct[num_tables] { // FIXME sorted by tag
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
    offset_tables: uint32[num_fonts] @offset => OffsetTable
}

TTCHeader2: struct {
    ttc_tag: uint32 = 'ttcf',
    version: uint32 = 0x00020000,
    num_fonts: uint32,
    offset_tables: uint32[num_fonts] @offset => OffsetTable,
    dsig_tag: uint32 = 0 | 'DSIG',
    dsig_length: uint32,
    dsig_offset: uint32 = 0 | @offset => DSIG // FIXME byte[dsig_length]
    // FIXME if dsig_tag = 0 -> dsig_length = 0
    // FIXME if dsig_tag = 0 -> dsig_offset = 0
}

// Common structs for OpenType Layout

ScriptList: struct {
    script_count: uint16,
    script_records: struct[script_count] { // FIXME sorted by script_tag
	script_tag: uint32,
	script: uint16 @offset(ScriptList) => Script
    }
}

Script: struct {
    default_lang_sys: uint16 = 0 | @offset => LangSys,
    lang_sys_count: uint16,
    lang_sys_records: struct[lang_sys_count] { // FIXME sorted by lang_sys_tag
	lang_sys_tag: uint32,
	lang_sys: uint16 @offset(Script) => LangSys
    }
}

LangSys: struct {
    lookup_order: uint16 = 0,
    req_feature_index: uint16, // FIXME feature index or 0xFFFF
    feature_count: uint16,
    feature_index: uint16[feature_count] // FIXME index into FeatureList
}

FeatureList: struct {
    feature_count: uint16,
    feature_records: struct[feature_count] { // FIXME sorted by feature_tag
	feature_tag: uint32,
	feature: uint16 @offset(FeatureList) => Feature
    }
}

Feature: struct {
    feature_params: uint16 = 0,
    lookup_count: uint16,
    lookup_list_index: uint16[lookup_count] // FIXME index into LookupList
}

// FIXME duplicated below for GSUB and GPOS
LookupList: struct {
    lookup_count: uint16,
    lookup: uint16[lookup_count] @offset => Lookup
}

// FIXME duplicated below for GSUB and GPOS
Lookup: struct {
    lookup_type: uint16,
    lookup_flag: uint16, // bitfield
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] // FIXME @offset => SubTable
    //FIXME mark_filtering_set: uint16 // index into GDEF, only present if UseMarkFilteringSet
}

Coverage: union {
    CoverageFormat1,
    CoverageFormat2
}

CoverageFormat1: struct {
    coverage_format: uint16 = 1,
    glyph_count: uint16,
    glyph_array: uint16[glyph_count] // FIXME sorted numerically
}

CoverageFormat2: struct {
    coverage_format: uint16 = 2,
    range_count: uint16,
    range_record: struct[range_count] {
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
    class_value_array: uint16[glyph_count]
}

ClassDefFormat2: struct {
    class_format: uint16 = 2,
    class_range_count: uint16,
    class_range_record: struct[class_range_count] {
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
    lookup: uint16[lookup_count] @offset => LookupGSUB
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
    sub_table: uint16[sub_table_count] @offset => SingleSubst
}

LookupGSUB2: struct {
    lookup_type: uint16 = 2,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => MultipleSubst
}

LookupGSUB3: struct {
    lookup_type: uint16 = 3,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => AlternateSubst
}

LookupGSUB4: struct {
    lookup_type: uint16 = 4,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => LigatureSubst
}

LookupGSUB5: struct {
    lookup_type: uint16 = 5,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ContextSubst
}

LookupGSUB6: struct {
    lookup_type: uint16 = 6,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ChainContextSubst
}

LookupGSUB7: struct {
    lookup_type: uint16 = 7,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ExtensionSubst
}

LookupGSUB8: struct {
    lookup_type: uint16 = 8,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ReverseChainSingleSubst
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
    substitute: uint16[glyph_count]
}

MultipleSubst: union {
    MultipleSubstFormat1
}

MultipleSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    sequence_count: uint16,
    sequence: uint16[sequence_count] @offset => Sequence
}

Sequence: struct {
    glyph_count: uint16, // FIXME > 0
    substitute: uint16[glyph_count]
}

AlternateSubst: union {
    AlternateSubstFormat1
}

AlternateSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    alternate_set_count: uint16,
    alternate_set: uint16[alternate_set_count] @offset => AlternateSet
}

AlternateSet: struct {
    glyph_count: uint16,
    alternate: uint16[glyph_count]
}

LigatureSubst: union {
    LigatureSubstFormat1
}

LigatureSubstFormat1: struct {
    subst_format: uint16 = 1,
    coverage: uint16 @offset => Coverage,
    lig_set_count: uint16,
    ligature_set: uint16[lig_set_count] @offset => LigatureSet
}

LigatureSet: struct {
    ligature_count: uint16,
    ligature: uint16[ligature_count] @offset => Ligature
}

Ligature: struct {
    lig_glyph: uint16,
    comp_count: uint16,
    component: uint16[comp_count - 1]
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
    sub_rule_set: uint16[sub_rule_set_count] @offset => SubRuleSet
}

SubRuleSet: struct {
    sub_rule_count: uint16,
    sub_rule: uint16[sub_rule_count] @offset => SubRule
}

SubRule: struct {
    glyph_count: uint16,
    subst_count: uint16,
    input: uint16[glyph_count - 1],
    substs: SubstLookupRecord[subst_count]
}

ContextSubstFormat2: struct {
    subst_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    class_def: uint16 @offset => ClassDef,
    sub_class_count: uint16,
    sub_class_set: uint16[sub_class_count] @offset => SubClassSet
}

SubClassSet: struct {
    sub_class_rule_count: uint16,
    sub_class_rule: uint16[sub_class_rule_count] @offset => SubClassRule
}

SubClassRule: struct {
    glyph_count: uint16,
    subst_count: uint16,
    class: uint16[glyph_count - 1],
    substs: SubstLookupRecord[subst_count]
}

ContextSubstFormat3: struct {
    subst_format: uint16 = 3,
    glyph_count: uint16,
    subst_count: uint16,
    coverage: uint16[glyph_count] @offset => Coverage,
    substs: SubstLookupRecord[subst_count]
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
    chain_sub_rule_set: uint16[chain_sub_rule_set_count] @offset => ChainSubRuleSet
}

ChainSubRuleSet: struct {
    chain_sub_rule_count: uint16,
    chain_sub_rule: uint16[chain_sub_rule_count] @offset => ChainSubRule
}

ChainSubRule: struct {
    backtrack_glyph_count: uint16,
    backtrack: uint16[backtrack_glyph_count],
    input_glyph_count: uint16,
    input: uint16[input_glyph_count],
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count],
    subst_count: uint16,
    substs: SubstLookupRecord[subst_count]
}

ChainContextSubstFormat2: struct {
    subst_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    backtrack_class_def: uint16 @offset => ClassDef,
    input_class_def: uint16 @offset => ClassDef,
    lookahead_class_def: uint16 @offset => ClassDef,
    chain_sub_class_set_count: uint16,
    chain_sub_class_set: uint16[chain_sub_class_set_count] @offset => ChainSubClassSet
}

ChainSubClassSet: struct {
    chain_sub_class_rule_count: uint16,
    chain_sub_class_rule: uint16[chain_sub_class_rule_count] @offset => ChainSubClassRule
}

ChainSubClassRule: struct{
    backtrack_glyph_count: uint16,
    backtrack: uint16[backtrack_glyph_count],
    input_glyph_count: uint16,
    input: uint16[input_glyph_count - 1],
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count],
    subst_count: uint16,
    substs: SubstLookupRecord[subst_count]
}

ChainContextSubstFormat3: struct {
    subst_format: uint16 = 3,
    backtrack_glyph_count: uint16,
    backtrack: uint16[backtrack_glyph_count] @offset => Coverage,
    input_glyph_count: uint16,
    input: uint16[input_glyph_count] @offset => Coverage,
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count] @offset => Coverage,
    subst_count: uint16,
    substs: SubstLookupRecord[subst_count]
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
    backtrack: uint16[backtrack_glyph_count] @offset => Coverage,
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count] @offset => Coverage,
    glyph_count: uint16,
    substitute: uint16[glyph_count]
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
    lookup: uint16[lookup_count] @offset => LookupGPOS
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
    sub_table: uint16[sub_table_count] @offset => SinglePos
}

LookupGPOS2: struct {
    lookup_type: uint16 = 2,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => PairPos
}

LookupGPOS3: struct {
    lookup_type: uint16 = 3,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => CursivePos
}

LookupGPOS4: struct {
    lookup_type: uint16 = 4,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => MarkBasePos
}

LookupGPOS5: struct {
    lookup_type: uint16 = 5,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => MarkLigPos
}

LookupGPOS6: struct {
    lookup_type: uint16 = 6,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => MarkMarkPos
}

LookupGPOS7: struct {
    lookup_type: uint16 = 7,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ContextPos
}

LookupGPOS8: struct {
    lookup_type: uint16 = 8,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ChainContextPos
}

LookupGPOS9: struct {
    lookup_type: uint16 = 9,
    lookup_flag: uint16,
    sub_table_count: uint16,
    sub_table: uint16[sub_table_count] @offset => ExtensionPos
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
    value: ValueRecord(value_format)[value_count]
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
    pair_set_offset: uint16[pair_set_count] @offset => PairSet(value_format1, value_format2)
}

PairSet(value_format1, value_format2): struct {
    pair_value_count: uint16,
    pair_value_record: PairValueRecord(value_format1, value_format2)[pair_value_count]
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
    class1_record: struct[class1_count] {
	class2_record: struct[class2_count] {
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
    entry_exit_record: struct[entry_exit_count] {
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
    base_record: struct[base_count] {
	base_anchor: uint16[class_count] @offset => Anchor
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
    ligature_attach: uint16[ligature_count] @offset => LigatureAttach(class_count)
}

LigatureAttach(class_count): struct {
    component_count: uint16,
    component_record: struct[component_count] {
	ligature_anchor: uint16[class_count] // FIXME 0 | @offset => Anchor
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
    mark2_record: struct[mark2_count] {
	mark2_anchor: uint16[class_count] @offset => Anchor
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
    pos_rule_set: uint16[pos_rule_set_count] @offset => PosRuleSet
}

PosRuleSet: struct {
    pos_rule_count: uint16,
    pos_rule: uint16[pos_rule_count] @offset => PosRule
}

PosRule: struct {
    glyph_count: uint16,
    pos_count: uint16,
    input: uint16[glyph_count - 1],
    pos_lookup_record: PosLookupRecord[pos_count]
}

ContextPosFormat2: struct {
    pos_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    class_def: uint16 @offset => ClassDef,
    pos_class_set_count: uint16,
    pos_class_set: uint16[pos_class_set_count] // FIXME 0 | @offset => PosClassSet
}

PosClassSet: struct {
    pos_class_rule_count: uint16,
    pos_class_rule: uint16[pos_class_rule_count] @offset => PosClassRule
}

PosClassRule: struct {
    glyph_count: uint16,
    pos_count: uint16,
    class: uint16[glyph_count - 1],
    pos_lookup_record: PosLookupRecord[pos_count]
}

ContextPosFormat3: struct {
    pos_format: uint16 = 3,
    glyph_count: uint16,
    pos_count: uint16,
    coverage: uint16[glyph_count] @offset => Coverage,
    pos_lookup_record: PosLookupRecord[pos_count]
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
    chain_pos_rule_set: uint16[chain_pos_rule_set_count] @offset => ChainPosRuleSet
}

ChainPosRuleSet: struct {
    chain_pos_rule_count: uint16,
    chain_pos_rule: uint16[chain_pos_rule_count] @offset => ChainPosRule
}

ChainPosRule: struct {
    backtrack_glyph_count: uint16,
    backtrack: uint16[backtrack_glyph_count],
    input_glyph_count: uint16,
    input: uint16[input_glyph_count - 1],
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count],
    pos_count: uint16,
    pos_lookup_record: PosLookupRecord[pos_count]
}

ChainContextPosFormat2: struct {
    pos_format: uint16 = 2,
    coverage: uint16 @offset => Coverage,
    backtrack_class_def: uint16 @offset => ClassDef,
    input_class_def: uint16 @offset => ClassDef,
    lookahead_class_def: uint16 @offset => ClassDef,
    chain_pos_class_set_count: uint16,
    chain_pos_class_set: uint16[chain_pos_class_set_count] // FIXME 0 | @offset => ChainPosClassSet
}

ChainPosClassSet: struct {
    chain_pos_class_rule_count: uint16,
    chain_pos_class_rule: uint16[chain_pos_class_rule_count] @offset => ChainPosClassRule
}

ChainPosClassRule: struct {
    backtrack_glyph_count: uint16,
    backtrack: uint16[backtrack_glyph_count],
    input_glyph_count: uint16,
    input: uint16[input_glyph_count - 1],
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count],
    pos_count: uint16,
    pos_lookup_record: PosLookupRecord[pos_count]
}

ChainContextPosFormat3: struct {
    pos_format: uint16 = 3,
    backtrack_glyph_count: uint16,
    backtrack: uint16[backtrack_glyph_count] @offset => Coverage,
    input_glyph_count: uint16,
    input: uint16[input_glyph_count] @offset => Coverage,
    lookahead_glyph_count: uint16,
    lookahead: uint16[lookahead_glyph_count] @offset => Coverage,
    pos_count: uint16,
    pos_lookup_record: PosLookupRecord[pos_count]
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
    @if value_format & 0x10: x_pla_device: uint16, // FIXME  => 0 | @offset(PosTable) => Device
    @if value_format & 0x20: y_pla_device: uint16, // FIXME  => 0 | @offset(PosTable) => Device
    @if value_format & 0x40: x_adv_device: uint16, // FIXME  => 0 | @offset(PosTable) => Device
    @if value_format & 0x80: y_adv_device: uint16 // FIXME  => 0 | @offset(PosTable) => Device
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
    mark_record: struct[mark_count] {
	class: uint16,
	mark_anchor: uint16 @offset => Anchor
    }
}
