
// @pragma big_endian;

ICC: struct {
    header: Header,
    tag_table: TagTable
}

Header: struct {
    profile_size: uint32,
    cmm_type_signature: byte[4],
    profile_version_number: struct {
	major_revision: byte,
	minor_revision: byte,
	_reserved1: byte = 0,
	_reserved2: byte = 0
    },
    profile_device_class_signature: byte[4] =
	'scnr' | 'mntr' | 'prtr' | 'link' | 'spac' | 'abst' | 'nmcl',
    color_space_signature: byte[4] =
	'XYZ ' | 'Lab ' | 'Luv ' | 'YCbr' | 'Yxy ' | 'RGB ' | 'GRAY' |
	'HSV ' | 'HLS ' | 'CMYK' | 'CMY ' |
	'2CLR' | '3CLR' | '4CLR' | '5CLR' | '6CLR' | '7CLR' | '8CLR' |
	'9CLR' | 'ACLR' | 'BCLR' | 'CCLR' | 'DCLR' | 'ECLR' | 'FCLR',
    profile_connection_space: byte[4] = 'XYZ ' | 'Lab ',
    creation_date_time: byte[12],
    profile_file_signature: byte[4] = 'acsp',
    primary_platform_signature: byte[4] =
	'APPL' | 'MSFT' | 'SGI ' | 'SUNW' | 'TGNT' | 0 | *,
    cmm_flags: byte[4], // bitfield
    device_manufacturer: byte[4],
    device_model: byte[4],
    device_attributes: byte[8], // bitfield
    rendering_intent: byte[4],
    xyz_illuminant: byte[12],
    profile_creator_signature: byte[4],
    // ICC 4 has profile_id: byte[16],
    _padding: byte[44]
}

TagTable: struct {
    num_tags: uint32,
    tag_array: struct[num_tags] {
	tag_signature: byte[4],
	tag_offset: uint32,
	tag_length: uint32
    }
}

