
// @pragma big_endian;

struct Header: byte[128] {
    profile_size: byte[4],
    cmm_type_signature: byte[4],
    struct profile_version_number: byte[4] {
	major_revision: byte[1],
	minor_revision: byte[1],
	_reserved: byte[2] = 0
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

