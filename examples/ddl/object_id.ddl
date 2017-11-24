/// https://docs.mongodb.com/manual/reference/method/ObjectId/
ObjectId = struct {
    epoch_time: u32le,
    machine_id: [u8; 3u32], // FIXME: should be u24le
    process_id: u16le,
    counter: [u8; 3u32], // FIXME: should be u24le
};
