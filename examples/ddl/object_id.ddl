//! endian: le

/// https://docs.mongodb.com/manual/reference/method/ObjectId/
ObjectId = struct {
    epoch_time: u32,
    machine_id: u24,
    process_id: u16,
    counter: u24,
};
