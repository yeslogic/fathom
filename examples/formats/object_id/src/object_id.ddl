/// https://docs.mongodb.com/manual/reference/method/ObjectId/
ObjectId = struct {
    epoch_time: u32le,
    machine_id: u24le,
    process_id: u16le,
    counter: u24le,
};
