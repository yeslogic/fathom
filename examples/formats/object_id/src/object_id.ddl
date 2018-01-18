import prelude (..);

/// https://docs.mongodb.com/manual/reference/method/ObjectId/
ObjectId = struct {
    epoch_time: i32be,
    machine_id: u24be,
    process_id: u16be,
    counter: u24be,
};
