#![allow(dead_code)]
#![allow(unused_variables)]

enum ExprBool {
    Or(Box<ExprBool>, Box<ExprBool>),
    Eq(Box<ExprInt>, Box<ExprInt>),
    Gt(Box<ExprInt>, Box<ExprInt>)
}

enum ExprInt {
    Const(u32),
    Field(String)
}

enum Type {
    Int(IntType),
    Struct(Box<StructType>),
    Array(Box<ArrayType>)
}

enum IntType {
    Uint8,
    Uint16,
    Uint32,
    Int8,
    Int16,
    Int32
}

struct StructType {
    items: Vec<StructItem>,
    conds: Vec<ExprBool>
}

enum StructItem {
    Field(Field),
    CondSection(ExprBool, StructType)
}

struct Field {
    field_name: String,
    field_type: Type
}

struct ArrayType {
    array_length: ExprInt,
    array_type: Type
}

fn match_type(buf: &[u8], ddl_type: Type) -> bool {
    false // FIXME not yet implemented
}

fn main() {
    let f1 = Field {
        field_name: String::from("version"),
        field_type: Type::Int(IntType::Uint8)
    };

    let f2 = Field {
        field_name: String::from("length"),
        field_type: Type::Int(IntType::Uint8)
    };

    let atype = ArrayType {
        array_length: ExprInt::Field(String::from("length")),
        array_type: Type::Int(IntType::Uint8)
    };

    let f3 = Field {
        field_name: String::from("values"),
        field_type: Type::Array(Box::new(atype))
    };

    let f4 = Field {
        field_name: String::from("extra"),
        field_type: Type::Int(IntType::Uint8)
    };

    let c1 =
        ExprBool::Or(
            Box::new(ExprBool::Eq(
                Box::new(ExprInt::Field(String::from("version"))),
                Box::new(ExprInt::Const(1))
            )),
            Box::new(ExprBool::Eq(
                Box::new(ExprInt::Field(String::from("version"))),
                Box::new(ExprInt::Const(2))
            ))
        );

    let c2 =
        ExprBool::Gt(
            Box::new(ExprInt::Field(String::from("version"))),
            Box::new(ExprInt::Const(1))
        );

    let s1 = StructType {
        items: vec![StructItem::Field(f4)],
        conds: vec![]
    };

    let s = StructType {
        items: vec![
            StructItem::Field(f1),
            StructItem::Field(f2),
            StructItem::Field(f3),
            StructItem::CondSection(c2, s1)
        ],
        conds: vec![c1]
    };

    let ddl = Type::Struct(Box::new(s));

    let buf: [u8; 0] = []; // FIXME load from file

    let b = match_type(&buf, ddl);

    println!("match result: {}", b);
}

