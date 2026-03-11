use std::fmt::Display;

pub struct Function<'a> {
    return_type: Type,
    identifier: &'a str,
    args: Vec<Arg<'a>>,
    start: Block<'a>,
}

impl<'a> Display for Function<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| format!("{} {}", arg.arg_type, arg.identifier))
            .collect::<Vec<String>>()
            .join(", ");
        write!(
            f,
            "function {} ${}({}) {{\n{}}}",
            self.return_type, self.identifier, args, self.start
        )
    }
}

pub struct Arg<'a> {
    arg_type: Type,
    identifier: &'a str,
}

pub struct Block<'a> {
    symbol: &'a str,
    instructions: Vec<Instruction<'a>>,
    jump: Instruction<'a>,
}

impl<'a> Display for Block<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub enum Type {
    Byte,
    HalfWord,
    Word,
    Long,
    Single,
    Double,
    Aggregate(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Byte => "b",
                Self::HalfWord => "h",
                Self::Word => "w",
                Self::Long => "l",
                Self::Single => "s",
                Self::Double => "d",
                Self::Aggregate(id) => id,
            }
        )
    }
}

pub struct TypeDef {
    name: String,
    alignment: Option<u32>,
    types: Vec<(Type, i32)>,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let types = self
            .types
            .iter()
            .map(|(t, n)| {
                let mut type_pair = t.to_string();
                if *n != 1 {
                    type_pair.push_str(" ");
                    type_pair.push_str(&n.to_string());
                }
                type_pair
            })
            .collect::<Vec<String>>()
            .join(", ");
        let alignment = if let Some(a) = self.alignment {
            format!("align {}", a)
        } else {
            "".to_string()
        };
        write!(f, "type {} = {} {{ {} }}", self.name, alignment, types)
    }
}

pub enum Instruction<'a> {
    Add,
    Subtract,
    Multiply,
    Divide,
    UnsignedDivide,
    Remainder,
    UnsignedRemainder,
    Negate,
    And,
    Or,
    Xor,
    ArithmeticShift,
    ShiftRight,
    ShiftLeft,

    Alloc16,
    Alloc4,
    Alloc8,
    Blit,
    Call {
        identifier: &'a str,
        args: Vec<Arg<'a>>,
    },
    Return(&'a str),
}

fn whatever() {
    let f = Function {
        return_type: Type::Word,
        identifier: "$main",
        args: vec![],
        start: Block {
            symbol: "start",
            instructions: vec![Instruction::Call {
                identifier: "__fea20e18__std_io_println",
                args: vec![],
            }],
            jump: Instruction::Return("0"),
        },
    };
}
