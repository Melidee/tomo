

pub enum TopLevelAst<'a> {
    Module(Identifier<'a>),
    Use(Identifier<'a>),
    Const(Const<'a>),
    Fn(Function<'a>),
}

struct Identifier<'a>(&'a str);

struct Const<'a> {
    identifier: Identifier<'a>,
    type_: Type<'a>,
    expression: Expression<'a>,
}

struct Function<'a> {
    identifier: Identifier<'a>,
    args: Vec<DefArg<'a>>,
    return_type: Type<'a>,
    block: Block<'a>,
}

struct DefArg<'a> {
    identifier: Identifier<'a>,
    type_: Type<'a>,
}

struct Block<'a> {
    statements: Vec<Statement<'a>>
}

enum Statement<'a> {
    Expression(Expression<'a>),
}

enum Type<'a> {
    Identifier(Identifier<'a>),
}

enum Expression<'a> {
    Identifier(Identifier<'a>),
}