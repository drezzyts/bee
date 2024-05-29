use crate::{
    expressions::Expression, position::Position, token::Token, typechecker::Type, visitors::{StatementVisitable, StatementVisitor}
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    Echo(EchoStatement),
    Variable(VariableStatement),
    Block(BlockStatement),
    If(IfStatement),
    While(WhileStatement),
    Function(FunctionStatement),
    Return(ReturnStatement),
}

impl Statement {
    pub fn position(&self) -> Position {
        match self {
            Statement::Expression(stmt) => {
                Expression::position(*stmt.expression.clone()).clone()
            }
            Statement::Echo(stmt) => {
                Expression::position(*stmt.expression.clone()).clone()
            }
            Statement::Variable(stmt) => {
                stmt.name.position.clone()
            }
            Statement::Return(stmt) => stmt.keyword.position.clone(), 
            Statement::Block(_) => unreachable!(),
            Statement::If(_) => unreachable!(),
            Statement::While(_) => unreachable!(),
            Statement::Function(_) => unreachable!(),
        }
    }
}

#[allow(unreachable_patterns)]
impl<T> StatementVisitable<T> for Statement {
    fn accept(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
        match self {
            Statement::Expression(stmt) => visitor.visit_expr_stmt(stmt),
            Statement::Echo(stmt) => visitor.visit_echo_stmt(stmt),
            Statement::Variable(stmt) => visitor.visit_var_stmt(stmt),
            Statement::Block(stmt) => visitor.visit_block_stmt(stmt),
            Statement::If(stmt) => visitor.visit_if_stmt(stmt),
            Statement::While(stmt) => visitor.visit_while_stmt(stmt),
            Statement::Function(stmt) => visitor.visit_fun_stmt(stmt),
            Statement::Return(stmt) => visitor.visist_return_stmt(stmt),
            _ => unimplemented!(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}

impl ExpressionStatement {
    pub fn new(expression: Box<Expression>) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct EchoStatement {
    pub expression: Box<Expression>,
}

impl EchoStatement {
    pub fn new(expression: Box<Expression>) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct VariableStatement {
    pub name: Token,
    pub initializer: Option<Expression>,
    pub constant: bool,
    pub typing: Option<Token>, 
}

impl VariableStatement {
    pub fn new(name: Token, initializer: Option<Expression>, constant: bool, typing: Option<Token>) -> Self {
        Self { name, initializer, constant, typing }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub left: Token,
    pub statements: Vec<Box<Statement>>,
    pub right: Token
}

impl BlockStatement {
    pub fn new(left: Token, statements: Vec<Box<Statement>>, right: Token) -> Self {
        Self { left, statements, right }
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>
}

impl IfStatement {
    pub fn new(
        condition: Box<Expression>,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>
    ) -> Self {
        Self { condition, consequent, alternate }
    }
}


#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Box<Expression>,
    pub body: Box<Statement>
}

impl WhileStatement {
    pub fn new(
        condition: Box<Expression>,
        body: Box<Statement>
    ) -> Self {
        Self { condition, body }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionStatement {
    pub name: Token,
    pub params: Vec<(Token, Token)>,
    pub body: Box<Statement>,
    pub typing: Option<Token>,
}

impl FunctionStatement {
    pub fn new(name: Token, params: Vec<(Token, Token)>, body: Box<Statement>, typing: Option<Token>) -> Self {
        Self { name, params, body, typing }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub keyword: Token,
    pub value: Option<Box<Expression>>
}

impl ReturnStatement {
    pub fn new(keyword: Token, value: Option<Box<Expression>>) -> Self {
        Self { keyword, value }
    }
}