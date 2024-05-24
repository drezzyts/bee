use crate::{
    expressions::{self, AssignmentExpression, Expression, *}, program::Program, statements::{EchoStatement, ExpressionStatement, Statement, VariableStatement}, token::{self, Token, TokenKind}
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements: Vec<Statement> = vec![];

        while !self.is_eof() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    fn synchronize(&mut self) -> () {
        todo!()
    }

    fn declaration(&mut self) -> Result<Statement, String> {
        let result = if self.is_curr(TokenKind::Mut) {
            self.var_declaration()
        } else {
            self.statement()
        };

        match result {
            Ok(stmt) => Ok(stmt),
            Err(a) => Err(a), //self.synchronize()
        }   
    }

    fn var_declaration(&mut self) -> Result<Statement, String> {
        self.eat(TokenKind::Mut)?;

        let constant = if self.is_curr(TokenKind::Optional) {
            self.next();
            true
        } else {
            false
        };
        
        let name = self.eat(TokenKind::Identifier)?;
        let initializer: Option<Expression> = if self.is_curr(TokenKind::Equal) {
            self.eat(TokenKind::Equal)?;
            Some(self.expression()?)
        } else {
            None
        }; 
        self.eat(TokenKind::SemiColon)?;

        let stmt = VariableStatement::new(name, initializer, constant);
        Ok(Statement::Variable(stmt))
    }

    fn statement(&mut self) -> Result<Statement, String> {
        if self.is_curr(TokenKind::Echo) {
            self.echo_stmt()
        } else {
            self.expression_stmt()
        }
    }

    fn echo_stmt(&mut self) -> Result<Statement, String> {
        self.eat(TokenKind::Echo)?;
        let expression = self.expression()?;
        self.eat(TokenKind::SemiColon)?;
        let statement = EchoStatement::new(Box::new(expression));
        Ok(Statement::Echo(statement))
    }

    fn expression_stmt(&mut self) -> Result<Statement, String> {
        let expression = self.expression()?;
        self.eat(TokenKind::SemiColon)?;
        let statement = ExpressionStatement::new(Box::new(expression));
        Ok(Statement::Expression(statement))
    }

    fn expression(&mut self) -> Result<Expression, String> {
        self.assignment_expr()
    }

    fn assignment_expr(&mut self) -> Result<Expression, String> {
        let expression = self.equality()?;

        

        if self.is_curr(TokenKind::Equal) {
            let equals = self.next();
            let value = self.assignment_expr()?;

            match expression {
                Expression::Variable(expr) => {
                    let assignment = AssignmentExpression::new(expr.name.clone(), Box::new(value));
                    return Ok(Expression::Assignment(assignment));
                },
                _ => return Err(Program::report(equals.position.clone(), "parser", "invalid assignment target has founded while parsing."))
            }
        }

        Ok(expression)
    }

    fn equality(&mut self) -> Result<Expression, String> {
        let mut left: Expression = self.comparison()?;

        while self.match_tokens(vec![TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator: Token = self.previous();
            let right: Expression = self.comparison()?;
            let expression = BinaryExpression::new(Box::new(left), operator, Box::new(right));
            left = Expression::Binary(expression);
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expression, String> {
        let mut left: Expression = self.term()?;

        while self.match_tokens(vec![
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let operator: Token = self.previous();
            let right: Expression = self.term()?;
            let expression = BinaryExpression::new(Box::new(left), operator, Box::new(right));
            left = Expression::Binary(expression);
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Expression, String> {
        let mut left: Expression = self.factor()?;

        while self.match_tokens(vec![TokenKind::Plus, TokenKind::Minus]) {
            let operator: Token = self.previous();
            let right: Expression = self.factor()?;
            let expression = BinaryExpression::new(Box::new(left), operator, Box::new(right));
            left = Expression::Binary(expression);
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expression, String> {
        let mut left: Expression = self.unary()?;

        while self.match_tokens(vec![TokenKind::Star, TokenKind::Slash]) {
            let operator: Token = self.previous();
            let right: Expression = self.unary()?;
            let expression = BinaryExpression::new(Box::new(left), operator, Box::new(right));
            left = Expression::Binary(expression);
        }

        Ok(left)
    }

    fn unary(&mut self) -> Result<Expression, String> {
        while self.match_tokens(vec![TokenKind::Bang, TokenKind::Minus]) {
            let operator: Token = self.previous();
            let right: Expression = self.unary()?;
            let expression = UnaryExpression::new(operator, Box::new(right));
            return Ok(Expression::Unary(expression));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expression, String> {
        match self.peek().kind {
            TokenKind::True => {
                let expression = LiteralExpression::new(LiteralValue::True, self.next());
                Ok(Expression::Literal(expression))
            }
            TokenKind::False => {
                let expression = LiteralExpression::new(LiteralValue::False, self.next());
                Ok(Expression::Literal(expression))
            }
            TokenKind::Nil => {
                let expression = LiteralExpression::new(LiteralValue::Nil, self.next());
                Ok(Expression::Literal(expression))
            }
            TokenKind::Integer | TokenKind::Float | TokenKind::Char | TokenKind::String => {
                let value: LiteralValue = self.next().value.unwrap().to_literal_value();
                let expression = LiteralExpression::new(value, self.peek());
                Ok(Expression::Literal(expression))
            }
            TokenKind::LeftParen => {
                let left: Token = self.eat(TokenKind::LeftParen)?;
                let expr: Expression = self.expression()?;
                let right: Token = self.eat(TokenKind::RightParen)?;
                let expression = GroupExpression::new(left, Box::new(expr), right);
                Ok(Expression::Group(expression))
            },
            TokenKind::Identifier => {
                let name: Token = self.eat(TokenKind::Identifier)?;
                let expression = VariableExpression::new(name);
                Ok(Expression::Variable(expression))
            }
            TokenKind::Eof => {
                Err(Program::report(self.peek().position, "parser", "unexpected end of input."))
            }
            _ => Err(Program::report(
                self.peek().position,
                "parser",
                format!(
                    "invalid token has founded while parsing: {:?}",
                    self.peek().kind
                )
                .as_str(),
            )),
        }
    }

    fn match_tokens(&mut self, kinds: Vec<TokenKind>) -> bool {
        for kind in kinds {
            if self.is_curr(kind) {
                self.next();
                return true;
            }
        }

        false
    }

    fn is_curr(&self, kind: TokenKind) -> bool {
        if self.is_eof() {
            return false;
        }

        self.peek().kind == kind
    }

    fn next(&mut self) -> Token {
        if !self.is_eof() {
            self.current += 1;
            self.previous()
        } else {
            self.previous()
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Result<Token, String> {
        if self.is_curr(kind.clone()) {
            Ok(self.next())
        } else if self.peek().kind == TokenKind::Eof {
            Err(Program::report(
                self.peek().position,
                "parser",
                format!(
                    "unexpected end of input founded while parsing - expected {:?}",
                    kind
                )
                .as_str(),
            ))
        } else {
            Err(Program::report(
                self.peek().position,
                "parser",
                format!(
                    "unexpected token founded while parsing - expected {:?}, founded {:?}",
                    kind,
                    self.peek().kind
                )
                .as_str(),
            ))
        }
    }
    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }
}
