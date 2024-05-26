use crate::{
    error::BeeError, expressions::*, position, statements::{self, *}, token::{Token, TokenKind}
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String) -> Self {
        Self {
            tokens,
            current: 0,
            source,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, BeeError> {
        let mut statements: Vec<Statement> = vec![];

        while !self.is_eof() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(message) => {
                    let position = if self.peek().kind == TokenKind::Eof {
                        let mut aux = self.previous().position.clone();
                        aux.cend += 1;
                        aux
                    } else {
                        self.peek().position.clone()
                    };

                    let error = BeeError::report(
                        &position,  
                        message.as_str(),
                        "parser",
                        self.source.clone(),
                    );

                    return Err(error);
                }
            }
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
            Err(err) => Err(err), //self.synchronize()
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
        match self.peek().kind {
            TokenKind::LeftBrace => Ok(self.block_stmt()?),
            TokenKind::Echo => Ok(self.echo_stmt()?),
            _ => self.expression_stmt()
        }
    }

    fn block_stmt(&mut self) -> Result<Statement, String> {
        let left: Token = self.eat(TokenKind::LeftBrace)?;
        
        let mut statements: Vec<Box<Statement>> = vec![];

        while !self.is_curr(TokenKind::RightBrace) && !self.is_eof() {
            statements.push(Box::new(self.declaration()?));
        }

        let right: Token = self.eat(TokenKind::RightBrace)?;
        let statement = BlockStatement::new(left, statements, right);
        
        Ok(Statement::Block(statement))
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
            self.next();
            let value = self.assignment_expr()?;

            match expression {
                Expression::Variable(expr) => {
                    let assignment = AssignmentExpression::new(expr.name.clone(), Box::new(value));
                    return Ok(Expression::Assignment(assignment));
                }
                _ => {
                    return Err("invalid assignment target has founded while parsing.".to_string());
                }
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
            }
            TokenKind::Identifier => {
                let name: Token = self.eat(TokenKind::Identifier)?;
                let expression = VariableExpression::new(name);
                Ok(Expression::Variable(expression))
            }
            TokenKind::Eof => Err("unexpected end of input.".to_string()),
            _ => Err(format!(
                "invalid token has founded while parsing: {:?}",
                self.peek().kind
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
            Err(format!(
                "unexpected end of input founded while parsing\n ~ expected {:?}",
                kind
            ))
        } else {
            Err(format!(
                "unexpected token founded while parsing\n ~ expected {:?}, founded {:?}",
                kind,
                self.peek().kind
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
