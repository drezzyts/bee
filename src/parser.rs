use crate::{
    error::BeeError, expressions::{self, *}, statements::*, token::{Token, TokenKind}
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
        } else if self.is_curr(TokenKind::Fun) {
            self.fun_declaration("function")
        } else {
            self.statement()
        };

        match result {
            Ok(stmt) => Ok(stmt),
            Err(err) => Err(err),
        }
    }

    fn fun_declaration(&mut self, _: &str) -> Result<Statement, String> {
        self.eat(TokenKind::Fun)?;
        
        let name = self.eat(TokenKind::Identifier)?;
        self.eat(TokenKind::LeftParen)?;

        let mut parameters: Vec<Token> = vec![];

        if !self.is_curr(TokenKind::RightParen) {
            loop {
                if parameters.len() > 255 {
                    return Err("function cannot have more than 255 parameters".to_string());
                }

                parameters.push(self.eat(TokenKind::Identifier)?);

                if self.peek().kind != TokenKind::Comma {
                    break;
                } else {
                    self.next();
                }
            }
        }

        self.eat(TokenKind::RightParen)?;
        
        let body = Box::new(self.block_stmt()?);
        let stmt = FunctionStatement::new(name, parameters, body);
        Ok(Statement::Function(stmt))
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
        
        let typing = if self.is_curr(TokenKind::As) {
            self.eat(TokenKind::As)?;
            Some(self.eat(TokenKind::Identifier)?)
        } else {
            None
        };

        let initializer: Option<Expression> = if self.is_curr(TokenKind::Equal) {
            self.eat(TokenKind::Equal)?;
            Some(self.expression()?)
        } else {
            None
        };
        self.eat(TokenKind::SemiColon)?;

        let stmt = VariableStatement::new(name, initializer, constant, typing);
        Ok(Statement::Variable(stmt))
    }

    fn statement(&mut self) -> Result<Statement, String> {
        match self.peek().kind {
            TokenKind::LeftBrace => Ok(self.block_stmt()?),
            TokenKind::Echo => Ok(self.echo_stmt()?),
            TokenKind::If => Ok(self.if_stmt()?),
            TokenKind::While => Ok(self.while_stmt()?),
            TokenKind::For => Ok(self.for_stmt()?),
            TokenKind::Return => Ok(self.return_stmt()?),
            _ => self.expression_stmt()
        }
    }

    fn return_stmt(&mut self) -> Result<Statement, String> {
        let keyword: Token = self.eat(TokenKind::Return)?;
        let mut value: Option<Box<Expression>> = None;

        if !self.is_curr(TokenKind::SemiColon) {
            value = Some(Box::new(self.expression()?));
        }

        self.eat(TokenKind::SemiColon)?;

        let stmt = ReturnStatement::new(keyword, value);
        Ok(Statement::Return(stmt))
    }

    fn for_stmt(&mut self) -> Result<Statement, String> {
        self.eat(TokenKind::For)?;
        
        let left = self.eat(TokenKind::LeftParen)?;
        let initializer: Option<Statement> = match self.peek().kind {
            TokenKind::SemiColon => { self.eat(TokenKind::SemiColon)?; None },
            TokenKind::Mut => Some(self.var_declaration()?),
            _ => Some(self.expression_stmt()?),
        };

        let condition = self.expression()?;
        self.eat(TokenKind::SemiColon)?;

        let increment = self.expression()?;

        let expression = ExpressionStatement::new(
            Box::new(increment),
        );

        let right = self.eat(TokenKind::RightParen)?;

        let stmt = self.statement()?;
        let mut stmts: Vec<Box<Statement>> = vec![
            Box::new(stmt),
            Box::new(Statement::Expression(expression))
        ];

        let mut body = Statement::Block(BlockStatement::new(left.clone(), stmts, right.clone()));
        body = Statement::While(WhileStatement::new(Box::new(condition), Box::new(body)));

        if let Some(init) = initializer {
            stmts = vec![
                Box::new(init),
                Box::new(body)
            ];
            body = Statement::Block(BlockStatement::new(left, stmts, right));
        }

        Ok(body)
    }

    fn while_stmt(&mut self) -> Result<Statement, String> {
        self.eat(TokenKind::While)?;

        self.eat(TokenKind::LeftParen)?;
        let condition = self.expression()?;
        self.eat(TokenKind::RightParen)?;

        let body = self.statement()?;
        let statement = WhileStatement::new(
            Box::new(condition),
            Box::new(body)
        );

        Ok(Statement::While(statement))
    }

    fn if_stmt(&mut self) -> Result<Statement, String> {
        self.eat(TokenKind::If)?;

        self.eat(TokenKind::LeftParen)?;
        let condition = self.expression()?;
        self.eat(TokenKind::RightParen)?;

        let consequent = self.statement()?;
        let mut alternate: Option<Box<Statement>> = None;

        if self.is_curr(TokenKind::Else) {
            self.eat(TokenKind::Else)?;
            let statement = self.statement()?;
            alternate = Some(Box::new(statement));
        }

        let statement = IfStatement::new(
            Box::new(condition), 
            Box::new(consequent), 
            alternate
        );

        Ok(Statement::If(statement))
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
        let expression = self.or()?;

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

    fn or(&mut self) -> Result<Expression, String> {
        let mut left = self.and()?;

        while self.is_curr(TokenKind::Or) {
            let operator = self.eat(TokenKind::Or)?;
            let right = self.and()?;
            let expression = LogicalExpression::new(Box::new(left), operator, Box::new(right));
            left = Expression::Logical(expression);
        }

        Ok(left)
    }

    fn and(&mut self) -> Result<Expression, String> {
        let mut left = self.equality()?;

        while self.is_curr(TokenKind::And) {
            let operator = self.eat(TokenKind::And)?;
            let right = self.equality()?;
            let expression = LogicalExpression::new(Box::new(left), operator, Box::new(right));
            left = Expression::Logical(expression);
        }

        Ok(left)    
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

        self.call()
    }

    fn call(&mut self) -> Result<Expression, String> {
        let mut expr = self.cast()?;

        loop {
            if self.is_curr(TokenKind::LeftParen) {
                self.eat(TokenKind::LeftParen)?;
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, String> {
        let mut args: Vec<Box<Expression>> = vec![];

        if !self.is_curr(TokenKind::RightParen) {

            loop {
                args.push(Box::new(self.expression()?));
                
                if args.len() >= 255 {
                    return Err("function cannot have more than 255 arguments.".to_string());
                }
                
                if self.next().kind != TokenKind::Comma {
                    break;
                } 

            }
        } else {
            self.eat(TokenKind::RightParen)?;
        }

        let call = CallExpression::new(
            Box::new(callee),
            self.peek(),
            args
        );

        Ok(Expression::Call(call))
    }

    fn cast(&mut self) -> Result<Expression, String> {
        if self.peek().kind == TokenKind::Identifier {
            let typing = self.eat(TokenKind::Identifier)?;

            if self.peek().kind == TokenKind::As {
                self.eat(TokenKind::As)?;
                let casted = self.primary()?; 
                let expression = CastExpression::new(typing, Box::new(casted));
                return Ok(Expression::Cast(expression));
            } else {
                let expression = VariableExpression::new(typing);
                return Ok(Expression::Variable(expression))
            }
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
                self.cast()
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
