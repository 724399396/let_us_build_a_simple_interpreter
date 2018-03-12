INTEGER,PLUS,MINUS,MUL,DIV,LPAREN,RPAREN,EOF = (
    'INTEGER', '+', '-', '*', '/', '(', ')', 'EOF')

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return 'Token ({type}, {value})'.format(
            type=self.type, value=self.value
        )

    def __repl__(self):
        return self.__str__()

class Lexer(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = text[self.pos]

    def advance(self):
        self.pos += 1
        if self.pos > len(self.text) - 1 :
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]

    def error(self):
        'parse error: {pos}'.format(pos=self.pos)

    def skip_white_space(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def integer(self):
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)

    def get_next_token(self):
        if self.current_char is None:
            return Token(EOF, EOF)
        if self.current_char is not None and self.current_char.isspace():
            self.skip_white_space()
        if self.current_char is not None and self.current_char.isdigit():
            return Token(INTEGER, self.integer())
        if self.current_char == '+':
            self.advance()
            return Token(PLUS, '+')
        if self.current_char == '-':
            self.advance()
            return Token(MINUS, '-')
        if self.current_char == '*':
            self.advance()
            return Token(MUL, '*')
        if self.current_char == '/':
            self.advance()
            return Token(DIV, '/')
        if self.current_char == '(':
            self.advance()
            return Token(LPAREN, '(')
        if self.current_char == ')':
            self.advance()
            return Token(RPAREN, ')')
        self.error()

class Interpreter(object):
    def __init__(self,lexer):
        self.lexer=lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, type):
        if self.current_token.type == type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.lexer.error()

    def factor(self):
        if self.current_token.type == INTEGER:
            value = self.current_token.value
            self.eat(INTEGER)
            return value
        if self.current_token.type == LPAREN:
            self.eat(LPAREN)
            value = self.expr()
            self.eat(RPAREN)
            return value

    def term(self):
        v = self.factor()
        while self.current_token.type is MUL or self.current_token.type is DIV:
            type = self.current_token.type
            self.eat(type)
            r = self.factor()
            if type is MUL:
                v = v * r
            elif type is DIV:
                v = v / r
        return v

    def expr(self):
        v = self.term()
        while self.current_token.type is PLUS or self.current_token.type is MINUS:
            type = self.current_token.type
            self.eat(self.current_token.type)
            r = self.term()
            if type is PLUS:
                v = v + r
            elif type is MINUS:
                v = v - r
        return v

def main():
    while True:
        try:
            text = raw_input('calc> ')
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr()
        print(result)

if __name__ == '__main__':
    main()

