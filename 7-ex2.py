from spi import Lexer, Parser, NodeVisitor, PLUS, MINUS, MUL, DIV

class Translator(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return '(+ ' + self.visit(node.left) + self.visit(node.right) + ')'
        elif node.op.type == MINUS:
            return '(- ' + self.visit(node.left) + self.visit(node.right) + ')'
        elif node.op.type == MUL:
            return '(* ' + self.visit(node.left) + self.visit(node.right) + ')'
        elif node.op.type == DIV:
            return '(/ ' + self.visit(node.left) + self.visit(node.right) + ')'

    def visit_Num(self, node):
        return str(node.value) + ' '

    def translate(self):
        tree = self.parser.parse()
        return self.visit(tree)

def main():
    while True:
        try:
            try:
                text = raw_input('spi> ')
            except NameError:  # Python3
                text = input('spi> ')
        except EOFError:
            break
        if not text:
            continue

        lexer = Lexer(text)
        parser = Parser(lexer)
        translator = Translator(parser)
        result = translator.translate()
        print result

if __name__ == '__main__':
    main()
