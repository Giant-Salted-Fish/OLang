from scaner import Scaner
from parser import Syntax
import lang_spec


if __name__ == "__main__":
	with open("main.oo", "r") as f:
		source_code = f.read()
	
	scaner = Scaner.Build(lang_spec.TOKEN_TYPES)
	syntax = Syntax.Build(lang_spec.SYNTAX_RULES, lang_spec.TERMINALS.__contains__)
	ast = syntax.BruteLR1Parse(scaner.Tokenize(source_code))
	print("\n".join(ast.GenText()))
	
	parser = syntax.BuildLR1Parser()
	ast2 = parser.Parse(scaner.Tokenize(source_code))
	print(ast2)
