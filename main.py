from scaner import Scaner
from parser import Syntax, Parser
import lang_spec


if __name__ == "__main__":
	scaner = Scaner.Build(lang_spec.TOKEN_TYPES)
	syntax = Syntax.Build(lang_spec.SYNTAX_RULES, lang_spec.TERMINALS.__contains__)
	# parser = syntax.BuildLR1Parser()
	
	with open("main.oo", "r") as f:
		source_code = f.read()
	
	ast = syntax.BruteLR1Parse(scaner.Tokenize(source_code))
	# ast1 = parser.Parse(scaner.Tokenize(source_code))
	print("\n".join(ast.GenText()))
