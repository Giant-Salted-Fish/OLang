from scaner import Scaner
from parser import Syntax
from interpreter import EvaluationContext
import lang_spec


if __name__ == "__main__":
	with open("test.olang", "r", encoding="utf-8") as f:
		source_code = f.read()
	
	scaner = Scaner.Build(lang_spec.TOKEN_TYPES)
	syntax = Syntax.Build(lang_spec.SYNTAX_RULES, lang_spec.TERMINALS.__contains__)
	ast = syntax.BruteLR1Parse(scaner.Tokenize(source_code))
	print("\n".join(ast.GenCode()))
	
	parser = syntax.BuildLR1Parser()
	print(parser)
	ast2 = parser.Parse(scaner.Tokenize(source_code))
	print(ast2)
	
	scope = EvaluationContext()
	_, _ = ast.RawEval(scope)
	func = scope.Lookup("test")[0]  # type: ignore
	result, _ = func.Invoke((5, 7), EvaluationContext())
	print(f"{result = }")
