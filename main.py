from scanner import Scanner
from parser import Syntax
from interpreter import EvaluationContext
from printer import ToOLangCode
import lang_spec


if __name__ == "__main__":
	with open("test.olang", "r", encoding="utf-8") as f:
		source_code = f.read()
	
	scanner = Scanner.Build(lang_spec.TOKEN_TYPES)
	syntax = Syntax.Build(lang_spec.SYNTAX_RULES, lang_spec.TERMINALS.__contains__)
	ast = syntax.BruteLR1Parse(scanner.Tokenize(source_code))
	print("===== Reproduced Source Code (May not be 100%% correct) =====")
	print("\n".join(ast.Accept(ToOLangCode())))
	print("")
	
	parser = syntax.BuildLR1Parser()
	print("===== LR(1) Parser State =====")
	print(parser)
	print("")
	
	ast2 = parser.Parse(scanner.Tokenize(source_code))
	print("===== Source Code AST =====")
	print(ast2)
	print("")
	
	scope = EvaluationContext.New()
	_, _ = ast.RawEval(scope)
	func = scope.Resolve("test")
	arg = 5, 7
	# arg = 5, 1
	result, ctrl = func(arg)
	print("===== Result of Invoking test Function =====")
	print(f"test{arg} -> {result} (control state = {ctrl})")
