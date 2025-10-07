from scanner import Scanner
from parser import Syntax
from interpreter import Environment, Evaluate
from printer import ToOLangCode
import lang_spec
import lang_ast


if __name__ == "__main__":
	with open("test.olang", "r", encoding="utf-8") as f:
		source_code = f.read()
	
	scanner = Scanner[str].Build(lang_spec.TOKEN_TYPES)
	syntax = Syntax[str, lang_ast.Node].Build(lang_spec.PRODUCTIONS, lang_spec.TERMINALS.__contains__)
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
	
	scope = Environment.New()
	assert isinstance(ast, lang_ast.NodeCompound)
	_, _ = Evaluate(scope).EvalCompound(ast)
	func = scope.Resolve("test")
	arg = 5, 7
	# arg = 5, 1
	result, ctrl = func(arg)
	print("===== Result of Invoking test Function =====")
	print(f"test{arg} -> {result} (control state = {ctrl})")
