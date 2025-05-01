from scaner import Scaner
from parser import Syntax, Parser
import lang_spec


if __name__ == "__main__":
	scaner = Scaner.Build(lang_spec.token_types)
	from parser import Production
	syntax_rules = [
		Production("S", ("stmt_lst",), lambda x: x),
		Production("stmt_lst", ("stmt_lst'",), lambda lst: lst),
		Production("stmt_lst", (), lambda: ()),
		Production("stmt_lst'", ("stmt_lst'", "stmt"), lambda lst, s: (*lst, s)),
		Production("stmt_lst'", ("stmt",), lambda stmt: (stmt,)),
		Production("stmt", ("expr", ";"), lambda expr, semicolon: expr),
		Production("stmt", ("decl", ";"), lambda decl, semicolon: decl),
		Production("decl", ("ID", "=", "expr"), lambda id, equal, expr: (id, "=", expr)),
		Production("expr", ("expr", "op-add", "term"), lambda left, op, right: (op, left, right)),
		Production("expr", ("term",), lambda x: x),
		Production("term", ("term", "op-mul", "factor"), lambda left, op, right: (op, left, right)),
		Production("term", ("factor",), lambda x: x),
		Production("factor", ("(", "expr", ")"), lambda lpr, expr, rpr: expr),
		Production("factor", ("ID",), lambda t: t.GetValue()),
		Production("factor", ("INT",), lambda t: int(t.GetValue())),
		
		Production("op-add", ("+",), lambda plus: plus),
		Production("op-add", ("-",), lambda minus: minus),
		Production("op-mul", ("*",), lambda mul: mul),
		Production("op-mul", ("/",), lambda div: div),
	]
	syntax = Syntax.Build(syntax_rules, lang_spec.terminals.__contains__)
	parser = syntax.BuildParser()
	
	with open("main.oo", "r") as f:
		source_code = f.read()
	
	ast = syntax.BruteParse(scaner.Tokenize(source_code))
	ast1 = parser.Parse(scaner.Tokenize(source_code))
	print(ast)
