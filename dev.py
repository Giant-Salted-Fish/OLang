import lang_spec
from lang_ast import (
	NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeCall, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)

import typing
if typing.TYPE_CHECKING:
	from typing import Any
	from collections.abc import Callable

source_code = """
if match a
	case b if c d
	case e f
	g
else if h
	if i j
"""
SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable[..., Any]]] = [
	("S", ("all",), lambda x: x),
	
	("if", ("IF", "all", "(if|if$-case|match|prim)", "ELSE", "(if|prim)"), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	("if$-else", ("IF", "all", "(if|if$-else|prim)"), lambda IF, cond, true_br: NodeIfElse(cond, true_br, NodeCompound())),
	("if$-else", ("IF", "all", "(if|if$-case|match|prim)", "ELSE", "if$-else"), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	("if$-case", ("IF", "all", "(if|if$-case|match|prim)", "ELSE", "(if$-case|match)"), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	("if$-else-case", ("IF", "all", "(if$-case|if$-else-case|match|match$-else)"), lambda IF, cond, true_br: NodeIfElse(cond, true_br, NodeCompound())),
	("if$-else-case", ("IF", "all", "(if|if$-case|match|prim)", "ELSE", "(if$-else-case|match$-else)"), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	("match", ("MATCH", "(if|if$-case|match|prim)"), lambda MATCH, expr: ("match", expr, ())),
	("match", ("MATCH", "(if|if$-else|prim)", "case.."), lambda MATCH, expr, xs: ("match", expr, xs)),
	("case..", ("CASE", "all", "(if|if$-else|prim)", "case.."), lambda CASE, pattern, expr, xs: (("case", pattern, expr), *xs)),
	("case..", ("CASE", "all", "(if|if$-case|match|prim)"), lambda CASE, pattern, expr: (("case", pattern, expr),)),
	
	("match$-else", ("MATCH", "(if$-else|if$-else-case|match$-else)"), lambda MATCH, expr: ("match", expr, ())),
	("match$-else", ("MATCH", "(if|if$-else|prim)", "..case$-else"), lambda MATCH, expr, xs: ("match", expr, ())),
	("..case$-else", ("CASE", "all", "(if|if$-else|prim)", "..case$-else"),lambda CASE, pattern, expr, xs: (("case", pattern, expr), *xs)),
	("..case$-else", ("CASE", "all", "(if$-else|if$-else-case|match$-else)"), lambda CASE, pattern, expr: (("case", pattern, expr),)),
	
	
	
	("all", ("if",), lambda x: x),
	("all", ("if$-else",), lambda x: x),
	("all", ("if$-case",), lambda x: x),
	("all", ("if$-else-case",), lambda x: x),
	("all", ("match",), lambda x: x),
	("all", ("match$-else",), lambda x: x),
	("all", ("prim",), lambda x: x),
	
	("(if|if$-case|match|prim)", ("if",), lambda x: x),
	("(if|if$-case|match|prim)", ("if$-case",), lambda x: x),
	("(if|if$-case|match|prim)", ("match",), lambda x: x),
	("(if|if$-case|match|prim)", ("prim",), lambda x: x),
	
	("(if|prim)", ("if",), lambda x: x),
	("(if|prim)", ("prim",), lambda x: x),
	
	("(if|if$-else|prim)", ("if",), lambda x: x),
	("(if|if$-else|prim)", ("if$-else",), lambda x: x),
	("(if|if$-else|prim)", ("prim",), lambda x: x),
	
	("(if$-case|match)", ("if$-case",), lambda x: x),
	("(if$-case|match)", ("match",), lambda x: x),
	
	("(if$-case|if$-else-case|match|match$-else)", ("if$-case",), lambda x: x),
	("(if$-case|if$-else-case|match|match$-else)", ("if$-else-case",), lambda x: x),
	("(if$-case|if$-else-case|match|match$-else)", ("match",), lambda x: x),
	("(if$-case|if$-else-case|match|match$-else)", ("match$-else",), lambda x: x),
	
	("(if$-else-case|match$-else)", ("if$-else-case",), lambda x: x),
	("(if$-else-case|match$-else)", ("match$-else",), lambda x: x),
	
	("(if$-else|if$-else-case|match$-else)", ("if$-else",), lambda x: x),
	("(if$-else|if$-else-case|match$-else)", ("if$-else-case",), lambda x: x),
	("(if$-else|if$-else-case|match$-else)", ("match$-else",), lambda x: x),
	
	("prim", ("IDENT",), NodeLabel),
	("prim", ("INT",), NodeInt),
]

TOKEN_TYPES = [
	("MATCH", r"match"),
	("CASE", r"case"),
	("DEFAULT", r"default"),
	*lang_spec.TOKEN_TYPES,
]
TERMINALS = set(t for t, _ in TOKEN_TYPES)

if __name__ == "__main__":
	from scanner import Scanner
	from parser import Syntax, Production
	import lang_ast
	
	with open("result.txt", "w") as f:
		def write(obj: Any = ""):
			print(obj)
			msg = str(obj)
			f.write(f"{msg}\n")
		
		scanner = Scanner[str].Build(TOKEN_TYPES)
		syntax = Syntax[str, str, lang_ast.Node].Build([Production(*p) for p in SYNTAX_RULES], TERMINALS.__contains__)
		parser = syntax.BuildLR1Parser()
		write("===== LR(1) Parser State =====")
		write(parser)
		write()
		
		ast = parser.Parse(scanner.Tokenize(source_code))
		write("===== Reproduced Source Code (May not be 100%% correct) =====")
		# write("\n".join(ast.Accept(ToOLangCode("    "))))
		write()
		write(ast)
