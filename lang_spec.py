from parser import Production
from lang_ast import (
	NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeCall, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)

import typing
if typing.TYPE_CHECKING:
	from collections.abc import Callable
	from typing import Any
	from lang_ast import Node


TOKEN_TYPES = [
	("TMPLT", r"template"),
	("CONTINUE", r"continue"),
	("STRUCT", r"struct"),
	("RETURN", r"return"),
	# ("INLAY", r"inlay"),  # Comment out this to disable inlay keyword.
	("BREAK", r"break"),
	("TUPLE", r"tuple"),
	("WHILE", r"while"),
	("ELSE", r"else"),
	("LET", r"let"),
	("FOR", r"for"),
	("IF", r"if"),
	("FN", r"fn"),
	
	("BOOL", r"TRUE|FALSE|true|false"),
	("INT", r"[0-9]\d*"),
	("IDENT", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	("STR", r"\"(?:[^\"\\]|\\[tnr])*\""),  # See https://github.com/antlr/grammars-v4/blob/master/java/java8/Java8Lexer.g4
	
	("#[", r"#\["),
	(".{", r"\.\{"),
	(".[", r"\.\["),
	(".(", r"\.\("),
	("{", r"\{"),
	("}", r"\}"),
	("[", r"\["),
	("]", r"\]"),
	("(", r"\("),
	(")", r"\)"),
	
	(";", r";"),
	("=", r"="),
	("|", r"\|"),
	(",", r","),
	("||", r"\|\|"),
	("&&", r"&&"),
	("==", r"=="),
	("!=", r"!="),
	("<=", r"<="),
	(">=", r">="),
	("<", r"<"),
	(">", r">"),
	("+", r"\+"),
	("-", r"-"),
	("*", r"\*"),
	("/", r"/"),
	("%", r"%"),
	("&", r"&"),
	("!", r"!"),
	("~", r"~"),
	("#", r"#"),
	("@", r"@"),
	(":", r":"),
	("->", r"->"),
	(".", r"\."),
	
	# TODO: Maybe forbid mult-line comment
	("COMMENT", r"//.*"),
]

TERMINALS = set(t for t, _ in TOKEN_TYPES)

SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable[..., Any]]] = [
	("S", ("stmt_lst",), lambda lst: NodeStruct(*lst)),
	
	# stmt_lst: norm_stmt* last_stmt
	("stmt_lst", ("norm_stmt", "stmt_lst"), lambda x, xs: (*x, *xs)),
	("stmt_lst", ("last_stmt",), lambda x: x),
	
	# norm_stmt: decl
	#          | norm_ctrl
	#          | (stmt|assign)? ;
	("norm_stmt", ("decl",), lambda x: (x,)),
	("norm_stmt", ("norm_ctrl",), lambda x: x),
	("norm_stmt", ("stmt", ";"), lambda x, SEMI: (x,)),
	("norm_stmt", ("assign", ";"), lambda x, SEMI: (x,)),
	("norm_stmt", (";",), lambda SEMI: ()),
	
	# last_stmt: last_ctrl
	#          | (stmt|assign)?
	("last_stmt", ("last_ctrl",), lambda x: x),
	("last_stmt", ("stmt",), lambda x: (x,)),
	("last_stmt", ("assign",), lambda x: (x,)),
	("last_stmt", (), lambda: ()),
	
	# decl: prefix* (TMPLT|FN) ..bound ..bound ..bound
	#     | prefix* (STRUCT|TUPLE) ..bound ..bound
	("decl", ("prefix*", "TMPLT", "..bound", "..bound", "..bound"), lambda attr, TMPLT, label, param, body: NodeAssign(NodeDecl(label).AppendPrefix(*attr), NodeTemplate(param, EnsureCompound(body)))),
	("decl", ("prefix*", "FN", "..bound", "..bound", "..bound"), lambda attr, FN, label, param, body: NodeAssign(NodeDecl(label).AppendPrefix(*attr), NodeFunc(param, EnsureCompound(body)))),
	("decl", ("prefix*", "STRUCT", "..bound", "..bound"), lambda attr, STRUCT, label, body: NodeAssign(NodeDecl(label), NodeNamedStruct(body).AppendPrefix(*attr))),
	("decl", ("prefix*", "TUPLE", "..bound", "..bound"), lambda attr, TUPLE, label, body: NodeAssign(NodeDecl(label), NodeNamedTuple(body).AppendPrefix(*attr))),
	
	# norm_ctrl: prefix* IF ..bound ..bound norm_else
	#          | prefix* WHILE ..bound ..bound norm_else
	#          | prefix* FOR ..bound ..bound ..bound norm_else
	("norm_ctrl", ("prefix*", "IF", "..bound", "..bound", "norm_else"), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, EnsureCompound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "WHILE", "..bound", "..bound", "norm_else"), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "FOR", "..bound", "..bound", "..bound", "norm_else"), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	
	# norm_else: prefix* ELSE norm_ctrl
	#          | prefix* ELSE bound norm_stmt
	#          | norm_stmt
	("norm_else", ("prefix*", "ELSE", "norm_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("norm_else", ("prefix*", "ELSE", "bound", "norm_stmt"), lambda attr, ELSE, x, follow: (EnsureCompound(x).AppendPrefix(*attr), *follow)),
	("norm_else", ("norm_stmt",), lambda x: (NodeCompound(), *x)),
	
	# last_ctrl: prefix* IF ..bound ..bound last_else
	#          | prefix* WHILE ..bound ..bound last_else
	#          | prefix* FOR ..bound ..bound ..bound last_else
	("last_ctrl", ("prefix*", "IF", "..bound", "..bound", "last_else"), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, EnsureCompound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "WHILE", "..bound", "..bound", "last_else"), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "FOR", "..bound", "..bound", "..bound", "last_else"), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	
	# last_else: prefix* ELSE last_ctrl
	#          | prefix* ELSE bound last_stmt
	#          | last_stmt
	("last_else", ("prefix*", "ELSE", "last_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("last_else", ("prefix*", "ELSE", "bound", "last_stmt"), lambda attr, ELSE, x, follow: (EnsureCompound(x).AppendPrefix(*attr), *follow)),
	("last_else", ("last_stmt",), lambda x: (NodeCompound(), *x)),
	
	# stmt: prefix* RETURN expr
	#     | prefix* BREAK expr?
	#     | prefix* CONTINUE
	("stmt", ("prefix*", "RETURN", "expr"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK", "expr"), lambda attr, BREAK, x: NodeBreak(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK"), lambda attr, BREAK: NodeBreak(NodeTuple()).AppendPrefix(*attr)),
	("stmt", ("prefix*", "CONTINUE"), lambda attr, CONT: NodeContinue().AppendPrefix(*attr)),
	
	# expr: (assign|..assign)
	("expr", ("assign",), lambda x: x),
	("expr", ("..assign",), lambda x: x),
	
	# assign: let (= (let|..union))*
	("assign", ("let", "assign.."), lambda var, val: NodeAssign(var, val) if val is not None else var),
	("assign..", ("=", "(let|..union)", "assign.."), lambda EQ, var, val: NodeAssign(var, val) if val is not None else var),
	("assign..", (), lambda: None),
	("(let|..union)", ("let",), lambda x: x),
	("(let|..union)", ("..union",), lambda x: x),
	
	# ..assign: ..union (= (let|..union))*
	("..assign", ("..union", "assign.."), lambda var, val: NodeAssign(var, val) if val is not None else var),
	
	# let: prefix* LET (union|..union)
	#    | union
	("let", ("prefix*", "LET", "(union|..union)"), lambda attr, LET, var: NodeDecl(var).AppendPrefix(*attr)),
	("let", ("union",), lambda x: x),
	("(union|..union)", ("union",), lambda x: x),
	("(union|..union)", ("..union",), lambda x: x),
	
	# union: tuple (| (tuple|..tuple))* |?
	("union", ("tuple", "union..",), lambda x, xs: NodeUnion(x, *xs) if xs is not None else x),
	("union..", ("|", "(tuple|..tuple)", "union.."), lambda PIPE, x, xs: (x, *xs) if xs is not None else x),
	("union..", ("|",), lambda PIPE: ()),
	("union..", (), lambda: None),
	("(tuple|..tuple)", ("tuple",), lambda x: x),
	("(tuple|..tuple)", ("..tuple",), lambda x: x),
	
	# ..union: ..tuple (| (tuple|..tuple))* |?
	("..union", ("..tuple", "union.."), lambda x, xs: NodeUnion(x, *xs) if xs is not None else x),
	
	# tuple: suffixed (, (suffixed|..suffixed))* ,?
	("tuple", ("suffixed",), lambda x: x),
	("tuple", ("suffixed", ",", "element_lst"), lambda x, COMMA, xs: NodeTuple(x, *xs)),
	("(suffixed|..suffixed)", ("suffixed",), lambda x: x),
	("(suffixed|..suffixed)", ("..suffixed",), lambda x: x),
	
	# ..tuple: ..suffixed (, (suffixed|..suffixed))* ,?
	("..tuple", ("..suffixed",), lambda x: x),
	("..tuple", ("..suffixed", ",", "element_lst"), lambda x, COMMA, xs: NodeTuple(x, *xs)),
	
	# suffixed: lmbd suffix*
	("suffixed", ("lmbd", "suffix*"), lambda x, hint: x.AppendSuffix(*hint)),
	("suffix*", ("suffix*", ":", "(lmbd|..lmbd)"), lambda xs, COLON, x: (*xs, x)),
	("suffix*", (), lambda: ()),
	("(lmbd|..lmbd)", ("lmbd",), lambda x: x),
	("(lmbd|..lmbd)", ("..lmbd",), lambda x: x),
	
	# ..suffixed: ..lmbd suffix*
	("..suffixed", ("..lmbd", "suffix*"), lambda x, hint: x.AppendSuffix(*hint)),
	
	# lmbd: or (-> (or|..or))*
	("lmbd", ("or", "lmbd.."), lambda param, body: NodeFunc(param, body) if body is not None else param),
	("lmbd..", ("->", "(or|..or)", "lmbd.."), lambda ARROW, param, body: NodeFunc(param, EnsureCompound(body)) if body else param),
	("lmbd..", (), lambda: None),
	("(or|..or)", ("or",), lambda x: x),
	("(or|..or)", ("..or",), lambda x: x),
	
	# ..lmbd: ..or (-> (or|..or))*
	("..lmbd", ("..or", "lmbd.."), lambda param, body: NodeFunc(param, EnsureCompound(body)) if body is not None else param),
	
	# or: and (|| (and|..and))*
	("or", ("or", "||", "(and|..and)"), lambda lhs, OR, rhs: NodeLogicalOp(OR, lhs, rhs)),
	("or", ("and",), lambda x: x),
	("(and|..and)", ("and",), lambda x: x),
	("(and|..and)", ("..and",), lambda x: x),
	
	# ..or: ..and (|| (and|..and))*
	("..or", ("..or", "||", "(and|..and)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..or", ("..and",), lambda x: x),
	
	# and: eq (&& (eq|..eq))*
	("and", ("and", "&&", "(eq|..eq)"), lambda lhs, OP, rhs: NodeLogicalOp(OP, lhs, rhs)),
	("and", ("eq",), lambda x: x),
	("(eq|..eq)", ("eq",), lambda x: x),
	("(eq|..eq)", ("..eq",), lambda x: x),
	
	# ..and: ..eq (&& (eq|..eq))*
	("..and", ("..and", "&&", "(eq|..eq)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..and", ("..eq",), lambda x: x),
	
	# eq: rel ((==|!=) (rel|..rel))*
	("eq", ("eq", "(==|!=)", "(rel|..rel)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("eq", ("rel",), lambda x: x),
	("(==|!=)", ("==",), lambda EQ: EQ),
	("(==|!=)", ("!=",), lambda NEQ: NEQ),
	("(rel|..rel)", ("rel",), lambda x: x),
	("(rel|..rel)", ("..rel",), lambda x: x),
	
	# ..eq: ..rel ((==|!=) (rel|..rel))*
	("..eq", ("..eq", "(==|!=)", "(rel|..rel)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..eq", ("..rel",), lambda x: x),
	
	# rel: add ((<=|>=|<|>) (add|..add))*
	("rel", ("rel", "(<=|>=|<|>)", "(add|..add)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("rel", ("add",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda GTE: GTE),
	("(<=|>=|<|>)", ("<=",), lambda LSE: LSE),
	("(<=|>=|<|>)", (">",), lambda GT: GT),
	("(<=|>=|<|>)", ("<",), lambda LS: LS),
	("(add|..add)", ("add",), lambda x: x),
	("(add|..add)", ("..add",), lambda x: x),
	
	# ..rel: ..add ((<=|>=|<|>) (add|..add))*
	("..rel", ("..rel", "(<=|>=|<|>)", "(add|..add)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..rel", ("..add",), lambda x: x),
	
	# add: mul ((+|-) (mul|..mul))*
	("add", ("add", "(+|-)", "(mul|..mul)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("add", ("mul",), lambda x: x),
	("(+|-)", ("+",), lambda PLUS: PLUS),
	("(+|-)", ("-",), lambda MINUS: MINUS),
	("(mul|..mul)", ("mul",), lambda x: x),
	("(mul|..mul)", ("..mul",), lambda x: x),
	
	# ..add: ..mul ((+|-) (mul|..mul))*
	("..add", ("..add", "(+|-)", "(mul|..mul)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..add", ("..mul",), lambda x: x),
	
	# mul: unary ((*|/|%) (unary|decl|ctrl))*
	("mul", ("mul", "(*|/|%)", "(unary|decl|ctrl)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("mul", ("unary",), lambda x: x),
	("(*|/|%)", ("*",), lambda MUL: MUL),
	("(*|/|%)", ("/",), lambda DIV: DIV),
	("(*|/|%)", ("%",), lambda MOD: MOD),
	("(unary|decl|ctrl)", ("unary",), lambda x: x),
	("(unary|decl|ctrl)", ("decl",), NodeCompound),
	("(unary|decl|ctrl)", ("ctrl",), lambda x: x),
	
	# ..mul: (decl|ctrl) ((*|/|%) (unary|decl|ctrl))*
	("..mul", ("..mul", "(*|/|%)", "(unary|decl|ctrl)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..mul", ("decl",), NodeCompound),
	("..mul", ("ctrl",), lambda x: x),
	
	# unary: (+|-|!|&|*)* (call|bound|prefixed)
	#      | (+|-|!|&|*)+ (decl|ctrl)
	("unary", ("(+|-|!|&|*)", "unary"), lambda OP, val: NodeUnaryOp(OP, val)),
	("unary", ("(+|-|!|&|*)", "decl"), lambda OP, val: NodeUnaryOp(OP, val)),
	("unary", ("(+|-|!|&|*)", "ctrl"), lambda OP, val: NodeUnaryOp(OP, val)),
	("unary", ("call",), lambda x: x),
	("unary", ("bound",), lambda x: x),
	("unary", ("prefixed",), lambda x: x),
	("(+|-|!|&|*)", ("+",), lambda PLUS: PLUS),
	("(+|-|!|&|*)", ("-",), lambda MINUS: MINUS),
	("(+|-|!|&|*)", ("!",), lambda NOT: NOT),
	("(+|-|!|&|*)", ("&",), lambda AMPERSAND: AMPERSAND),
	("(+|-|!|&|*)", ("*",), lambda MUL: MUL),
	
	# call: postfixed+ (decl|..bound|prefixed)
	("call", ("postfixed+", "decl"), lambda func, arg: NodeCall(func, arg)),
	("call", ("postfixed+", "..bound"), lambda func, arg: NodeCall(func, arg)),
	("call", ("postfixed+", "prefixed"), lambda func, arg: NodeCall(func, arg)),
	
	# ..bound: ctrl
	#        | bound
	("..bound", ("ctrl",), lambda x: x),
	("..bound", ("bound",), lambda x: x),
	
	# ctrl: prefix* IF ..bound ..bound else
	#     | prefix* WHILE ..bound ..bound else
	#     | prefix* FOR ..bound ..bound ..bound else
	("ctrl", ("prefix*", "IF", "..bound", "..bound", "else"), lambda attr, IF, cond, expr, otherwise: NodeIfElse(cond, EnsureCompound(expr), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "WHILE", "..bound", "..bound", "else"), lambda attr, WHILE, cond, loop, otherwise: NodeWhileElse(cond, EnsureCompound(loop), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "FOR", "..bound", "..bound", "..bound", "else"), lambda attr, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, EnsureCompound(loop), otherwise).AppendPrefix(*attr)),
	
	# else: prefix* ELSE ..bound
	("else", ("prefix*", "ELSE", "..bound"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	
	# It is named "bound" even if its length can go infinite, as it has a clear terminator that indicates the end of this element.
	# bound: prefix* #[ (stmt|expr) ] postfixed
	#      | prefix* (TMPLT|FN) ..bound ..bound
	#      | prefix* (STRUCT|TUPLE) ..bound
	#      | prefix* { stmt_lst }
	#      | prefix* .{ stmt_lst }
	#      | postfixed
	("bound", ("prefix*", "#[", "(stmt|expr)", "]", "postfixed",), lambda attr_lst, HASH, attr, RBR, x: (x[1](x[0])).AppendPrefix(*attr_lst, attr)),
	("bound", ("prefix*", "TMPLT", "..bound", "..bound"), lambda attr, TMPLT, param, body: NodeTemplate(param, EnsureCompound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "FN", "..bound", "..bound"), lambda attr, FN, param, body: NodeFunc(param, EnsureCompound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "STRUCT", "..bound"), lambda attr, STRCT, body: NodeNamedStruct(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "TUPLE", "..bound"), lambda attr, TUPLE, body: NodeNamedTuple(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "{", "stmt_lst", "}", "postfix*"), lambda attr, LCB, lst, RCB, pst: pst(NodeCompound(*lst)).AppendPrefix(*attr)),
	("bound", ("prefix*", ".{", "stmt_lst", "}", "postfix*"), lambda attr, LCB, lst, RCB, pst: pst(NodeStruct(*lst)).AppendPrefix(*attr)),
	("bound", ("postfixed",), lambda x: x[1](x[0])),
	
	# prefixed: prefix* @ postfixed+ postfixed
	("prefixed", ("prefix*", "@", "postfixed+", "postfixed"), lambda attr_lst, AT, attr, x: x[1](x[0]).AppendPrefix(*attr_lst, attr)),
	("prefix*", ("prefix*", "@", "postfixed+"), lambda xs, AT, x: (*xs, x)),
	("prefix*", ("prefix*", "#[", "(stmt|expr)", "]"), lambda xs, HASH, x, RBR: (*xs, x)),
	("prefix*", (), lambda: ()),
	("postfixed+", ("postfixed+", "postfixed"), lambda head, x: x[1](NodeCall(head, x[0]))),
	("postfixed+", ("postfixed",), lambda x: x[1](x[0])),
	
	# postfixed: prim postfix*
	("postfixed", ("prim", "postfix*"), lambda x, pst: (x, pst)),
	("postfix*", ("postfix*", "postfix"), lambda head, pst: lambda node: pst(head(node))),
	("postfix*", (), lambda: lambda x: x),
	("postfix", (".", "prim"), lambda DOT, label: lambda node: NodeAccess(node, label)),
	("postfix", ("[", "(stmt|expr)", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("#",), lambda HASH: lambda x: x),
	
	# prim: (INT|IDENT|STR|BOOL)
	#     | ( )
	#     | ( | )
	#     | ( (stmt|expr) )
	#     | .( field_lst )
	#     | .[ element_lst ]
	("prim", ("INT",), NodeInt),
	("prim", ("IDENT",), NodeLabel),
	("prim", ("STR",), NodeStr),
	("prim", ("BOOL",), NodeBool),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "(stmt|expr)", ")"), lambda LPR, x, RPR: x),
	("prim", (".(", "field_lst", ")"), lambda LPR, x, RPR: NodeStruct(*x)),
	("prim", (".[", "element_lst", "]"), lambda LPR, x, RPR: NodeTuple(*x)),
	("(stmt|expr)", ("stmt",), lambda x: x),
	("(stmt|expr)", ("expr",), lambda x: x),
	
	# element_lst: element , element_lst
	#            | element?
	("element_lst", ("(suffixed|..suffixed)", ",", "element_lst"), lambda x, COMMA, xs: (x, *xs)),
	("element_lst", ("(suffixed|..suffixed)",), lambda x: (x,)),
	("element_lst", (), lambda: ()),
	
	# field_lst: field_set (;|,) field_lst
	#          | field_set?
	("field_lst", ("field_set", "(;|,)", "field_lst"), lambda x, SEMI, xs: (x, *xs)),
	("field_lst", ("field_set",), lambda x: (x,)),
	("field_lst", (), lambda: ()),
	("field_set", ("prefix*", "LET", "(suffixed|..suffixed)", "=", "(suffixed|..suffixed)"), lambda attr, LET, prop, EQ, val: NodeAssign(NodeDecl(prop).AppendPrefix(*attr), val)),
	("field_set", ("prefix*", "LET", "(suffixed|..suffixed)"), lambda attr, LET, prop: NodeDecl(prop).AppendPrefix(*attr)),
	("field_set", ("(suffixed|..suffixed)", "=", "(suffixed|..suffixed)"), lambda prop, EQ, val: NodeAssign(NodeDecl(prop), val)),
	("field_set", ("(suffixed|..suffixed)",), lambda prop: NodeDecl(prop)),
	("(;|,)", (";",), lambda SEMI: SEMI),
	("(;|,)", (",",), lambda COMMA: COMMA),
]
PRODUCTIONS = [Production(*p) for p in SYNTAX_RULES]


def EnsureCompound(node: Node) -> NodeCompound | NodeStruct:
	return node if isinstance(node, (NodeCompound, NodeStruct)) else NodeCompound(node)
