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
	("INLAY", r"inlay"),  # Comment out this to disable inlay keyword.
	("BREAK", r"break"),
	("TUPLE", r"tuple"),
	("WHILE", r"while"),
	("THEN", r"then"),
	("ELSE", r"else"),
	("LET", r"let"),
	("FOR", r"for"),
	("IF", r"if"),
	("FN", r"fn"),
	
	("BOOL", r"TRUE|FALSE|true|false"),
	("INT", r"[0-9]\d*"),
	("IDENT", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	("STR", r"\"(?:[^\"\\]|\\[tnr])*\""),  # See https://github.com/antlr/grammars-v4/blob/master/java/java8/Java8Lexer.g4
	
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
	("stmt_lst", ("norm_stmt", "stmt_lst"), lambda x, lst: (*x, *lst)),
	("stmt_lst", ("last_stmt",), lambda x: x),
	
	# norm_stmt: stmt? ;
	#          | decl
	#          | norm_ctrl
	("norm_stmt", ("stmt", ";"), lambda x, SEMI: (x,)),
	("norm_stmt", (";",), lambda SEMI: ()),
	("norm_stmt", ("decl",), lambda x: (x,)),
	("norm_stmt", ("norm_ctrl",), lambda x: x),
	
	# last_stmt: stmt?
	#          | last_ctrl
	("last_stmt", ("stmt",), lambda x: (x,)),
	("last_stmt", (), lambda: ()),
	("last_stmt", ("last_ctrl",), lambda x: x),
	
	# stmt: assign
	#     | prefix* RETURN expr
	#     | prefix* BREAK expr?
	#     | prefix* CONTINUE
	("stmt", ("assign",), lambda x: x),
	("stmt", ("prefix*", "RETURN", "expr"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK", "expr"), lambda attr, BREAK, x: NodeBreak(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK"), lambda attr, BREAK: NodeBreak(NodeTuple()).AppendPrefix(*attr)),
	("stmt", ("prefix*", "CONTINUE"), lambda attr, CONT: NodeContinue().AppendPrefix(*attr)),
	
	# decl: prefix* (TMPLT|FN) bound bound bound
	#     | prefix* (STRUCT|TUPLE) bound bound
	("decl", ("prefix*", "TMPLT", "bound", "bound", "bound"), lambda attr, TMPLT, label, param, body: NodeAssign(NodeDecl(label), NodeTemplate(param, EnsureCompound(body)).AppendPrefix(*attr))),
	("decl", ("prefix*", "FN", "bound", "bound", "bound"), lambda attr, FN, label, param, body: NodeAssign(NodeDecl(label), NodeFunc(param, EnsureCompound(body)).AppendPrefix(*attr))),
	("decl", ("prefix*", "STRUCT", "bound", "bound"), lambda attr, STRUCT, label, body: NodeAssign(NodeDecl(label), NodeNamedStruct(body).AppendPrefix(*attr))),
	("decl", ("prefix*", "TUPLE", "bound", "bound"), lambda attr, TUPLE, label, body: NodeAssign(NodeDecl(label), NodeNamedTuple(body).AppendPrefix(*attr))),
	
	# norm_ctrl: prefix* IF bound (prefix* THEN)? bound norm_else
	#          | prefix* WHILE bound bound norm_else
	#          | prefix* FOR bound bound bound norm_else
	# ("norm_ctrl", ("prefix*", "IF", "bound", "prefix*", "THEN", "bound", "norm_else"), lambda attr, IF, cond, attr2, THEN, expr, x: (NodeIfElse(cond, EnsureCompound(expr).AppendPrefix(*attr2), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "IF", "bound", "bound", "norm_else"), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, EnsureCompound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "WHILE", "bound", "bound", "norm_else"), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "norm_else"), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	
	# norm_else: prefix* ELSE norm_ctrl
	#          | prefix* ELSE bound norm_stmt
	#          | norm_stmt
	("norm_else", ("prefix*", "ELSE", "norm_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("norm_else", ("prefix*", "ELSE", "bound", "norm_stmt"), lambda attr, ELSE, x, follow: (EnsureCompound(x).AppendPrefix(*attr), *follow)),
	("norm_else", ("norm_stmt",), lambda x: (NodeCompound(), *x)),
	
	# last_ctrl: prefix* IF bound (prefix* THEN)? bound last_else
	#          | prefix* WHILE bound bound last_else
	#          | prefix* FOR bound bound bound last_else
	# ("last_ctrl", ("prefix*", "IF", "bound", "prefix*", "THEN", "bound", "last_else"), lambda attr, IF, cond, attr2, THEN, expr, x: (NodeIfElse(cond, EnsureCompound(expr).AppendPrefix(*attr2), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "IF", "bound", "bound", "last_else"), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, EnsureCompound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "WHILE", "bound", "bound", "last_else"), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "last_else"), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, EnsureCompound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	
	# last_else: prefix* ELSE last_ctrl
	#          | prefix* ELSE bound last_stmt
	#          | last_stmt
	("last_else", ("prefix*", "ELSE", "last_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("last_else", ("prefix*", "ELSE", "bound", "last_stmt"), lambda attr, ELSE, x, follow: (EnsureCompound(x).AppendPrefix(*attr), *follow)),
	("last_else", ("last_stmt",), lambda x: (NodeCompound(), *x)),
	
	# expr: (assign|..assign)
	("expr", ("assign",), lambda x: x),
	("expr", ("..assign",), lambda x: x),
	
	# assign: let (= (let|..union))*
	("assign", ("let", "assign.."), lambda var, expr: NodeAssign(var, expr) if expr else var),
	("assign..", ("=", "(let|..union)", "assign.."), lambda EQ, var, expr: NodeAssign(var, expr) if expr else var),
	("assign..", (), lambda: None),
	("(let|..union)", ("let",), lambda x: x),
	("(let|..union)", ("..union",), lambda x: x),
	
	# let: prefix* LET (union|..union)
	#    | union
	("let", ("prefix*", "LET", "(union|..union)"), lambda attr, LET, var: NodeDecl(var).AppendPrefix(*attr)),
	("let", ("union",), lambda x: x),
	("(union|..union)", ("union",), lambda x: x),
	("(union|..union)", ("..union",), lambda x: x),
	
	# ..assign: ..union (= (union|..union))*
	("..assign", ("..union", "assign.."), lambda var, EQ, expr: NodeAssign(var, expr) if expr else var),
	
	# union: tuple (| (tuple|..tuple))* |?
	("union", ("tuple", "union.."), lambda x, lst: x if lst is None else NodeUnion(x, *lst)),
	("union..", ("|", "(tuple|..tuple)", "union.."), lambda PIPE, x, lst: (x, *(lst or ()))),
	("union..", ("|",), lambda PIPE: ()),
	("union..", (), lambda: None),
	("(tuple|..tuple)", ("tuple",), lambda x: x),
	("(tuple|..tuple)", ("..tuple",), lambda x: x),
	
	# ..union: ..tuple (| (tuple|..tuple))* |?
	("..union", ("..tuple", "union.."), lambda x, lst: x if lst is None else NodeUnion(x, *lst)),
	
	# tuple: suffixed (, (suffixed|..suffixed))* ,?
	("tuple", ("suffixed", "tuple.."), lambda x, lst: x if lst is None else NodeTuple(x, *lst)),
	("tuple..", (",", "(suffixed|..suffixed)", "tuple.."), lambda COMMA, x, lst: (x, *(lst or ()))),
	("tuple..", (",",), lambda COMMA: ()),
	("tuple..", (), lambda: None),
	("(suffixed|..suffixed)", ("suffixed",), lambda x: x),
	("(suffixed|..suffixed)", ("..suffixed",), lambda x: x),
	
	# ..tuple: ..suffixed (, (suffixed|..suffixed))* ,?
	("..tuple", ("..suffixed", "tuple.."), lambda x, lst: x if lst is None else NodeTuple(x, *lst)),
	
	# suffixed: lmbd suffix*
	("suffixed", ("lmbd", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	("suffix*", ("suffix*", ":", "lmbd"), lambda lst, COLON, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	
	# ..suffixed: ..lmbd suffix*
	("..suffixed", ("..lmbd", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	
	# lmbd: or (-> (or|..or))*
	("lmbd", ("or", "lmbd.."), lambda param, body: NodeFunc(param, EnsureCompound(body)) if body else param),
	("lmbd..", ("->", "(or|..or)", "lmbd.."), lambda ARROW, param, body: NodeFunc(param, EnsureCompound(body)) if body else param),
	("lmbd..", (), lambda: None),
	("(or|..or)", ("or",), lambda x: x),
	("(or|..or)", ("..or",), lambda x: x),
	
	# ..lmbd: ..or (-> (or|..or))*
	("..lmbd", ("..or", "lmbd.."), lambda param, body: NodeFunc(param, EnsureCompound(body)) if body else param),
	
	# or: and (|| (and|..and))*
	("or", ("or", "||", "(and|..and)"), lambda left, OR, right: NodeLogicalOp(OR, left, right)),
	("or", ("and",), lambda x: x),
	("(and|..and)", ("and",), lambda x: x),
	("(and|..and)", ("..and",), lambda x: x),
	
	# ..or: ..and (|| (and|..and))*
	("..or", ("..or", "||", "(and|..and)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("..or", ("..and",), lambda x: x),
	
	# and: eq (&& (eq|..eq))*
	("and", ("and", "&&", "(eq|..eq)"), lambda left, AND, right: NodeLogicalOp(AND, left, right)),
	("and", ("eq",), lambda x: x),
	("(eq|..eq)", ("eq",), lambda x: x),
	("(eq|..eq)", ("..eq",), lambda x: x),
	
	# ..and: ..eq (&& (eq|..eq))*
	("..and", ("..and", "&&", "(eq|..eq)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("..and", ("..eq",), lambda x: x),
	
	# eq: rel ((==|!=) (rel|..rel))*
	("eq", ("eq", "(==|!=)", "(rel|..rel)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("eq", ("rel",), lambda x: x),
	("(==|!=)", ("==",), lambda EQ: EQ),
	("(==|!=)", ("!=",), lambda NEQ: NEQ),
	("(rel|..rel)", ("rel",), lambda x: x),
	("(rel|..rel)", ("..rel",), lambda x: x),
	
	# ..eq: ..rel ((==|!=) (rel|..rel))*
	("..eq", ("..eq", "(==|!=)", "(rel|..rel)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("..eq", ("..rel",), lambda x: x),
	
	# rel: add ((<=|>=|<|>) (add|..add))*
	("rel", ("rel", "(<=|>=|<|>)", "(add|..add)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("rel", ("add",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda GTE: GTE),
	("(<=|>=|<|>)", ("<=",), lambda LSE: LSE),
	("(<=|>=|<|>)", (">",), lambda GT: GT),
	("(<=|>=|<|>)", ("<",), lambda LS: LS),
	("(add|..add)", ("add",), lambda x: x),
	("(add|..add)", ("..add",), lambda x: x),
	
	# ..rel: ..add ((<=|>=|<|>) (add|..add))*
	("..rel", ("..rel", "(<=|>=|<|>)", "(add|..add)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("..rel", ("..add",), lambda x: x),
	
	# add: mul ((+|-) (mul|..mul))*
	("add", ("add", "(+|-)", "(mul|..mul)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("add", ("mul",), lambda x: x),
	("(+|-)", ("+",), lambda PLUS: PLUS),
	("(+|-)", ("-",), lambda MINUS: MINUS),
	("(mul|..mul)", ("mul",), lambda x: x),
	("(mul|..mul)", ("..mul",), lambda x: x),
	
	# ..add: ..mul ((+|-) (mul|..mul))*
	("..add", ("..add", "(+|-)", "(mul|..mul)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("..add", ("..mul",), lambda x: x),
	
	# mul: unary ((*|/|%) (unary|decl|ctrl))*
	("mul", ("mul", "(*|/|%)", "(unary|decl|ctrl)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("mul", ("unary",), lambda x: x),
	("(*|/|%)", ("*",), lambda MUL: MUL),
	("(*|/|%)", ("/",), lambda DIV: DIV),
	("(*|/|%)", ("%",), lambda MOD: MOD),
	("(unary|decl|ctrl)", ("unary",), lambda x: x),
	("(unary|decl|ctrl)", ("decl",), NodeCompound),
	("(unary|decl|ctrl)", ("ctrl",), lambda x: x),
	
	# ..mul: (decl|ctrl) ((*|/|%) (unary|decl|ctrl))*
	("..mul", ("..mul", "(*|/|%)", "(unary|decl|ctrl)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("..mul", ("decl",), NodeCompound),
	("..mul", ("ctrl",), lambda x: x),
	
	# unary: (+|-|!|&|*)* (call|inlay|prefixed)
	#      | (+|-|!|&|*)+ (decl|ctrl)
	("unary", ("(+|-|!|&|*)", "unary"), lambda op, val: NodeUnaryOp(op, val)),
	("unary", ("(+|-|!|&|*)", "decl"), lambda op, val: NodeUnaryOp(op, NodeCompound(val))),
	("unary", ("(+|-|!|&|*)", "ctrl"), lambda op, val: NodeUnaryOp(op, val)),
	("unary", ("call",), lambda x: x),
	("unary", ("inlay",), lambda x: x),
	("unary", ("prefixed",), lambda x: x),
	("(+|-|!|&|*)", ("+",), lambda PLUS: PLUS),
	("(+|-|!|&|*)", ("-",), lambda MINUS: MINUS),
	("(+|-|!|&|*)", ("!",), lambda NOT: NOT),
	("(+|-|!|&|*)", ("&",), lambda AMPERSAND: AMPERSAND),
	("(+|-|!|&|*)", ("*",), lambda MUL: MUL),
	
	# call: postfixed+ (inlay|decl|ctrl|prefixed)
	("call", ("postfixed+", "inlay"), lambda func, arg: NodeCall(func, arg)),
	("call", ("postfixed+", "decl"), lambda func, arg: NodeCall(func, NodeCompound(arg))),
	("call", ("postfixed+", "ctrl"), lambda func, arg: NodeCall(func, arg)),
	("call", ("postfixed+", "prefixed"), lambda func, arg: NodeCall(func, arg)),
	
	# inlay: prefix* INLAY (decl|ctrl|bound)
	#      | bound
	("inlay", ("prefix*", "INLAY", "decl"), lambda attr, INLAY, x: NodeCompound(x.AppendPrefix(*attr))),
	("inlay", ("prefix*", "INLAY", "ctrl"), lambda attr, INLAY, x: x.AppendPrefix(*attr)),
	("inlay", ("prefix*", "INLAY", "bound"), lambda attr, INLAY, x: x.AppendPrefix(*attr)),  # This works, but may not be necessary.
	("inlay", ("bound",), lambda x: x),
	
	# ctrl: prefix* IF bound (prefix* THEN)? bound else?
	#     | prefix* WHILE bound bound else?
	#     | prefix* FOR bound bound bound else?
	("ctrl", ("prefix*", "IF", "bound", "prefix*", "THEN", "bound", "else"), lambda attr, IF, cond, attr2, THEN, expr, otherwise: NodeIfElse(cond, EnsureCompound(expr).AppendPrefix(*attr2), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "IF", "bound", "bound", "else"), lambda attr, IF, cond, expr, otherwise: NodeIfElse(cond, EnsureCompound(expr), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "WHILE", "bound", "bound", "else"), lambda attr, WHILE, cond, loop, otherwise: NodeWhileElse(cond, EnsureCompound(loop), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "else"), lambda attr, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, EnsureCompound(loop), otherwise).AppendPrefix(*attr)),
	
	# else: prefix* ELSE (decl|ctrl|bound)
	("else", ("prefix*", "ELSE", "decl"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),  # This works, but can make syntax confusing.
	("else", ("prefix*", "ELSE", "ctrl"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else", ("prefix*", "ELSE", "bound"), lambda attr, ELSE, x: EnsureCompound(x).AppendPrefix(*attr)),
	("else", (), lambda: NodeCompound()),
	
	# It is named "bound" even if its length can go infinite, as it has a clear terminator that indicates the end of this element.
	# bound: prefix* (TMPLT|FN) bound bound
	#      | prefix* (STRUCT|TUPLE) bound
	#      | prefix* { stmt_lst }
	#      | prefix* .{ stmt_lst }
	#      | postfixed
	("bound", ("prefix*", "TMPLT", "bound", "bound"), lambda attr, TMPLT, param, body: NodeTemplate(param, EnsureCompound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "FN", "bound", "bound"), lambda attr, FN, param, body: NodeFunc(param, EnsureCompound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "STRUCT", "bound"), lambda attr, STRCT, body: NodeNamedStruct(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "TUPLE", "bound"), lambda attr, TUPLE, body: NodeNamedTuple(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "{", "stmt_lst", "}", "postfix*"), lambda attr, LCB, lst, RCB, pst: pst(NodeCompound(*lst)).AppendPrefix(*attr)),
	("bound", ("prefix*", ".{", "stmt_lst", "}", "postfix*"), lambda attr, LCB, lst, RCB, pst: pst(NodeStruct(*lst)).AppendPrefix(*attr)),
	("bound", ("postfixed",), lambda x: x[1](x[0])),
	
	# prefixed: prefix* @ postfixed+ postfixed
	("prefixed", ("prefix*", "@", "postfixed+", "postfixed"), lambda attr_lst, AT, attr, x: x[1](x[0]).AppendPrefix(*attr_lst, attr)),
	("prefix*", ("prefix*", "@", "postfixed+"), lambda lst, AT, attr: (*lst, attr)),
	("prefix*", (), lambda: ()),
	("postfixed+", ("postfixed+", "postfixed"), lambda head, x: x[1](NodeCall(head, x[0]))),
	("postfixed+", ("postfixed",), lambda x: x[1](x[0])),
	
	# postfixed: prim postfix*
	("postfixed", ("prim", "postfix*"), lambda x, pst: (x, pst)),
	("postfix*", ("postfix*", "postfix"), lambda head, pst: lambda node: pst(head(node))),
	("postfix*", (), lambda: lambda x: x),
	("postfix", (".", "prim"), lambda DOT, label: lambda node: NodeAccess(node, label)),
	("postfix", ("[", "stmt", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("[", "decl", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, NodeCompound(idx))),
	("postfix", ("[", "ctrl", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("#",), lambda HASH: lambda node: node),
	
	# prim: (INT|IDENT|STR|BOOL)
	#     | ( )
	#     | ( | )
	#     | ( (stmt|decl|ctrl) )
	#     | .( field_lst )
	#     | .[ element_lst ]
	("prim", ("INT",), NodeInt),
	("prim", ("IDENT",), NodeLabel),
	("prim", ("STR",), NodeStr),
	("prim", ("BOOL",), NodeBool),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "stmt", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "decl", ")"), lambda LPR, x, RPR: NodeCompound(x)),
	("prim", ("(", "ctrl", ")"), lambda LPR, x, RPR: x),
	("prim", (".(", "field_lst", ")"), lambda LPR, x, RPR: NodeStruct(*x)),
	("prim", (".[", "element_lst", "]"), lambda LPR, x, RPR: NodeTuple(*x)),
	
	# element_lst: element , element_lst
	#            | element?
	("element_lst", ("(suffixed|..suffixed)", ",", "element_lst"), lambda e, COMMA, lst: (e, *lst)),
	("element_lst", ("(suffixed|..suffixed)",), lambda e: (e,)),
	("element_lst", (), lambda: ()),
	
	# field_lst: field_set (;|,) field_lst
	#          | field_set?
	("field_lst", ("field_set", "(;|,)", "field_lst"), lambda x, SEMI, lst: (x, *lst)),
	("field_lst", ("field_set",), lambda x: (x,)),
	("field_lst", (), lambda: ()),
	("field_set", ("prefix*", "LET", "suffixed", "=", "suffixed"), lambda attr, LET, label, EQ, expr: NodeAssign(NodeDecl(label).AppendPrefix(*attr), expr)),
	("field_set", ("prefix*", "LET", "suffixed"), lambda attr, LET, label: NodeDecl(label).AppendPrefix(*attr)),
	("field_set", ("suffixed", "=", "suffixed"), lambda label, EQ, expr: NodeAssign(NodeDecl(label), expr)),
	("field_set", ("suffixed",), lambda x: x),
	("(;|,)", (";",), lambda SEMI: SEMI),
	("(;|,)", (",",), lambda COMMA: COMMA),
]
PRODUCTIONS = [Production(*p) for p in SYNTAX_RULES]


def EnsureCompound(node: Node) -> NodeCompound | NodeStruct:
	return node if isinstance(node, (NodeCompound, NodeStruct)) else NodeCompound(node)
