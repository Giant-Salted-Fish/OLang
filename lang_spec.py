from parser import Production
from lang_ast import (
	Node, NodeInt, NodeLabel, NodeStr, NodeCompound, NodeDecl, NodeAssign, NodeFunc, NodeTmplt,
	NodeApply, NodeUnion, NodeTuple, NodeStruct, NodeBinaryOp, NodeUnaryOp, NodeAccess, NodeIndex,
	NodeReturn, NodeIfElse, NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)
from typing import Callable


TOKEN_TYPES = [
	("TMPLT", r"template"),
	("STRUCT", r"struct"),
	("RETURN", r"return"),
	("TUPLE", r"tuple"),
	("WHILE", r"while"),
	("ELSE", r"else"),
	("LET", r"let"),
	("FOR", r"for"),
	("IF", r"if"),
	("FN", r"fn"),
	
	("INT", r"[0-9]\d*"),
	("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	("STR", r"\"(?:[^\"\\]|\\[tnr])*\""),  # See https://github.com/antlr/grammars-v4/blob/master/java/java8/Java8Lexer.g4
	
	(".(", r"\.\("),
	(".{", r"\.\{"),
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

SYNTAX_RULES = [
	("S", ("stmt_lst",), lambda lst: NodeCompound(*lst)),
	
	# stmt_lst: ((stmt ;)|decl|ctrl_else..|;) stmt_lst
	#         | (stmt|ctrl..)?
	("stmt_lst", ("stmt", ";", "stmt_lst"), lambda x, SEMI, lst: (x, *lst)),
	("stmt_lst", ("decl", "stmt_lst"), lambda x, lst: (x, *lst)),
	("stmt_lst", ("ctrl_else..", "stmt_lst"), lambda x, lst: (*x, *lst)),
	("stmt_lst", (";", "stmt_lst"), lambda SEMI, lst: lst),
	("stmt_lst", ("stmt",), lambda x: (x,)),
	("stmt_lst", ("ctrl..",), lambda x: x),
	("stmt_lst", (), lambda: ()),
	
	# stmt: assign
	#     | prefix* RETURN assign
	("stmt", ("assign",), lambda x: x),
	("stmt", ("prefix*", "RETURN", "expr"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	# ("stmt", ("prefix*", "RETURN", "ctrl_else"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	
	# decl: prefix* (TEMPL|FN) bound bound bound
	# decl: prefix* (STRUCT|TUPLE) bound bound
	("decl", ("prefix*", "TMPLT", "bound", "bound", "bound"), lambda attr, TMPLT, label, param, body: NodeAssign(NodeDecl(label), NodeTmplt(param, body).AppendPrefix(*attr))),
	("decl", ("prefix*", "FN", "bound", "bound", "bound"), lambda attr, FN, label, param, body: NodeAssign(NodeDecl(label), NodeFunc(param, body).AppendPrefix(*attr))),
	("decl", ("prefix*", "STRUCT", "bound", "bound"), lambda attr, STRUCT, label, body: NodeAssign(NodeDecl(label), NodeNamedStruct(body).AppendPrefix(*attr))),
	("decl", ("prefix*", "TUPLE", "bound", "bound"), lambda attr, TUPLE, label, body: NodeAssign(NodeDecl(label), NodeNamedTuple(body).AppendPrefix(*attr))),
	
	# ctrl_else..: prefix* (IF|WHILE) bound bound else..
	#            | prefix* FOR bound bound bound else..
	("ctrl_else..", ("prefix*", "IF", "bound", "bound", "else.."), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, expr, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl_else..", ("prefix*", "WHILE", "bound", "bound", "else.."), lambda attr, FOR, cond, loop, x: (NodeWhileElse(cond, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl_else..", ("prefix*", "FOR", "bound", "bound", "bound", "else.."), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	
	# else..: prefix* ELSE x_else..
	#       | ((stmt ;)|decl)
	("else..", ("prefix*", "ELSE", "ctrl_else.."), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("else..", ("prefix*", "ELSE", "bound"), lambda attr, ELSE, x: (x.AppendPrefix(*attr),)),
	("else..", ("stmt", ";"), lambda x, SEMI: (NodeCompound(), x)),
	("else..", ("decl",), lambda x: (NodeCompound(), x)),
	("else..", ("ctrl_else..",), lambda x: (NodeCompound(), *x)),
	("else..", (";",), lambda SEMI: (NodeCompound(),)),
	
	# ctrl..: prefix* (IF|WHILE) bound bound else_x..
	#       | prefix* FOR bound bound bound else_x..
	("ctrl..", ("prefix*", "IF", "bound", "bound", "else_ctrl.."), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, expr, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl..", ("prefix*", "WHILE", "bound", "bound", "else_ctrl.."), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl..", ("prefix*", "FOR", "bound", "bound", "bound", "else_ctrl.."), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	
	# else_ctrl..: prefix* ELSE x..
	#            | stmt?
	("else_ctrl..", ("prefix*", "ELSE", "ctrl.."), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("else_ctrl..", ("stmt",), lambda x: (NodeCompound(), x)),
	("else_ctrl..", (), lambda: (NodeCompound(),)),
	
	# ctrl_else: prefix* (IF|WHILE) bound bound else?
	#          | prefix* FOR bound bound bound else?
	("ctrl_else", ("prefix*", "IF", "bound", "bound", "else?"), lambda attr, IF, cond, expr, otherwise: NodeIfElse(cond, expr, otherwise).AppendPrefix(*attr)),
	("ctrl_else", ("prefix*", "WHILE", "bound", "bound", "else?"), lambda attr, WHILE, cond, loop, otherwise: NodeWhileElse(cond, loop, otherwise).AppendPrefix(*attr)),
	("ctrl_else", ("prefix*", "FOR", "bound", "bound", "bound", "else?"), lambda attr, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, loop, otherwise).AppendPrefix(*attr)),
	("else?", ("prefix*", "ELSE", "ctrl_else"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else?", ("prefix*", "ELSE", "bound"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else?", (), lambda: NodeCompound()),
	
	# assign: (prefix* LET)? expr = assign
	#       | (prefix* LET)? expr
	("assign", ("(expr|let)", "=", "assign"), lambda var, EQ, expr: NodeAssign(var, expr)),
	("assign", ("(expr|let)",), lambda x: x),
	("(expr|let)", ("expr",), lambda x: x),
	("(expr|let)", ("prefix*", "LET", "expr"), lambda attr, LET, var: NodeDecl(var).AppendPrefix(*attr)),
	
	# expr: (union|tuple|suffixed)
	("expr", ("union",), lambda x: x),
	("expr", ("tuple",), lambda x: x),
	("expr", ("suffixed",), lambda x: x),
	
	# union: (tuple|suffixed) | union..
	# union..: (tuple|suffixed) | union..
	# union..: (tuple|suffixed)?
	("union", ("(tuple|suffixed)", "|", "union.."), lambda x, PIPE, lst: NodeUnion(x, *lst)),
	("union..", ("(tuple|suffixed)", "|", "union.."), lambda x, PIPE, lst: (x, *lst)),
	("union..", ("(tuple|suffixed)",), lambda x: (x,)),
	("union..", (), lambda: ()),
	("(tuple|suffixed)", ("tuple",), lambda x: x),
	("(tuple|suffixed)", ("suffixed",), lambda x: x),
	
	# tuple: suffixed , tuple..
	# tuple..: suffixed , tuple..
	# tuple..: suffixed?
	("tuple", ("suffixed", ",", "tuple.."), lambda x, COMMA, lst: NodeTuple(x, *lst)),
	("tuple..", ("suffixed", ",", "tuple.."), lambda x, COMMA, lst: (x, *lst)),
	("tuple..", ("suffixed",), lambda x: (x,)),
	("tuple..", (), lambda: ()),
	
	# suffixed: (lmbd|or) suffix*
	("suffixed", ("(lmbd|or)", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	("suffix*", ("suffix*", ":", "(lmbd|or)"), lambda lst, COLON, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	("(lmbd|or)", ("lmbd",), lambda x: x),
	("(lmbd|or)", ("or",), lambda x: x),
	
	# lmbd: or -> (lmbd|or)
	("lmbd", ("or", "->", "(lmbd|or)"), lambda param, ARROW, body: NodeFunc(param, body if isinstance(body, NodeCompound) else NodeCompound(body))),
	
	# or: or || and
	#   | and
	("or", ("or", "||", "and"), lambda left, OR, right: NodeBinaryOp(OR, left, right)),
	("or", ("and",), lambda x: x),
	
	# and: and && eq
	#    | eq
	("and", ("and", "&&", "eq"), lambda left, AND, right: NodeBinaryOp(AND, left, right)),
	("and", ("eq",), lambda x: x),
	
	# eq: eq (==|!=) rel
	#   | rel
	("eq", ("eq", "(==|!=)", "rel"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("eq", ("rel",), lambda x: x),
	("(==|!=)", ("==",), lambda EQ: EQ),
	("(==|!=)", ("!=",), lambda NEQ: NEQ),
	
	# rel: rel (<=|>=|<|>) add
	#    | add
	("rel", ("rel", "(<=|>=|<|>)", "add"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("rel", ("add",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda GTE: GTE),
	("(<=|>=|<|>)", ("<=",), lambda LSE: LSE),
	("(<=|>=|<|>)", (">",), lambda GT: GT),
	("(<=|>=|<|>)", ("<",), lambda LS: LS),
	
	# add: add (+|-) mul
	#    | mul
	("add", ("add", "(+|-)", "mul"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("add", ("mul",), lambda x: x),
	("(+|-)", ("+",), lambda PLUS: PLUS),
	("(+|-)", ("-",), lambda MINUS: MINUS),
	
	# mul: mul (*|/|%) unary
	#    | unary
	("mul", ("mul", "(*|/|%)", "unary"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("mul", ("unary",), lambda x: x),
	("(*|/|%)", ("*",), lambda MUL: MUL),
	("(*|/|%)", ("/",), lambda DIV: DIV),
	("(*|/|%)", ("%",), lambda MOD: MOD),
	
	# unary: (+|-|!) unary
	#      | app
	("unary", ("(+|-|!)", "unary"), lambda OP, val: OP(val)),
	("unary", ("app",), lambda x: x),
	("(+|-|!)", ("+",), lambda PLUS: lambda node: NodeUnaryOp(PLUS, node)),
	("(+|-|!)", ("-",), lambda MINUS: lambda node: NodeUnaryOp(MINUS, node)),
	("(+|-|!)", ("!",), lambda NOT: lambda node: NodeUnaryOp(NOT, node)),
	
	# app: post* (bound|prefixed)
	("app", ("post+", "(bound|prefixed)"), lambda func, arg: NodeApply(make_applied(*func), arg)),
	("app", ("(bound|prefixed)",), lambda x: x),
	("(bound|prefixed)", ("bound",), lambda x: x),
	("(bound|prefixed)", ("prefixed",), lambda x: x),
	
	# It is named "bound" even if its length can go infinite, as it has a clear terminator that indicates the end of this element.
	# bound: prefix* (TMPLT|FN) bound bound
	#      | prefix* (STRUCT|FN) bound
	#      | prefix* { stmt_lst }
	#      | post
	("bound", ("prefix*", "TMPLT", "bound", "bound"), lambda attr, TMPLT, param, body: NodeTmplt(param, body if isinstance(body, NodeCompound) else NodeCompound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "FN", "bound", "bound"), lambda attr, FN, param, body: NodeFunc(param, body if isinstance(body, NodeCompound) else NodeCompound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "STRUCT", "bound"), lambda attr, STRCT, body: NodeNamedStruct(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "TUPLE", "bound"), lambda attr, TUPLE, body: NodeNamedTuple(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "{", "stmt_lst", "}"), lambda attr, LCB, lst, RCB: NodeCompound(*lst).AppendPrefix(*attr)),
	("bound", ("prefix*", ".{", "stmt_lst", "}"), lambda attr, LPR, x, RPR: NodeStruct(*x).AppendPrefix(*attr)),
	("bound", ("post",), lambda x: make_applied(x)),
	
	# prefixed: prefix* @ post+ post
	("prefixed", ("prefix*", "@", "post+", "post"), lambda attr_lst, AT, attr, x: make_applied(x).AppendPrefix(*attr_lst, make_applied(*attr))),
	("prefix*", ("prefix*", "@", "post+"), lambda lst, AT, attr: (*lst, make_applied(*attr))),
	("prefix*", (), lambda: ()),
	("post+", ("post+", "post"), lambda lst, x: (*lst, x)),
	("post+", ("post",), lambda x: (x,)),
	
	# post: prim postfix*
	("post", ("prim", "postfix*"), lambda x, post: (x, post)),
	("postfix*", ("postfix*", "postfix"), lambda head, post: lambda node: post(head(node))),
	("postfix*", (), lambda: lambda x: x),
	("postfix", (".", "prim"), lambda DOT, label: lambda node: NodeAccess(node, label)),
	("postfix", ("[", "stmt", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("[", "decl", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("[", "ctrl_else", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("#",), lambda HASH: lambda node: node),
	
	# prim: (INT|LABEL|STR)
	#     | ( )
	#     | ( | )
	#     | ( expr )
	#     | ( decl )
	#     | ( ctrl_else )
	#     | .( field_lst )
	#     | .{ stmt_lst }
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("STR",), NodeStr),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "stmt", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "decl", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "ctrl_else", ")"), lambda LPR, x, RPR: x),
	("prim", (".(", "field_lst", ")"), lambda LPR, x, RPR: NodeStruct(*x)),
	
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
SYNTAX_RULES = [Production(*p) for p in SYNTAX_RULES]


def make_applied(
	head: tuple[Node, Callable[[Node], Node]],
	*items: tuple[Node, Callable[[Node], Node]]
):
	func = head[1](head[0])
	for node, postfix in items:
		node = NodeApply(func, node)
		func = postfix(node)
	return func
