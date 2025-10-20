from parser import Production
from lang_ast import (
	Node, NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeApply, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)
from typing import Callable


TOKEN_TYPES = [
	("TMPLT", r"template"),
	("CONTINUE", r"continue"),
	("STRUCT", r"struct"),
	("RETURN", r"return"),
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

SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable[..., Node]]] = [
	("S", ("stmt_lst",), lambda lst: NodeCompound(*lst)),
	
	# stmt_lst: (norm_stmt|;) stmt_lst
	#         | last_stmt?
	("stmt_lst", ("norm_stmt", "stmt_lst"), lambda x, lst: (*x, *lst)),
	("stmt_lst", (";", "stmt_lst"), lambda SEMI, lst: lst),
	("stmt_lst", ("last_stmt",), lambda x: x),
	("stmt_lst", (), lambda: ()),
	
	# norm_stmt: stmt ;
	#          | decl
	#          | norm_ctrl
	("norm_stmt", ("stmt", ";"), lambda x, SEMI: (x,)),
	("norm_stmt", ("decl",), lambda x: (x,)),
	("norm_stmt", ("norm_ctrl",), lambda x: x),
	
	# last_stmt: stmt
	#          | last_ctrl
	("last_stmt", ("stmt",), lambda x: (x,)),
	("last_stmt", ("last_ctrl",), lambda x: x),
	
	# stmt: assign
	#     | prefix* RETURN assign
	#     | prefix* BREAK assign?
	#     | prefix* CONTINUE
	("stmt", ("assign",), lambda x: x),
	("stmt", ("prefix*", "RETURN", "expr"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK", "expr"), lambda attr, BREAK, x: NodeBreak(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK"), lambda attr, BREAK: NodeBreak(NodeTuple()).AppendPrefix(*attr)),
	("stmt", ("prefix*", "CONTINUE"), lambda attr, CONT: NodeContinue().AppendPrefix(*attr)),
	
	# decl: prefix* (TMPLT|FN) bound bound bound
	# decl: prefix* (STRUCT|TUPLE) bound bound
	("decl", ("prefix*", "TMPLT", "bound", "bound", "bound"), lambda attr, TMPLT, label, param, body: NodeAssign(NodeDecl(label), NodeTemplate(param, check_compound(body)).AppendPrefix(*attr))),
	("decl", ("prefix*", "FN", "bound", "bound", "bound"), lambda attr, FN, label, param, body: NodeAssign(NodeDecl(label), NodeFunc(param, check_compound(body)).AppendPrefix(*attr))),
	("decl", ("prefix*", "STRUCT", "bound", "bound"), lambda attr, STRUCT, label, body: NodeAssign(NodeDecl(label), NodeNamedStruct(body).AppendPrefix(*attr))),
	("decl", ("prefix*", "TUPLE", "bound", "bound"), lambda attr, TUPLE, label, body: NodeAssign(NodeDecl(label), NodeNamedTuple(body).AppendPrefix(*attr))),
	
	# norm_ctrl: prefix* (IF|WHILE) bound bound norm_ctrl..
	#          | prefix* FOR bound bound bound norm_ctrl..
	# norm_ctrl..: prefix* ELSE norm_ctrl
	#            | prefix* ELSE bound norm_stmt
	#            | norm_stmt
	("norm_ctrl", ("prefix*", "IF", "bound", "bound", "norm_ctrl.."), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, check_compound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "WHILE", "bound", "bound", "norm_ctrl.."), lambda attr, FOR, cond, loop, x: (NodeWhileElse(cond, check_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "norm_ctrl.."), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, check_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl..", ("prefix*", "ELSE", "norm_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("norm_ctrl..", ("prefix*", "ELSE", "bound", "norm_stmt"), lambda attr, ELSE, x, follow: (check_compound(x).AppendPrefix(*attr), *follow)),
	("norm_ctrl..", ("norm_stmt",), lambda x: (NodeCompound(), *x)),
	
	# last_ctrl: prefix* (IF|WHILE) bound bound last_ctrl..
	#          | prefix* FOR bound bound bound last_ctrl..
	# last_ctrl..: prefix* ELSE last_ctrl
	#            | prefix* ELSE bound last_stmt
	#            | last_stmt
	("last_ctrl", ("prefix*", "IF", "bound", "bound", "last_ctrl.."), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, check_compound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "WHILE", "bound", "bound", "last_ctrl.."), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, check_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "last_ctrl.."), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, check_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl..", ("prefix*", "ELSE", "last_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("last_ctrl..", ("prefix*", "ELSE", "bound", "last_stmt"), lambda attr, ELSE, x, follow: (x.AppendPrefix(*attr), *follow)),
	("last_ctrl..", ("last_stmt",), lambda x: (NodeCompound(), *x)),
	
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
	#        | (tuple|suffixed)?
	("union", ("(tuple|suffixed)", "|", "union.."), lambda x, PIPE, lst: NodeUnion(x, *lst)),
	("union..", ("(tuple|suffixed)", "|", "union.."), lambda x, PIPE, lst: (x, *lst)),
	("union..", ("(tuple|suffixed)",), lambda x: (x,)),
	("union..", (), lambda: ()),
	("(tuple|suffixed)", ("tuple",), lambda x: x),
	("(tuple|suffixed)", ("suffixed",), lambda x: x),
	
	# tuple: suffixed , tuple..
	# tuple..: suffixed , tuple..
	#        | suffixed?
	("tuple", ("suffixed", ",", "tuple.."), lambda x, COMMA, lst: NodeTuple(x, *lst)),
	("tuple..", ("suffixed", ",", "tuple.."), lambda x, COMMA, lst: (x, *lst)),
	("tuple..", ("suffixed",), lambda x: (x,)),
	("tuple..", (), lambda: ()),
	
	# suffixed: (lmbd|ctrl_else|or) suffix*
	("suffixed", ("(lmbd|ctrl_else|or)", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	("suffix*", ("suffix*", ":", "(lmbd|ctrl_else|or)"), lambda lst, COLON, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	("(lmbd|ctrl_else|or)", ("lmbd",), lambda x: x),
	("(lmbd|ctrl_else|or)", ("ctrl_else",), lambda x: x),
	("(lmbd|ctrl_else|or)", ("or",), lambda x: x),
	
	# lmbd: (ctrl_else|or) -> (lmbd|ctrl_else|or)
	("lmbd", ("(ctrl_else|or)", "->", "(lmbd|ctrl_else|or)"), lambda param, ARROW, body: NodeFunc(param, check_compound(body))),
	("(ctrl_else|or)", ("ctrl_else",), lambda x: x),
	("(ctrl_else|or)", ("or",), lambda x: x),
	
	# ctrl_else: prefix* (IF|WHILE) bound bound else?
	#          | prefix* FOR bound bound bound else?
	("ctrl_else", ("prefix*", "IF", "bound", "bound", "else?"), lambda attr, IF, cond, expr, otherwise: NodeIfElse(cond, check_compound(expr), otherwise).AppendPrefix(*attr)),
	("ctrl_else", ("prefix*", "WHILE", "bound", "bound", "else?"), lambda attr, WHILE, cond, loop, otherwise: NodeWhileElse(cond, check_compound(loop), otherwise).AppendPrefix(*attr)),
	("ctrl_else", ("prefix*", "FOR", "bound", "bound", "bound", "else?"), lambda attr, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, check_compound(loop), otherwise).AppendPrefix(*attr)),
	("else?", ("prefix*", "ELSE", "ctrl_else"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else?", ("prefix*", "ELSE", "bound"), lambda attr, ELSE, x: check_compound(x).AppendPrefix(*attr)),
	("else?", (), lambda: NodeCompound()),
	
	# or: or || and
	#   | and
	("or", ("or", "||", "and"), lambda left, OR, right: NodeLogicalOp(OR, left, right)),
	("or", ("and",), lambda x: x),
	
	# and: and && eq
	#    | eq
	("and", ("and", "&&", "eq"), lambda left, AND, right: NodeLogicalOp(AND, left, right)),
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
	
	# unary: (+|-|!|&) unary
	#      | call
	("unary", ("(+|-|!|&)", "unary"), lambda OP, val: OP(val)),
	("unary", ("call",), lambda x: x),
	("(+|-|!|&)", ("+",), lambda PLUS: lambda node: NodeUnaryOp(PLUS, node)),
	("(+|-|!|&)", ("-",), lambda MINUS: lambda node: NodeUnaryOp(MINUS, node)),
	("(+|-|!|&)", ("!",), lambda NOT: lambda node: NodeUnaryOp(NOT, node)),
	("(+|-|!|&)", ("&",), lambda AMPERSAND: lambda node: NodeUnaryOp(AMPERSAND, node)),
	
	# call: post* (bound|prefixed)
	("call", ("post+", "(bound|prefixed)"), lambda func, arg: NodeApply(make_applied(*func), arg)),
	("call", ("(bound|prefixed)",), lambda x: x),
	("(bound|prefixed)", ("bound",), lambda x: x),
	("(bound|prefixed)", ("prefixed",), lambda x: x),
	
	# It is named "bound" even if its length can go infinite, as it has a clear terminator that indicates the end of this element.
	# bound: prefix* (TMPLT|FN) bound bound
	#      | prefix* (STRUCT|TUPLE) bound
	#      | prefix* { stmt_lst }
	#      | prefix* .{ stmt_lst }
	#      | post
	("bound", ("prefix*", "TMPLT", "bound", "bound"), lambda attr, TMPLT, param, body: NodeTemplate(param, check_compound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "FN", "bound", "bound"), lambda attr, FN, param, body: NodeFunc(param, check_compound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "STRUCT", "bound"), lambda attr, STRCT, body: NodeNamedStruct(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "TUPLE", "bound"), lambda attr, TUPLE, body: NodeNamedTuple(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "{", "stmt_lst", "}"), lambda attr, LCB, lst, RCB: NodeCompound(*lst).AppendPrefix(*attr)),
	("bound", ("prefix*", ".{", "stmt_lst", "}"), lambda attr, LPR, lst, RPR: NodeStruct(*lst).AppendPrefix(*attr)),
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
	("postfix", ("#",), lambda HASH: lambda node: node),
	
	# prim: (INT|LABEL|STR)
	#     | ( )
	#     | ( | )
	#     | ( expr )
	#     | ( decl )
	#     | .( field_lst )
	#     | .{ stmt_lst }
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("STR",), NodeStr),
	("prim", ("BOOL",), NodeBool),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "stmt", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "decl", ")"), lambda LPR, x, RPR: x),
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
PRODUCTIONS: list[Production[str, str, Node]] = [Production(*p) for p in SYNTAX_RULES]


def make_applied(
	head: tuple[Node, Callable[[Node], Node]],
	*items: tuple[Node, Callable[[Node], Node]]
):
	func = head[1](head[0])
	for node, postfix in items:
		node = NodeApply(func, node)
		func = postfix(node)
	return func


def check_compound(node: Node):
	return node if isinstance(node, NodeCompound) else NodeCompound(node)
