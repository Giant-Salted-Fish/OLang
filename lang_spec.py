from parser import Production
from lang_ast import *


TOKEN_TYPES = [
	("RETURN", r"return"),
	("LET", r"let"),
	("FN", r"fn"),
	
	("INT", r"[1-9]\d*"),
	("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	
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
	
	# stmt_lst: stmt stmt_lst
	#         | (expr|var_decl|outer_assign|return|ε)
	("stmt_lst", ("stmt", "stmt_lst"), lambda x, lst: (x, *lst)),
	("stmt_lst", ("expr",), lambda x: (x,)),
	("stmt_lst", ("var_decl",), lambda x: (x,)),
	("stmt_lst", ("outer_assign",), lambda x: (x,)),
	("stmt_lst", ("return",), lambda x: (x,)),
	("stmt_lst", (), lambda: ()),
	
	# stmt: (expr|var_decl|outer_assign|return) ;
	#     | fn_decl ;?
	("stmt", ("expr", ";"), lambda x, SEMI: x),
	("stmt", ("var_decl", ";"), lambda x, SEMI: x),
	("stmt", ("outer_assign", ";"), lambda x, SEMI: x),
	("stmt", ("return", ";"), lambda x, SEMI: x),
	("stmt", ("fn_decl", ";?"), lambda x, SEMI: x),
	(";?", (";",), lambda SEMI: SEMI),
	(";?", (), lambda: None),
	
	("var_decl", ("prefix*", "LET", "expr", "=", "expr"), lambda attr, LET, var, EQ, expr: NodeDecl(var, expr).AppendPrefix(*attr)),
	("outer_assign", ("expr", "=", "expr"), lambda var, EQ, expr: NodeAssign(var, expr)),
	("return", ("prefix*", "RETURN", "expr"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	("fn_decl", ("prefix*", "FN", "(fn|cmpd|post)", "(fn|cmpd|post)", "(fn|cmpd|post)"), lambda attr, FN, label, param, body: NodeDecl(label, NodeCallable(param, body)).AppendPrefix(*attr)),
	
	# expr: (union|tuple|suffixed)
	("expr", ("union",), lambda x: x),
	("expr", ("tuple",), lambda x: x),
	("expr", ("suffixed",), lambda x: x),
	
	# union: (tuple|suffixed) | union..
	("union", ("(tuple|suffixed)", "|", "union.."), lambda x, PIPE, lst: NodeUnion(x, *lst)),
	("union..", ("(tuple|suffixed)", "|", "union.."), lambda x, PIPE, lst: (x, *lst)),
	("union..", ("(tuple|suffixed)",), lambda x: (x,)),
	("union..", (), lambda: ()),
	("(tuple|suffixed)", ("tuple",), lambda x: x),
	("(tuple|suffixed)", ("suffixed",), lambda x: x),
	
	# tuple: suffixed , tuple..
	("tuple", ("suffixed", ",", "tuple.."), lambda x, COMMA, lst: NodeTuple(x, *lst)),
	("tuple..", ("suffixed", ",", "tuple.."), lambda x, COMMA, lst: (x, *lst)),
	("tuple..", ("suffixed",), lambda x: (x,)),
	("tuple..", (), lambda: ()),
	
	# assign: suffixed = (assign|suffixed)
	("assign", ("suffixed", "=", "(assign|suffixed)"), lambda var, EQ, expr: NodeAssign(var, expr)),
	("(assign|suffixed)", ("assign",), lambda x: x),
	("(assign|suffixed)", ("suffixed",), lambda x: x),
	
	# suffixed: (lmbd|or) suffix*
	("suffixed", ("(lmbd|or)", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	("suffix*", ("suffix*", ":", "(lmbd|or)"), lambda lst, COLON, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	("(lmbd|or)", ("lmbd",), lambda x: x),
	("(lmbd|or)", ("or",), lambda x: x),
	
	# lmbd: or -> (lmbd|or)
	("lmbd", ("or", "->", "(lmbd|or)"), lambda param, ARROW, body: NodeCallable(param, body)),
	
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
	#      | (app|fn|cmpd|prefixed)
	("unary", ("(+|-|!)", "unary"), lambda OP, val: OP(val)),
	("unary", ("app",), lambda x: x),
	("unary", ("fn",), lambda x: x),
	("unary", ("cmpd",), lambda x: x),
	("unary", ("prefixed",), lambda x: x),
	("(+|-|!)", ("+",), lambda PLUS: lambda node: NodeUnaryOp(PLUS, node)),
	("(+|-|!)", ("-",), lambda MINUS: lambda node: NodeUnaryOp(MINUS, node)),
	("(+|-|!)", ("!",), lambda NOT: lambda node: NodeUnaryOp(NOT, node)),
	
	# app: post+ (fn|cmpd|prefixed)?
	("app", ("post+", "(fn|cmpd|prefixed)?"), lambda func, arg: NodeApply(make_applied(*func), arg) if arg else make_applied(*func)),
	("(fn|cmpd|prefixed)?", ("fn",), lambda x: x),
	("(fn|cmpd|prefixed)?", ("cmpd",), lambda x: x),
	("(fn|cmpd|prefixed)?", ("prefixed",), lambda x: x),
	("(fn|cmpd|prefixed)?", (), lambda: None),
	
	# fn: prefix* FN post (fn|cmpd|post)
	("fn", ("prefix*", "FN", "(fn|cmpd|post)", "(fn|cmpd|post)"), lambda attr, FN, param, body: NodeCallable(param, body).AppendPrefix(*attr)),
	("(fn|cmpd|post)", ("fn",), lambda x: x),
	("(fn|cmpd|post)", ("cmpd",), lambda x: x),
	("(fn|cmpd|post)", ("post",), lambda x: make_applied(x)),
	
	# cmpd: prefix* { stmt_lst }
	("cmpd", ("prefix*", "{", "stmt_lst", "}"), lambda attr, LCB, lst, RCB: NodeCompound(*lst).AppendPrefix(*attr)),
	
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
	("postfix", ("[", "expr", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	# ("postfix", ("[", "assign", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("#",), lambda HASH: lambda node: node),
	
	# prim: INT
	#      | ID
	#      | ()
	#      | ( | )
	#      | ( expr )
	#      | ( assign )
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "expr", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "assign", ")"), lambda LPR, x, RPR: x),
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
