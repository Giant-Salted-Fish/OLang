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
	#     | (fn_decl|cmpd) ;?
	("stmt", ("expr", ";"), lambda x, SEMI: x),
	("stmt", ("var_decl", ";"), lambda x, SEMI: x),
	("stmt", ("outer_assign", ";"), lambda x, SEMI: x),
	("stmt", ("return", ";"), lambda x, SEMI: x),
	("stmt", ("fn_decl", ";?"), lambda x, SEMI: x),
	("stmt", ("cmpd", ";?"), lambda attr, x, SEMI: x.AppendPrefix(*attr)),
	(";?", (";",), lambda SEMI: SEMI),
	(";?", (), lambda: None),
	
	("var_decl", ("prefix*", "LET", "expr", "=", "(cmpd|expr)"), lambda attr, LET, var, EQ, expr: NodeDecl(var, expr).AppendPrefix(*attr)),
	("outer_assign", ("expr", "=", "(cmpd|expr)"), lambda var, EQ, expr: NodeAssign(var, expr)),
	("return", ("prefix*", "RETURN", "(cmpd|expr)"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	("fn_decl", ("prefix*", "FN", "post", "post", "(cmpd|fn|post)"), lambda attr, FN, label, param, body: NodeDecl(make_applied(label), NodeCallable(make_applied(param), body)).AppendPrefix(*attr)),
	
	# cmpd: prefix* { stmt_lst }
	("cmpd", ("prefix*", "{", "stmt_lst", "}"), lambda attr, LCB, lst, RCB: NodeCompound(*lst).AppendPrefix(*attr)),
	("(cmpd|expr)", ("cmpd",), lambda x: x),
	("(cmpd|expr)", ("expr",), lambda x: x),
	
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
	
	# suffixed: (lmbd|fn|or) suffix*
	("suffixed", ("(lmbd|fn|or)", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	("suffix*", ("suffix*", ":", "(lmbd|fn|or)"), lambda lst, COLON, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	("(lmbd|fn|or)", ("lmbd",), lambda x: x),
	("(lmbd|fn|or)", ("fn",), lambda x: x),
	("(lmbd|fn|or)", ("or",), lambda x: x),
	
	# lmbd: or -> (lmbd|fn|or)
	("lmbd", ("or", "->", "(lmbd|fn|or)"), lambda param, ARROW, body: NodeCallable(param, body)),
	
	# fn: prefix* FN post (cmpd|fn|post)
	("fn", ("prefix*", "FN", "post", "(cmpd|fn|post)"), lambda attr, FN, param, body: NodeCallable(make_applied(param), body).AppendPrefix(*attr)),
	("(cmpd|fn|post)", ("cmpd",), lambda x: x),
	("(cmpd|fn|post)", ("fn",), lambda x: x),
	("(cmpd|fn|post)", ("post",), lambda x: make_applied(x)),
	
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
	#      | (prefixed|app)
	("unary", ("(+|-|!)", "unary"), lambda OP, val: OP(val)),
	("unary", ("prefixed",), lambda x: x),
	("unary", ("app",), lambda x: x),
	("(+|-|!)", ("+",), lambda PLUS: lambda node: NodeUnaryOp(PLUS, node)),
	("(+|-|!)", ("-",), lambda MINUS: lambda node: NodeUnaryOp(MINUS, node)),
	("(+|-|!)", ("!",), lambda NOT: lambda node: NodeUnaryOp(NOT, node)),
	
	# app: post+ (cmpd|fn|prefixed)?
	("app", ("post+", "(cmpd|fn|prefixed)?"), lambda func, arg: make_applied(*func, *arg)),
	("(cmpd|fn|prefixed)?", ("cmpd",), lambda x: (x,)),
	("(cmpd|fn|prefixed)?", ("fn",), lambda x: (x,)),
	("(cmpd|fn|prefixed)?", ("prefixed",), lambda x: (x,)),
	("(cmpd|fn|prefixed)?", (), lambda: ()),
	
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
	#      | ( cmpd )
	#      | ( expr )
	#      | ( assign )
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "cmpd", ")"), lambda LPR, x, RPR: x),
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
