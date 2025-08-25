from parser import Production
from lang_ast import *


TOKEN_TYPES = [
	("RETURN", r"return"),
	("WHILE", r"while"),
	("ELSE", r"else"),
	("LET", r"let"),
	("FOR", r"for"),
	("IF", r"if"),
	("FN", r"fn"),
	
	("INT", r"[0-9]\d*"),
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
	#     | prefix* LET expr = assign
	#     | prefix* RETURN assign
	("stmt", ("assign",), lambda x: x),
	("stmt", ("prefix*", "LET", "expr", "=", "assign"), lambda attr, LET, var, EQ, expr: NodeDecl(var, expr).AppendPrefix(*attr)),
	("stmt", ("prefix*", "RETURN", "assign"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	# ("stmt", ("prefix*", "RETURN", "ctrl_else"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	
	# decl: prefix* FN (fn|cmpd|post) (fn|cmpd|post) (fn|cmpd|post)
	("decl", ("prefix*", "FN", "(fn|cmpd|post)", "(fn|cmpd|post)", "(fn|cmpd|post)"), lambda attr, FN, label, param, body: NodeDecl(label, NodeCallable(param, body)).AppendPrefix(*attr)),
	
	# ctrl_else..: prefix* (IF|WHILE) (fn|cmpd|post) (fn|cmpd|post) else..
	#            | prefix* FOR (fn|cmpd|post) (fn|cmpd|post) (fn|cmpd|post) else..
	("ctrl_else..", ("prefix*", "IF", "(fn|cmpd|post)", "(fn|cmpd|post)", "else.."), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, expr, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl_else..", ("prefix*", "WHILE", "(fn|cmpd|post)", "(fn|cmpd|post)", "else.."), lambda attr, FOR, cond, loop, x: (NodeWhileElse(cond, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl_else..", ("prefix*", "FOR", "(fn|cmpd|post)", "(fn|cmpd|post)", "(fn|cmpd|post)", "else.."), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	
	# else..: prefix* ELSE x_else..
	#       | ((stmt ;)|decl)
	("else..", ("prefix*", "ELSE", "ctrl_else.."), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("else..", ("prefix*", "ELSE", "(fn|cmpd|post)"), lambda attr, ELSE, x: (x.AppendPrefix(*attr),)),
	("else..", ("stmt", ";"), lambda x, SEMI: (NodeCompound(), x)),
	("else..", ("decl",), lambda x: (NodeCompound(), x)),
	("else..", (";",), lambda SEMI: (NodeCompound(),)),
	
	# ctrl..: prefix* (IF|WHILE) (fn|cmpd|post) (fn|cmpd|post) else_x..
	#       | prefix* FOR (fn|cmpd|post) (fn|cmpd|post) (fn|cmpd|post) else_x..
	("ctrl..", ("prefix*", "IF", "(fn|cmpd|post)", "(fn|cmpd|post)", "else_ctrl.."), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, expr, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl..", ("prefix*", "WHILE", "(fn|cmpd|post)", "(fn|cmpd|post)", "else_ctrl.."), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	("ctrl..", ("prefix*", "FOR", "(fn|cmpd|post)", "(fn|cmpd|post)", "(fn|cmpd|post)", "else_ctrl.."), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, loop, x[0]).AppendPrefix(*attr), *x[1:])),
	
	# else_ctrl..: prefix* ELSE x..
	#            | stmt?
	("else_ctrl..", ("prefix*", "ELSE", "ctrl.."), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("else_ctrl..", ("stmt",), lambda x: (NodeCompound(), x)),
	("else_ctrl..", (), lambda: (NodeCompound(),)),
	
	# ctrl_else: prefix* (IF|WHILE) (fn|cmpd|post) (fn|cmpd|post) else?
	#          | prefix* FOR (fn|cmpd|post) (fn|cmpd|post) (fn|cmpd|post) else?
	("ctrl_else", ("prefix*", "IF", "(fn|cmpd|post)", "(fn|cmpd|post)", "else?"), lambda attr, IF, cond, expr, otherwise: NodeIfElse(cond, expr, otherwise).AppendPrefix(*attr)),
	("ctrl_else", ("prefix*", "WHILE", "(fn|cmpd|post)", "(fn|cmpd|post)", "else?"), lambda attr, WHILE, cond, loop, otherwise: NodeWhileElse(cond, loop, otherwise).AppendPrefix(*attr)),
	("ctrl_else", ("prefix*", "FOR", "(fn|cmpd|post)", "(fn|cmpd|post)", "(fn|cmpd|post)", "else?"), lambda attr, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, loop, otherwise).AppendPrefix(*attr)),
	("else?", ("prefix*", "ELSE", "ctrl_else"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else?", ("prefix*", "ELSE", "(fn|cmpd|post)"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else?", (), lambda: NodeCompound()),
	
	# assign: expr = assign
	#       | expr
	("assign", ("expr", "=", "assign"), lambda var, EQ, expr: NodeAssign(var, expr)),
	("assign", ("expr",), lambda x: x),
	
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
	("postfix", ("[", "stmt", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),  # TODO: Stmt
	("postfix", ("[", "decl", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),  # TODO: Stmt
	("postfix", ("#",), lambda HASH: lambda node: node),
	
	# prim: INT
	#      | ID
	#      | ()
	#      | ( | )
	#      | ( expr )
	#      | ( decl )
	#      | ( if_else )
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "stmt", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "decl", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "ctrl_else", ")"), lambda LPR, x, RPR: x),
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
