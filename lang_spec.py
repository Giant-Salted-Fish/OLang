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
	("S", ("stmt*",), lambda x: x),
	("stmt*", ("stmt*", "stmt"), lambda cmpd, stmt: cmpd.Append(stmt)),
	("stmt*", (), lambda: NodeCompound()),
	
	# stmt: (assign|expr) ;
	#     | prefix* LET expr = (assign|expr) ;
	#     | RETURN (assign|expr) ;
	#     | fn_decl
	#     | prefix* @ ((+|-|!)* post) post* fn_decl
	("stmt", ("(assign|expr)", ";"), lambda expr, SEMI: expr),
	("stmt", ("prefix*", "LET", "expr", "=", "(assign|expr)", ";"), lambda attr, LET, param, EQ, expr, SEMI: NodeDecl(param, expr).AppendPrefix(*attr)),
	("stmt", ("prefix*", "RETURN", "(assign|expr)", ";"), lambda attr, RET, expr, SEMI: expr.AppendPrefix(*attr)),
	("stmt", ("fn_decl",), lambda x: x),
	("stmt", ("prefix*", "@", "((+|-|!)* post)", "post*", "fn_decl"), lambda attr, AT, attr2, attr3, x: x.AppendPrefix(*attr, make_applied(*attr2, *attr3))),
	
	# fn_decl: FN ((+|-|!)* post) ((+|-|!)* post) cmpd ;?
	("fn_decl", ("FN", "((+|-|!)* post)", "((+|-|!)* post)", "cmpd", ";?"), lambda FN, label, param, body, SEMI: NodeDecl(make_applied(*label), NodeCallable(make_applied(*param), body))),
	(";?", (";",), lambda SEMI: SEMI),
	(";?", (), lambda: None),
	
	# assign: expr = (assign|expr)
	("assign", ("expr", "=", "(assign|expr)"), lambda var, EQ, expr: NodeAssign(var, expr)),
	("(assign|expr)", ("expr",), lambda x: x),
	("(assign|expr)", ("assign",), lambda x: x),
	
	# expr: union
	#     | suffixed
	#     | tuple
	("expr", ("union",), lambda x: x),
	("expr", ("tuple",), lambda x: x),
	("expr", ("suffixed",), lambda x: x),
	
	# union: (suffixed|tuple) |
	#      | ((suffixed|tuple) |)+ (suffixed|tuple) |?
	("union", ("(suffixed|tuple)", "|"), lambda x, PIPE: NodeUnion(x)),
	("union", ("((suffixed|tuple) |)+", "(suffixed|tuple)", "|?"), lambda union, x, PIPE: union.Append(x)),
	("((suffixed|tuple) |)+", ("((suffixed|tuple) |)+", "(suffixed|tuple)", "|"), lambda union, x, PIPE: union.Append(x)),
	("((suffixed|tuple) |)+", ("(suffixed|tuple)", "|"), lambda x, PIPE: NodeUnion(x)),
	("(suffixed|tuple)", ("suffixed",), lambda x: x),
	("(suffixed|tuple)", ("tuple",), lambda x: x),
	("|?", ("|",), lambda PIPE: PIPE),
	("|?", (), lambda: None),
	
	# tuple: suffixed ,
	#      | (suffixed ,)+ suffixed ,?
	("tuple", ("suffixed", ","), lambda x, COMMA: NodeTuple(x)),
	("tuple", ("(suffixed ,)+", "suffixed", ",?"), lambda tup, x, COMMA: tup.Append(x)),
	("(suffixed ,)+", ("(suffixed ,)+", "suffixed", ","), lambda tup, x, COMMA: tup.Append(x)),
	("(suffixed ,)+", ("suffixed", ","), lambda x, COMMA: NodeTuple(x)),
	(",?", (",",), lambda COMMA: COMMA),
	(",?", (), lambda: None),
	
	# suffixed: (or|lmbd) suffix*
	("suffixed", ("(or|lmbd)", "suffix*"), lambda expr, attr: expr.AppendSuffix(*attr)),
	("suffix*", ("suffix*", "suffix"), lambda lst, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	("suffix", (":", "(or|lmbd)"), lambda COLON, attr: attr),
	("(or|lmbd)", ("or",), lambda x: x),
	("(or|lmbd)", ("lmbd",), lambda x: x),
	
	# lmbd: or -> (or|lmbd)
	("lmbd", ("or", "->", "(or|lmbd)"), lambda param, ARROW, body: NodeCallable(param, body)),
	
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
	#        | (prefixed|app|fn)
	("unary", ("(+|-|!)", "unary"), lambda OP, val: OP(val)),
	("unary", ("prefixed",), lambda x: x),
	("unary", ("app",), lambda x: x),
	("unary", ("fn",), lambda x: x),
	("(+|-|!)", ("+",), lambda PLUS: lambda node: NodeUnaryOp(PLUS, node)),
	("(+|-|!)", ("-",), lambda MINUS: lambda node: NodeUnaryOp(MINUS, node)),
	("(+|-|!)", ("!",), lambda NOT: lambda node: NodeUnaryOp(NOT, node)),
	
	# app: post post* prefixed?
	("app", ("post", "post*", "prefixed?"), lambda func, arg1, arg2: make_applied(lambda x: x, func, arg1, *arg2)),
	("prefixed?", ("prefixed",), lambda x: (x,)),
	("prefixed?", (), lambda: ()),
	
	# prefixed: prefix* @ ((+|-|!)* post) post* (post|fn)
	("prefixed", ("prefix*", "@", "((+|-|!)* post)", "post*", "(post|fn)"), lambda attr, AT, func, follow, x: (x.AppendPrefix(*attr, make_applied(*func, *follow)), x[1])),
	("prefix", ("@", "((+|-|!)* post)", "post*"), lambda AT, func, follow: make_applied(*func, *follow)),
	("prefix*", ("prefix*", "prefix"), lambda lst, prefix: (*lst, prefix)),
	("prefix*", (), lambda: ()),
	("((+|-|!)* post)", ("(+|-|!)*", "post"), lambda OP, x: (OP, x)),
	("(+|-|!)*", ("(+|-|!)*", "(+|-|!)"), lambda head, unary: lambda node: head(unary(node))),
	("(+|-|!)*", (), lambda: lambda x: x),
	("post*", ("post*", "post"), lambda lst, x: (*lst, x)),
	("post*", (), lambda: ()),
	("(post|fn)", ("post",), lambda x: x),
	("(post|fn)", ("fn",), lambda x: (x, lambda y: y)),
	
	# fn: FN ((+|-|!)* post) (+|-|!)* (prefixed|post|fn)
	("fn", ("FN", "((+|-|!)* post)", "(+|-|!)*", "(prefixed|post|fn)"), lambda FN, param, UNARY, body: NodeCallable(make_applied(*param), make_applied(UNARY, body))),
	("(prefixed|post|fn)", ("prefixed",), lambda x: x),
	("(prefixed|post|fn)", ("post",), lambda x: x),
	("(prefixed|post|fn)", ("fn",), lambda x: (x, lambda y: y)),
	
	# post: prim postfix*
	("post", ("prim", "postfix*"), lambda x, post: (x, post)),
	("postfix*", ("postfix*", "postfix"), lambda head, post: lambda node: post(head(node))),
	("postfix*", (), lambda: lambda x: x),
	("postfix", (".", "prim"), lambda DOT, label: lambda node: NodeAccess(node, label)),  # . (+|-|!)* prim
	("postfix", ("[", "(assign|expr)", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("#",), lambda HASH: HASH),
	
	# prim: INT
	#      | ID
	#      | ()
	#      | ( | )
	#      | ( (assign|expr) )
	#      | cmpd
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "(assign|expr)", ")"), lambda LPR, x, RPR: x),
	("prim", ("cmpd",), lambda x: x),
	
	# cmpd: { stmt* (assign|expr)? }
	("cmpd", ("{", "stmt*", "(assign|expr)", "}"), lambda LPR, cmpd, expr, RPR: cmpd.Append(expr)),
	("cmpd", ("{", "stmt*", "}"), lambda LPR, cmpd, RPR: cmpd),
]
SYNTAX_RULES = [Production(*p) for p in SYNTAX_RULES]

def make_applied(
	unary: Callable[[Node], Node],
	head: tuple[Node, Callable[[Node], Node]],
	*items: tuple[Node, Callable[[Node], Node]]
):
	func = head[1](head[0])
	for node, postfix in items[1:]:
		node = NodeApply(func, node)
		func = postfix(node)
	return unary(func)
