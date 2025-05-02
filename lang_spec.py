from scaner import Token
from parser import Production
from interpreter import EvaluationContext


class Node:
	def Eval(self, ctx: EvaluationContext) -> any:
		raise NotImplementedError


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return int(self.token.GetValue())


class NodeRef(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())


class NodeList(Node):
	def __init__(self, node_lst: tuple[Node, ...]):
		self.node_lst = node_lst
	
	def __repr__(self):
		return f"{self.__class__.__name__}{self.node_lst}"
	
	def Eval(self, ctx):
		val = None
		for node in self.node_lst:
			val = node.Eval(ctx)
		return val


class NodeDecl(Node):
	def __init__(self, label: Token, expr: Node):
		self.label = label
		self.expr = expr
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.label.GetValue()}, {self.expr})"
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		ctx.Declare(self.label.GetValue(), val)
		return None


class NodeAssign(Node):
	def __init__(self, label: Token, expr: Node):
		self.label = label
		self.expr = expr
	
	def Eval(self, ctx):
		old = ctx.Lookup(self.label.GetValue())
		val = self.expr.Eval(ctx)
		ctx.Declare(self.label.GetValue(), val)
		return None


type Param = Token | tuple[Param, ...]
class NodeCallable(Node):
	def __init__(self, param: Param, body: Node):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.param}, {self.body})"
	
	def Eval(self, ctx):
		return self


type Arg = Node | tuple[Arg, ...]
class NodeApply(Node):
	def __init__(self, callable: Node, arg: Arg):
		self.callable = callable
		self.arg = arg
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.callable}, {self.callable})"
	
	def Eval(self, ctx):
		func = self.callable.Eval(ctx)
		if not isinstance(func, NodeCallable):
			raise ValueError(f"{self.callable} is not callable")
		
		def match_param(param, arg):
			if isinstance(param, tuple):
				assert isinstance(arg, tuple)
				assert len(param) == len(arg)
				for p, a in zip(param, arg):
					match_param(p, a)
			else:
				assert isinstance(param, Token)
				assert isinstance(arg, Node)
				ctx.Declare(param.GetValue(), arg.Eval(ctx))
		ctx.PushScope()
		match_param(func.param, self.arg)
		ret_val = func.body.Eval(ctx)
		ctx.PopScope()
		return ret_val


TOKEN_TYPES = [
	("RETURN", r"return"),
	("DEF", r"def"),
	("LET", r"let"),
	
	("INT", r"[1-9]\d*"),
	("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	
	("->", r"->"),
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
	("=", r"="),
	("{", r"\{"),
	("}", r"\}"),
	("[", r"\["),
	("]", r"\]"),
	("(", r"\("),
	(")", r"\)"),
	(",", r","),
	(";", r";"),
	
	# TODO: Handle mult-line comment
	("COMMENT", r"//.*"),
]

TERMINALS = set(t for t, _ in TOKEN_TYPES)

SYNTAX_RULES = [
	("S", ("stmt*",), lambda x: x),
	
	("stmt*", ("stmt+",), lambda lst: lst),
	("stmt*", (), lambda: NodeList(())),
	("stmt+", ("stmt+", "stmt"), lambda lst, s: NodeList(lst.node_lst + (s,))),
	("stmt+", ("stmt",), lambda stmt: NodeList((stmt,))),
	
	# stmt -> expr ; | decl ; | fun_decl ; | RETURN expr ; | ;
	("stmt", ("expr", ";"), lambda expr, end: expr),
	("stmt", ("decl", ";"), lambda decl, end: decl),
	("stmt", ("fun_decl", ";"), lambda decl: decl),
	("stmt", ("asgn", ";"), lambda asgn, end: asgn),
	("stmt", ("RETURN", "expr", ";"), lambda ret, expr, end: expr),
#	("stmt", (";",), lambda end: NodeDummy()),
	
	# decl -> let id = expr
	("decl", ("LET", "ID", "=", "expr"), lambda let, label, equal, expr: NodeDecl(label, expr)),
	# fun_decl -> def id arg body
	("fun_decl", ("DEF", "ID", "param", "comp_stmt"), lambda _def, label, arg, body: NodeDecl(label, NodeCallable(arg, body))),
	# asgn -> id = expr
	("asgn", ("ID", "=", "expr"), lambda label, equal, expr: NodeAssign(label, expr)),
	# expr -> lgc_or | fun_def
	("expr", ("lgc_or",), lambda x: x),
	("expr", ("fun_def",), lambda x: x),
	
	# fun_def -> param -> comp_stmt
	("fun_def", ("param", "->", "comp_stmt"), lambda arg, op, body: NodeCallable(arg, body)),
	# param -> ( field* )
	("param", ("(", "field*", ")"), lambda lpr, lst, rpr: lst),
	("field*", ("field+",), lambda lst: lst),
	("field*", (), lambda: ()),
	("field+", ("field+", ",", "ID"), lambda lst, comma, label: (*lst, label)),
	("field+", ("ID",), lambda label: (label,)),
	# comp_stmt -> { stmt* }
	("comp_stmt", ("{", "stmt*", "}"), lambda lpr, stmt_lst, rpr: stmt_lst),
	
	# lgc_or -> lgc_or || lgc_and | lgc_and
	("lgc_or", ("lgc_or", "||", "lgc_and"), lambda left, op, right: NodeApply(op, (left, right))),
	("lgc_or", ("lgc_and",), lambda x: x),
	
	# lgc_and -> lgc_and && equality | equality
	("lgc_and", ("lgc_and", "&&", "equality"), lambda left, op, right: NodeApply(op, (left, right))),
	("lgc_and", ("equality",), lambda x: x),
	
	# equality -> equality ==|!= relational | relational
	("equality", ("equality", "op-eq", "relational"), lambda left, op, right: NodeApply(op, (left, right))),
	("equality", ("relational",), lambda x: x),
	
	# relational -> relational >=|<=|>|< additive | additive
	("relational", ("relational", "op-cmp", "additive"), lambda left, op, right: NodeApply(op, (left, right))),
	("relational", ("additive",), lambda x: x),
	
	# additive -> additive +|- multiplicative | multiplicative
	("additive", ("additive", "op-add", "multiplicative"), lambda left, op, right: NodeApply(op, (left, right))),
	("additive", ("multiplicative",), lambda x: x),
	
	# multiplicative -> multiplicative *|/ unary | unary
	("multiplicative", ("multiplicative", "op-mul", "unary"), lambda left, op, right: NodeApply(op, (left, right))),
	("multiplicative", ("unary",), lambda x: x),
	
	# unary -> -|!|~ unary | applicative
	("unary", ("op-pre", "unary"), lambda op, unary: NodeApply(op, (unary,))),
	("unary", ("applicative",), lambda x: x),
	
	# applicative -> applicative arg | primary
	("applicative", ("applicative", "arg"), NodeApply),
	("applicative", ("primary",), lambda primary: primary),
	
	# arg -> ( expr ) | secondary
	("arg", ("(", "expr", ")"), lambda lpr, expr, rpr: (expr,)),
	("arg", ("raw",), lambda x: x),
	
	# raw -> int | id | tup
	("raw", ("INT",), NodeInt),
	("raw", ("ID",), NodeRef),
	("primary", ("tup",), lambda x: x),
	
	# primary -> raw | ( expr )
	("primary", ("raw",), lambda x: x),
	("primary", ("(", "expr", ")"), lambda lpr, expr, rpr: expr),
	
	# tup -> ( ) | ( expr , ) | ( expr (,expr)+ ,? )
	("tup", ("(", ")",), lambda lpr, rpr: ()),
	("tup", ("(", "expr", ",", ")"), lambda lpr, expr, rpr: (expr,)),
	("tup", ("(", "expr", "(,expr)+", ",?", ")"), lambda lpr, expr, expr_lst, comma, rpr: (expr,) + expr_lst),
	("(,expr)+", (",", "expr", "(,expr)+",), lambda comma, expr, expr_lst: (expr, *expr_lst)),
	("(,expr)+", (",", "expr",), lambda comma, expr: (expr,)),
	(",?", (",",), lambda x: x),
	(",?", (), lambda: None),
	
	("op-pre", ("!",), lambda not_: not_),
	("op-pre", ("~",), lambda bit_not: bit_not),
	("op-pre", ("-",), lambda minus: minus),
	("op-add", ("+",), lambda plus: plus),
	("op-add", ("-",), lambda minus: minus),
	("op-mul", ("*",), lambda mul: mul),
	("op-mul", ("/",), lambda div: div),
	("op-mul", ("%",), lambda mod: mod),
	("op_eq", ("==",), lambda eq: eq),
	("op-eq", ("!=",), lambda neq: neq),
	("op-cmp", (">=",), lambda gte: gte),
	("op-cmp", ("<=",), lambda lse: lse),
	("op-cmp", (">",), lambda gt: gt),
	("op-cmp", ("<",), lambda ls: ls),
]
SYNTAX_RULES = [Production(*p) for p in SYNTAX_RULES]
