from typing import Self
from scaner import Token
from parser import Production
from interpreter import EvaluationContext


class Node:
	annotations = ()
	
	def Annotate(self, *annotations: "Node") -> Self:
		self.annotations = self.annotations + annotations
		return self
	
	def Eval(self, ctx: EvaluationContext) -> any:
		raise NotImplementedError
	
	def Unwind(self, val, ctx: EvaluationContext) -> None:
		raise NotImplementedError
	
	def Invoke(self, arg, ctx: EvaluationContext) -> any:
		raise NotImplementedError


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return int(self.token.GetValue())
	
	def Unwind(self, val, ctx):
		pass


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())
	
	def Unwind(self, val, ctx):
		ctx.Push(self.token.GetValue(), val)


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return f"{self.__class__.__name__}{self.nodes}"
	
	def Append(self, node: Node) -> Self:
		self.nodes = (*self.nodes, node)
		return self
	
	def Eval(self, ctx):
		val = None
		for node in self.nodes:
			val = node.Eval(ctx)
		return val


class NodeDecl(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.var}, {self.expr})"
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		self.var.Unwind(val, ctx)
		return ()


class NodeAssign(Node):
	def __init__(self, label: NodeLabel, expr: Node):
		self.label = label
		self.expr = expr
	
	def Eval(self, ctx):
		ctx.Pop(self.label.token.GetValue())
		val = self.expr.Eval(ctx)
		self.label.Unwind(val, ctx)
		return val


class NodeCallable(Node):
	def __init__(self, param: Node, body: Node):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.param}, {self.body})"
	
	def Eval(self, ctx):
		return self
	
	def Invoke(self, arg, ctx):
		ctx.PushScope()
		self.param.Unwind(arg, ctx)
		ret_val = self.body.Eval(ctx)
		ctx.PopScope()
		return ret_val


class NodeApply(Node):
	def __init__(self, callable: Node, arg: Node):
		self.callable = callable
		self.arg = arg
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.callable}, {self.arg})"
	
	def Eval(self, ctx):
		func = self.callable.Eval(ctx)
		arg = self.arg.Eval(ctx)
		return func.Invoke(arg, ctx)


class NodeTuple(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return f"{self.__class__.__name__}{self.nodes}"
	
	def Merge(self, other: "NodeTuple") -> Self:
		self.nodes = self.nodes + other.nodes
		return self
	
	def Append(self, node: Node) -> Self:
		self.nodes = (*self.nodes, node)
		return self
	
	def Eval(self, ctx):
		return tuple(node.Eval(ctx) for node in self.nodes)
	
	def Unwind(self, val, ctx):
		if not isinstance(val, tuple):
			raise ValueError(f"Expected tuple, got {val}")
		
		assert len(val) == len(self.nodes)
		for i, node in enumerate(self.nodes):
			node.Unwind(val[i], ctx)


class NodeBinaryOp(Node):
	_op_table = {
		"+": lambda x, y: x + y,
		"-": lambda x, y: x - y,
		"*": lambda x, y: x * y,
		"/": lambda x, y: x / y,
		"%": lambda x, y: x % y,
		"==": lambda x, y: x == y,
		"!=": lambda x, y: x != y,
		"<=": lambda x, y: x <= y,
		">=": lambda x, y: x >= y,
		"<": lambda x, y: x < y,
		">": lambda x, y: x > y,
		"||": lambda x, y: x or y,
		"&&": lambda x, y: x and y,
	}
	
	def __init__(self, token: Token):
		self.token = token
		self.op = self._op_table[token.GetValue()]
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token})"
	
	def Eval(self, ctx):
		return self
	
	def Invoke(self, arg, ctx):
		self.op(arg[0], arg[1])


class NodeUnaryOp(Node):
	_op_table = {
		"+": lambda x: +x,
		"-": lambda x: -x,
		"!": lambda x: not x,
		"~": lambda x: ~x,
	}
	
	def __init__(self, token: Token):
		self.token = token
		self.op = self._op_table[token.GetValue()]
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token})"
	
	def Eval(self, ctx):
		return self
	
	def Invoke(self, arg, ctx):
		return self.op(arg)


TOKEN_TYPES = [
	("RETURN", r"return"),
	("DEF", r"def"),
	("LET", r"let"),
	
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
	("@", r"@"),
	(":", r":"),
	("->", r"->"),
	(",", r","),
	
	# TODO: Handle mult-line comment
	("COMMENT", r"//.*"),
]

TERMINALS = set(t for t, _ in TOKEN_TYPES)

SYNTAX_RULES = [
	("S", ("stmt*",), lambda x: x),
	
	# stmt: expr ; | decl ; | fun_decl ; | RETURN expr ; | ;
	("stmt", ("expr", ";"), lambda expr, end: expr),
	("stmt", ("decl", ";"), lambda decl, end: decl),
	("stmt", ("fun_decl", ";"), lambda decl, end: decl),
	("stmt", ("asgn", ";"), lambda asgn, end: asgn),
	("stmt", ("RETURN", "expr", ";"), lambda ret, expr, end: expr),
#	("stmt", (";",), lambda end: NodeDummy()),
	
	# decl: LET expr = expr
	("decl", ("LET", "expr", "=", "expr"), lambda let, var, eq, expr: NodeDecl(var, expr)),
	# fun_decl: DEF ID expr cmpd_stmt
	("fun_decl", ("DEF", "ID", "expr", "cmpd_stmt"), lambda _def, label, param, body: NodeDecl(label, NodeCallable(param, body))),
	# asgn: label = expr
	("asgn", ("label", "=", "expr"), lambda label, equal, expr: NodeAssign(label, expr)),
	# expr: lgc_or
	("expr", ("lgc_or",), lambda expr: expr),
	
	# cmpd_stmt: { stmt* }
	("cmpd_stmt", ("{", "stmt*", "}"), lambda lpr, cmpd, rpr: cmpd),
	("stmt*", ("stmt+",), lambda cmpd: cmpd),
	("stmt*", (), lambda: NodeCompound()),
	("stmt+", ("stmt+", "stmt"), lambda cmpd, stmt: cmpd.Append(stmt)),
	("stmt+", ("stmt",), lambda stmt: NodeCompound(stmt)),
	
	# lgc_or: lgc_or || lgc_and | lgc_and
	("lgc_or", ("lgc_or", "||", "lgc_and"), lambda left, op, right: NodeApply(NodeBinaryOp(op), NodeTuple(left, right))),
	("lgc_or", ("lgc_and",), lambda x: x),
	
	# lgc_and: lgc_and && equality | equality
	("lgc_and", ("lgc_and", "&&", "equality"), lambda left, op, right: NodeApply(NodeBinaryOp(op), NodeTuple(left, right))),
	("lgc_and", ("equality",), lambda x: x),
	
	# equality: equality (==|!=) relational | relational
	("equality", ("equality", "(==|!=)", "relational"), lambda left, op, right: NodeApply(NodeBinaryOp(op), NodeTuple(left, right))),
	("equality", ("relational",), lambda x: x),
	("(==|!=)", ("==",), lambda eq: eq),
	("(==|!=)", ("!=",), lambda neq: neq),
	
	# relational: relational (<=|>=|<|>) additive | additive
	("relational", ("relational", "(<=|>=|<|>)", "additive"), lambda left, op, right: NodeApply(NodeBinaryOp(op), NodeTuple(left, right))),
	("relational", ("additive",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda gte: gte),
	("(<=|>=|<|>)", ("<=",), lambda lse: lse),
	("(<=|>=|<|>)", (">",), lambda gt: gt),
	("(<=|>=|<|>)", ("<",), lambda ls: ls),
	
	# additive: additive (+|-) multiplicative | multiplicative
	("additive", ("additive", "(+|-)", "multiplicative"), lambda left, op, right: NodeApply(NodeBinaryOp(op), NodeTuple(left, right))),
	("additive", ("multiplicative",), lambda x: x),
	("(+|-)", ("+",), lambda plus: plus),
	("(+|-)", ("-",), lambda minus: minus),
	
	# multiplicative: multiplicative (*|/|%) unary | unary
	("multiplicative", ("multiplicative", "(*|/|%)", "unary"), lambda left, op, right: NodeApply(NodeBinaryOp(op), NodeTuple(left, right))),
	("multiplicative", ("unary",), lambda x: x),
	("(*|/|%)", ("*",), lambda mul: mul),
	("(*|/|%)", ("/",), lambda div: div),
	("(*|/|%)", ("%",), lambda mod: mod),
	
	# unary: (+|-|!|~) unary | applicative
	("unary", ("(+|-|!|~)", "unary"), lambda op, unary: NodeApply(NodeUnaryOp(op), unary)),
	("unary", ("applicative",), lambda x: x),
	("(+|-|!|~)", ("+",), lambda plus: plus),
	("(+|-|!|~)", ("-",), lambda minus: minus),
	("(+|-|!|~)", ("!",), lambda not_: not_),
	("(+|-|!|~)", ("~",), lambda bit_not: bit_not),
	
	# applicative: applicative annotated | annotated
	("applicative", ("applicative", "annotated"), NodeApply),
	("applicative", ("annotated",), lambda x: x),
	
	# annotated: prefix? lambda suffix?
	("annotated", ("prefix?", "lambda", "suffix?"), lambda prefix, x, suffix: x.Annotate(*prefix, *suffix)),
	("prefix?", ("prefix",), lambda prefix: prefix),
	("prefix?", (), lambda: ()),
	("suffix?", ("suffix",), lambda suffix: suffix),
	("suffix?", (), lambda: ()),
	
	# prefix: (@ lambda)+
	("prefix", ("(@ lambda)+",), lambda lmbd: (lmbd,)),
	("(@ lambda)+", ("(@ lambda)+", "@", "lambda"), lambda lst, at, lmbd: (*lst, lmbd)),
	("(@ lambda)+", ("@", "lambda",), lambda at, lmbd: (lmbd,)),
	
	# suffix: (: lambda)+
	("suffix", ("(: lambda)+",), lambda lst: lst,),
	("(: lambda)+", ("(: lambda)+", ":", "lambda",), lambda lst, colon, lmbd: (*lst, lmbd)),
	("(: lambda)+", (":", "lambda",), lambda colon, lmbd: (lmbd,)),
	
	# lambda: primary -> primary
	("lambda", ("primary", "->", "primary"), lambda primary, op, body: NodeCallable(primary, body)),
	("lambda", ("primary",), lambda primary: primary),
	
	# primary: int | id | ( expr ) | tup | cmpd_stmt
	("primary", ("INT",), NodeInt),
	("primary", ("ID",), NodeLabel),
	("primary", ("(", "expr", ")"), lambda lpr, expr, rpr: expr),
	("primary", ("tup",), lambda x: x),
	("primary", ("cmpd_stmt",), lambda cmpd: cmpd),
	
	# label: prefix? ID suffix?
	("label", ("prefix?", "ID", "suffix?"), lambda prefix, label, suffix: NodeLabel(label).Annotate(*prefix, *suffix)),
	
	# tup: ( ) | ( expr , ) | ( expr (, expr)+ ,? )
	("tup", ("(", ")",), lambda lpr, rpr: NodeTuple()),
	("tup", ("(", "expr", ",", ")"), lambda lpr, expr, rpr: NodeTuple(expr,)),
	("tup", ("(", "expr", "(, expr)+", ",?", ")"), lambda lpr, expr, tup, comma, rpr: NodeTuple(expr).Merge(tup)),
	("(, expr)+", ("(, expr)+", ",", "expr"), lambda comma, tup, expr: tup.Append(expr)),
	("(, expr)+", (",", "expr",), lambda comma, expr: NodeTuple(expr)),
	(",?", (",",), lambda comma: comma),
	(",?", (), lambda: None),
]
SYNTAX_RULES = [Production(*p) for p in SYNTAX_RULES]
