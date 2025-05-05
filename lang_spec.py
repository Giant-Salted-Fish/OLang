from typing import Self, Sequence
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
	
	def GenText(self) -> Sequence[str]:
		raise NotImplementedError
	
	def AppendAnnotText(self, lines: Sequence[str]) -> Sequence[str]:
		elements = [node.GenText() for node in self.annotations]
		if all(len(lines) == 1 for lines in elements):
			annot = ["".join(f"@{lines[0]} " for lines in elements)]
		else:
			annot = [f": {line}" for lines in elements for line in lines]
		return self.JoinText("", annot, lines)
	
	@staticmethod
	def JoinText(joiner: str, first: Sequence[str], second: Sequence[str]) -> Sequence[str]:
		return [
			*first[:-1],
			f"{first[-1]}{joiner}{second[0]}",
			*second[1:],
		]


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return int(self.token.GetValue())
	
	def Unwind(self, val, ctx):
		pass
	
	def GenText(self):
		return self.AppendAnnotText([self.token.GetValue()])


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())
	
	def Unwind(self, val, ctx):
		ctx.Push(self.token.GetValue(), val)
	
	def GenText(self):
		return self.AppendAnnotText([self.token.GetValue()])


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
	
	def GenText(self):
		def process(n):
			lines = n.GenText()
			return [*lines[:-1], f"{lines[-1]};"]
		return self.AppendAnnotText([
			"{",
			*(f"\t{line}" for node in self.nodes for line in process(node)),
			"}",
		])


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
	
	def GenText(self):
		lines = self.JoinText(" = ", self.var.GenText(), self.expr.GenText())
		lines = [f"let {lines[0]}", *lines[1:]]
		return self.AppendAnnotText(lines)


class NodeAssign(Node):
	def __init__(self, label: NodeLabel, expr: Node):
		self.label = label
		self.expr = expr
	
	def Eval(self, ctx):
		ctx.Pop(self.label.token.GetValue())
		val = self.expr.Eval(ctx)
		self.label.Unwind(val, ctx)
		return val
	
	def GenText(self):
		lines = self.JoinText(" = ", self.label.GenText(), self.expr.GenText())
		return self.AppendAnnotText(lines)


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
	
	def GenText(self):
		lines = self.JoinText(" -> ", self.param.GenText(), self.body.GenText())
		return self.AppendAnnotText(lines)


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
	
	def GenText(self):
		lines = self.JoinText(" ", self.callable.GenText(), self.arg.GenText())
		return self.AppendAnnotText(lines)


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
	
	def GenText(self):
		elements = [node.GenText() for node in self.nodes]
		if all(len(lines) == 1 for lines in elements):
			inner = ", ".join(lines[0] for lines in elements)
			if len(elements) == 1:
				inner += ","
			lines = [f"({inner})"]
		else:
			lines = [
				"(",
				*(f"\t{line}," for lines in elements for line in [*lines[:-1], f"{lines[-1]},"]),
				")",
			]
		return self.AppendAnnotText(lines)


class NodeBinaryOp(Node):
	def __init__(self, op: Token, left: Node, right: Node):
		self.op = op
		self.left = left
		self.right = right
	
	def __repr__(self):
		return f"{self.__class__.__name__}(op={self.op}, left={self.left}, right={self.right})"
	
	def Eval(self, ctx):
		x = self.left.Eval(ctx)
		y = self.right.Eval(ctx)
		match self.op.GetValue():
			case "+":
				return x + y
			case "-":
				return x - y
			case "*":
				return x * y
			case "/":
				return x / y
			case "%":
				return x % y
			case "==":
				return x == y
			case "!=":
				return x != y
			case "<=":
				return x <= y
			case ">=":
				return x >= y
			case "<":
				return x < y
			case ">":
				return x > y
			case "||":
				return x or y
			case "&&":
				return x and y
			case _:
				raise ValueError(f"Unknown operator: {self.op}")
	
	def GenText(self):
		lines = self.JoinText(f" {self.op.GetValue()} ", self.left.GenText(), self.right.GenText())
		return self.AppendAnnotText(lines)


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	def __repr__(self):
		return f"{self.__class__.__name__}(op={self.op}, node={self.node})"
	
	def Eval(self, ctx):
		val = self.node.Eval(ctx)
		match self.op.GetValue():
			case "+":
				return val
			case "-":
				return -val
			case "!":
				return not val
			case "~":
				return ~val
			case _:
				raise ValueError(f"Unknown operator: {self.op}")
	
	def GenText(self):
		val = self.node.GenText()
		return [f"{self.op.GetValue()}{val[0]}", *val[1:]]


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
	
	# cmpd_stmt: { stmt* }
	("cmpd_stmt", ("{", "stmt*", "}"), lambda lpr, cmpd, rpr: cmpd),
	("stmt*", ("stmt+",), lambda cmpd: cmpd),
	("stmt*", (), lambda: NodeCompound()),
	("stmt+", ("stmt+", "stmt"), lambda cmpd, stmt: cmpd.Append(stmt)),
	("stmt+", ("stmt",), lambda stmt: NodeCompound(stmt)),
	
	# stmt: expr ; | decl ; | fun_decl ; | RETURN expr ; | ;
	("stmt", ("expr", ";"), lambda expr, end: expr),
	("stmt", ("decl", ";"), lambda decl, end: decl),
	("stmt", ("fun_decl", ";"), lambda decl, end: decl),
	("stmt", ("asgn", ";"), lambda asgn, end: asgn),
	("stmt", ("RETURN", "expr", ";"), lambda ret, expr, end: expr),
#	("stmt", (";",), lambda end: NodeDummy()),
	
	# expr: annot | !annot
	("expr", ("annot",), lambda x: x),
	("expr", ("!annot",), lambda x: x),
	
	# decl: LET prim = expr
	("decl", ("LET", "prim", "=", "expr"), lambda let, var, eq, expr: NodeDecl(var, expr)),
	# fun_decl: DEF ID prim cmpd_stmt
	("fun_decl", ("DEF", "ID", "prim", "cmpd_stmt"), lambda _def, label, param, body: NodeDecl(NodeLabel(label), NodeCallable(param, body))),
	# asgn: label = expr
	("asgn", ("label", "=", "expr"), lambda label, eq, expr: NodeAssign(label, expr)),
	
	# annot: prefix? prim suffix?
	("annot", ("prefix?", "prim", "suffix?"), lambda prefix, x, suffix: x.Annotate(*prefix, *suffix)),
	("prefix?", ("prefix",), lambda prefix: prefix),
	("prefix?", (), lambda: ()),
	("suffix?", ("suffix",), lambda suffix: suffix),
	("suffix?", (), lambda: ()),
	
	# !annot: prefix? (!lmbd|!add) suffix?
	("!annot", ("prefix?", "(!lmbd|!add)", "suffix?"), lambda prefix, x, suffix: x.Annotate(*prefix, *suffix)),
	("(!lmbd|!add)", ("!lmbd",), lambda x: x),
	("(!lmbd|!add)", ("!add",), lambda x: x),
	
	# prefix: (@ attr)+
	("prefix", ("(@ attr)+",), lambda attr: (attr,)),
	("(@ attr)+", ("(@ attr)+", "@", "attr"), lambda lst, at, attr: (*lst, attr)),
	("(@ attr)+", ("@", "attr",), lambda at, attr: (attr,)),
	
	# suffix: (: attr)+
	("suffix", ("(: attr)+",), lambda lst: lst,),
	("(: attr)+", ("(: attr)+", ":", "attr",), lambda lst, colon, attr: (*lst, attr)),
	("(: attr)+", (":", "attr",), lambda colon, attr: (attr,)),
	
	# attr -> (!lmbd|!add|prim)
	("attr", ("!lmbd",), lambda x: x),
	("attr", ("!add",), lambda x: x),
	("attr", ("prim",), lambda x: x),
	
	# !lmbd:  -> prim -> !lmbd | prim -> (!or|prim)
	("!lmbd", ("prim", "->", "!lmbd"), lambda param, op, body: NodeCallable(param, body)),
	("!lmbd", ("prim", "->", "(!or|prim)",), lambda param, op, body: NodeCallable(param, body)),
	
	# !or: (!or|prim) || (!and|prim) | !and
	("!or", ("(!or|prim)", "||", "(!and|prim)"), lambda left, op, right: NodeBinaryOp(op, left, right)),
	("!or", ("!and",), lambda x: x),
	("(!or|prim)", ("!or",), lambda x: x),
	("(!or|prim)", ("prim",), lambda x: x),
	
	# !and: (!and|prim) && (!eq|prim) | !eq
	("!and", ("(!and|prim)", "&&", "(!eq|prim)"), lambda left, op, right: NodeBinaryOp(op, left, right)),
	("!and", ("!eq",), lambda x: x),
	("(!and|prim)", ("!and",), lambda x: x),
	("(!and|prim)", ("prim",), lambda x: x),
	
	# !eq: (!eq|prim) (==|!=) (!rel|prim) | !rel
	("!eq", ("(!eq|prim)", "(==|!=)", "(!rel|prim)"), lambda left, op, right: NodeBinaryOp(op, left, right)),
	("!eq", ("!rel",), lambda x: x),
	("(!eq|prim)", ("!eq",), lambda x: x),
	("(!eq|prim)", ("prim",), lambda x: x),
	("(==|!=)", ("==",), lambda eq: eq),
	("(==|!=)", ("!=",), lambda neq: neq),
	
	# !rel: (!rel|prim) (<=|>=|<|>) (!add|prim) | !add
	("!rel", ("(!rel|prim)", "(<=|>=|<|>)", "(!add|prim)"), lambda left, op, right: NodeBinaryOp(op, left, right)),
	("!rel", ("!add",), lambda x: x),
	("(!rel|prim)", ("!rel",), lambda x: x),
	("(!rel|prim)", ("prim",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda gte: gte),
	("(<=|>=|<|>)", ("<=",), lambda lse: lse),
	("(<=|>=|<|>)", (">",), lambda gt: gt),
	("(<=|>=|<|>)", ("<",), lambda ls: ls),
	
	# !add: (!add|prim) (+|-) (!mul|prim) | !mul
	("!add", ("(!add|prim)", "(+|-)", "(!mul|prim)"), lambda left, op, right: NodeBinaryOp(op, left, right)),
	("!add", ("!mul",), lambda x: x),
	("(!add|prim)", ("!add",), lambda x: x),
	("(!add|prim)", ("prim",), lambda x: x),
	("(+|-)", ("+",), lambda plus: plus),
	("(+|-)", ("-",), lambda minus: minus),
	
	# !mul: (!mul|prim) (*|/|%) (!unary|prim) | !unary
	("!mul", ("(!mul|prim)", "(*|/|%)", "(!unary|prim)"), lambda left, op, right: NodeBinaryOp(op, left, right)),
	("!mul", ("!unary",), lambda x: x),
	("(!mul|prim)", ("!mul",), lambda x: x),
	("(!mul|prim)", ("prim",), lambda x: x),
	("(*|/|%)", ("*",), lambda mul: mul),
	("(*|/|%)", ("/",), lambda div: div),
	("(*|/|%)", ("%",), lambda mod: mod),
	
	# !unary: (+|-|!|~) (!unary|prim) | !app
	("!unary", ("(+|-|!|~)", "(!unary|prim)"), lambda op, unary: NodeUnaryOp(op, unary)),
	("!unary", ("!app",), lambda x: x),
	("(!unary|prim)", ("!unary",), lambda x: x),
	("(!unary|prim)", ("prim",), lambda x: x),
	("(+|-|!|~)", ("+",), lambda plus: plus),
	("(+|-|!|~)", ("-",), lambda minus: minus),
	("(+|-|!|~)", ("!",), lambda not_: not_),
	("(+|-|!|~)", ("~",), lambda bit_not: bit_not),
	
	# !app: (!app|prim) (!prim|prim) | !prim
	("!app", ("(!app|prim)", "(!prim|prim)"), NodeApply),
	("!app", ("!prim",), lambda x: x),
	("(!app|prim)", ("!app",), lambda x: x),
	("(!app|prim)", ("prim",), lambda x: x),
	("(!prim|prim)", ("!prim",), lambda x: x),
	("(!prim|prim)", ("prim",), lambda x: x),
	
	# prim: INT | ID | ( annot ) | tup
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("(", "annot", ")"), lambda lpr, x, rpr: x),
	("prim", ("tup",), lambda x: x),
	
	# !prim: ( !annot ) | !tup | cmpd_stmt
	("!prim", ("(", "!annot", ")"), lambda lpr, expr, rpr: expr),
	("!prim", ("!tup",), lambda x: x),
	("!prim", ("cmpd_stmt",), lambda cmpd: cmpd),
	# TODO: !cmpd_stmt
	
	# label: prefix? ID suffix?
	("label", ("prefix?", "ID", "suffix?"), lambda prefix, label, suffix: NodeLabel(label).Annotate(*prefix, *suffix)),
	
	# tup: ( ) | ( annot , ) | ( (annot ,)+ annot ,? )
	("tup", ("(", ")",), lambda lpr, rpr: NodeTuple()),
	("tup", ("(", "annot", ",", ")"), lambda lpr, x, rpr: NodeTuple(x)),
	("tup", ("(", "(annot ,)+", "annot", ",?", ")"), lambda lpr, tup, x, comma, rpr: tup.Append(x)),
	("(annot ,)+", ("(annot ,)+", "annot", ","), lambda tup, x, comma: tup.Append(x)),
	("(annot ,)+", ("annot", ","), lambda x, comma: NodeTuple(x)),
	(",?", (",",), lambda comma: comma),
	(",?", (), lambda: None),
	
	# !tup: ( !annot , ) | ( !annot, (, (!annot|annot))+ ,? ) | ( (annot ,)+ !annot (, (!annot|annot))* ,? )
	("!tup", ("(", "!annot", ",", ")"), lambda lpr, x, comma, rpr: NodeTuple(x)),
	("!tup", ("(", "!annot", "(, (!annot|annot))+", ",?", ")"), lambda lpr, x, tup, comma, rpr: NodeTuple(x).Merge(tup)),
	("!tup", ("(", "(annot ,)+", "!annot" ,"(, (!annot|annot))*", ",?", ")"), lambda lpr, tup, x, cont, comma, rpr: tup.Append(x).Merge(cont)),
	("(, (!annot|annot))*", ("(, (!annot|annot))+",), lambda x: x),
	("(, (!annot|annot))*", (), lambda: NodeTuple()),
	("(, (!annot|annot))+", ("(, (!annot|annot))+", ",", "(!annot|annot)"), lambda tup, comma, x: tup.Append(x)),
	("(, (!annot|annot))+", (",", "(!annot|annot)"), lambda comma, x: x),
	("(!annot|annot)", ("!annot",), lambda x: x),
	("(!annot|annot)", ("annot",), lambda x: x),
]
SYNTAX_RULES = [Production(*p) for p in SYNTAX_RULES]
