from scaner import Token
from parser import Production
from interpreter import EvaluationContext


class Node:
	def Eval(self, ctx: EvaluationContext) -> any:
		raise NotImplementedError


class NodeUnary(Node):
	def __init__(self, op: Token, operand: Node):
		self.op = op
		self.operand = operand
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.op.GetValue()}, {self.operand})"
	
	def Eval(self, ctx):
		if self.op == "+":
			return +self.operand.Eval(ctx)
		elif self.op == "-":
			return -self.operand.Eval(ctx)
		assert False


class NodeBinary(Node):
	def __init__(self, op: Token, left: Node, right: Node):
		self.op = op
		self.left = left
		self.right = right
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.op.GetValue()}, {self.left}, {self.right})"
	
	def Eval(self, ctx):
		match self.op.GetValue():
			case "+":
				return self.left.Eval(ctx) + self.right.Eval(ctx)
			case "-":
				return self.left.Eval(ctx) - self.right.Eval(ctx)
			case "*":
				return self.left.Eval(ctx) * self.right.Eval(ctx)
			case "/":
				return self.left.Eval(ctx) / self.right.Eval(ctx)
			case _:
				assert False


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return int(self.token.GetValue())


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
	def __init__(self, id: Token, expr: Node):
		self.id = id
		self.expr = expr
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.id.GetValue()}, {self.expr})"
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		ctx.PushVar(self.id.GetValue(), val)
		return val


class NodeRef(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.token.GetValue()})"
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())


class NodeDummy(Node):
	def __init__(self):
		pass
	
	def __repr__(self):
		return f"{self.__class__.__name__}()"
	
	def Eval(self, ctx):
		return None


class NodeCallable(Node):
	def __init__(self, arg_lst: tuple[Token, ...], body: NodeList):
		self.arg_lst = arg_lst
		self.body = body
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.arg_lst}, {self.body})"
	
	def Eval(self, ctx):
		ctx.PushVar(self.id.GetValue(), self)
		return None


class NodeApply(Node):
	def __init__(self, id: Token, arg_lst: tuple[Node]):
		self.id = id
		self.arg_lst = arg_lst
	
	def __repr__(self):
		return f"{self.__class__.__name__}({self.id.GetValue()}, {self.arg_lst})"
	
	def Eval(self, ctx):
		func = ctx.Lookup(self.id.GetValue())
		if not isinstance(func, NodeCallable):
			raise ValueError(f"{self.id.GetValue()} is not a function")
		
		if len(func.arg_lst) != len(self.arg_lst):
			raise ValueError(f"Function {func.id.GetValue()} expects {len(func.arg_lst)} arguments, got {len(self.arg_lst)}")
		
		ctx.PushScope()
		for arg, param in zip(self.arg_lst, func.arg_lst):
			ctx.PushVar(param.GetValue(), arg.Eval(ctx))
		
		return func.body.Eval(ctx)

token_types = [
	("INT", r"[1-9]\d*"),
	("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	("RETURN", r"return"),
	("DEF", r"def"),
	("LET", r"let"),
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
	("=", r"="),
	("{", r"\{"),
	("}", r"\}"),
	("[", r"\["),
	("]", r"\]"),
	("(", r"\("),
	(")", r"\)"),
	(",", r","),
	(";", r";"),
	("COMMENT", r"//.*"),
]

terminals = set(t for t, _ in token_types)

syntax_rules = [
	("S", ("stmt_lst",), lambda x: x),
	# fun_decl -> def id arg body
	("fun_decl", ("DEF", "ID", "tup_def", "body"), lambda _def, _id, arg, body: NodeDecl(_id, NodeCallable(arg, body))),
	("fun_def", ("tup_def", "->", "comp_stmt"), lambda tup, op, body: NodeCallable(tup, body)),
	("tup_def", ("(", "fld_lst", ")"), lambda lpr, lst, rpr: None),
	# fld_lst -> fld_lst' | ε
	("fld_lst", ("fld_lst'",), lambda lst: lst),
	("fld_lst", (), lambda: NodeList(())),
	# fld_lst' -> fld_lst' , id | id
	("fld_lst'", ("fld_lst'", ",", "ID"), lambda lst, comma, _id: (*lst, _id)),
	("fld_lst'", ("ID",), lambda _id: _id),
	# comp_stmt -> { stmt_lst }
	("comp_stmt", ("{", "stmt_lst", "}"), lambda lpr, stmt_lst, rpr: stmt_lst),
	# stmt_lst -> stmt_lst' | ε
	("stmt_lst", ("stmt_lst'",), lambda lst: lst),
	("stmt_lst", (), lambda: NodeList(())),
	# stmt_lst' -> stmt_lst' stmt | stmt
	("stmt_lst'", ("stmt_lst'", "stmt"), lambda lst, s: NodeList(lst.node_lst + (s,))),
	("stmt_lst'", ("stmt",), lambda stmt: NodeList((stmt,))),
	# stmt -> expr ; | decl ; | fun_decl ; | RETURN expr ; | ;
	("stmt", ("expr", ";"), lambda expr, end: expr),
	("stmt", ("decl", ";"), lambda decl, end: decl),
	("stmt", ("fun_decl", ";"), lambda decl: decl),
	("stmt", ("asgn", ";"), lambda asgn, end: asgn),
	("stmt", ("RETURN", "expr", ";"), lambda ret, expr, end: expr),
	# (Production("stmt", (";",)), lambda end: NodeDummy()),
	# decl -> let id = expr
	("decl", ("LET", "ID", "=", "expr"), lambda let, _id, equal, expr: NodeDecl(_id, expr)),
	# asgn -> id = expr
	("asgn", ("ID", "=", "expr"), lambda _id, equal, expr: NodeAssign(_id, expr)),
	# expr -> expr + term | expr - term | term
	("expr", ("expr", "op-add", "term"), lambda left, op, right: NodeBinary(op, left, right)),
	("expr", ("term",), lambda x: x),
	# term -> term * factor | term / factor | factor
	("term", ("term", "op-mul", "factor"), lambda left, op, right: NodeBinary(op, left, right)),
	("term", ("factor",), lambda x: x),
	# factor -> ( expr ) | id | int
	("factor", ("(", "expr", ")"), lambda lpr, expr, rpr: expr),
	("factor", ("ID",), NodeRef),
	("factor", ("INT",), NodeInt),
	
	("op-add", ("+",), lambda plus: plus),
	("op-add", ("-",), lambda minus: minus),
	("op-mul", ("*",), lambda mul: mul),
	("op-mul", ("/",), lambda div: div),
	("op_cmp", ("==",), lambda eq: eq),
	("op-cmp", ("!=",), lambda neq: neq),
	("op-cmp", (">=",), lambda gte: gte),
	("op-cmp", ("<=",), lambda lse: lse),
	("op-cmp", (">",), lambda gt: gt),
	("op-cmp", ("<",), lambda ls: ls),
]
syntax_rules = [Production(*p) for p in syntax_rules]
