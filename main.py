from scaner import build_scaner, Token
from parser import Production, build_syntax
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


if __name__ == "__main__":
	token_types = [
		("INT", r"[1-9]\d*"),
		("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
		("RETURN", r"return"),
		("DEF", r"def"),
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
	
	scaner = build_scaner(token_types)
	
	# Production rules for the grammar
	terminals = set(t for t, _ in token_types)
	syntax = build_syntax(
		[
			(Production("S", ("stmt_lst",)), lambda x: x),
			# func_def -> id ( arg_lst ) body
			(Production("func_def", ("DEF", "ID", "(", "arg_lst", ")", "body")), lambda id, lpr, arg_lst, rpr, body: NodeCallable(id, arg_lst, body)),
			# arg_lst -> arg_lst' | ε
			(Production("arg_lst", ("arg_lst'",)), lambda lst: lst),
			(Production("arg_lst", ()), lambda: NodeList(())),
			# arg_lst' -> arg_lst' , id | id
			(Production("arg_lst'", ("arg_lst'", ",", "ID")), lambda lst, comma, id: (*lst, id)),
			(Production("arg_lst'", ("ID",)), lambda id: id),
			# compound_statement -> { stmt_lst }
			(Production("comp_stmt", ("{", "stmt_lst", "}")), lambda lpr, stmt_lst, rpr: stmt_lst),
			# stmt_lst -> stmt_lst' | ε
			(Production("stmt_lst", ("stmt_lst'",)), lambda lst: lst),
			(Production("stmt_lst", ()), lambda: NodeList(())),
			# stmt_lst' -> stmt_lst' stmt | stmt
			(Production("stmt_lst'", ("stmt_lst'", "stmt")), lambda lst, s: NodeList(lst.node_lst + (s,))),
			(Production("stmt_lst'", ("stmt",)), lambda stmt: NodeList((stmt,))),
			# stmt -> expr ; | decl ; | RETURN expr ; | ;
			(Production("stmt", ("expr", ";")), lambda expr, semicolon: expr),
			(Production("stmt", ("decl", ";")), lambda decl, semicolon: decl),
			(Production("stmt", ("RETURN", "expr", ";")), lambda func: func),
			(Production("stmt", (";",)), lambda semicolon: NodeDummy()),
			# decl -> id = expr
			(Production("decl", ("ID", "=", "expr")), lambda id, equal, expr: NodeDecl(id, expr)),
			# expr -> expr + term | expr - term | term
			(Production("expr", ("expr", "op-add", "term")), lambda left, op, right: NodeBinary(op, left, right)),
			(Production("expr", ("term",)), lambda x: x),
			# term -> term * factor | term / factor | factor
			(Production("term", ("term", "op-mul", "factor")), lambda left, op, right: NodeBinary(op, left, right)),
			(Production("term", ("factor",)), lambda x: x),
			# factor -> ( expr ) | id | int
			(Production("factor", ("(", "expr", ")")), lambda lpr, expr, rpr: expr),
			(Production("factor", ("ID",)), NodeRef),
			(Production("factor", ("INT",)), NodeInt),
			
			(Production("op-add", ("+",)), lambda plus: plus),
			(Production("op-add", ("-",)), lambda minus: minus),
			(Production("op-mul", ("*",)), lambda mul: mul),
			(Production("op-mul", ("/",)), lambda div: div),
		],
		terminals.__contains__
	)
	
	with open("main.oo", "r") as f:
		source_code = f.read()
	result = syntax.Parse("S", scaner.tokenize(source_code))
	print(result)
