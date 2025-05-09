from typing import Self, Sequence, Callable, TypeVar
from scaner import Token
from interpreter import EvaluationContext


T = TypeVar("T")


class Node:
	annotations = ()
	
	def Annotate(self, *annotations: "Node") -> Self:
		self.annotations = self.annotations + annotations
		return self
	
	def Eval(self, ctx: EvaluationContext) -> any:
		raise NotImplementedError
	
	def Unwind(self, val: T, put: Callable[[str, T, Callable[[T], None]], None], ctx: EvaluationContext) -> None:
		raise NotImplementedError
	
	def Invoke(self, arg, ctx: EvaluationContext) -> any:
		raise NotImplementedError
	
	def GenStr(self, fields: str) -> str:
		attrs = f", annotations={''.join(f"{attr};" for attr in self.annotations)}" if self.annotations else ""
		return f"{self.__class__.__name__}({fields}{attrs})"
	
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
		return self.GenStr(self.token.GetValue())
	
	def Eval(self, ctx):
		return int(self.token.GetValue())
	
	def Unwind(self, val, put, ctx):
		pass
	
	def GenText(self):
		return self.AppendAnnotText([self.token.GetValue()])


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self.GenStr(repr(self.token.GetValue()))
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())[0]
	
	def Unwind(self, val, put, ctx):
		put(self.token.GetValue(), val, lambda _: None)
	
	def GenText(self):
		return self.AppendAnnotText([self.token.GetValue()])


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self.GenStr(repr(self.nodes)[1:-2])
	
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
		return self.GenStr(f"{self.var}, {self.expr}")
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		self.var.Unwind(val, ctx.Push, ctx)
		return ()
	
	def GenText(self):
		lines = self.JoinText(" = ", self.var.GenText(), self.expr.GenText())
		lines = [f"let {lines[0]}", *lines[1:]]
		return self.AppendAnnotText(lines)


class NodeAssign(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return self.GenStr(f"{self.var}, {self.expr}")
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		self.var.Unwind(val, ctx.Update, ctx)
		return val
	
	def GenText(self):
		lines = self.JoinText(" = ", self.var.GenText(), self.expr.GenText())
		return self.AppendAnnotText(lines)


class NodeCallable(Node):
	def __init__(self, param: Node, body: Node):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self.GenStr(f"{self.param}, {self.body}")
	
	def Eval(self, ctx):
		return self
	
	def Invoke(self, arg, ctx):
		new_ctx = ctx.PushScope()
		self.param.Unwind(arg, new_ctx.Push, new_ctx)
		ret_val = self.body.Eval(new_ctx)
		new_ctx.PopScope()
		return ret_val
	
	def GenText(self):
		lines = self.JoinText(" -> ", self.param.GenText(), self.body.GenText())
		return self.AppendAnnotText(lines)


class NodeApply(Node):
	def __init__(self, callable: Node, arg: Node):
		self.callable = callable
		self.arg = arg
	
	def __repr__(self):
		return self.GenStr(f"{self.callable}, {self.arg}")
	
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
		return self.GenStr(repr(self.nodes))
	
	def Merge(self, other: "NodeTuple") -> Self:
		self.nodes = self.nodes + other.nodes
		return self
	
	def Append(self, node: Node) -> Self:
		self.nodes = (*self.nodes, node)
		return self
	
	def Eval(self, ctx):
		return tuple(node.Eval(ctx) for node in self.nodes)
	
	def Unwind(self, val, put, ctx):
		if not isinstance(val, tuple):
			raise ValueError(f"Expected tuple, got {val}")
		
		assert len(val) == len(self.nodes)
		for i, node in enumerate(self.nodes):
			node.Unwind(val[i], put, ctx)
	
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
		return self.GenStr(f"{self.op.GetValue()}, {self.left}, {self.right}")
	
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
		lines = [f"({lines[0]}", *lines[1:]]
		lines[-1] += ")"
		return self.AppendAnnotText(lines)


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	def __repr__(self):
		return self.GenStr(f"{self.op}, {self.node}")
	
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


class NodeDeref(Node):
	def __init__(self, obj: Node, attr: Token):
		self.obj = obj
		self.attr = attr
	
	def __repr__(self):
		return self.GenStr(f"{self.obj}, {repr(self.attr.GetValue())}")
	
	def Eval(self, ctx):
		return getattr(self.obj.Eval(ctx), self.attr.GetValue())
