from typing import Self, Sequence, Callable, TypeVar, Any
from scaner import Token
from interpreter import EvaluationContext


T = TypeVar("T")


class Node:
	prefix = ()
	suffix = ()
	
	def AppendPrefix(self, *attr: "Node") -> Self:
		self.prefix = self.prefix + attr
		return self
	
	def AppendSuffix(self, *attr: "Node") -> Self:
		self.suffix = self.suffix + attr
		return self
	
	def Eval(self, ctx: EvaluationContext) -> Any:
		raise NotImplementedError
	
	def Unwind(self, val: T, put: Callable[[str, T, Callable[[T], None]], None], ctx: EvaluationContext) -> None:
		raise NotImplementedError
	
	def Invoke(self, arg, ctx: EvaluationContext) -> Any:
		raise NotImplementedError
	
	def GenCode(self) -> list[str]:
		raise NotImplementedError
	
	def _GenStr(self, fields: str) -> str:
		pre = f", prefix={';'.join(str(attr) for attr in self.prefix)}" if self.prefix else ""
		suf = f", suffix={';'.join(str(attr) for attr in self.suffix)}" if self.suffix else ""
		return f"{self.__class__.__name__}({fields}{pre}{suf})"
	
	def _AppendAttrText(self, lines: list[str]) -> list[str]:
		def gen_attr_text(prefix: str, attr: list[Sequence[str]]):
			assert all(len(lines) > 0 for lines in attr)
			if attr and all(len(lines) == 1 for lines in attr):
				return [" ".join(f"{prefix}{lines[0]}" for lines in attr)]
			else:
				return [line for lines in attr for line in (f"{prefix}{lines[0]}", *lines[1:])]
		text = gen_attr_text("@", [node.GenCode() for node in self.prefix])
		if len(text) == 1:
			text = self._JoinText(" ", text, lines)
		else:
			text += lines
		
		suffix = gen_attr_text(":", [node.GenCode() for node in self.suffix])
		if len(suffix) == 1:
			text = self._JoinText(" ", text, suffix)
		else:
			text += suffix
		return text
	
	@staticmethod
	def _JoinText(joiner: str, *text: list[str]) -> list[str]:
		while text:
			result, text = text[0], text[1:]
			if result:
				break
		else:
			result = []
		
		while text:
			lines, text = text[0], text[1:]
			if lines:
				result = [
					*result[:-1],
					f"{result[-1]}{joiner}{lines[0]}",
					*lines[1:],
				]
		return result


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(self.token.GetValue())
	
	def Eval(self, ctx):
		return int(self.token.GetValue())
	
	def Unwind(self, val, put, ctx):
		pass
	
	def GenCode(self):
		return self._AppendAttrText([self.token.GetValue()])


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetValue()))
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())[0]
	
	def Unwind(self, val, put, ctx):
		put(self.token.GetValue(), val, lambda _: None)
	
	def GenCode(self):
		return self._AppendAttrText([self.token.GetValue()])


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Eval(self, ctx):
		val = ()
		for node in self.nodes:
			val = node.Eval(ctx)
		return val
	
	def GenCode(self):
		def process(n):
			lines = n.GenCode()
			return [*lines[:-1], f"{lines[-1]};"]
		return self._AppendAttrText([
			"{",
			*(f"\t{line}" for node in self.nodes for line in process(node)),
			"}",
		])


class NodeDecl(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.var}, {self.expr}")
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		self.var.Unwind(val, ctx.Push, ctx)
		return ()
	
	def GenCode(self):
		lines = self._JoinText(" = ", self.var.GenCode(), self.expr.GenCode())
		lines = [f"let {lines[0]}", *lines[1:]]
		return self._AppendAttrText(lines)


class NodeAssign(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.var}, {self.expr}")
	
	def Eval(self, ctx):
		val = self.expr.Eval(ctx)
		self.var.Unwind(val, ctx.Update, ctx)
		return val
	
	def GenCode(self):
		lines = self._JoinText(" = ", self.var.GenCode(), self.expr.GenCode())
		return self._AppendAttrText(lines)


class NodeCallable(Node):
	def __init__(self, param: Node, body: Node):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Eval(self, ctx):
		return self
	
	def Invoke(self, arg, ctx):
		new_ctx = ctx.PushScope()
		self.param.Unwind(arg, new_ctx.Push, new_ctx)
		ret_val = self.body.Eval(new_ctx)
		new_ctx.PopScope()
		return ret_val
	
	def GenCode(self):
		lines = self._JoinText(" -> ", self.param.GenCode(), self.body.GenCode())
		lines[0] = f"fn {lines[0]}"
		return self._AppendAttrText(lines)


class NodeApply(Node):
	def __init__(self, func: Node, arg: Node):
		self.func = func
		self.arg = arg
	
	def __repr__(self):
		return self._GenStr(f"{self.func}, {self.arg}")
	
	def Eval(self, ctx):
		func = self.func.Eval(ctx)
		arg = self.arg.Eval(ctx)
		return func.Invoke(arg, ctx)
	
	def GenCode(self):
		lines = self._JoinText(" ", self.func.GenCode(), self.arg.GenCode())
		return self._AppendAttrText(lines)


class NodeUnion(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Eval(self, ctx):
		raise NotImplementedError("Try to evaluate union expression")
	
	def GenCode(self):
		elements = [node.GenCode() for node in self.nodes]
		if all(len(lines) == 1 for lines in elements):
			inner = "|".join(lines[0] for lines in elements)
			if len(elements) == 1:
				inner += "|"
			lines = [f"({inner})"]
		else:
			lines = [
				"(",
				*(f"\t{line}," for lines in elements for line in (*lines[:-1], f"{lines[-1]}|")),
				")",
			]
		return self._AppendAttrText(lines)


class NodeTuple(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Eval(self, ctx):
		return tuple(node.Eval(ctx) for node in self.nodes)
	
	def Unwind(self, val, put, ctx):
		if not isinstance(val, tuple):
			raise ValueError(f"Expected tuple, got {val}")
		
		assert len(val) == len(self.nodes)
		for i, node in enumerate(self.nodes):
			node.Unwind(val[i], put, ctx)
	
	def GenCode(self):
		elements = [node.GenCode() for node in self.nodes]
		if all(len(lines) == 1 for lines in elements):
			inner = ", ".join(lines[0] for lines in elements)
			if len(elements) == 1:
				inner += ","
			lines = [f"({inner})"]
		else:
			lines = [
				"(",
				*(f"\t{line}," for lines in elements for line in (*lines[:-1], f"{lines[-1]},")),
				")",
			]
		return self._AppendAttrText(lines)


class NodeBinaryOp(Node):
	def __init__(self, op: Token, left: Node, right: Node):
		self.op = op
		self.left = left
		self.right = right
	
	def __repr__(self):
		return self._GenStr(f"{self.op.GetValue()}, {self.left}, {self.right}")
	
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
	
	def GenCode(self):
		lines = self._JoinText(f" {self.op.GetValue()} ", self.left.GenCode(), self.right.GenCode())
		lines = [f"({lines[0]}", *lines[1:]]
		lines[-1] += ")"
		return self._AppendAttrText(lines)


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	def __repr__(self):
		return self._GenStr(f"{self.op}, {self.node}")
	
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
	
	def GenCode(self):
		val = self.node.GenCode()
		lines = [f"{self.op.GetValue()}{val[0]}", *val[1:]]
		return self._AppendAttrText(lines)


class NodeAccess(Node):
	def __init__(self, obj: Node, field: Node):
		self.obj = obj
		self.field = field
	
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.field}")
	
	def Eval(self, ctx):
		assert isinstance(self.field, NodeLabel)
		return getattr(self.obj.Eval(ctx), self.field.token.GetValue())
	
	def GenCode(self):
		obj = self.obj.GenCode()
		field = self.field.GenCode()
		lines = self._JoinText(".", obj, field)
		return self._AppendAttrText(lines)


class NodeIndex(Node):
	def __init__(self, obj: Node, index: Node):
		self.obj = obj
		self.index = index
	
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.index}")
	
	def Eval(self, ctx):
		return self.obj.Eval(ctx)[self.index.Eval(ctx)]
	
	def GenCode(self):
		val = self.obj.GenCode()
		index = self.index.GenCode()
		if len(index) == 1:
			lines = [*val[:-1], f"{val[-1]}[{index[0]}]"]
		else:
			lines = [
				*val[:-1],
				f"{val[-1]}[",
				*(f"\t{line}," for line in index),
				"]",
			]
		return self._AppendAttrText(lines)


class NodeReturn(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	def Eval(self, ctx):
		# TODO: Handle control flow transfer
		return super().Eval(ctx)
	
	def GenCode(self):
		expr = self.expr.GenCode()
		lines = self._JoinText(" ", ["return"], expr)
		return self._AppendAttrText(lines)


class NodeIfElse(Node):
	def __init__(self, cond: Node, true_branch: Node, false_branch: Node):
		self.cond = cond
		self.true_branch = true_branch
		self.false_branch = false_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.true_branch}, {self.false_branch}")
	
	def Eval(self, ctx):
		if self.cond.Eval(ctx):
			return self.true_branch.Eval(ctx)
		else:
			return self.false_branch.Eval(ctx)
	
	def GenCode(self):
		cond = self.cond.GenCode()
		true_branch = self.true_branch.GenCode()
		false_branch = self.false_branch.GenCode()
		lines = self._JoinText(" ", ["if"], cond, true_branch, ["else"], false_branch)
		return self._AppendAttrText(lines)


class NodeWhileElse(Node):
	def __init__(self, cond: Node, loop_body: Node, else_branch: Node):
		self.cond = cond
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.loop_body}, {self.else_branch}")
	
	def Eval(self, ctx):
		pass
	
	def GenCode(self):
		cond = self.cond.GenCode()
		loop_body = self.loop_body.GenCode()
		else_branch = self.else_branch.GenCode()
		lines = self._JoinText(" ", ["while"], cond, loop_body, ["else"], else_branch)
		return self._AppendAttrText(lines)


class NodeForElse(Node):
	def __init__(self, iterable: Node, var: Node, loop_body: Node, else_branch: Node):
		self.iterable = iterable
		self.var = var
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.iterable}, {self.var}, {self.loop_body}, {self.else_branch}")
	
	def Eval(self, ctx):
		pass
	
	def GenCode(self):
		iterable = self.iterable.GenCode()
		var = self.var.GenCode()
		loop_body = self.loop_body.GenCode()
		else_branch = self.else_branch.GenCode()
		lines = self._JoinText(" ", ["for"], iterable, var, loop_body, ["else"], else_branch)
		return self._AppendAttrText(lines)
