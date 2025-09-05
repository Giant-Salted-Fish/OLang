from typing import Self, Any
from scaner import Token
from interpreter import EvaluationContext, ControlState


class Node:
	prefix = ()
	suffix = ()
	
	def AppendPrefix(self, *attr: "Node") -> Self:
		self.prefix = self.prefix + attr
		return self
	
	def AppendSuffix(self, *attr: "Node") -> Self:
		self.suffix = self.suffix + attr
		return self
	
	def Eval(self, ctx: EvaluationContext) -> tuple[Any, ControlState]:
		raise NotImplementedError
	
	def Decl(self, ctx: EvaluationContext) -> None:
		raise NotImplementedError
	
	def Unwind(self, val: Any, ctx: EvaluationContext) -> None:
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
		def gen_attr_text(prefix: str, attr: list[list[str]]):
			assert all(len(lines) > 0 for lines in attr)
			if attr and all(len(lines) == 1 for lines in attr):
				return [" ".join(f"{prefix}{lines[0]}" for lines in attr)]
			else:
				return [line for lines in attr for line in self._PrefixText(prefix, lines)]
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
	def _PrefixText(pre: str, text: list[str]) -> list[str]:
		if len(text) == 0:
			return [pre]
		else:
			return [f"{pre}{text[0]}", *text[1:]]
	
	@staticmethod
	def _SuffixText(suf: str, text: list[str]) -> list[str]:
		if len(text) == 0:
			return [suf]
		else:
			return [*text[:-1], f"{text[-1]}{suf}"]
	
	@staticmethod
	def _EncloseText(left: str, right: str, text: list[str], force_new_line=False) -> list[str]:
		def block():
			return [left, *(f"\t{line}" for line in text), right]
		
		if force_new_line:
			return block()
		
		match len(text):
			case 0:
				return [left + right]
			case 1:
				return [left + text[0] + right]
			case _:
				return block()
	
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
		return int(self.token.GetValue()), ControlState.PASS
	
	def Decl(self, ctx):
		pass
	
	def Unwind(self, val, ctx):
		assert isinstance(val, int) and val == int(self.token.GetValue())
	
	def GenCode(self):
		return self._AppendAttrText([self.token.GetValue()])


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetValue()))
	
	def Eval(self, ctx):
		return ctx.Lookup(self.token.GetValue())[0], ControlState.PASS
	
	def Decl(self, ctx):
		ctx.Push(self.token.GetValue(), None, lambda _: None)
	
	def Unwind(self, val, ctx):
		ctx.Update(self.token.GetValue(), val, lambda _: None)
	
	def GenCode(self):
		return self._AppendAttrText([self.token.GetValue()])


class NodeStr(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetValue()))
	
	def Eval(self, ctx):
		return self.token.GetValue(), ControlState.PASS
	
	def Unwind(self, val, ctx):
		assert isinstance(val, str) and val == self.token.GetValue()
	
	def GenCode(self):
		return self._AppendAttrText([self.token.GetValue()])


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Eval(self, ctx):
		val, ctrl = (), ControlState.PASS
		for node in self.nodes:
			val, ctrl = node.Eval(ctx)
			if ctrl is not ControlState.PASS:
				break
		return val, ctrl
	
	def GenCode(self):
		lines = [line for node in self.nodes for line in self._SuffixText(";", node.GenCode())]
		lines = self._EncloseText("{", "}", lines, force_new_line=True)
		return self._AppendAttrText(lines)


class NodeDecl(Node):
	def __init__(self, var: Node):
		self.var = var
	
	def __repr__(self):
		return self._GenStr(f"{self.var}")
	
	def Eval(self, ctx):
		self.var.Decl(ctx)
		return None, ControlState.PASS
	
	def Unwind(self, val, ctx):
		self.var.Decl(ctx)
		self.var.Unwind(val, ctx)
	
	def GenCode(self):
		lines = self.var.GenCode()
		lines = self._PrefixText("let ", lines)
		return self._AppendAttrText(lines)


class NodeAssign(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.var}, {self.expr}")
	
	def Eval(self, ctx):
		val, ctrl = self.expr.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		self.var.Unwind(val, ctx)
		return val, ControlState.PASS
	
	def GenCode(self):
		lines = self._JoinText(" = ", self.var.GenCode(), self.expr.GenCode())
		return self._AppendAttrText(lines)


class NodeFunc(Node):
	def __init__(self, param: Node, body: Node):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Eval(self, ctx):
		return self, ControlState.PASS
	
	def Invoke(self, arg, ctx):
		scope = ctx.PushScope()
		self.param.Decl(scope)
		self.param.Unwind(arg, scope)
		ret_val = self.body.Eval(scope)
		scope.PopScope()
		return ret_val
	
	def GenCode(self):
		lines = self._JoinText(" -> ", self.param.GenCode(), self.body.GenCode())
		lines[0] = f"fn {lines[0]}"
		return self._AppendAttrText(lines)


class NodeTmplt(Node):
	def __init__(self, param: Node, body: Node):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Eval(self, ctx):
		return self, ControlState.PASS
	
	def Invoke(self, arg, ctx):
		scope = ctx.PushScope()
		self.param.Decl(scope)
		self.param.Unwind(arg, scope)
		ret_val = self.body.Eval(scope)
		scope.PopScope()
		return ret_val
	
	def GenCode(self):
		lines = self._JoinText(" #> ", self.param.GenCode(), self.body.GenCode())
		lines[0] = f"template {lines[0]}"
		return self._AppendAttrText(lines)


class NodeApply(Node):
	def __init__(self, func: Node, arg: Node):
		self.func = func
		self.arg = arg
	
	def __repr__(self):
		return self._GenStr(f"{self.func}, {self.arg}")
	
	def Eval(self, ctx):
		func, ctrl = self.func.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return func, ctrl
		
		arg, ctrl = self.arg.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return arg, ctrl
		
		return func.Invoke(arg, ctx), ControlState.PASS
	
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
			lines = ["|".join(lines[0] for lines in elements)]
			if len(elements) == 1:
				lines[0] += "|"
		else:
			lines = [f"\t{line}" for lines in elements for line in self._SuffixText("|", lines)]
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(lines)


class NodeTuple(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Eval(self, ctx):
		nodes = []
		for node in self.nodes:
			val, ctrl = node.Eval(ctx)
			if ctrl is not ControlState.PASS:
				return val, ctrl
			nodes.append(val)
		
		return tuple(nodes), ControlState.PASS
	
	def Decl(self, ctx):
		for node in self.nodes:
			node.Decl(ctx)
	
	def Unwind(self, val, ctx):
		if not isinstance(val, tuple):
			raise ValueError(f"Expected tuple, got {val}")
		
		assert len(val) == len(self.nodes)
		for i, node in enumerate(self.nodes):
			node.Unwind(val[i], ctx)
	
	def GenCode(self):
		elements = [node.GenCode() for node in self.nodes]
		if all(len(lines) == 1 for lines in elements):
			lines = [", ".join(lines[0] for lines in elements)]
			if len(elements) == 1:
				lines[0] += ","
		else:
			lines = [f"\t{line}" for lines in elements for line in self._SuffixText(",", lines)]
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(lines)


class NodeStruct(Node):
	def __init__(self, *fields: Node):
		self.fields = fields
	
	def __repr__(self):
		return self._GenStr(repr(self.fields)[1:-2])
	
	def Eval(self, ctx):
		# FIXME
		return super().Eval(ctx)
	
	def GenCode(self):
		elements = [field.GenCode() for field in self.fields]
		if all(len(lines) == 1 for lines in elements):
			lines = ["; ".join(lines[0] for lines in elements)]
		else:
			lines = [f"\t{line}" for lines in elements for line in self._SuffixText(";", lines)]
		lines = self._EncloseText(".{", "}", lines)
		return self._AppendAttrText(lines)


class NodeBinaryOp(Node):
	def __init__(self, op: Token, left: Node, right: Node):
		self.op = op
		self.left = left
		self.right = right
	
	def __repr__(self):
		return self._GenStr(f"{self.op.GetValue()}, {self.left}, {self.right}")
	
	def Eval(self, ctx):
		x, ctrl = self.left.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return x, ctrl
		
		y, ctrl = self.right.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return y, ctrl
		
		return self._Execute(x, y), ControlState.PASS
	
	def _Execute(self, x, y):
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
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(lines)


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	def __repr__(self):
		return self._GenStr(f"{self.op}, {self.node}")
	
	def Eval(self, ctx):
		val, ctrl = self.node.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		return self._Execute(val), ControlState.PASS
	
	def _Execute(self, val):
		match self.op.GetValue():
			case "+":
				return val
			case "-":
				return -val
			case "!":
				return not val
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
		obj, ctrl = self.obj.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		assert isinstance(self.field, NodeLabel)
		return getattr(obj, self.field.token.GetValue())
	
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
		obj, ctrl = self.obj.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		idx, ctrl = self.index.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return idx, ctrl
		
		return obj[idx], ControlState.PASS
	
	def GenCode(self):
		val = self.obj.GenCode()
		index = self.index.GenCode()
		lines = self._JoinText("", val, self._EncloseText("[", "]", index))
		return self._AppendAttrText(lines)


class NodeReturn(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	def Eval(self, ctx):
		val, ctrl = self.expr.Eval(ctx)
		return val, ControlState.RETURN if ctrl is ControlState.PASS else ctrl
	
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
		cond, ctrl = self.cond.Eval(ctx)
		assert ctrl is ControlState.PASS
		
		if cond:
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
		while True:
			cond, ctrl = self.cond.Eval(ctx)
			assert ctrl is ControlState.PASS
			
			if not cond:
				return self.else_branch.Eval(ctx)
			
			val, ctrl = self.loop_body.Eval(ctx)
			if ctrl not in (ControlState.PASS, ControlState.CONT_LOOP):
				return val, ctrl
	
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
		# FIXME
		return super().Eval(ctx)
	
	def GenCode(self):
		iterable = self.iterable.GenCode()
		var = self.var.GenCode()
		loop_body = self.loop_body.GenCode()
		else_branch = self.else_branch.GenCode()
		lines = self._JoinText(" ", ["for"], iterable, var, loop_body, ["else"], else_branch)
		return self._AppendAttrText(lines)


class NodeNamedTuple(Node):
	def __init__(self, body: Node):
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	def Eval(self, ctx):
		return self, ControlState.PASS
	
	def Invoke(self, arg, ctx):
		return arg, ControlState.PASS
	
	def GenCode(self):
		lines = self.body.GenCode()
		lines = self._PrefixText("tuple ", lines)
		return self._AppendAttrText(lines)


class NodeNamedStruct(Node):
	def __init__(self, body: Node):
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	def Eval(self, ctx):
		return self, ControlState.PASS
	
	def Invoke(self, arg, ctx):
		return arg, ControlState.PASS
	
	def GenCode(self):
		lines = self.body.GenCode()
		lines = self._PrefixText("struct ", lines)
		return self._AppendAttrText(lines)
