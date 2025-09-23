from typing import Self, Any
from scanner import Token
from interpreter import EvaluationContext, ControlState


class Node:
	prefix: tuple["Node", ...] = ()
	suffix: tuple["Node", ...] = ()
	
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		raise NotImplementedError
	
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
	
	def _GenStr(self, fields: str) -> str:
		pre = f", prefix={';'.join(str(attr) for attr in self.prefix)}" if self.prefix else ""
		suf = f", suffix={';'.join(str(attr) for attr in self.suffix)}" if self.suffix else ""
		return f"{self.__class__.__name__}({fields}{pre}{suf})"


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(self.token.GetValue())
	
	def Accept(self, visitor):
		return visitor.VisitInt(self)
	
	def Eval(self, ctx):
		return int(self.token.GetValue()), ControlState.PASS
	
	def Decl(self, ctx):
		pass
	
	def Unwind(self, val, ctx):
		assert isinstance(val, int) and val == int(self.token.GetValue())


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetValue()))
	
	def Accept(self, visitor):
		return visitor.VisitLabel(self)
	
	def Eval(self, ctx):
		val = ctx.Resolve(self.token.GetValue())
		assert val is not None
		return val, ControlState.PASS
	
	def Decl(self, ctx):
		ctx.Push(self.token.GetValue(), None)
	
	def Unwind(self, val, ctx):
		ctx.Update(self.token.GetValue(), val)


class NodeStr(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetValue()))
	
	def Accept(self, visitor):
		return visitor.VisitStr(self)
	
	def Eval(self, ctx):
		return self.token.GetValue(), ControlState.PASS
	
	def Unwind(self, val, ctx):
		assert isinstance(val, str) and val == self.token.GetValue()


class NodeBool(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetValue()))
	
	def Accept(self, visitor):
		return visitor.VisitBool(self)
	
	def Eval(self, ctx):
		return self._GetVal(), ControlState.PASS
	
	def _GetVal(self):
		return self.token.GetValue().lower() == "true"
	
	def Unwind(self, val, ctx):
		assert isinstance(val, bool) and val == self._GetVal()


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitCompound(self)
	
	def Eval(self, ctx):
		scope = EvaluationContext.Nest(ctx)
		result = self.RawEval(scope)
		return result
	
	def Decl(self, ctx):
		assert len(self.nodes) == 1, "Declare compound node with more than one statement"
		self.nodes[0].Decl(ctx)
	
	def Unwind(self, val, ctx):
		assert len(self.nodes) == 1, "Unwind compound node with more than one statement"
		self.nodes[0].Unwind(val, ctx)
	
	def RawEval(self, ctx: EvaluationContext) -> tuple[Any, ControlState]:
		val, ctrl = (), ControlState.PASS
		for node in self.nodes:
			val, ctrl = node.Eval(ctx)
			if ctrl is not ControlState.PASS:
				break
		return val, ctrl


class NodeDecl(Node):
	def __init__(self, var: Node):
		self.var = var
	
	def __repr__(self):
		return self._GenStr(f"{self.var}")
	
	def Accept(self, visitor):
		return visitor.VisitDecl(self)
	
	def Eval(self, ctx):
		self.var.Decl(ctx)
		return None, ControlState.PASS
	
	def Unwind(self, val, ctx):
		self.var.Decl(ctx)
		self.var.Unwind(val, ctx)


class NodeAssign(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.var}, {self.expr}")
	
	def Accept(self, visitor):
		return visitor.VisitAssign(self)
	
	def Eval(self, ctx):
		val, ctrl = self.expr.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		self.var.Unwind(val, ctx)
		return val, ControlState.PASS
	
	def Decl(self, ctx):
		"""For struct unwind."""
		self.expr.Decl(ctx)
	
	def Unwind(self, val, ctx):
		"""For struct unwind."""
		assert isinstance(self.var, NodeDecl)
		scope = EvaluationContext(None, [], val)
		data, ctrl = self.var.var.Eval(scope)
		assert ctrl is ControlState.PASS
		self.expr.Unwind(data, ctx)


class NodeFunc(Node):
	def __init__(self, param: Node, body: NodeCompound):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitFunc(self)
	
	def Eval(self, ctx):
		def func(arg):
			scope = EvaluationContext.Nest(ctx)
			self.param.Decl(scope)
			self.param.Unwind(arg, scope)
			val, ctrl = self.body.RawEval(scope)
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return func, ControlState.PASS


class NodeTemplate(Node):
	def __init__(self, param: Node, body: NodeCompound):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitTemplate(self)
	
	def Eval(self, ctx):
		def tmplt(arg):
			scope = EvaluationContext.Nest(ctx)
			self.param.Decl(scope)
			self.param.Unwind(arg, scope)
			val, ctrl = self.body.RawEval(scope)
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return tmplt, ControlState.PASS


class NodeApply(Node):
	def __init__(self, func: Node, arg: Node):
		self.func = func
		self.arg = arg
	
	def __repr__(self):
		return self._GenStr(f"{self.func}, {self.arg}")
	
	def Accept(self, visitor):
		return visitor.VisitApply(self)
	
	def Eval(self, ctx):
		func, ctrl = self.func.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return func, ctrl
		
		arg, ctrl = self.arg.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return arg, ctrl
		
		return func(arg)


class NodeUnion(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitUnion(self)
	
	def Eval(self, ctx):
		raise NotImplementedError("Try to evaluate union expression")


class NodeTuple(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitTuple(self)
	
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
		assert isinstance(val, tuple), f"Expected tuple, got {type(val)}"
		assert len(val) == len(self.nodes)
		for i, node in enumerate(self.nodes):
			node.Unwind(val[i], ctx)


class NodeStruct(Node):
	def __init__(self, *fields: Node):
		self.fields = fields
	
	def __repr__(self):
		return self._GenStr(repr(self.fields)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitStruct(self)
	
	def Eval(self, ctx):
		scope = EvaluationContext.Nest(ctx)
		for node in self.fields:
			val, ctrl = node.Eval(scope)
			if ctrl is not ControlState.PASS:
				return val, ctrl
		return scope.GetLocals(), ControlState.PASS
	
	def Decl(self, ctx):
		for field in self.fields:
			assert isinstance(field, NodeAssign)
			field.Decl(ctx)
	
	def Unwind(self, val, ctx):
		assert isinstance(val, dict), f"Expect dict (struct), got {type(val)}"
		for field in self.fields:
			assert isinstance(field, NodeAssign)
			field.Unwind(val, ctx)


class NodeBinaryOp(Node):
	def __init__(self, op: Token, left: Node, right: Node):
		self.op = op
		self.left = left
		self.right = right
	
	def __repr__(self):
		return self._GenStr(f"{self.op.GetValue()}, {self.left}, {self.right}")
	
	def Accept(self, visitor):
		return visitor.VisitBinaryOp(self)
	
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


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	def __repr__(self):
		return self._GenStr(f"{self.op}, {self.node}")
	
	def Accept(self, visitor):
		return visitor.VisitUnaryOp(self)
	
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


class NodeAccess(Node):
	def __init__(self, obj: Node, field: Node):
		self.obj = obj
		self.field = field
	
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.field}")
	
	def Accept(self, visitor):
		return visitor.VisitAccess(self)
	
	def Eval(self, ctx):
		obj, ctrl = self.obj.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		assert isinstance(self.field, NodeLabel)
		return getattr(obj, self.field.token.GetValue())


class NodeIndex(Node):
	def __init__(self, obj: Node, index: Node):
		self.obj = obj
		self.index = index
	
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.index}")
	
	def Accept(self, visitor):
		return visitor.VisitIndex(self)
	
	def Eval(self, ctx):
		obj, ctrl = self.obj.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		idx, ctrl = self.index.Eval(ctx)
		if ctrl is not ControlState.PASS:
			return idx, ctrl
		
		return obj[idx], ControlState.PASS


class NodeReturn(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	def Accept(self, visitor):
		return visitor.VisitReturn(self)
	
	def Eval(self, ctx):
		val, ctrl = self.expr.Eval(ctx)
		return val, ControlState.RETURN if ctrl is ControlState.PASS else ctrl


class NodeBreak(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	def Accept(self, visitor):
		return visitor.VisitBreak(self)
	
	def Eval(self, ctx):
		val, ctrl = self.expr.Eval(ctx)
		return val, ControlState.BREAK_LOOP if ctrl is ControlState.PASS else ctrl


class NodeContinue(Node):
	def __repr__(self):
		return self._GenStr("")
	
	def Accept(self, visitor):
		return visitor.VisitContinue(self)
	
	def Eval(self, ctx):
		return None, ControlState.CONT_LOOP


class NodeIfElse(Node):
	def __init__(self, cond: Node, true_branch: NodeCompound, false_branch: NodeCompound):
		self.cond = cond
		self.true_branch = true_branch
		self.false_branch = false_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.true_branch}, {self.false_branch}")
	
	def Accept(self, visitor):
		return visitor.VisitIfElse(self)
	
	def Eval(self, ctx):
		scope = EvaluationContext.Nest(ctx)
		cond, ctrl = self.cond.Eval(scope)
		assert ctrl is ControlState.PASS
		return self.true_branch.Eval(scope) if cond else self.false_branch.Eval(ctx)


class NodeWhileElse(Node):
	def __init__(self, cond: Node, loop_body: NodeCompound, else_branch: NodeCompound):
		self.cond = cond
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.loop_body}, {self.else_branch}")
	
	def Accept(self, visitor):
		return visitor.VisitWhileElse(self)
	
	def Eval(self, ctx):
		scope = EvaluationContext.Nest(ctx)
		while True:
			cond, ctrl = self.cond.Eval(scope)
			assert ctrl is ControlState.PASS
			
			if not cond:
				return self.else_branch.Eval(ctx)
			
			val, ctrl = self.loop_body.Eval(scope)
			match ctrl:
				case ControlState.RETURN:
					return val, ControlState.RETURN
				case ControlState.BREAK_LOOP:
					return val, ControlState.PASS
				case ControlState.PASS | ControlState.CONT_LOOP:
					pass


class NodeForElse(Node):
	def __init__(self, iterable: Node, var: Node, loop_body: NodeCompound, else_branch: NodeCompound):
		self.iterable = iterable
		self.var = var
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.iterable}, {self.var}, {self.loop_body}, {self.else_branch}")
	
	def Accept(self, visitor):
		return visitor.VisitForElse(self)
	
	def Eval(self, ctx):
		# FIXME
		return super().Eval(ctx)


class NodeNamedTuple(Node):
	def __init__(self, body: Node):
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitNamedTuple(self)
	
	def Eval(self, ctx):
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS


class NodeNamedStruct(Node):
	def __init__(self, body: Node):
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitNamedStruct(self)
	
	def Eval(self, ctx):
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS


class Visitor[T]:
	def VisitInt(self, node: NodeInt) -> T:
		raise NotImplementedError
	
	def VisitLabel(self, node: NodeLabel) -> T:
		raise NotImplementedError
	
	def VisitStr(self, node: NodeStr) -> T:
		raise NotImplementedError
	
	def VisitBool(self, node: NodeBool) -> T:
		raise NotImplementedError
	
	def VisitCompound(self, node: NodeCompound) -> T:
		raise NotImplementedError
	
	def VisitDecl(self, node: NodeDecl) -> T:
		raise NotImplementedError
	
	def VisitAssign(self, node: NodeAssign) -> T:
		raise NotImplementedError
	
	def VisitFunc(self, node: NodeFunc) -> T:
		raise NotImplementedError
	
	def VisitTemplate(self, node: NodeTemplate) -> T:
		raise NotImplementedError
	
	def VisitApply(self, node: NodeApply) -> T:
		raise NotImplementedError
	
	def VisitUnion(self, node: NodeUnion) -> T:
		raise NotImplementedError
	
	def VisitTuple(self, node: NodeTuple) -> T:
		raise NotImplementedError
	
	def VisitStruct(self, node: NodeStruct) -> T:
		raise NotImplementedError
	
	def VisitBinaryOp(self, node: NodeBinaryOp) -> T:
		raise NotImplementedError
	
	def VisitUnaryOp(self, node: NodeUnaryOp) -> T:
		raise NotImplementedError
	
	def VisitAccess(self, node: NodeAccess) -> T:
		raise NotImplementedError
	
	def VisitIndex(self, node: NodeIndex) -> T:
		raise NotImplementedError
	
	def VisitReturn(self, node: NodeReturn) -> T:
		raise NotImplementedError
	
	def VisitBreak(self, node: NodeBreak) -> T:
		raise NotImplementedError
	
	def VisitContinue(self, node: NodeContinue) -> T:
		raise NotImplementedError
	
	def VisitIfElse(self, node: NodeIfElse) -> T:
		raise NotImplementedError
	
	def VisitWhileElse(self, node: NodeWhileElse) -> T:
		raise NotImplementedError
	
	def VisitForElse(self, node: NodeForElse) -> T:
		raise NotImplementedError
	
	def VisitNamedTuple(self, node: NodeNamedTuple) -> T:
		raise NotImplementedError
	
	def VisitNamedStruct(self, node: NodeNamedStruct) -> T:
		raise NotImplementedError
