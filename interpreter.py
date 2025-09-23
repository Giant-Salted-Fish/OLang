from typing import Any
from enum import Enum, auto, unique
import scanner
import lang_ast


class Context:
	def __init__(self, parent: "Context | None", local_stack: list[str], local_table: dict[str, Any]):
		self._parent = parent
		self._local_stack = local_stack
		self._local_table = local_table
	
	@classmethod
	def New(cls):
		return cls(None, [], {})
	
	@classmethod
	def Nest(cls, parent: "Context"):
		return cls(parent, [], {})
	
	def Resolve(self, symbol: str) -> Any:
		if symbol in self._local_table:
			return self._local_table[symbol]
		
		assert self._parent is not None, f"No variable named \"{symbol}\" to lookup"
		return self._parent.Resolve(symbol)
	
	def Push(self, symbol: str, value: Any):
		assert symbol not in self._local_table
		self._local_stack.append(symbol)
		self._local_table[symbol] = value
	
	def Pop(self, symbol: str) -> Any:
		self._local_stack.remove(symbol)
		return self._local_table.pop(symbol)
	
	def Update(self, symbol: str, value: Any):
		if symbol in self._local_table:
			self._local_table[symbol] = value
		else:
			assert self._parent is not None, f"No variable named \"{symbol}\" to update"
			self._parent.Update(symbol, value)
	
	def GetLocals(self):
		return self._local_table


@unique
class ControlState(Enum):
	PASS = auto()
	BREAK_LOOP = auto()
	CONT_LOOP = auto()
	RETURN = auto()


class Evaluate(lang_ast.Visitor[tuple[Any, ControlState]]):
	def __init__(self, ctx: Context):
		self.ctx = ctx
	
	def VisitInt(self, node):
		return int(node.token.GetText()), ControlState.PASS
	
	def VisitStr(self, node):
		return node.token.GetText(), ControlState.PASS
	
	def VisitBool(self, node):
		return node.token.GetText().lower() == "true", ControlState.PASS
	
	def VisitLabel(self, node):
		val = self.ctx.Resolve(node.token.GetText())
		assert val is not None
		return val, ControlState.PASS
	
	def VisitCompound(self, node):
		scope = Context.Nest(self.ctx)
		return Evaluate(scope).EvalCompound(node)
	
	def EvalCompound(self, cmpd: lang_ast.NodeCompound) -> tuple[Any, ControlState]:
		val, ctrl = (), ControlState.PASS
		for node in cmpd.nodes:
			val, ctrl = node.Accept(self)
			if ctrl is not ControlState.PASS:
				break
		return val, ctrl
	
	def VisitDecl(self, node):
		node.var.Accept(Declare(self.ctx))
		return None, ControlState.PASS
	
	def VisitAssign(self, node):
		val, ctrl = node.expr.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		node.var.Accept(Unwind(val, self.ctx))
		return val, ControlState.PASS
	
	def VisitFunc(self, node):
		def func(arg):
			scope = Context.Nest(self.ctx)
			node.param.Accept(Declare(scope))
			node.param.Accept(Unwind(arg, scope))
			val, ctrl = Evaluate(scope).EvalCompound(node.body)
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return func, ControlState.PASS
	
	def VisitTemplate(self, node):
		def tmplt(arg):
			scope = Context.Nest(self.ctx)
			node.param.Accept(Declare(scope))
			node.param.Accept(Unwind(arg, scope))
			val, ctrl = Evaluate(scope).EvalCompound(node.body)
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return tmplt, ControlState.PASS
	
	def VisitApply(self, node):
		func, ctrl = node.func.Accept(self)
		if ctrl is not ControlState.PASS:
			return func, ctrl
		
		arg, ctrl = node.arg.Accept(self)
		if ctrl is not ControlState.PASS:
			return arg, ctrl
		
		return func(arg)
	
	def VisitTuple(self, node):
		nodes = []
		for node in node.nodes:
			val, ctrl = node.Accept(self)
			if ctrl is not ControlState.PASS:
				return val, ctrl
			nodes.append(val)
		
		return tuple(nodes), ControlState.PASS
	
	def VisitStruct(self, node):
		scope = Context.Nest(self.ctx)
		ev = Evaluate(scope)
		for node in node.fields:
			val, ctrl = node.Accept(ev)
			if ctrl is not ControlState.PASS:
				return val, ctrl
		return scope.GetLocals(), ControlState.PASS
	
	def VisitBinaryOp(self, node):
		x, ctrl = node.left.Accept(self)
		if ctrl is not ControlState.PASS:
			return x, ctrl
		
		y, ctrl = node.right.Accept(self)
		if ctrl is not ControlState.PASS:
			return y, ctrl
		
		return self._ExecuteBinaryOp(node.op, x, y), ControlState.PASS
	
	@staticmethod
	def _ExecuteBinaryOp(op: scanner.Token, x: Any, y: Any) -> Any:
		match op.GetText():
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
				raise ValueError(f"Unknown operator: {op}")
	
	def VisitUnaryOp(self, node):
		val, ctrl = node.node.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		return self._ExecuteUnaryOp(node.op, val), ControlState.PASS
	
	@staticmethod
	def _ExecuteUnaryOp(op: scanner.Token, val: Any) -> Any:
		match op.GetText():
			case "+":
				return val
			case "-":
				return -val
			case "!":
				return not val
			case _:
				raise ValueError(f"Unknown operator: {op}")
	
	def VisitAccess(self, node):
		obj, ctrl = node.obj.Accept(self)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		assert isinstance(node.field, lang_ast.NodeLabel)
		return getattr(obj, node.field.token.GetText())
	
	def VisitIndex(self, node):
		obj, ctrl = node.obj.Accept(self)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		idx, ctrl = node.index.Accept(self)
		if ctrl is not ControlState.PASS:
			return idx, ctrl
		
		return obj[idx], ControlState.PASS
	
	def VisitReturn(self, node):
		val, ctrl = node.expr.Accept(self)
		return val, ControlState.RETURN if ctrl is ControlState.PASS else ctrl
	
	def VisitBreak(self, node):
		val, ctrl = node.expr.Accept(self)
		return val, ControlState.BREAK_LOOP if ctrl is ControlState.PASS else ctrl
	
	def VisitContinue(self, node):
		return None, ControlState.CONT_LOOP
	
	def VisitIfElse(self, node):
		scope = Context.Nest(self.ctx)
		ev = Evaluate(scope)
		cond, ctrl = node.cond.Accept(ev)
		assert ctrl is ControlState.PASS
		return node.true_branch.Accept(ev) if cond else node.false_branch.Accept(ev)
	
	def VisitWhileElse(self, node):
		scope = Context.Nest(self.ctx)
		ev = Evaluate(scope)
		while True:
			cond, ctrl = node.cond.Accept(ev)
			assert ctrl is ControlState.PASS
			
			if not cond:
				return node.else_branch.Accept(ev)
			
			val, ctrl = node.loop_body.Accept(ev)
			match ctrl:
				case ControlState.RETURN:
					return val, ControlState.RETURN
				case ControlState.BREAK_LOOP:
					return val, ControlState.PASS
				case ControlState.PASS | ControlState.CONT_LOOP:
					pass
	
	def VisitForElse(self, node):
		# FIXME
		return super().VisitForElse(node)
	
	def VisitNamedTuple(self, node):
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS
	
	def VisitNamedStruct(self, node):
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS


class Declare(lang_ast.Visitor[None]):
	def __init__(self, ctx: Context):
		self.ctx = ctx
	
	def VisitInt(self, node):
		pass
	
	def VisitStr(self, node):
		pass
	
	def VisitBool(self, node):
		pass
	
	def VisitLabel(self, node):
		self.ctx.Push(node.token.GetText(), None)
	
	def VisitCompound(self, node):
		assert len(node.nodes) == 1, "Declare compound node with more than one statement"
		node.nodes[0].Accept(self)
	
	def VisitAssign(self, node):
		"""For struct unwind."""
		node.expr.Accept(self)
	
	def VisitTuple(self, node):
		for node in node.nodes:
			node.Accept(self)
	
	def VisitStruct(self, node):
		for field in node.fields:
			assert isinstance(field, lang_ast.NodeAssign)
			field.Accept(self)


class Unwind(lang_ast.Visitor[None]):
	def __init__(self, val: Any, ctx: Context):
		self.val = val
		self.ctx = ctx
	
	def VisitInt(self, node):
		assert isinstance(self.val, int) and self.val == int(node.token.GetText())
	
	def VisitStr(self, node):
		assert isinstance(self.val, str) and self.val == node.token.GetText()
	
	def VisitBool(self, node):
		assert isinstance(self.val, bool) and self.val == (node.token.GetText().lower() == "true")
	
	def VisitLabel(self, node):
		self.ctx.Update(node.token.GetText(), self.val)
	
	def VisitCompound(self, node):
		assert len(node.nodes) == 1, "Unwind compound node with more than one statement"
		node.nodes[0].Accept(self)
	
	def VisitDecl(self, node):
		node.var.Accept(Declare(self.ctx))
		node.var.Accept(self)
	
	def VisitAssign(self, node):
		"""For struct unwind."""
		assert isinstance(node.var, lang_ast.NodeDecl)
		scope = Context(None, [], self.val)
		ev = Evaluate(scope)
		data, ctrl = node.var.var.Accept(ev)
		assert ctrl is ControlState.PASS
		node.expr.Accept(Unwind(data, self.ctx))
	
	def VisitTuple(self, node):
		assert isinstance(self.val, tuple), f"Expected tuple, got {type(self.val)}"
		assert len(self.val) == len(node.nodes)
		for i, node in enumerate(node.nodes):
			node.Accept(Unwind(self.val[i], self.ctx))
	
	def VisitStruct(self, node):
		assert isinstance(self.val, dict), f"Expect dict (struct), got {type(self.val)}"
		for field in node.fields:
			assert isinstance(field, lang_ast.NodeAssign)
			field.Accept(self)
