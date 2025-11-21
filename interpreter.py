from typing import Any, override
from enum import Enum, auto, unique
import scanner
import lang_ast


class Environment:
	def __init__(self, parent: "Environment | None", local_stack: list[str], local_table: dict[str, Any]):
		self._parent = parent
		self._local_stack = local_stack
		self._local_table = local_table
	
	@classmethod
	def New(cls):
		return cls(None, [], {})
	
	@classmethod
	def Nest(cls, parent: "Environment"):
		return cls(parent, [], {})
	
	def Resolve(self, symbol: str) -> Any:
		if symbol in self._local_table:
			return self._local_table[symbol]
		
		if self._parent is None:
			raise RuntimeError(f"Cannot resolve variable <{symbol}>")
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
	def __init__(self, env: Environment):
		self.env = env
	
	@override
	def VisitInt(self, node):
		return int(node.token.GetText()), ControlState.PASS
	
	@override
	def VisitStr(self, node):
		return node.token.GetText(), ControlState.PASS
	
	@override
	def VisitBool(self, node):
		return node.token.GetText().lower() == "true", ControlState.PASS
	
	@override
	def VisitLabel(self, node):
		val = self.env.Resolve(node.token.GetText())
		assert val is not None
		return val, ControlState.PASS
	
	@override
	def VisitCompound(self, node):
		scope = Environment.Nest(self.env)
		return Evaluate(scope).EvalCompound(node)
	
	def EvalCompound(self, cmpd: lang_ast.NodeCompound) -> tuple[Any, ControlState]:
		val, ctrl = (), ControlState.PASS
		for node in cmpd.nodes:
			val, ctrl = node.Accept(self)
			if ctrl is not ControlState.PASS:
				break
		return val, ctrl
	
	@override
	def VisitDecl(self, node):
		node.var.Accept(Declare(self.env))
		return None, ControlState.PASS
	
	@override
	def VisitAssign(self, node):
		val, ctrl = node.expr.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		node.var.Accept(Unwind(val, self.env))
		return val, ControlState.PASS
	
	@override
	def VisitFunc(self, node):
		def func(arg):
			scope = Environment.Nest(self.env)
			node.param.Accept(Declare(scope))
			node.param.Accept(Unwind(arg, scope))
			val, ctrl = Evaluate(scope).EvalCompound(node.body)
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return func, ControlState.PASS
	
	@override
	def VisitTemplate(self, node):
		def tmplt(arg):
			scope = Environment.Nest(self.env)
			node.param.Accept(Declare(scope))
			node.param.Accept(Unwind(arg, scope))
			val, ctrl = Evaluate(scope).EvalCompound(node.body)
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return tmplt, ControlState.PASS
	
	@override
	def VisitApply(self, node):
		func, ctrl = node.func.Accept(self)
		if ctrl is not ControlState.PASS:
			return func, ctrl
		
		arg, ctrl = node.arg.Accept(self)
		if ctrl is not ControlState.PASS:
			return arg, ctrl
		
		return func(arg)
	
	@override
	def VisitUnion(self, node):
		raise RuntimeError("Cannot evaluate union")
	
	@override
	def VisitTuple(self, node):
		nodes = []
		for node in node.nodes:
			val, ctrl = node.Accept(self)
			if ctrl is not ControlState.PASS:
				return val, ctrl
			nodes.append(val)
		
		return tuple(nodes), ControlState.PASS
	
	@override
	def VisitStruct(self, node):
		scope = Environment.Nest(self.env)
		ev = Evaluate(scope)
		for node in node.fields:
			val, ctrl = node.Accept(ev)
			if ctrl is not ControlState.PASS:
				return val, ctrl
		return scope.GetLocals(), ControlState.PASS
	
	@override
	def VisitLogicalOp(self, node):
		short_val = node.op.GetType() == "||"
		val, ctrl = node.lhs.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		assert val in (True, False)
		if val == short_val:
			return val, ControlState.PASS
		
		return node.rhs.Accept(self)
	
	@override
	def VisitBinaryOp(self, node):
		x, ctrl = node.lhs.Accept(self)
		if ctrl is not ControlState.PASS:
			return x, ctrl
		
		y, ctrl = node.rhs.Accept(self)
		if ctrl is not ControlState.PASS:
			return y, ctrl
		
		return self._ExecuteBinaryOp(node.op, x, y), ControlState.PASS
	
	@staticmethod
	def _ExecuteBinaryOp(op: scanner.Token, x: Any, y: Any):
		match op.GetType():
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
			case _:
				raise ValueError(f"Unknown operator: {op}")
	
	@override
	def VisitUnaryOp(self, node):
		val, ctrl = node.expr.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		return self._ExecuteUnaryOp(node.op, val), ControlState.PASS
	
	@staticmethod
	def _ExecuteUnaryOp(op: scanner.Token, val: Any):
		match op.GetText():
			case "+":
				return val
			case "-":
				return -val
			case "!":
				return not val
			case _:
				raise ValueError(f"Unknown operator: {op}")
	
	@override
	def VisitAccess(self, node):
		obj, ctrl = node.obj.Accept(self)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		assert isinstance(node.field, lang_ast.NodeLabel)
		return getattr(obj, node.field.token.GetText())
	
	@override
	def VisitIndex(self, node):
		obj, ctrl = node.obj.Accept(self)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		idx, ctrl = node.index.Accept(self)
		if ctrl is not ControlState.PASS:
			return idx, ctrl
		
		return obj[idx], ControlState.PASS
	
	@override
	def VisitReturn(self, node):
		val, ctrl = node.expr.Accept(self)
		return val, ControlState.RETURN if ctrl is ControlState.PASS else ctrl
	
	@override
	def VisitBreak(self, node):
		val, ctrl = node.expr.Accept(self)
		return val, ControlState.BREAK_LOOP if ctrl is ControlState.PASS else ctrl
	
	@override
	def VisitContinue(self, node):
		return None, ControlState.CONT_LOOP
	
	@override
	def VisitIfElse(self, node):
		scope = Environment.Nest(self.env)
		ev = Evaluate(scope)
		cond, ctrl = node.cond.Accept(ev)
		assert ctrl is ControlState.PASS
		return node.true_branch.Accept(ev) if cond else node.false_branch.Accept(ev)
	
	@override
	def VisitWhileElse(self, node):
		scope = Environment.Nest(self.env)
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
	
	@override
	def VisitForElse(self, node):
		# FIXME
		return super().VisitForElse(node)
	
	@override
	def VisitNamedTuple(self, node):
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS
	
	@override
	def VisitNamedStruct(self, node):
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS


class Declare(lang_ast.Visitor[None]):
	def __init__(self, env: Environment):
		self.env = env
	
	@override
	def VisitInt(self, node):
		pass
	
	@override
	def VisitStr(self, node):
		pass
	
	@override
	def VisitBool(self, node):
		pass
	
	@override
	def VisitLabel(self, node):
		self.env.Push(node.token.GetText(), None)
	
	@override
	def VisitCompound(self, node):
		assert len(node.nodes) == 1, "Declare compound node with more than one statement"
		node.nodes[0].Accept(self)
	
	@override
	def VisitDecl(self, node):
		raise RuntimeError
	
	@override
	def VisitAssign(self, node):
		"""For struct unwind."""
		node.expr.Accept(self)
	
	@override
	def VisitFunc(self, node):
		raise RuntimeError
	
	@override
	def VisitTemplate(self, node):
		raise RuntimeError
	
	@override
	def VisitApply(self, node):
		raise RuntimeError
	
	@override
	def VisitUnion(self, node):
		raise RuntimeError
	
	@override
	def VisitTuple(self, node):
		for node in node.nodes:
			node.Accept(self)
	
	@override
	def VisitStruct(self, node):
		for field in node.fields:
			assert isinstance(field, lang_ast.NodeAssign)
			field.Accept(self)
	
	@override
	def VisitLogicalOp(self, node):
		raise RuntimeError
	
	@override
	def VisitBinaryOp(self, node):
		raise RuntimeError
	
	@override
	def VisitUnaryOp(self, node):
		raise RuntimeError
	
	@override
	def VisitAccess(self, node):
		raise RuntimeError
	
	@override
	def VisitIndex(self, node):
		raise RuntimeError
	
	@override
	def VisitReturn(self, node):
		raise RuntimeError
	
	@override
	def VisitBreak(self, node):
		raise RuntimeError
	
	@override
	def VisitContinue(self, node):
		raise RuntimeError
	
	@override
	def VisitIfElse(self, node):
		raise RuntimeError
	
	@override
	def VisitWhileElse(self, node):
		raise RuntimeError
	
	@override
	def VisitForElse(self, node):
		raise RuntimeError
	
	@override
	def VisitNamedTuple(self, node):
		raise RuntimeError
	
	@override
	def VisitNamedStruct(self, node):
		raise RuntimeError


class Unwind(lang_ast.Visitor[None]):
	def __init__(self, val: Any, env: Environment):
		self.val = val
		self.env = env
	
	@override
	def VisitInt(self, node):
		assert isinstance(self.val, int) and self.val == int(node.token.GetText())
	
	@override
	def VisitStr(self, node):
		assert isinstance(self.val, str) and self.val == node.token.GetText()
	
	@override
	def VisitBool(self, node):
		assert isinstance(self.val, bool) and self.val == (node.token.GetText().lower() == "true")
	
	@override
	def VisitLabel(self, node):
		self.env.Update(node.token.GetText(), self.val)
	
	@override
	def VisitCompound(self, node):
		assert len(node.nodes) == 1, "Unwind compound node with more than one statement"
		node.nodes[0].Accept(self)
	
	@override
	def VisitDecl(self, node):
		node.var.Accept(Declare(self.env))
		node.var.Accept(self)
	
	@override
	def VisitAssign(self, node):
		"""For struct unwind."""
		assert isinstance(node.var, lang_ast.NodeDecl)
		scope = Environment(None, [], self.val)
		ev = Evaluate(scope)
		data, ctrl = node.var.var.Accept(ev)
		assert ctrl is ControlState.PASS
		node.expr.Accept(Unwind(data, self.env))
	
	@override
	def VisitFunc(self, node):
		raise RuntimeError
	
	@override
	def VisitTemplate(self, node):
		raise RuntimeError
	
	@override
	def VisitApply(self, node):
		raise RuntimeError
	
	@override
	def VisitUnion(self, node):
		raise RuntimeError
	
	@override
	def VisitTuple(self, node):
		assert isinstance(self.val, tuple), f"Expected tuple, got {type(self.val)}"
		assert len(self.val) == len(node.nodes)
		for i, node in enumerate(node.nodes):
			node.Accept(Unwind(self.val[i], self.env))
	
	@override
	def VisitStruct(self, node):
		assert isinstance(self.val, dict), f"Expect dict (struct), got {type(self.val)}"
		for field in node.fields:
			assert isinstance(field, lang_ast.NodeAssign)
			field.Accept(self)
	
	@override
	def VisitLogicalOp(self, node):
		raise RuntimeError
	
	@override
	def VisitBinaryOp(self, node):
		raise RuntimeError
	
	@override
	def VisitUnaryOp(self, node):
		raise RuntimeError
	
	@override
	def VisitAccess(self, node):
		raise RuntimeError
	
	@override
	def VisitIndex(self, node):
		raise RuntimeError
	
	@override
	def VisitReturn(self, node):
		raise RuntimeError
	
	@override
	def VisitBreak(self, node):
		raise RuntimeError
	
	@override
	def VisitContinue(self, node):
		raise RuntimeError
	
	@override
	def VisitIfElse(self, node):
		raise RuntimeError
	
	@override
	def VisitWhileElse(self, node):
		raise RuntimeError
	
	@override
	def VisitForElse(self, node):
		raise RuntimeError
	
	@override
	def VisitNamedTuple(self, node):
		raise RuntimeError
	
	@override
	def VisitNamedStruct(self, node):
		raise RuntimeError
