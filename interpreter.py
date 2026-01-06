from typing import Any, Self, override
from enum import Enum, auto, unique
import scanner
import lang_ast


class Environment:
	@override
	def __init__(self, parent: Environment | None, local_stack: list[str], local_table: dict[str, Any]) -> None:
		self._parent = parent
		self._local_stack = local_stack
		self._local_table = local_table
	
	@classmethod
	def New(cls) -> Self:
		return cls(None, [], {})
	
	@classmethod
	def Nest(cls, parent: Environment) -> Self:
		return cls(parent, [], {})
	
	def Resolve(self, symbol: str) -> Any:
		if symbol in self._local_table:
			return self._local_table[symbol]
		elif self._parent is not None:
			return self._parent.Resolve(symbol)
		else:
			raise RuntimeError(f"Cannot resolve variable <{symbol}>")
	
	def Push(self, symbol: str, value: Any) -> None:
		assert symbol not in self._local_table
		self._local_stack.append(symbol)
		self._local_table[symbol] = value
	
	def Pop(self, symbol: str) -> Any:
		self._local_stack.remove(symbol)
		return self._local_table.pop(symbol)
	
	def Update(self, symbol: str, value: Any) -> None:
		if symbol in self._local_table:
			self._local_table[symbol] = value
		else:
			assert self._parent is not None, f"No variable named \"{symbol}\" to update"
			self._parent.Update(symbol, value)
	
	def GetLocals(self) -> dict[str, Any]:
		return self._local_table


@unique
class ControlState(Enum):
	PASS = auto()
	BREAK_LOOP = auto()
	CONT_LOOP = auto()
	RETURN = auto()


class Evaluate(lang_ast.Visitor[tuple[Any, ControlState]]):
	@override
	def __init__(self, env: Environment) -> None:
		self.env = env
	
	@override
	def VisitInt(self, node: lang_ast.NodeInt) -> tuple[Any, ControlState]:
		return int(node.token.GetText()), ControlState.PASS
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> tuple[Any, ControlState]:
		# TODO: Handle escape characters.
		return node.token.GetText(), ControlState.PASS
	
	@override
	def VisitBool(self, node: lang_ast.NodeBool) -> tuple[Any, ControlState]:
		return node.token.GetText().lower() == "true", ControlState.PASS
	
	@override
	def VisitLabel(self, node: lang_ast.NodeLabel) -> tuple[Any, ControlState]:
		val = self.env.Resolve(node.token.GetText())
		assert val is not None
		return val, ControlState.PASS
	
	@override
	def VisitCompound(self, node: lang_ast.NodeCompound) -> tuple[Any, ControlState]:
		scope = Environment.Nest(self.env)
		evaluator = Evaluate(scope)
		val, ctrl = (), ControlState.PASS
		for stmt in node.nodes:
			val, ctrl = stmt.Accept(evaluator)
			if ctrl is not ControlState.PASS:
				break
		return val, ctrl
	
	@override
	def VisitDecl(self, node: lang_ast.NodeDecl) -> tuple[Any, ControlState]:
		node.var.Accept(Declare(self.env))
		return None, ControlState.PASS
	
	@override
	def VisitAssign(self, node: lang_ast.NodeAssign) -> tuple[Any, ControlState]:
		val, ctrl = node.expr.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		node.var.Accept(Unwind(val, self.env))
		return val, ControlState.PASS
	
	@override
	def VisitFunc(self, node: lang_ast.NodeFunc) -> tuple[Any, ControlState]:
		def func(arg: Any) -> tuple[Any, ControlState]:
			scope = Environment.Nest(self.env)
			node.param.Accept(Declare(scope))
			node.param.Accept(Unwind(arg, scope))
			val, ctrl = node.body.Accept(Evaluate(scope))
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return func, ControlState.PASS
	
	@override
	def VisitTemplate(self, node: lang_ast.NodeTemplate) -> tuple[Any, ControlState]:
		def tmplt(arg: Any) -> tuple[Any, ControlState]:
			scope = Environment.Nest(self.env)
			node.param.Accept(Declare(scope))
			node.param.Accept(Unwind(arg, scope))
			val, ctrl = node.body.Accept(Evaluate(scope))
			assert ctrl in (ControlState.PASS, ControlState.RETURN)
			return val, ControlState.PASS
		return tmplt, ControlState.PASS
	
	@override
	def VisitCall(self, node: lang_ast.NodeCall) -> tuple[Any, ControlState]:
		func, ctrl = node.func.Accept(self)
		if ctrl is not ControlState.PASS:
			return func, ctrl
		
		arg, ctrl = node.arg.Accept(self)
		if ctrl is not ControlState.PASS:
			return arg, ctrl
		
		return func(arg)
	
	@override
	def VisitUnion(self, node: lang_ast.NodeUnion) -> tuple[Any, ControlState]:
		raise RuntimeError("Cannot evaluate union")
	
	@override
	def VisitTuple(self, node: lang_ast.NodeTuple) -> tuple[Any, ControlState]:
		nodes = []
		for n in node.nodes:
			val, ctrl = n.Accept(self)
			if ctrl is not ControlState.PASS:
				return val, ctrl
			nodes.append(val)
		
		return tuple(nodes), ControlState.PASS
	
	@override
	def VisitStruct(self, node: lang_ast.NodeStruct) -> tuple[Any, ControlState]:
		scope = Environment.Nest(self.env)
		evaluator = Evaluate(scope)
		for fld in node.fields:
			val, ctrl = fld.Accept(evaluator)
			if ctrl is not ControlState.PASS:
				return val, ctrl
		return scope.GetLocals(), ControlState.PASS
	
	@override
	def VisitLogicalOp(self, node: lang_ast.NodeLogicalOp) -> tuple[Any, ControlState]:
		short_val = node.op.GetType() == "||"
		val, ctrl = node.lhs.Accept(self)
		if ctrl is not ControlState.PASS:
			return val, ctrl
		
		assert val in (True, False)
		if val == short_val:
			return val, ControlState.PASS
		
		return node.rhs.Accept(self)
	
	@override
	def VisitBinaryOp(self, node: lang_ast.NodeBinaryOp) -> tuple[Any, ControlState]:
		x, ctrl = node.lhs.Accept(self)
		if ctrl is not ControlState.PASS:
			return x, ctrl
		
		y, ctrl = node.rhs.Accept(self)
		if ctrl is not ControlState.PASS:
			return y, ctrl
		
		return self._ExecuteBinaryOp(node.op, x, y), ControlState.PASS
	
	@staticmethod
	def _ExecuteBinaryOp(op: scanner.Token, x: Any, y: Any) -> Any:
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
	def VisitUnaryOp(self, node: lang_ast.NodeUnaryOp) -> tuple[Any, ControlState]:
		val, ctrl = node.expr.Accept(self)
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
	
	@override
	def VisitAccess(self, node: lang_ast.NodeAccess) -> tuple[Any, ControlState]:
		obj, ctrl = node.obj.Accept(self)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		assert isinstance(node.field, lang_ast.NodeLabel)
		return getattr(obj, node.field.token.GetText())
	
	@override
	def VisitIndex(self, node: lang_ast.NodeIndex) -> tuple[Any, ControlState]:
		obj, ctrl = node.obj.Accept(self)
		if ctrl is not ControlState.PASS:
			return obj, ctrl
		
		idx, ctrl = node.index.Accept(self)
		if ctrl is not ControlState.PASS:
			return idx, ctrl
		
		return obj[idx], ControlState.PASS
	
	@override
	def VisitReturn(self, node: lang_ast.NodeReturn) -> tuple[Any, ControlState]:
		val, ctrl = node.expr.Accept(self)
		return val, ControlState.RETURN if ctrl is ControlState.PASS else ctrl
	
	@override
	def VisitBreak(self, node: lang_ast.NodeBreak) -> tuple[Any, ControlState]:
		val, ctrl = node.expr.Accept(self)
		return val, ControlState.BREAK_LOOP if ctrl is ControlState.PASS else ctrl
	
	@override
	def VisitContinue(self, node: lang_ast.NodeContinue) -> tuple[Any, ControlState]:
		return None, ControlState.CONT_LOOP
	
	@override
	def VisitIfElse(self, node: lang_ast.NodeIfElse) -> tuple[Any, ControlState]:
		scope = Environment.Nest(self.env)
		ev = Evaluate(scope)
		cond, ctrl = node.cond.Accept(ev)
		assert ctrl is ControlState.PASS
		return node.true_branch.Accept(ev) if cond else node.false_branch.Accept(ev)
	
	@override
	def VisitWhileElse(self, node: lang_ast.NodeWhileElse) -> tuple[Any, ControlState]:
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
	def VisitForElse(self, node: lang_ast.NodeForElse) -> tuple[Any, ControlState]:
		# FIXME
		return super().VisitForElse(node)
	
	@override
	def VisitNamedTuple(self, node: lang_ast.NodeNamedTuple) -> tuple[Any, ControlState]:
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS
	
	@override
	def VisitNamedStruct(self, node: lang_ast.NodeNamedStruct) -> tuple[Any, ControlState]:
		return lambda arg: (arg, ControlState.PASS), ControlState.PASS


class Declare(lang_ast.Visitor[None]):
	@override
	def __init__(self, env: Environment) -> None:
		self.env = env
	
	@override
	def VisitInt(self, node: lang_ast.NodeInt) -> None:
		return None
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> None:
		return None
	
	@override
	def VisitBool(self, node: lang_ast.NodeBool) -> None:
		return None
	
	@override
	def VisitLabel(self, node: lang_ast.NodeLabel) -> None:
		self.env.Push(node.token.GetText(), None)
	
	@override
	def VisitCompound(self, node: lang_ast.NodeCompound) -> None:
		assert len(node.nodes) == 1, "Declare compound node with more than one statement"
		node.nodes[0].Accept(self)
	
	@override
	def VisitDecl(self, node: lang_ast.NodeDecl) -> None:
		raise RuntimeError
	
	@override
	def VisitAssign(self, node: lang_ast.NodeAssign) -> None:
		raise RuntimeError
	
	@override
	def VisitFunc(self, node: lang_ast.NodeFunc) -> None:
		raise RuntimeError
	
	@override
	def VisitTemplate(self, node: lang_ast.NodeTemplate) -> None:
		raise RuntimeError
	
	@override
	def VisitCall(self, node: lang_ast.NodeCall) -> None:
		raise RuntimeError
	
	@override
	def VisitUnion(self, node: lang_ast.NodeUnion) -> None:
		raise RuntimeError
	
	@override
	def VisitTuple(self, node: lang_ast.NodeTuple) -> None:
		for n in node.nodes:
			n.Accept(self)
	
	@override
	def VisitStruct(self, node: lang_ast.NodeStruct) -> None:
		for field in node.fields:
			assert isinstance(field, lang_ast.NodeAssign)
			field.expr.Accept(self)
	
	@override
	def VisitLogicalOp(self, node: lang_ast.NodeLogicalOp) -> None:
		raise RuntimeError
	
	@override
	def VisitBinaryOp(self, node: lang_ast.NodeBinaryOp) -> None:
		raise RuntimeError
	
	@override
	def VisitUnaryOp(self, node: lang_ast.NodeUnaryOp) -> None:
		raise RuntimeError
	
	@override
	def VisitAccess(self, node: lang_ast.NodeAccess) -> None:
		raise RuntimeError
	
	@override
	def VisitIndex(self, node: lang_ast.NodeIndex) -> None:
		raise RuntimeError
	
	@override
	def VisitReturn(self, node: lang_ast.NodeReturn) -> None:
		raise RuntimeError
	
	@override
	def VisitBreak(self, node: lang_ast.NodeBreak) -> None:
		raise RuntimeError
	
	@override
	def VisitContinue(self, node: lang_ast.NodeContinue) -> None:
		raise RuntimeError
	
	@override
	def VisitIfElse(self, node: lang_ast.NodeIfElse) -> None:
		raise RuntimeError
	
	@override
	def VisitWhileElse(self, node: lang_ast.NodeWhileElse) -> None:
		raise RuntimeError
	
	@override
	def VisitForElse(self, node: lang_ast.NodeForElse) -> None:
		raise RuntimeError
	
	@override
	def VisitNamedTuple(self, node: lang_ast.NodeNamedTuple) -> None:
		raise RuntimeError
	
	@override
	def VisitNamedStruct(self, node: lang_ast.NodeNamedStruct) -> None:
		raise RuntimeError


class Unwind(lang_ast.Visitor[None]):
	@override
	def __init__(self, val: Any, env: Environment) -> None:
		self.val = val
		self.env = env
	
	@override
	def VisitInt(self, node: lang_ast.NodeInt) -> None:
		assert isinstance(self.val, int) and self.val == int(node.token.GetText())
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> None:
		assert isinstance(self.val, str) and self.val == node.token.GetText()
	
	@override
	def VisitBool(self, node: lang_ast.NodeBool) -> None:
		assert isinstance(self.val, bool) and self.val == (node.token.GetText().lower() == "true")
	
	@override
	def VisitLabel(self, node: lang_ast.NodeLabel) -> None:
		self.env.Update(node.token.GetText(), self.val)
	
	@override
	def VisitCompound(self, node: lang_ast.NodeCompound) -> None:
		assert len(node.nodes) == 1, "Unwind compound node with more than one statement"
		node.nodes[0].Accept(self)
	
	@override
	def VisitDecl(self, node: lang_ast.NodeDecl) -> None:
		node.var.Accept(Declare(self.env))
		node.var.Accept(self)
	
	@override
	def VisitAssign(self, node: lang_ast.NodeAssign) -> None:
		raise RuntimeError
	
	@override
	def VisitFunc(self, node: lang_ast.NodeFunc) -> None:
		raise RuntimeError
	
	@override
	def VisitTemplate(self, node: lang_ast.NodeTemplate) -> None:
		raise RuntimeError
	
	@override
	def VisitCall(self, node: lang_ast.NodeCall) -> None:
		raise RuntimeError
	
	@override
	def VisitUnion(self, node: lang_ast.NodeUnion) -> None:
		raise RuntimeError
	
	@override
	def VisitTuple(self, node: lang_ast.NodeTuple) -> None:
		assert isinstance(self.val, tuple), f"Expected tuple, got {type(self.val)}"
		assert len(self.val) == len(node.nodes)
		for i, n in enumerate(node.nodes):
			n.Accept(Unwind(self.val[i], self.env))
	
	@override
	def VisitStruct(self, node: lang_ast.NodeStruct) -> None:
		assert isinstance(self.val, dict), f"Expect dict (struct), got {type(self.val)}"
		for field in node.fields:
			assert isinstance(field, lang_ast.NodeAssign)
			assert isinstance(field.var, lang_ast.NodeDecl)
			scope = Environment(None, [], self.val)
			eva = Evaluate(scope)
			data, ctrl = field.var.var.Accept(eva)
			assert ctrl is ControlState.PASS
			field.expr.Accept(Unwind(data, self.env))
	
	@override
	def VisitLogicalOp(self, node: lang_ast.NodeLogicalOp) -> None:
		raise RuntimeError
	
	@override
	def VisitBinaryOp(self, node: lang_ast.NodeBinaryOp) -> None:
		raise RuntimeError
	
	@override
	def VisitUnaryOp(self, node: lang_ast.NodeUnaryOp) -> None:
		raise RuntimeError
	
	@override
	def VisitAccess(self, node: lang_ast.NodeAccess) -> None:
		raise RuntimeError
	
	@override
	def VisitIndex(self, node: lang_ast.NodeIndex) -> None:
		raise RuntimeError
	
	@override
	def VisitReturn(self, node: lang_ast.NodeReturn) -> None:
		raise RuntimeError
	
	@override
	def VisitBreak(self, node: lang_ast.NodeBreak) -> None:
		raise RuntimeError
	
	@override
	def VisitContinue(self, node: lang_ast.NodeContinue) -> None:
		raise RuntimeError
	
	@override
	def VisitIfElse(self, node: lang_ast.NodeIfElse) -> None:
		raise RuntimeError
	
	@override
	def VisitWhileElse(self, node: lang_ast.NodeWhileElse) -> None:
		raise RuntimeError
	
	@override
	def VisitForElse(self, node: lang_ast.NodeForElse) -> None:
		raise RuntimeError
	
	@override
	def VisitNamedTuple(self, node: lang_ast.NodeNamedTuple) -> None:
		raise RuntimeError
	
	@override
	def VisitNamedStruct(self, node: lang_ast.NodeNamedStruct) -> None:
		raise RuntimeError
