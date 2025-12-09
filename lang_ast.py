from typing import Self, override
from abc import ABC, abstractmethod
from scanner import Token


class Node(ABC):
	prefix: tuple["Node", ...] = ()
	suffix: tuple["Node", ...] = ()
	
	@abstractmethod
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		pass
	
	def AppendPrefix(self, *attr: "Node") -> Self:
		self.prefix = self.prefix + attr
		return self
	
	def AppendSuffix(self, *attr: "Node") -> Self:
		self.suffix = self.suffix + attr
		return self
	
	def _GenStr(self, fields: str) -> str:
		pre = f", prefix={';'.join(str(attr) for attr in self.prefix)}" if self.prefix else ""
		suf = f", suffix={';'.join(str(attr) for attr in self.suffix)}" if self.suffix else ""
		return f"{self.__class__.__name__}({fields}{pre}{suf})"


class NodeInt(Node):
	@override
	def __init__(self, token: Token) -> None:
		self.token = token
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(self.token.GetText())
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitInt(self)


class NodeLabel(Node):
	@override
	def __init__(self, token: Token) -> None:
		self.token = token
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.token.GetText()))
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitLabel(self)


class NodeStr(Node):
	@override
	def __init__(self, token: Token) -> None:
		self.token = token
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.token.GetText()))
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitStr(self)


class NodeBool(Node):
	@override
	def __init__(self, token: Token) -> None:
		self.token = token
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.token.GetText()))
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitBool(self)


class NodeCompound(Node):
	@override
	def __init__(self, *nodes: Node) -> None:
		self.nodes = nodes
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.nodes)[1:-2])
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitCompound(self)


class NodeDecl(Node):
	@override
	def __init__(self, var: Node) -> None:
		self.var = var
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.var}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitDecl(self)


class NodeAssign(Node):
	@override
	def __init__(self, var: Node, expr: Node) -> None:
		self.var = var
		self.expr = expr
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.var}, {self.expr}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitAssign(self)


class NodeFunc(Node):
	@override
	def __init__(self, param: Node, body: NodeCompound) -> None:
		self.param = param
		self.body = body
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.param}, {self.body}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitFunc(self)


class NodeTemplate(Node):
	@override
	def __init__(self, param: Node, body: NodeCompound) -> None:
		self.param = param
		self.body = body
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.param}, {self.body}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitTemplate(self)


class NodeCall(Node):
	@override
	def __init__(self, func: Node, arg: Node) -> None:
		self.func = func
		self.arg = arg
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.func}, {self.arg}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitCall(self)


class NodeUnion(Node):
	@override
	def __init__(self, *nodes: Node) -> None:
		self.nodes = nodes
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.nodes)[1:-2])
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitUnion(self)


class NodeTuple(Node):
	@override
	def __init__(self, *nodes: Node) -> None:
		self.nodes = nodes
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.nodes)[1:-2])
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitTuple(self)


class NodeStruct(Node):
	@override
	def __init__(self, *fields: Node) -> None:
		self.fields = fields
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(repr(self.fields)[1:-2])
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitStruct(self)


class NodeLogicalOp(Node):
	@override
	def __init__(self, op: Token, lhs: Node, rhs: Node) -> None:
		self.op = op
		self.lhs = lhs
		self.rhs = rhs
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.op.GetText()}, {self.lhs}, {self.rhs}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitLogicalOp(self)


class NodeBinaryOp(Node):
	@override
	def __init__(self, op: Token, lhs: Node, rhs: Node) -> None:
		self.op = op
		self.lhs = lhs
		self.rhs = rhs
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.op.GetText()}, {self.lhs}, {self.rhs}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitBinaryOp(self)


class NodeUnaryOp(Node):
	@override
	def __init__(self, op: Token, node: Node) -> None:
		self.op = op
		self.expr = node
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.op}, {self.expr}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitUnaryOp(self)


class NodeAccess(Node):
	@override
	def __init__(self, obj: Node, field: Node) -> None:
		self.obj = obj
		self.field = field
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.obj}, {self.field}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitAccess(self)


class NodeIndex(Node):
	@override
	def __init__(self, obj: Node, index: Node) -> None:
		self.obj = obj
		self.index = index
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.obj}, {self.index}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitIndex(self)


class NodeReturn(Node):
	@override
	def __init__(self, expr: Node) -> None:
		self.expr = expr
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.expr}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitReturn(self)


class NodeBreak(Node):
	@override
	def __init__(self, expr: Node) -> None:
		self.expr = expr
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.expr}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitBreak(self)


class NodeContinue(Node):
	@override
	def __repr__(self) -> str:
		return self._GenStr("")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitContinue(self)


class NodeIfElse(Node):
	@override
	def __init__(self, cond: Node, true_branch: NodeCompound, false_branch: NodeCompound) -> None:
		self.cond = cond
		self.true_branch = true_branch
		self.false_branch = false_branch
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.cond}, {self.true_branch}, {self.false_branch}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitIfElse(self)


class NodeWhileElse(Node):
	@override
	def __init__(self, cond: Node, loop_body: NodeCompound, else_branch: NodeCompound) -> None:
		self.cond = cond
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.cond}, {self.loop_body}, {self.else_branch}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitWhileElse(self)


class NodeForElse(Node):
	@override
	def __init__(self, iterable: Node, var: Node, loop_body: NodeCompound, else_branch: NodeCompound) -> None:
		self.iterable = iterable
		self.var = var
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.iterable}, {self.var}, {self.loop_body}, {self.else_branch}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitForElse(self)


class NodeNamedTuple(Node):
	@override
	def __init__(self, body: Node) -> None:
		self.body = body
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.body}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitNamedTuple(self)


class NodeNamedStruct(Node):
	@override
	def __init__(self, body: Node) -> None:
		self.body = body
	
	@override
	def __repr__(self) -> str:
		return self._GenStr(f"{self.body}")
	
	@override
	def Accept[T](self, visitor: "Visitor[T]") -> T:
		return visitor.VisitNamedStruct(self)


class Visitor[T](ABC):
	@abstractmethod
	def VisitInt(self, node: NodeInt) -> T:
		pass
	
	@abstractmethod
	def VisitLabel(self, node: NodeLabel) -> T:
		pass
	
	@abstractmethod
	def VisitStr(self, node: NodeStr) -> T:
		pass
	
	@abstractmethod
	def VisitBool(self, node: NodeBool) -> T:
		pass
	
	@abstractmethod
	def VisitCompound(self, node: NodeCompound) -> T:
		pass
	
	@abstractmethod
	def VisitDecl(self, node: NodeDecl) -> T:
		pass
	
	@abstractmethod
	def VisitAssign(self, node: NodeAssign) -> T:
		pass
	
	@abstractmethod
	def VisitFunc(self, node: NodeFunc) -> T:
		pass
	
	@abstractmethod
	def VisitTemplate(self, node: NodeTemplate) -> T:
		pass
	
	@abstractmethod
	def VisitCall(self, node: NodeCall) -> T:
		pass
	
	@abstractmethod
	def VisitUnion(self, node: NodeUnion) -> T:
		pass
	
	@abstractmethod
	def VisitTuple(self, node: NodeTuple) -> T:
		pass
	
	@abstractmethod
	def VisitStruct(self, node: NodeStruct) -> T:
		pass
	
	@abstractmethod
	def VisitLogicalOp(self, node: NodeLogicalOp) -> T:
		pass
	
	@abstractmethod
	def VisitBinaryOp(self, node: NodeBinaryOp) -> T:
		pass
	
	@abstractmethod
	def VisitUnaryOp(self, node: NodeUnaryOp) -> T:
		pass
	
	@abstractmethod
	def VisitAccess(self, node: NodeAccess) -> T:
		pass
	
	@abstractmethod
	def VisitIndex(self, node: NodeIndex) -> T:
		pass
	
	@abstractmethod
	def VisitReturn(self, node: NodeReturn) -> T:
		pass
	
	@abstractmethod
	def VisitBreak(self, node: NodeBreak) -> T:
		pass
	
	@abstractmethod
	def VisitContinue(self, node: NodeContinue) -> T:
		pass
	
	@abstractmethod
	def VisitIfElse(self, node: NodeIfElse) -> T:
		pass
	
	@abstractmethod
	def VisitWhileElse(self, node: NodeWhileElse) -> T:
		pass
	
	@abstractmethod
	def VisitForElse(self, node: NodeForElse) -> T:
		pass
	
	@abstractmethod
	def VisitNamedTuple(self, node: NodeNamedTuple) -> T:
		pass
	
	@abstractmethod
	def VisitNamedStruct(self, node: NodeNamedStruct) -> T:
		pass
