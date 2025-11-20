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
	def __init__(self, token: Token):
		self.token = token
	
	@override
	def __repr__(self):
		return self._GenStr(self.token.GetText())
	
	@override
	def Accept(self, visitor):
		return visitor.VisitInt(self)


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.token.GetText()))
	
	@override
	def Accept(self, visitor):
		return visitor.VisitLabel(self)


class NodeStr(Node):
	def __init__(self, token: Token):
		self.token = token
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.token.GetText()))
	
	@override
	def Accept(self, visitor):
		return visitor.VisitStr(self)


class NodeBool(Node):
	def __init__(self, token: Token):
		self.token = token
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.token.GetText()))
	
	@override
	def Accept(self, visitor):
		return visitor.VisitBool(self)


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	@override
	def Accept(self, visitor):
		return visitor.VisitCompound(self)


class NodeDecl(Node):
	def __init__(self, var: Node):
		self.var = var
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.var}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitDecl(self)


class NodeAssign(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.var}, {self.expr}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitAssign(self)


class NodeFunc(Node):
	def __init__(self, param: Node, body: NodeCompound):
		self.param = param
		self.body = body
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitFunc(self)


class NodeTemplate(Node):
	def __init__(self, param: Node, body: NodeCompound):
		self.param = param
		self.body = body
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitTemplate(self)


class NodeApply(Node):
	def __init__(self, func: Node, arg: Node):
		self.func = func
		self.arg = arg
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.func}, {self.arg}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitApply(self)


class NodeUnion(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	@override
	def Accept(self, visitor):
		return visitor.VisitUnion(self)


class NodeTuple(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	@override
	def Accept(self, visitor):
		return visitor.VisitTuple(self)


class NodeStruct(Node):
	def __init__(self, *fields: Node):
		self.fields = fields
	
	@override
	def __repr__(self):
		return self._GenStr(repr(self.fields)[1:-2])
	
	@override
	def Accept(self, visitor):
		return visitor.VisitStruct(self)


class NodeLogicalOp(Node):
	def __init__(self, op: Token, lhs: Node, rhs: Node):
		self.op = op
		self.lhs = lhs
		self.rhs = rhs
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.op.GetText()}, {self.lhs}, {self.rhs}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitLogicalOp(self)


class NodeBinaryOp(Node):
	def __init__(self, op: Token, lhs: Node, rhs: Node):
		self.op = op
		self.lhs = lhs
		self.rhs = rhs
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.op.GetText()}, {self.lhs}, {self.rhs}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitBinaryOp(self)


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.op}, {self.node}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitUnaryOp(self)


class NodeAccess(Node):
	def __init__(self, obj: Node, field: Node):
		self.obj = obj
		self.field = field
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.field}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitAccess(self)


class NodeIndex(Node):
	def __init__(self, obj: Node, index: Node):
		self.obj = obj
		self.index = index
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.index}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitIndex(self)


class NodeReturn(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitReturn(self)


class NodeBreak(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitBreak(self)


class NodeContinue(Node):
	@override
	def __repr__(self):
		return self._GenStr("")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitContinue(self)


class NodeIfElse(Node):
	def __init__(self, cond: Node, true_branch: NodeCompound, false_branch: NodeCompound):
		self.cond = cond
		self.true_branch = true_branch
		self.false_branch = false_branch
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.true_branch}, {self.false_branch}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitIfElse(self)


class NodeWhileElse(Node):
	def __init__(self, cond: Node, loop_body: NodeCompound, else_branch: NodeCompound):
		self.cond = cond
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.loop_body}, {self.else_branch}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitWhileElse(self)


class NodeForElse(Node):
	def __init__(self, iterable: Node, var: Node, loop_body: NodeCompound, else_branch: NodeCompound):
		self.iterable = iterable
		self.var = var
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.iterable}, {self.var}, {self.loop_body}, {self.else_branch}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitForElse(self)


class NodeNamedTuple(Node):
	def __init__(self, body: Node):
		self.body = body
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	@override
	def Accept(self, visitor):
		return visitor.VisitNamedTuple(self)


class NodeNamedStruct(Node):
	def __init__(self, body: Node):
		self.body = body
	
	@override
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	@override
	def Accept(self, visitor):
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
	def VisitApply(self, node: NodeApply) -> T:
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
