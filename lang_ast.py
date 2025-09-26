from typing import Self
from scanner import Token


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
	
	def _GenStr(self, fields: str) -> str:
		pre = f", prefix={';'.join(str(attr) for attr in self.prefix)}" if self.prefix else ""
		suf = f", suffix={';'.join(str(attr) for attr in self.suffix)}" if self.suffix else ""
		return f"{self.__class__.__name__}({fields}{pre}{suf})"


class NodeInt(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(self.token.GetText())
	
	def Accept(self, visitor):
		return visitor.VisitInt(self)


class NodeLabel(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetText()))
	
	def Accept(self, visitor):
		return visitor.VisitLabel(self)


class NodeStr(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetText()))
	
	def Accept(self, visitor):
		return visitor.VisitStr(self)


class NodeBool(Node):
	def __init__(self, token: Token):
		self.token = token
	
	def __repr__(self):
		return self._GenStr(repr(self.token.GetText()))
	
	def Accept(self, visitor):
		return visitor.VisitBool(self)


class NodeCompound(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitCompound(self)


class NodeDecl(Node):
	def __init__(self, var: Node):
		self.var = var
	
	def __repr__(self):
		return self._GenStr(f"{self.var}")
	
	def Accept(self, visitor):
		return visitor.VisitDecl(self)


class NodeAssign(Node):
	def __init__(self, var: Node, expr: Node):
		self.var = var
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.var}, {self.expr}")
	
	def Accept(self, visitor):
		return visitor.VisitAssign(self)


class NodeFunc(Node):
	def __init__(self, param: Node, body: NodeCompound):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitFunc(self)


class NodeTemplate(Node):
	def __init__(self, param: Node, body: NodeCompound):
		self.param = param
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.param}, {self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitTemplate(self)


class NodeApply(Node):
	def __init__(self, func: Node, arg: Node):
		self.func = func
		self.arg = arg
	
	def __repr__(self):
		return self._GenStr(f"{self.func}, {self.arg}")
	
	def Accept(self, visitor):
		return visitor.VisitApply(self)


class NodeUnion(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitUnion(self)


class NodeTuple(Node):
	def __init__(self, *nodes: Node):
		self.nodes = nodes
	
	def __repr__(self):
		return self._GenStr(repr(self.nodes)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitTuple(self)


class NodeStruct(Node):
	def __init__(self, *fields: Node):
		self.fields = fields
	
	def __repr__(self):
		return self._GenStr(repr(self.fields)[1:-2])
	
	def Accept(self, visitor):
		return visitor.VisitStruct(self)


class NodeLogicalOp(Node):
	def __init__(self, op: Token, lhs: Node, rhs: Node):
		self.op = op
		self.lhs = lhs
		self.rhs = rhs
	
	def __repr__(self):
		return self._GenStr(f"{self.op.GetText()}, {self.lhs}, {self.rhs}")
	
	def Accept(self, visitor):
		return visitor.VisitLogicalOp(self)


class NodeBinaryOp(Node):
	def __init__(self, op: Token, lhs: Node, rhs: Node):
		self.op = op
		self.lhs = lhs
		self.rhs = rhs
	
	def __repr__(self):
		return self._GenStr(f"{self.op.GetText()}, {self.lhs}, {self.rhs}")
	
	def Accept(self, visitor):
		return visitor.VisitBinaryOp(self)


class NodeUnaryOp(Node):
	def __init__(self, op: Token, node: Node):
		self.op = op
		self.node = node
	
	def __repr__(self):
		return self._GenStr(f"{self.op}, {self.node}")
	
	def Accept(self, visitor):
		return visitor.VisitUnaryOp(self)


class NodeAccess(Node):
	def __init__(self, obj: Node, field: Node):
		self.obj = obj
		self.field = field
	
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.field}")
	
	def Accept(self, visitor):
		return visitor.VisitAccess(self)


class NodeIndex(Node):
	def __init__(self, obj: Node, index: Node):
		self.obj = obj
		self.index = index
	
	def __repr__(self):
		return self._GenStr(f"{self.obj}, {self.index}")
	
	def Accept(self, visitor):
		return visitor.VisitIndex(self)


class NodeReturn(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	def Accept(self, visitor):
		return visitor.VisitReturn(self)


class NodeBreak(Node):
	def __init__(self, expr: Node):
		self.expr = expr
	
	def __repr__(self):
		return self._GenStr(f"{self.expr}")
	
	def Accept(self, visitor):
		return visitor.VisitBreak(self)


class NodeContinue(Node):
	def __repr__(self):
		return self._GenStr("")
	
	def Accept(self, visitor):
		return visitor.VisitContinue(self)


class NodeIfElse(Node):
	def __init__(self, cond: Node, true_branch: NodeCompound, false_branch: NodeCompound):
		self.cond = cond
		self.true_branch = true_branch
		self.false_branch = false_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.true_branch}, {self.false_branch}")
	
	def Accept(self, visitor):
		return visitor.VisitIfElse(self)


class NodeWhileElse(Node):
	def __init__(self, cond: Node, loop_body: NodeCompound, else_branch: NodeCompound):
		self.cond = cond
		self.loop_body = loop_body
		self.else_branch = else_branch
	
	def __repr__(self):
		return self._GenStr(f"{self.cond}, {self.loop_body}, {self.else_branch}")
	
	def Accept(self, visitor):
		return visitor.VisitWhileElse(self)


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


class NodeNamedTuple(Node):
	def __init__(self, body: Node):
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitNamedTuple(self)


class NodeNamedStruct(Node):
	def __init__(self, body: Node):
		self.body = body
	
	def __repr__(self):
		return self._GenStr(f"{self.body}")
	
	def Accept(self, visitor):
		return visitor.VisitNamedStruct(self)


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
	
	def VisitLogicalOp(self, node: NodeLogicalOp) -> T:
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
