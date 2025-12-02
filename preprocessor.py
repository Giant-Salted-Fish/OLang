import lang_ast
from typing import override


class ApplyPostOrder(lang_ast.Visitor[lang_ast.Node]):
	@override
	def __init__(self, transformer: lang_ast.Visitor[lang_ast.Node]) -> None:
		self.transformer = transformer
	
	@override
	def VisitInt(self, node: lang_ast.NodeInt) -> lang_ast.Node:
		return self.transformer.VisitInt(node)
	
	@override
	def VisitLabel(self, node: lang_ast.NodeLabel) -> lang_ast.Node:
		return self.transformer.VisitLabel(node)
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> lang_ast.Node:
		return self.transformer.VisitStr(node)
	
	@override
	def VisitBool(self, node: lang_ast.NodeBool) -> lang_ast.Node:
		return self.transformer.VisitBool(node)
	
	@override
	def VisitCompound(self, node: lang_ast.NodeCompound) -> lang_ast.Node:
		node.nodes = tuple(map(lambda n: n.Accept(self), node.nodes))
		return self.transformer.VisitCompound(node)
	
	@override
	def VisitDecl(self, node: lang_ast.NodeDecl) -> lang_ast.Node:
		node.var = node.var.Accept(self)
		return self.transformer.VisitDecl(node)
	
	@override
	def VisitAssign(self, node: lang_ast.NodeAssign) -> lang_ast.Node:
		node.var = node.var.Accept(self)
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitAssign(node)
	
	@override
	def VisitFunc(self, node: lang_ast.NodeFunc) -> lang_ast.Node:
		node.param = node.param.Accept(self)
		node.body = node.body.Accept(self)  # type: ignore
		return self.transformer.VisitFunc(node)
	
	@override
	def VisitTemplate(self, node: lang_ast.NodeTemplate) -> lang_ast.Node:
		node.param = node.param.Accept(self)
		node.body = node.body.Accept(self)  # type: ignore
		return self.transformer.VisitTemplate(node)
	
	@override
	def VisitApply(self, node: lang_ast.NodeApply) -> lang_ast.Node:
		node.arg = node.arg.Accept(self)
		node.func = node.func.Accept(self)
		return self.transformer.VisitApply(node)
	
	@override
	def VisitUnion(self, node: lang_ast.NodeUnion) -> lang_ast.Node:
		node.nodes = tuple(map(lambda n: n.Accept(self), node.nodes))
		return self.transformer.VisitUnion(node)
	
	@override
	def VisitTuple(self, node: lang_ast.NodeTuple) -> lang_ast.Node:
		node.nodes = tuple(map(lambda n: n.Accept(self), node.nodes))
		return self.transformer.VisitTuple(node)
	
	@override
	def VisitStruct(self, node: lang_ast.NodeStruct) -> lang_ast.Node:
		node.fields = tuple(map(lambda n: n.Accept(self), node.fields))
		return self.transformer.VisitStruct(node)
	
	@override
	def VisitLogicalOp(self, node: lang_ast.NodeLogicalOp) -> lang_ast.Node:
		node.lhs = node.lhs.Accept(self)
		node.rhs = node.rhs.Accept(self)
		return self.transformer.VisitLogicalOp(node)
	
	@override
	def VisitBinaryOp(self, node: lang_ast.NodeBinaryOp) -> lang_ast.Node:
		node.lhs = node.lhs.Accept(self)
		node.rhs = node.rhs.Accept(self)
		return self.transformer.VisitBinaryOp(node)
	
	@override
	def VisitUnaryOp(self, node: lang_ast.NodeUnaryOp) -> lang_ast.Node:
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitUnaryOp(node)
	
	@override
	def VisitAccess(self, node: lang_ast.NodeAccess) -> lang_ast.Node:
		node.obj = node.obj.Accept(self)
		node.field = node.field.Accept(self)
		return self.transformer.VisitAccess(node)
	
	@override
	def VisitIndex(self, node: lang_ast.NodeIndex) -> lang_ast.Node:
		node.obj = node.obj.Accept(self)
		node.index = node.index.Accept(self)
		return self.transformer.VisitIndex(node)
	
	@override
	def VisitReturn(self, node: lang_ast.NodeReturn) -> lang_ast.Node:
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitReturn(node)
	
	@override
	def VisitBreak(self, node: lang_ast.NodeBreak) -> lang_ast.Node:
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitBreak(node)
	
	@override
	def VisitContinue(self, node: lang_ast.NodeContinue) -> lang_ast.Node:
		return self.transformer.VisitContinue(node)
	
	@override
	def VisitIfElse(self, node: lang_ast.NodeIfElse) -> lang_ast.Node:
		node.cond = node.cond.Accept(self)
		node.true_branch = node.true_branch.Accept(self)  # type: ignore
		node.false_branch = node.false_branch.Accept(self)  # type: ignore
		return self.transformer.VisitIfElse(node)
	
	@override
	def VisitWhileElse(self, node: lang_ast.NodeWhileElse) -> lang_ast.Node:
		node.cond = node.cond.Accept(self)
		node.loop_body = node.loop_body.Accept(self)  # type: ignore
		node.else_branch = node.else_branch.Accept(self)  # type: ignore
		return self.transformer.VisitWhileElse(node)
	
	@override
	def VisitForElse(self, node: lang_ast.NodeForElse) -> lang_ast.Node:
		node.iterable = node.iterable.Accept(self)
		node.var = node.var.Accept(self)
		node.loop_body = node.loop_body.Accept(self)  # type: ignore
		node.else_branch = node.else_branch.Accept(self)  # type: ignore
		return self.transformer.VisitForElse(node)
	
	@override
	def VisitNamedTuple(self, node: lang_ast.NodeNamedTuple) -> lang_ast.Node:
		node.body = node.body.Accept(self)
		return self.transformer.VisitNamedTuple(node)
	
	@override
	def VisitNamedStruct(self, node: lang_ast.NodeNamedStruct) -> lang_ast.Node:
		node.body = node.body.Accept(self)
		return self.transformer.VisitNamedStruct(node)



class IdentityProcessor(lang_ast.Visitor[lang_ast.Node]):
	@override
	def VisitInt(self, node: lang_ast.NodeInt) -> lang_ast.Node:
		return node
	
	@override
	def VisitLabel(self, node: lang_ast.NodeLabel) -> lang_ast.Node:
		return node
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> lang_ast.Node:
		return node
	
	@override
	def VisitBool(self, node: lang_ast.NodeBool) -> lang_ast.Node:
		return node
	
	@override
	def VisitCompound(self, node: lang_ast.NodeCompound) -> lang_ast.Node:
		return node
	
	@override
	def VisitDecl(self, node: lang_ast.NodeDecl) -> lang_ast.Node:
		return node
	
	@override
	def VisitAssign(self, node: lang_ast.NodeAssign) -> lang_ast.Node:
		return node
	
	@override
	def VisitFunc(self, node: lang_ast.NodeFunc) -> lang_ast.Node:
		return node
	
	@override
	def VisitTemplate(self, node: lang_ast.NodeTemplate) -> lang_ast.Node:
		return node
	
	@override
	def VisitApply(self, node: lang_ast.NodeApply) -> lang_ast.Node:
		return node
	
	@override
	def VisitUnion(self, node: lang_ast.NodeUnion) -> lang_ast.Node:
		return node
	
	@override
	def VisitTuple(self, node: lang_ast.NodeTuple) -> lang_ast.Node:
		return node
	
	@override
	def VisitStruct(self, node: lang_ast.NodeStruct) -> lang_ast.Node:
		return node
	
	@override
	def VisitLogicalOp(self, node: lang_ast.NodeLogicalOp) -> lang_ast.Node:
		return node
	
	@override
	def VisitBinaryOp(self, node: lang_ast.NodeBinaryOp) -> lang_ast.Node:
		return node
	
	@override
	def VisitUnaryOp(self, node: lang_ast.NodeUnaryOp) -> lang_ast.Node:
		return node
	
	@override
	def VisitAccess(self, node: lang_ast.NodeAccess) -> lang_ast.Node:
		return node
	
	@override
	def VisitIndex(self, node: lang_ast.NodeIndex) -> lang_ast.Node:
		return node
	
	@override
	def VisitReturn(self, node: lang_ast.NodeReturn) -> lang_ast.Node:
		return node
	
	@override
	def VisitBreak(self, node: lang_ast.NodeBreak) -> lang_ast.Node:
		return node
	
	@override
	def VisitContinue(self, node: lang_ast.NodeContinue) -> lang_ast.Node:
		return node
	
	@override
	def VisitIfElse(self, node: lang_ast.NodeIfElse) -> lang_ast.Node:
		return node
	
	@override
	def VisitWhileElse(self, node: lang_ast.NodeWhileElse) -> lang_ast.Node:
		return node
	
	@override
	def VisitForElse(self, node: lang_ast.NodeForElse) -> lang_ast.Node:
		return node
	
	@override
	def VisitNamedTuple(self, node: lang_ast.NodeNamedTuple) -> lang_ast.Node:
		return node
	
	@override
	def VisitNamedStruct(self, node: lang_ast.NodeNamedStruct) -> lang_ast.Node:
		return node


class IdAnnotationProcessor(IdentityProcessor):
	@override
	def __init__(self, annotation="id") -> None:
		self.annotation = annotation
	
	def CheckAnnotation(self, annotation: lang_ast.Node) -> bool:
		return isinstance(annotation, lang_ast.NodeLabel) and annotation.token.GetText() == self.annotation
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> lang_ast.Node:
		rest_attr = tuple(filter(lambda x: not self.CheckAnnotation(x), node.prefix))
		if len(rest_attr) == len(node.prefix):
			return node
		
		replace = lang_ast.NodeLabel(node.token)  # FIXME: Trim quotes
		replace.prefix = rest_attr
		replace.suffix = node.suffix
		return replace
