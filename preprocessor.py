import lang_ast
from typing import override


class ApplyPostOrder(lang_ast.Visitor[lang_ast.Node]):
	def __init__(self, transformer: lang_ast.Visitor[lang_ast.Node]):
		self.transformer = transformer
	
	@override
	def VisitInt(self, node):
		return self.transformer.VisitInt(node)
	
	@override
	def VisitLabel(self, node):
		return self.transformer.VisitLabel(node)
	
	@override
	def VisitStr(self, node):
		return self.transformer.VisitStr(node)
	
	@override
	def VisitBool(self, node):
		return self.transformer.VisitBool(node)
	
	@override
	def VisitCompound(self, node):
		node.nodes = tuple(map(lambda n: n.Accept(self), node.nodes))
		return self.transformer.VisitCompound(node)
	
	@override
	def VisitDecl(self, node):
		node.var = node.var.Accept(self)
		return self.transformer.VisitDecl(node)
	
	@override
	def VisitAssign(self, node):
		node.var = node.var.Accept(self)
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitAssign(node)
	
	@override
	def VisitFunc(self, node):
		node.param = node.param.Accept(self)
		node.body = node.body.Accept(self)  # type: ignore
		return self.transformer.VisitFunc(node)
	
	@override
	def VisitTemplate(self, node):
		node.param = node.param.Accept(self)
		node.body = node.body.Accept(self)  # type: ignore
		return self.transformer.VisitTemplate(node)
	
	@override
	def VisitApply(self, node):
		node.arg = node.arg.Accept(self)
		node.func = node.func.Accept(self)
		return self.transformer.VisitApply(node)
	
	@override
	def VisitUnion(self, node):
		node.nodes = tuple(map(lambda n: n.Accept(self), node.nodes))
		return self.transformer.VisitUnion(node)
	
	@override
	def VisitTuple(self, node):
		node.nodes = tuple(map(lambda n: n.Accept(self), node.nodes))
		return self.transformer.VisitTuple(node)
	
	@override
	def VisitStruct(self, node):
		node.fields = tuple(map(lambda n: n.Accept(self), node.fields))
		return self.transformer.VisitStruct(node)
	
	@override
	def VisitLogicalOp(self, node):
		node.lhs = node.lhs.Accept(self)
		node.rhs = node.rhs.Accept(self)
		return self.transformer.VisitLogicalOp(node)
	
	@override
	def VisitBinaryOp(self, node):
		node.lhs = node.lhs.Accept(self)
		node.rhs = node.rhs.Accept(self)
		return self.transformer.VisitBinaryOp(node)
	
	@override
	def VisitUnaryOp(self, node):
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitUnaryOp(node)
	
	@override
	def VisitAccess(self, node):
		node.obj = node.obj.Accept(self)
		node.field = node.field.Accept(self)
		return self.transformer.VisitAccess(node)
	
	@override
	def VisitIndex(self, node):
		node.obj = node.obj.Accept(self)
		node.index = node.index.Accept(self)
		return self.transformer.VisitIndex(node)
	
	@override
	def VisitReturn(self, node):
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitReturn(node)
	
	@override
	def VisitBreak(self, node):
		node.expr = node.expr.Accept(self)
		return self.transformer.VisitBreak(node)
	
	@override
	def VisitContinue(self, node):
		return self.transformer.VisitContinue(node)
	
	@override
	def VisitIfElse(self, node):
		node.cond = node.cond.Accept(self)
		node.true_branch = node.true_branch.Accept(self)  # type: ignore
		node.false_branch = node.false_branch.Accept(self)  # type: ignore
		return self.transformer.VisitIfElse(node)
	
	@override
	def VisitWhileElse(self, node):
		node.cond = node.cond.Accept(self)
		node.loop_body = node.loop_body.Accept(self)  # type: ignore
		node.else_branch = node.else_branch.Accept(self)  # type: ignore
		return self.transformer.VisitWhileElse(node)
	
	@override
	def VisitForElse(self, node):
		node.iterable = node.iterable.Accept(self)
		node.var = node.var.Accept(self)
		node.loop_body = node.loop_body.Accept(self)  # type: ignore
		node.else_branch = node.else_branch.Accept(self)  # type: ignore
		return self.transformer.VisitForElse(node)
	
	@override
	def VisitNamedTuple(self, node):
		node.body = node.body.Accept(self)
		return self.transformer.VisitNamedTuple(node)
	
	@override
	def VisitNamedStruct(self, node):
		node.body = node.body.Accept(self)
		return self.transformer.VisitNamedStruct(node)



class IdentityProcessor(lang_ast.Visitor[lang_ast.Node]):
	@override
	def VisitInt(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitLabel(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitStr(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitBool(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitCompound(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitDecl(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitAssign(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitFunc(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitTemplate(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitApply(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitUnion(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitTuple(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitStruct(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitLogicalOp(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitBinaryOp(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitUnaryOp(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitAccess(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitIndex(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitReturn(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitBreak(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitContinue(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitIfElse(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitWhileElse(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitForElse(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitNamedTuple(self, node) -> lang_ast.Node:
		return node
	
	@override
	def VisitNamedStruct(self, node) -> lang_ast.Node:
		return node


class IdAnnotationProcessor(IdentityProcessor):
	def __init__(self, annotation="id"):
		self.annotation = annotation
	
	def CheckAnnotation(self, annotation: lang_ast.Node):
		return isinstance(annotation, lang_ast.NodeLabel) and annotation.token.GetText() == self.annotation
	
	@override
	def VisitStr(self, node) -> lang_ast.Node:
		rest_attr = tuple(filter(lambda x: not self.CheckAnnotation(x), node.prefix))
		if len(rest_attr) == len(node.prefix):
			return node
		
		replace = lang_ast.NodeLabel(node.token)  # FIXME: Trim quotes
		replace.prefix = rest_attr
		replace.suffix = node.suffix
		return replace
