from typing import override
import lang_ast


class ToOLangCode(lang_ast.Visitor[list[str]]):
	"""
	Convert given AST back to corresponding OLang code.
	"""
	
	@override
	def __init__(self, indent="    ") -> None:
		self._indent = indent
	
	@override
	def VisitInt(self, node: lang_ast.NodeInt) -> list[str]:
		lines = [node.token.GetText()]
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitLabel(self, node: lang_ast.NodeLabel) -> list[str]:
		lines = [node.token.GetText()]
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitStr(self, node: lang_ast.NodeStr) -> list[str]:
		lines = [node.token.GetText()]
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitBool(self, node: lang_ast.NodeBool) -> list[str]:
		lines = [node.token.GetText()]
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitCompound(self, node: lang_ast.NodeCompound) -> list[str]:
		lines = [line for n in node.nodes for line in self._SuffixText(";", n.Accept(self))]
		lines = self._EncloseText("{", "}", lines, force_new_line=True)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitDecl(self, node: lang_ast.NodeDecl) -> list[str]:
		lines = node.var.Accept(self)
		lines = self._PrefixText("let ", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitAssign(self, node: lang_ast.NodeAssign) -> list[str]:
		var = node.var.Accept(self)
		expr = node.expr.Accept(self)
		lines = self._JoinText(" = ", var, expr)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitFunc(self, node: lang_ast.NodeFunc) -> list[str]:
		param = node.param.Accept(self)
		body = node.body.Accept(self)
		lines = self._JoinText(" ", param, body)
		lines[0] = f"fn {lines[0]}"
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitTemplate(self, node: lang_ast.NodeTemplate) -> list[str]:
		param = node.param.Accept(self)
		body = node.body.Accept(self)
		lines = self._JoinText(" #> ", param, body)
		lines[0] = f"template {lines[0]}"
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitCall(self, node: lang_ast.NodeCall) -> list[str]:
		func = node.func.Accept(self)
		arg = node.arg.Accept(self)
		lines = self._JoinText(" ", func, arg)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitUnion(self, node: lang_ast.NodeUnion) -> list[str]:
		elements = [n.Accept(self) for n in node.nodes]
		if all(len(lines) == 1 for lines in elements):
			lines = ["|".join(lines[0] for lines in elements)]
			if len(elements) == 1:
				lines[0] += "|"
		else:
			lines = [f"{self._indent}{line}" for lines in elements for line in self._SuffixText("|", lines)]
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitTuple(self, node: lang_ast.NodeTuple) -> list[str]:
		elements = [n.Accept(self) for n in node.nodes]
		if all(len(lines) == 1 for lines in elements):
			lines = [", ".join(lines[0] for lines in elements)]
			if len(elements) == 1:
				lines[0] += ","
		else:
			lines = [f"{self._indent}{line}" for lines in elements for line in self._SuffixText(",", lines)]
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitStruct(self, node: lang_ast.NodeStruct) -> list[str]:
		elements = [field.Accept(self) for field in node.fields]
		if all(len(lines) == 1 for lines in elements) and sum(map(lambda lines: len(lines[0]), elements)) <= 100:
			lines = ["; ".join(lines[0] for lines in elements)]
		else:
			lines = [line for lines in elements for line in self._SuffixText(";", lines)]
		lines = self._EncloseText(".{", "}", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitLogicalOp(self, node: lang_ast.NodeLogicalOp) -> list[str]:
		lhs = node.lhs.Accept(self)
		rhs = node.rhs.Accept(self)
		lines = self._JoinText(f" {node.op.GetText()} ", lhs, rhs)
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitBinaryOp(self, node: lang_ast.NodeBinaryOp) -> list[str]:
		lhs = node.lhs.Accept(self)
		rhs = node.rhs.Accept(self)
		lines = self._JoinText(f" {node.op.GetText()} ", lhs, rhs)
		lines = self._EncloseText("(", ")", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitUnaryOp(self, node: lang_ast.NodeUnaryOp) -> list[str]:
		val = node.expr.Accept(self)
		lines = [f"{node.op.GetText()}{val[0]}", *val[1:]]
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitAccess(self, node: lang_ast.NodeAccess) -> list[str]:
		obj = node.obj.Accept(self)
		field = node.field.Accept(self)
		lines = self._JoinText(". " if field[0][0] in ('(', '[', '{') else ".", obj, field)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitIndex(self, node: lang_ast.NodeIndex) -> list[str]:
		val = node.obj.Accept(self)
		index = node.index.Accept(self)
		lines = self._JoinText("", val, self._EncloseText("[", "]", index))
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitReturn(self, node: lang_ast.NodeReturn) -> list[str]:
		expr = node.expr.Accept(self)
		lines = self._JoinText(" ", ["return"], expr)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitBreak(self, node: lang_ast.NodeBreak) -> list[str]:
		expr = node.expr.Accept(self)
		lines = self._JoinText(" ", ["break"], expr)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitContinue(self, node: lang_ast.NodeContinue) -> list[str]:
		return self._AppendAttrText(node, ["continue"])
	
	@override
	def VisitIfElse(self, node: lang_ast.NodeIfElse) -> list[str]:
		cond = node.cond.Accept(self)
		true_branch = node.true_branch.Accept(self)
		false_branch = node.false_branch.Accept(self)
		lines = self._JoinText(" ", ["if"], cond, true_branch, ["else"], false_branch)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitWhileElse(self, node: lang_ast.NodeWhileElse) -> list[str]:
		cond = node.cond.Accept(self)
		loop_body = node.loop_body.Accept(self)
		else_branch = node.else_branch.Accept(self)
		lines = self._JoinText(" ", ["while"], cond, loop_body, ["else"], else_branch)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitForElse(self, node: lang_ast.NodeForElse) -> list[str]:
		iterable = node.iterable.Accept(self)
		var = node.var.Accept(self)
		loop_body = node.loop_body.Accept(self)
		else_branch = node.else_branch.Accept(self)
		lines = self._JoinText(" ", ["for"], iterable, var, loop_body, ["else"], else_branch)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitNamedTuple(self, node: lang_ast.NodeNamedTuple) -> list[str]:
		lines = node.body.Accept(self)
		lines = self._PrefixText("tuple ", lines)
		return self._AppendAttrText(node, lines)
	
	@override
	def VisitNamedStruct(self, node: lang_ast.NodeNamedStruct) -> list[str]:
		lines = node.body.Accept(self)
		lines = self._PrefixText("struct ", lines)
		return self._AppendAttrText(node, lines)
	
	def _AppendAttrText(self, node: lang_ast.Node, lines: list[str]) -> list[str]:
		text = self._GenAttrText("@", node.prefix)
		if len(text) == 1:
			text = self._JoinText(" ", text, lines)
		else:
			text += lines
		
		suffix = self._GenAttrText(":", node.suffix)
		if len(suffix) == 1:
			text = self._JoinText(" ", text, suffix)
		else:
			text += suffix
		
		return text
	
	def _GenAttrText(self, prefix: str, attrs: tuple[lang_ast.Node, ...]) -> list[str]:
		allAttrLines = [node.Accept(self) for node in attrs]
		assert all(len(lines) > 0 for lines in allAttrLines)
		return [line for lines in allAttrLines for line in self._PrefixText(prefix, lines)]
	
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
	
	def _EncloseText(self, left: str, right: str, text: list[str], force_new_line=False) -> list[str]:
		def Block():
			return [left, *(f"{self._indent}{line}" for line in text), right]
		
		if force_new_line:
			return Block()
		
		match len(text):
			case 0:
				return [left + right]
			case 1:
				return [left + text[0] + right]
			case _:
				return Block()
	
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
