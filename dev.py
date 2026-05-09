import lang_spec
from lang_ast import (
	NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeCall, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)

import typing
if typing.TYPE_CHECKING:
	from typing import Any
	from collections.abc import Callable

def ignore(*args):
	return None

source_code = '''
,,,,
'''

SYNTAX_RULES = [
	('S', ('stmt_lst',), ignore),
	
	('stmt_lst', ('norm_stmt', 'stmt_lst'), ignore),
	('stmt_lst', ('last_stmt',), ignore),
	
	
	('norm_stmt', ('prefix*', 'if'), ignore),
	('norm_stmt', ('prefix*', 'if$-else norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'if$-case norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'if$-else-case norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'match norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'match$-else norm_stmt'), ignore),
	('norm_stmt', ('stmt', ';'), ignore),
	('norm_stmt', ('assign', ';'), ignore),
	('norm_stmt', (';',), ignore),
	
	
	('last_stmt', ('prefix*', 'if$-else last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-else-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'match last_stmt'), ignore),
	('last_stmt', ('prefix*', 'match$-else last_stmt'), ignore),
	('last_stmt', ('stmt',), ignore),
	('last_stmt', ('assign',), ignore),
	('last_stmt', (), ignore),
	
	
	('stmt', ('RETURN', 'expr'), ignore),
	('stmt', ('BREAK', 'expr'), ignore),
	('stmt', ('BREAK',), ignore),
	('stmt', ('CONTINUE',), ignore),
	
	
	('expr', ('assign',), ignore),
	('expr', ('..assign',), ignore),
	
	
	('assign', ('(let|union)', '=', 'expr'), ignore),
	('assign', ('(let|union)',), ignore),
	('..assign', ('..union', '=', 'expr'), ignore),
	('..assign', ('..union',), ignore),
	('(let|union)', ('prefix*', 'LET', '(union|..union)'), ignore),
	('(let|union)', ('union',), ignore),
	('(union|..union)', ('union',), ignore),
	('(union|..union)', ('..union',), ignore),
	
	
	('union', ('tuple',), ignore),
	('union', ('tuple', '|', 'union..'), ignore),
	('union', ('|', 'union..'), ignore),
	('..union', ('..tuple',), ignore),
	('..union', ('..tuple', '|', 'union..'), ignore),
	('union..', ('(tuple|..tuple)', '|', 'union..'), ignore),
	('union..', ('|', 'union..'), ignore),
	('union..', (), ignore),
	('(tuple|..tuple)', ('tuple',), ignore),
	('(tuple|..tuple)', ('..tuple',), ignore),
	
	
	('tuple', ('suffixed',), ignore),
	('tuple', ('suffixed', ',', 'tuple..'), ignore),
	('tuple', (',', 'tuple..',), ignore),
	('..tuple', ('..suffixed',), ignore),
	('..tuple', ('..suffixed', ',', 'tuple..'), ignore),
	('tuple..', ('(suffixed|..suffixed)', ',', 'tuple..'), ignore),
	('tuple..', (',', 'tuple..'), ignore),
	('tuple..', (), ignore),
	('(suffixed|..suffixed)', ('suffixed',), ignore),
	('(suffixed|..suffixed)', ('..suffixed',), ignore),
	
	
	('suffixed', ('lmbd', 'suffix*'), ignore),
	('..suffixed', ('..lmbd', 'suffix*'), ignore),
	('suffix*', ('suffix*', ':', '(lmbd|..lmbd)'), ignore),
	('suffix*', (), ignore),
	
	
	('lmbd', ('or', '->', '(lmbd|..lmbd)'), ignore),
	('lmbd', ('or',), ignore),
	('..lmbd', ('..or', '->', '(lmbd|..lmbd)'), ignore),
	('..lmbd', ('..or',), ignore),
	('(lmbd|..lmbd)', ('lmbd',), ignore),
	('(lmbd|..lmbd)', ('..lmbd',), ignore),
	
	
	("or", ("or", "||", "(and|..and)"), lambda lhs, OR, rhs: NodeLogicalOp(OR, lhs, rhs)),
	("or", ("and",), lambda x: x),
	("..or", ("..or", "||", "(and|..and)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..or", ("..and",), lambda x: x),
	("(and|..and)", ("and",), lambda x: x),
	("(and|..and)", ("..and",), lambda x: x),
	
	
	# and: eq (&& (eq|..eq))*
	("and", ("and", "&&", "(eq|..eq)"), lambda lhs, OP, rhs: NodeLogicalOp(OP, lhs, rhs)),
	("and", ("eq",), lambda x: x),
	("..and", ("..and", "&&", "(eq|..eq)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..and", ("..eq",), lambda x: x),
	("(eq|..eq)", ("eq",), lambda x: x),
	("(eq|..eq)", ("..eq",), lambda x: x),
	
	
	# ('expr', ('add',), ignore),
	# ('expr', ('..add',), ignore),
	("eq", ("eq", "(==|!=)", "(rel|..rel)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("eq", ("rel",), lambda x: x),
	("..eq", ("..eq", "(==|!=)", "(rel|..rel)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..eq", ("..rel",), lambda x: x),
	("(==|!=)", ("==",), lambda EQ: EQ),
	("(==|!=)", ("!=",), lambda NEQ: NEQ),
	("(rel|..rel)", ("rel",), lambda x: x),
	("(rel|..rel)", ("..rel",), lambda x: x),
	
	
	("rel", ("rel", "(<=|>=|<|>)", "(add|..add)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("rel", ("add",), lambda x: x),
	("..rel", ("..rel", "(<=|>=|<|>)", "(add|..add)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..rel", ("..add",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda GTE: GTE),
	("(<=|>=|<|>)", ("<=",), lambda LSE: LSE),
	("(<=|>=|<|>)", (">",), lambda GT: GT),
	("(<=|>=|<|>)", ("<",), lambda LS: LS),
	("(add|..add)", ("add",), lambda x: x),
	("(add|..add)", ("..add",), lambda x: x),
	
	
	("add", ("add", "(+|-)", "(mul|..mul)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("add", ("mul",), lambda x: x),
	("..add", ("..add", "(+|-)", "(mul|..mul)"), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	("..add", ("..mul",), lambda x: x),
	("(+|-)", ("+",), lambda PLUS: PLUS),
	("(+|-)", ("-",), lambda MINUS: MINUS),
	("(mul|..mul)", ("mul",), lambda x: x),
	("(mul|..mul)", ("..mul",), lambda x: x),
	
	
	('mul', ('mul', '(*|/|%)', '(unary|prefix* [^postfixed])'), ignore),
	('mul', ('unary',), ignore),
	('..mul', ('..mul', '(*|/|%)', '(unary|prefix* [^postfixed])'), ignore),
	('..mul', ('prefix*', '[^postfixed]'), ignore),
	("(*|/|%)", ("*",), lambda MUL: MUL),
	("(*|/|%)", ("/",), lambda DIV: DIV),
	("(*|/|%)", ("%",), lambda MOD: MOD),
	('(unary|prefix* [^postfixed])', ('unary',), ignore),
	('(unary|prefix* [^postfixed])', ('prefix*', '[^postfixed]'), ignore),  # TODO: What else can be here?
	
	
	('unary', ('(+|-|!|&|*)', '(unary|prefix* [^postfixed])'), ignore),
	('unary', ('(prefix* #[x])?', 'call'), ignore),
	('unary', ('prefixed',), ignore),
	("(+|-|!|&|*)", ("+",), lambda PLUS: PLUS),
	("(+|-|!|&|*)", ("-",), lambda MINUS: MINUS),
	("(+|-|!|&|*)", ("!",), lambda NOT: NOT),
	("(+|-|!|&|*)", ("&",), lambda AMPERSAND: AMPERSAND),
	("(+|-|!|&|*)", ("*",), lambda MUL: MUL),
	
	
	('call', ('postfixed+', 'prefix*', '#[', 'postfixed', ']', 'call'), ignore),
	('call', ('postfixed+',), ignore),
	('call', ('postfixed+', 'prefixed'), ignore),
	# TODO: decl can also be here.
	('call', ('postfixed+', 'prefix*', '[^postfixed]'), ignore),
	
	
	('if', ('(IF x x)$else', 'else'), ignore),
	('else', ('ELSE', 'prefix*', 'if'), ignore),
	('else', ('ELSE', '(prefix* #[x])?', 'postfixed'), ignore),
	
	
	('if$-else', ('(IF x)$postfixed', 'postfixed'), ignore),
	('if$-else', ('(IF x)$[^postfixed]', 'x$-?else'), ignore),
	('if$-else', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('if$-else prefix*', ('(IF x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('if$-else prefix*', ('(IF x)$[^postfixed]', 'x$-?else prefix*'), ignore),
	('if$-else prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	
	('if$-else (prefix* #[x])?', ('(IF x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x)$[^postfixed]', 'x$-?else (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	
	('if$-else norm_stmt', ('(IF x)$postfixed', 'postfixed', 'norm_stmt'), ignore),
	('if$-else norm_stmt', ('(IF x)$[^postfixed]', 'x$-?else norm_stmt'), ignore),
	('if$-else norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else norm_stmt'), ignore),
	
	('if$-else last_stmt', ('(IF x)$postfixed', 'postfixed', 'last_stmt'), ignore),
	('if$-else last_stmt', ('(IF x)$[^postfixed]', 'x$-?else last_stmt'), ignore),
	('if$-else last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else last_stmt'), ignore),
	
	
	('if$-case', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-case'), ignore),
	('if$-case prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-case prefix*'), ignore),
	('if$-case (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-case (prefix* #[x])?'), ignore),
	('if$-case norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-case norm_stmt'), ignore),
	('if$-case last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-case last_stmt'), ignore),
	
	
	('if$-else-case', ('(IF x)$[^postfixed]', 'x$-?else-case'), ignore),
	('if$-else-case', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else-case'), ignore),
	('if$-else-case prefix*', ('(IF x)$[^postfixed]', 'x$-?else-case prefix*'), ignore),
	('if$-else-case prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else-case prefix*'), ignore),
	('if$-else-case (prefix* #[x])?', ('(IF x)$[^postfixed]', 'x$-?else-case (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else-case (prefix* #[x])?'), ignore),
	('if$-else-case norm_stmt', ('(IF x)$[^postfixed]', 'x$-?else-case norm_stmt'), ignore),
	('if$-else-case norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else-case norm_stmt'), ignore),
	('if$-else-case last_stmt', ('(IF x)$[^postfixed]', 'x$-?else-case last_stmt'), ignore),
	('if$-else-case last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else-case last_stmt'), ignore),
	
	
	('match', ('MATCH', '(prefix* #[x])?', 'postfixed'), ignore),
	('match', ('MATCH', 'prefix*', 'x$-?case'), ignore),
	('match', ('(MATCH x)$case..', 'case..'), ignore),
	('case..', ('(CASE x x)$case..', 'case..'), ignore),
	('case..', ('(CASE x)$postfixed', 'postfixed'), ignore),
	('case..', ('(CASE x)$[^postfixed]', 'x$-?case'), ignore),
	
	('match prefix*', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('match prefix*', ('MATCH', 'prefix*', 'x$-?case prefix*'), ignore),
	('match prefix*', ('(MATCH x)$case..', 'case.. prefix*'), ignore),
	('case.. prefix*', ('(CASE x x)$case..', 'case.. prefix*'), ignore),
	('case.. prefix*', ('(CASE x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('case.. prefix*', ('(CASE x)$[^postfixed]', 'x$-?case prefix*'), ignore),
	
	('match (prefix* #[x])?', ('MATCH', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('MATCH', 'prefix*', 'x$-?case (prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('(MATCH x)$case..', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('(CASE x x)$case..', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('(CASE x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('(CASE x)$[^postfixed]', 'x$-?case (prefix* #[x])?'), ignore),
	
	('match norm_stmt', ('MATCH', '(prefix* #[x])?', 'postfixed', 'norm_stmt'), ignore),
	('match norm_stmt', ('MATCH', 'prefix*', 'x$-?case norm_stmt'), ignore),
	('match norm_stmt', ('(MATCH x)$case..', 'case.. norm_stmt'), ignore),
	('case.. norm_stmt', ('(CASE x x)$case..', 'case.. norm_stmt'), ignore),
	('case.. norm_stmt', ('(CASE x)$postfixed', 'postfixed', 'norm_stmt'), ignore),
	('case.. norm_stmt', ('(CASE x)$[^postfixed]', 'x$-?case norm_stmt'), ignore),
	
	('match last_stmt', ('MATCH', '(prefix* #[x])?', 'postfixed', 'last_stmt'), ignore),
	('match last_stmt', ('MATCH', 'prefix*', 'x$-?case last_stmt'), ignore),
	('match last_stmt', ('(MATCH x)$case..', 'case.. last_stmt'), ignore),
	('case.. last_stmt', ('(CASE x x)$case..', 'case.. last_stmt'), ignore),
	('case.. last_stmt', ('(CASE x)$postfixed', 'postfixed', 'last_stmt'), ignore),
	('case.. last_stmt', ('(CASE x)$[^postfixed]', 'x$-?case last_stmt'), ignore),
	
	
	('match$-else', ('MATCH', 'prefix*', 'x$-else-?case'), ignore),
	('match$-else', ('(MATCH x)$case..', 'case..$-else'), ignore),
	('case..$-else', ('(CASE x x)$case..', 'case..$-else'), ignore),
	('case..$-else', ('(CASE x)$[^postfixed]', 'x$-else-?case'), ignore),
	
	('match$-else prefix*', ('MATCH', 'prefix*', 'x$-else-?case prefix*'), ignore),
	('match$-else prefix*', ('(MATCH x)$case..', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('(CASE x x)$case..', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('(CASE x)$[^postfixed]', 'x$-else-?case prefix*'), ignore),
	
	('match$-else (prefix* #[x])?', ('MATCH', 'prefix*', 'x$-else-?case (prefix* #[x])?'), ignore),
	('match$-else (prefix* #[x])?', ('(MATCH x)$case..', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('(CASE x x)$case..', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('(CASE x)$[^postfixed]', 'x$-else-?case (prefix* #[x])?'), ignore),
	
	('match$-else norm_stmt', ('MATCH', 'prefix*', 'x$-else-?case norm_stmt'), ignore),
	('match$-else norm_stmt', ('(MATCH x)$case..', 'case..$-else norm_stmt'), ignore),
	('case..$-else norm_stmt', ('(CASE x x)$case..', 'case..$-else norm_stmt'), ignore),
	('case..$-else norm_stmt', ('(CASE x)$[^postfixed]', 'x$-else-?case norm_stmt'), ignore),
	
	('match$-else last_stmt', ('MATCH', 'prefix*', 'x$-else-?case last_stmt'), ignore),
	('match$-else last_stmt', ('(MATCH x)$case..', 'case..$-else last_stmt'), ignore),
	('case..$-else last_stmt', ('(CASE x x)$case..', 'case..$-else last_stmt'), ignore),
	('case..$-else last_stmt', ('(CASE x)$[^postfixed]', 'x$-else-?case last_stmt'), ignore),
	
	
	('(IF x x)$else', ('(IF x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('(IF x x)$else', ('(IF x)$[^postfixed]', 'x$-?case prefix*'), ignore),
	
	('(IF x)$postfixed', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('(IF x)$postfixed', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?'), ignore),
	
	('(IF x)$[^postfixed]', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(IF x)$[^postfixed]', ('IF', 'prefix*', '[^postfixed] prefix*'), ignore),
	
	('(MATCH x)$case..', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(MATCH x)$case..', ('MATCH', 'prefix*', 'x$-?else prefix*'), ignore),
	
	('(CASE x x)$case..', ('(CASE x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('(CASE x x)$case..', ('(CASE x)$[^postfixed]', 'x$-?else prefix*'), ignore),
	
	('(CASE x)$postfixed', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('(CASE x)$postfixed', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?'), ignore),
	
	('(CASE x)$[^postfixed]', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(CASE x)$[^postfixed]', ('CASE', 'prefix*', '[^postfixed] prefix*'), ignore),
	
	
	('[^postfixed]', ('if',), ignore),
	('[^postfixed]', ('if$-else',), ignore),
	('[^postfixed]', ('if$-case',), ignore),
	('[^postfixed]', ('if$-else-case',), ignore),
	('[^postfixed]', ('match',), ignore),
	('[^postfixed]', ('match$-else',), ignore),
	
	('[^postfixed] prefix*', ('if', 'prefix*'), ignore),
	('[^postfixed] prefix*', ('if$-else prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-case prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-else-case prefix*',), ignore),
	('[^postfixed] prefix*', ('match prefix*',), ignore),
	('[^postfixed] prefix*', ('match$-else prefix*',), ignore),
	
	('[^postfixed] (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-?else', ('if',), ignore),
	('x$-?else', ('if$-else',), ignore),
	
	('x$-?else prefix*', ('if', 'prefix*'), ignore),
	('x$-?else prefix*', ('if$-else prefix*',), ignore),
	
	('x$-?else (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('x$-?else (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	
	('x$-?else norm_stmt', ('if', 'norm_stmt'), ignore),
	('x$-?else norm_stmt', ('if$-else norm_stmt',), ignore),
	
	('x$-?else last_stmt', ('if', 'last_stmt'), ignore),
	('x$-?else last_stmt', ('if$-else last_stmt',), ignore),
	
	('x$-case', ('if$-case',), ignore),
	('x$-case', ('match',), ignore),
	
	('x$-case prefix*', ('if$-case prefix*',), ignore),
	('x$-case prefix*', ('match prefix*',), ignore),
	
	('x$-case (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('x$-case (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	('x$-case norm_stmt', ('if$-case norm_stmt',), ignore),
	('x$-case norm_stmt', ('match norm_stmt',), ignore),
	
	('x$-case last_stmt', ('if$-case last_stmt',), ignore),
	('x$-case last_stmt', ('match last_stmt',), ignore),
	
	('x$-?case', ('if',), ignore),
	('x$-?case', ('if$-case',), ignore),
	('x$-?case', ('match',), ignore),
	
	('x$-?case prefix*', ('if', 'prefix*'), ignore),
	('x$-?case prefix*', ('if$-case prefix*',), ignore),
	('x$-?case prefix*', ('match prefix*',), ignore),
	
	('x$-?case (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('x$-?case (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('x$-?case (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	('x$-?case norm_stmt', ('if', 'norm_stmt'), ignore),
	('x$-?case norm_stmt', ('if$-case norm_stmt',), ignore),
	('x$-?case norm_stmt', ('match norm_stmt',), ignore),
	
	('x$-?case last_stmt', ('if', 'last_stmt'), ignore),
	('x$-?case last_stmt', ('if$-case last_stmt',), ignore),
	('x$-?case last_stmt', ('match last_stmt',), ignore),
	
	('x$-else-case', ('if$-else-case',), ignore),
	('x$-else-case', ('match$-else',), ignore),
	
	('x$-else-case prefix*', ('if$-else-case prefix*',), ignore),
	('x$-else-case prefix*', ('match$-else prefix*',), ignore),
	
	('x$-else-case (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('x$-else-case (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-else-case norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('x$-else-case norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('x$-else-case last_stmt', ('if$-else-case last_stmt',), ignore),
	('x$-else-case last_stmt', ('match$-else last_stmt',), ignore),
	
	('x$-else-?case', ('if$-else',), ignore),
	('x$-else-?case', ('if$-else-case',), ignore),
	('x$-else-?case', ('match$-else',), ignore),
	
	('x$-else-?case prefix*', ('if$-else prefix*',), ignore),
	('x$-else-?case prefix*', ('if$-else-case prefix*',), ignore),
	('x$-else-?case prefix*', ('match$-else prefix*',), ignore),
	
	('x$-else-?case (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('x$-else-?case (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('x$-else-?case (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-else-?case norm_stmt', ('if$-else norm_stmt',), ignore),
	('x$-else-?case norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('x$-else-?case norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('x$-else-?case last_stmt', ('if$-else last_stmt',), ignore),
	('x$-else-?case last_stmt', ('if$-else-case last_stmt',), ignore),
	('x$-else-?case last_stmt', ('match$-else last_stmt',), ignore),
	
	('x$-?else-case', ('if$-case',), ignore),
	('x$-?else-case', ('if$-else-case',), ignore),
	('x$-?else-case', ('match',), ignore),
	('x$-?else-case', ('match$-else',), ignore),
	
	('x$-?else-case prefix*', ('if$-case prefix*',), ignore),
	('x$-?else-case prefix*', ('if$-else-case prefix*',), ignore),
	('x$-?else-case prefix*', ('match prefix*',), ignore),
	('x$-?else-case prefix*', ('match$-else prefix*',), ignore),
	
	('x$-?else-case (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-?else-case norm_stmt', ('if$-case norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('match norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('x$-?else-case last_stmt', ('if$-case last_stmt',), ignore),
	('x$-?else-case last_stmt', ('if$-else-case last_stmt',), ignore),
	('x$-?else-case last_stmt', ('match last_stmt',), ignore),
	('x$-?else-case last_stmt', ('match$-else last_stmt',), ignore),
	
	
	('(prefix* #[x])?', ('prefix*', '#[', 'postfixed', ']'), ignore),
	('(prefix* #[x])?', (), ignore),
	
	('prefixed', ('prefix*', '@', 'postfixed+', 'postfixed'), ignore),
	('prefix*', ('prefix*', '@', 'postfixed+'), ignore),
	('prefix*', ('prefix*', '#[', 'postfixed', ']'), ignore),
	('prefix*', (), ignore),
	('postfixed+', ('postfixed+', 'postfixed'), ignore),
	('postfixed+', ('postfixed',), ignore),
	
	('postfixed', ('prim', 'postfix*'), ignore),
	('postfix*', ('postfix*', 'postfix'), ignore),
	('postfix*', (), ignore),
	('postfix', ('.', 'prim'), ignore),
	
	('prim', ('INT',), NodeInt),
	('prim', ('IDENT',), NodeInt),
]

TOKEN_TYPES = [
	('MATCH', r'match'),
	('CASE', r'case'),
	('DEFAULT', r'default'),
	*lang_spec.TOKEN_TYPES,
]
TERMINALS = set(t for t, _ in TOKEN_TYPES)

if __name__ == '__main__':
	from scanner import Scanner
	from parser import Syntax, Production
	import lang_ast
	
	with open('result.txt', 'w') as f:
		def write(obj: Any = ''):
			print(obj)
			msg = str(obj)
			f.write(f'{msg}\n')
		
		scanner = Scanner[str].Build(TOKEN_TYPES)
		syntax = Syntax[str, str, lang_ast.Node].Build([Production(*p) for p in SYNTAX_RULES], TERMINALS.__contains__)
		parser = syntax.BuildLR1Parser()
		write('===== LR(1) Parser State =====')
		write(parser)
		write()
		
		ast = parser.Parse(scanner.Tokenize(source_code))
		write('===== Reproduced Source Code (May not be 100%% correct) =====')
		# write('\n'.join(ast.Accept(ToOLangCode('    '))))
		write()
		write(ast)
