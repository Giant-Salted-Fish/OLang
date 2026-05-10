import lang_spec
from lang_ast import (
	NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeCall, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)
from typing import Any

import typing
if typing.TYPE_CHECKING:
	from collections.abc import Callable

def ignore(*args):
	return None

source_code = '''
(,a)
'''

SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable]] = [
	('S', ('stmt_lst',), ignore),
	
	('stmt_lst', ('norm_stmt', 'stmt_lst'), ignore),
	('stmt_lst', ('last_stmt',), ignore),
	
	
	('norm_stmt', ('prefix*', 'fn_def'), ignore),
	('norm_stmt', ('prefix*', 'fn_def$-else norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'fn_def$-case norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'fn_def$-else-case norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'if'), ignore),
	('norm_stmt', ('prefix*', 'if$-else norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'if$-case norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'if$-else-case norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'match norm_stmt'), ignore),
	('norm_stmt', ('prefix*', 'match$-else norm_stmt'), ignore),
	('norm_stmt', ('stmt', ';'), ignore),
	('norm_stmt', ('assign', ';'), ignore),
	('norm_stmt', (';',), ignore),
	
	
	('last_stmt', ('prefix*', 'fn_def$-else last_stmt'), ignore),
	('last_stmt', ('prefix*', 'fn_def$-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'fn_def$-else-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-else last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-else-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'match last_stmt'), ignore),
	('last_stmt', ('prefix*', 'match$-else last_stmt'), ignore),
	('last_stmt', ('stmt',), ignore),
	('last_stmt', ('assign',), ignore),
	('last_stmt', (), ignore),
	
	
	('stmt', ('prefix*', 'RETURN', 'expr'), ignore),
	('stmt', ('prefix*', 'BREAK', 'expr'), ignore),
	('stmt', ('prefix*', 'BREAK',), ignore),
	('stmt', ('prefix*', 'CONTINUE',), ignore),
	
	
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
	('union..', ('union..', '|', '(tuple|..tuple)'), ignore),
	('union..', ('union..', '|'), ignore),
	('union..', ('(tuple|..tuple)',), ignore),
	('union..', (), ignore),
	('(tuple|..tuple)', ('tuple',), ignore),
	('(tuple|..tuple)', ('..tuple',), ignore),
	
	
	('tuple', ('suffixed',), ignore),
	('tuple', ('suffixed', ',', 'tuple..'), ignore),
	('tuple', (',', 'tuple..',), ignore),
	('..tuple', ('..suffixed',), ignore),
	('..tuple', ('..suffixed', ',', 'tuple..'), ignore),
	('tuple..', ('tuple..', ',', '(suffixed|..suffixed)'), ignore),
	('tuple..', ('tuple..', ','), ignore),
	('tuple..', ('(suffixed|..suffixed)',), ignore),
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
	
	
	('mul', ('mul', '(*|/|%)', '(unary|def|prefix* ctrl_flow)'), ignore),
	('mul', ('unary',), ignore),
	('..mul', ('..mul', '(*|/|%)', '(unary|def|prefix* ctrl_flow)'), ignore),
	('..mul', ('prefix*', 'def',), ignore),
	('..mul', ('prefix*', 'ctrl_flow'), ignore),
	("(*|/|%)", ("*",), lambda MUL: MUL),
	("(*|/|%)", ("/",), lambda DIV: DIV),
	("(*|/|%)", ("%",), lambda MOD: MOD),
	('(unary|def|prefix* ctrl_flow)', ('unary',), ignore),
	('(unary|def|prefix* ctrl_flow)', ('prefix*', 'def'), ignore),
	('(unary|def|prefix* ctrl_flow)', ('prefix*', 'ctrl_flow'), ignore),  # TODO: What else can be here?
	
	
	('unary', ('(+|-|!|&|*)', '(unary|def|prefix* ctrl_flow)'), ignore),
	('unary', ('(prefix* #[x])?', 'call'), ignore),
	('unary', ('prefix*', 'fn_like'), ignore),
	('unary', ('prefixed',), ignore),
	("(+|-|!|&|*)", ("+",), lambda PLUS: PLUS),
	("(+|-|!|&|*)", ("-",), lambda MINUS: MINUS),
	("(+|-|!|&|*)", ("!",), lambda NOT: NOT),
	("(+|-|!|&|*)", ("&",), lambda AMPERSAND: AMPERSAND),
	("(+|-|!|&|*)", ("*",), lambda MUL: MUL),
	
	
	('call', ('postfixed+', 'prefix*', '#[', 'postfixed', ']', 'call'), ignore),
	('call', ('postfixed+',), ignore),
	('call', ('postfixed+', 'prefix*', 'def'), ignore),
	('call', ('postfixed+', 'prefix*', '[^postfixed]'), ignore),
	('call', ('postfixed+', 'prefixed'), ignore),
	
	
	('def', ('fn_def',), ignore),
	('def', ('fn_def$-else',), ignore),
	('def', ('fn_def$-case',), ignore),
	('def', ('fn_def$-else-case',), ignore),
	
	
	('fn_def', ('(FN x x)$postfixed', 'postfixed'), ignore),
	('fn_def', ('(FN x x)$[^postfixed]', 'x'), ignore),
	
	('fn_def$-else', ('(FN x x)$[^postfixed]', 'x$-else'), ignore),
	('fn_def$-else norm_stmt', ('(FN x x)$[^postfixed]', 'x$-else norm_stmt'), ignore),
	('fn_def$-else last_stmt', ('(FN x x)$[^postfixed]', 'x$-else last_stmt'), ignore),
	
	('fn_def$-case', ('(FN x x)$[^postfixed]', 'x$-case'), ignore),
	('fn_def$-case norm_stmt', ('(FN x x)$[^postfixed]', 'x$-case norm_stmt'), ignore),
	('fn_def$-case last_stmt', ('(FN x x)$[^postfixed]', 'x$-case last_stmt'), ignore),
	
	('fn_def$-else-case', ('(FN x x)$[^postfixed]', 'x$-else-case'), ignore),
	('fn_def$-else-case norm_stmt', ('(FN x x)$[^postfixed]', 'x$-else-case norm_stmt'), ignore),
	('fn_def$-else-case last_stmt', ('(FN x x)$[^postfixed]', 'x$-else-case last_stmt'), ignore),
	
	
	('fn', ('(FN x)$postfixed', 'postfixed'), ignore),
	('fn', ('(FN x)$[^postfixed]', 'x'), ignore),
	
	
	('fn$-else', ('(FN x)$[^postfixed]', 'x$-else'), ignore),
	('fn$-else prefix*', ('(FN x)$[^postfixed]', 'x$-else prefix*'), ignore),
	('fn$-else (prefix* #[x])?', ('(FN x)$[^postfixed]', 'x$-else (prefix* #[x])?'), ignore),
	('fn$-else norm_stmt', ('(FN x)$[^postfixed]', 'x$-else norm_stmt'), ignore),
	('fn$-else last_stmt', ('(FN x)$[^postfixed]', 'x$-else last_stmt'), ignore),
	
	
	('fn$-case', ('(FN x)$[^postfixed]', 'x$-case'), ignore),
	('fn$-case prefix*', ('(FN x)$[^postfixed]', 'x$-case prefix*'), ignore),
	('fn$-case (prefix* #[x])?', ('(FN x)$[^postfixed]', 'x$-case (prefix* #[x])?'), ignore),
	('fn$-case norm_stmt', ('(FN x)$[^postfixed]', 'x$-case norm_stmt'), ignore),
	('fn$-case last_stmt', ('(FN x)$[^postfixed]', 'x$-case last_stmt'), ignore),
	
	
	('fn$-else-case', ('(FN x)$[^postfixed]', 'x$-else-case'), ignore),
	('fn$-else-case prefix*', ('(FN x)$[^postfixed]', 'x$-else-case prefix*'), ignore),
	('fn$-else-case (prefix* #[x])?', ('(FN x)$[^postfixed]', 'x$-else-case (prefix* #[x])?'), ignore),
	('fn$-else-case norm_stmt', ('(FN x)$[^postfixed]', 'x$-else-case norm_stmt'), ignore),
	('fn$-else-case last_stmt', ('(FN x)$[^postfixed]', 'x$-else-case last_stmt'), ignore),
	
	
	('if', ('(IF x x)$else', 'else'), ignore),
	('else', ('ELSE', 'prefix*', 'x'), ignore),
	('else', ('ELSE', '(prefix* #[x])?', 'postfixed'), ignore),
	
	
	('if$-else', ('(IF x)$postfixed', 'postfixed'), ignore),
	('if$-else', ('(IF x)$[^postfixed]', 'x$-?else'), ignore),
	('if$-else', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else'), ignore),
	
	('if$-else prefix*', ('(IF x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('if$-else prefix*', ('(IF x)$[^postfixed]', 'x$-?else prefix*'), ignore),
	('if$-else prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else prefix*'), ignore),
	
	('if$-else (prefix* #[x])?', ('(IF x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x)$[^postfixed]', 'x$-?else (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else (prefix* #[x])?'), ignore),
	
	('if$-else norm_stmt', ('(IF x)$postfixed', 'postfixed', 'norm_stmt'), ignore),
	('if$-else norm_stmt', ('(IF x)$[^postfixed]', 'x$-?else norm_stmt'), ignore),
	('if$-else norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else norm_stmt'), ignore),
	
	('if$-else last_stmt', ('(IF x)$postfixed', 'postfixed', 'last_stmt'), ignore),
	('if$-else last_stmt', ('(IF x)$[^postfixed]', 'x$-?else last_stmt'), ignore),
	('if$-else last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'x$-else last_stmt'), ignore),
	
	
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
	
	
	('(FN x x)$postfixed', ('(FN x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('(FN x x)$postfixed', ('(FN x)$[^postfixed]', '[^postfixed] (prefix* #[x])?'), ignore),
	
	('(FN x x)$[^postfixed]', ('(FN x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('(FN x x)$[^postfixed]', ('(FN x)$[^postfixed]', '[^postfixed] prefix*'), ignore),
	
	
	('(FN x)$postfixed', ('FN', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('(FN x)$postfixed', ('FN', 'prefix*', '[^postfixed] (prefix* #[x])?'), ignore),
	
	('(FN x)$[^postfixed]', ('FN', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(FN x)$[^postfixed]', ('FN', 'prefix*', '[^postfixed] prefix*'), ignore),
	
	
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
	
	
	('[^postfixed]', ('fn_like',), ignore),
	('[^postfixed]', ('ctrl_flow',), ignore),
	
	('fn_like', ('fn',), ignore),
	('fn_like', ('fn$-else',), ignore),
	('fn_like', ('fn$-case',), ignore),
	('fn_like', ('fn$-else-case',), ignore),
	
	('ctrl_flow', ('if',), ignore),
	('ctrl_flow', ('if$-else',), ignore),
	('ctrl_flow', ('if$-case',), ignore),
	('ctrl_flow', ('if$-else-case',), ignore),
	('ctrl_flow', ('match',), ignore),
	('ctrl_flow', ('match$-else',), ignore),
	
	('[^postfixed] prefix*', ('fn', 'prefix*'), ignore),
	('[^postfixed] prefix*', ('fn$-else prefix*',), ignore),
	('[^postfixed] prefix*', ('fn$-case prefix*',), ignore),
	('[^postfixed] prefix*', ('fn$-else-case prefix*',), ignore),
	('[^postfixed] prefix*', ('if', 'prefix*'), ignore),
	('[^postfixed] prefix*', ('if$-else prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-case prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-else-case prefix*',), ignore),
	('[^postfixed] prefix*', ('match prefix*',), ignore),
	('[^postfixed] prefix*', ('match$-else prefix*',), ignore),
	
	('[^postfixed] (prefix* #[x])?', ('fn', '(prefix* #[x])?'), ignore),
	('[^postfixed] (prefix* #[x])?', ('fn$-else (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('fn$-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('fn$-else-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x', ('fn',), ignore),
	('x', ('if',), ignore),
	
	('x$-else', ('fn$-else',), ignore),
	('x$-else', ('if$-else',), ignore),
	
	('x$-else prefix*', ('fn$-else prefix*',), ignore),
	('x$-else prefix*', ('if$-else prefix*',), ignore),
	
	('x$-else (prefix* #[x])?', ('fn$-else (prefix* #[x])?',), ignore),
	('x$-else (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	
	('x$-else norm_stmt', ('fn$-else norm_stmt',), ignore),
	('x$-else norm_stmt', ('if$-else norm_stmt',), ignore),
	
	('x$-else last_stmt', ('fn$-else last_stmt',), ignore),
	('x$-else last_stmt', ('if$-else last_stmt',), ignore),
	
	('x$-?else', ('fn',), ignore),
	('x$-?else', ('fn$-else',), ignore),
	('x$-?else', ('if',), ignore),
	('x$-?else', ('if$-else',), ignore),
	
	('x$-?else prefix*', ('fn', 'prefix*'), ignore),
	('x$-?else prefix*', ('fn$-else prefix*',), ignore),
	('x$-?else prefix*', ('if', 'prefix*'), ignore),
	('x$-?else prefix*', ('if$-else prefix*',), ignore),
	
	('x$-?else (prefix* #[x])?', ('fn', '(prefix* #[x])?'), ignore),
	('x$-?else (prefix* #[x])?', ('fn$-else (prefix* #[x])?',), ignore),
	('x$-?else (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('x$-?else (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	
	('x$-?else norm_stmt', ('fn', 'norm_stmt'), ignore),
	('x$-?else norm_stmt', ('fn$-else norm_stmt',), ignore),
	('x$-?else norm_stmt', ('if', 'norm_stmt'), ignore),
	('x$-?else norm_stmt', ('if$-else norm_stmt',), ignore),
	
	('x$-?else last_stmt', ('fn', 'last_stmt'), ignore),
	('x$-?else last_stmt', ('fn$-else last_stmt',), ignore),
	('x$-?else last_stmt', ('if', 'last_stmt'), ignore),
	('x$-?else last_stmt', ('if$-else last_stmt',), ignore),
	
	('x$-case', ('fn$-case',), ignore),
	('x$-case', ('if$-case',), ignore),
	('x$-case', ('match',), ignore),
	
	('x$-case prefix*', ('fn$-case prefix*',), ignore),
	('x$-case prefix*', ('if$-case prefix*',), ignore),
	('x$-case prefix*', ('match prefix*',), ignore),
	
	('x$-case (prefix* #[x])?', ('fn$-case (prefix* #[x])?',), ignore),
	('x$-case (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('x$-case (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	('x$-case norm_stmt', ('fn$-case norm_stmt',), ignore),
	('x$-case norm_stmt', ('if$-case norm_stmt',), ignore),
	('x$-case norm_stmt', ('match norm_stmt',), ignore),
	
	('x$-case last_stmt', ('fn$-case last_stmt',), ignore),
	('x$-case last_stmt', ('if$-case last_stmt',), ignore),
	('x$-case last_stmt', ('match last_stmt',), ignore),
	
	('x$-?case', ('fn',), ignore),
	('x$-?case', ('fn$-case',), ignore),
	('x$-?case', ('if',), ignore),
	('x$-?case', ('if$-case',), ignore),
	('x$-?case', ('match',), ignore),
	
	('x$-?case prefix*', ('fn', 'prefix*'), ignore),
	('x$-?case prefix*', ('fn$-case prefix*',), ignore),
	('x$-?case prefix*', ('if', 'prefix*'), ignore),
	('x$-?case prefix*', ('if$-case prefix*',), ignore),
	('x$-?case prefix*', ('match prefix*',), ignore),
	
	('x$-?case (prefix* #[x])?', ('fn', '(prefix* #[x])?'), ignore),
	('x$-?case (prefix* #[x])?', ('fn$-case (prefix* #[x])?',), ignore),
	('x$-?case (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('x$-?case (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('x$-?case (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	('x$-?case norm_stmt', ('fn', 'norm_stmt'), ignore),
	('x$-?case norm_stmt', ('fn$-case norm_stmt',), ignore),
	('x$-?case norm_stmt', ('if', 'norm_stmt'), ignore),
	('x$-?case norm_stmt', ('if$-case norm_stmt',), ignore),
	('x$-?case norm_stmt', ('match norm_stmt',), ignore),
	
	('x$-?case last_stmt', ('fn', 'last_stmt'), ignore),
	('x$-?case last_stmt', ('fn$-case last_stmt',), ignore),
	('x$-?case last_stmt', ('if', 'last_stmt'), ignore),
	('x$-?case last_stmt', ('if$-case last_stmt',), ignore),
	('x$-?case last_stmt', ('match last_stmt',), ignore),
	
	('x$-else-case', ('fn$-else-case',), ignore),
	('x$-else-case', ('if$-else-case',), ignore),
	('x$-else-case', ('match$-else',), ignore),
	
	('x$-else-case prefix*', ('fn$-else-case prefix*',), ignore),
	('x$-else-case prefix*', ('if$-else-case prefix*',), ignore),
	('x$-else-case prefix*', ('match$-else prefix*',), ignore),
	
	('x$-else-case (prefix* #[x])?', ('fn$-else-case (prefix* #[x])?',), ignore),
	('x$-else-case (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('x$-else-case (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-else-case norm_stmt', ('fn$-else-case norm_stmt',), ignore),
	('x$-else-case norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('x$-else-case norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('x$-else-case last_stmt', ('fn$-else-case last_stmt',), ignore),
	('x$-else-case last_stmt', ('if$-else-case last_stmt',), ignore),
	('x$-else-case last_stmt', ('match$-else last_stmt',), ignore),
	
	('x$-else-?case', ('fn$-else',), ignore),
	('x$-else-?case', ('fn$-else-case',), ignore),
	('x$-else-?case', ('if$-else',), ignore),
	('x$-else-?case', ('if$-else-case',), ignore),
	('x$-else-?case', ('match$-else',), ignore),
	
	('x$-else-?case prefix*', ('fn$-else prefix*',), ignore),
	('x$-else-?case prefix*', ('fn$-else-case prefix*',), ignore),
	('x$-else-?case prefix*', ('if$-else prefix*',), ignore),
	('x$-else-?case prefix*', ('if$-else-case prefix*',), ignore),
	('x$-else-?case prefix*', ('match$-else prefix*',), ignore),
	
	('x$-else-?case (prefix* #[x])?', ('fn$-else (prefix* #[x])?',), ignore),
	('x$-else-?case (prefix* #[x])?', ('fn$-else-case (prefix* #[x])?',), ignore),
	('x$-else-?case (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('x$-else-?case (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('x$-else-?case (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-else-?case norm_stmt', ('fn$-else norm_stmt',), ignore),
	('x$-else-?case norm_stmt', ('fn$-else-case norm_stmt',), ignore),
	('x$-else-?case norm_stmt', ('if$-else norm_stmt',), ignore),
	('x$-else-?case norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('x$-else-?case norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('x$-else-?case last_stmt', ('fn$-else last_stmt',), ignore),
	('x$-else-?case last_stmt', ('fn$-else-case last_stmt',), ignore),
	('x$-else-?case last_stmt', ('if$-else last_stmt',), ignore),
	('x$-else-?case last_stmt', ('if$-else-case last_stmt',), ignore),
	('x$-else-?case last_stmt', ('match$-else last_stmt',), ignore),
	
	('x$-?else-case', ('fn$-case',), ignore),
	('x$-?else-case', ('fn$-else-case',), ignore),
	('x$-?else-case', ('if$-case',), ignore),
	('x$-?else-case', ('if$-else-case',), ignore),
	('x$-?else-case', ('match',), ignore),
	('x$-?else-case', ('match$-else',), ignore),
	
	('x$-?else-case prefix*', ('fn$-case prefix*',), ignore),
	('x$-?else-case prefix*', ('fn$-else-case prefix*',), ignore),
	('x$-?else-case prefix*', ('if$-case prefix*',), ignore),
	('x$-?else-case prefix*', ('if$-else-case prefix*',), ignore),
	('x$-?else-case prefix*', ('match prefix*',), ignore),
	('x$-?else-case prefix*', ('match$-else prefix*',), ignore),
	
	('x$-?else-case (prefix* #[x])?', ('fn$-case (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('fn$-else-case (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('x$-?else-case (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('x$-?else-case norm_stmt', ('fn$-case norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('fn$-else-case norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('if$-case norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('match norm_stmt',), ignore),
	('x$-?else-case norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('x$-?else-case last_stmt', ('fn$-case last_stmt',), ignore),
	('x$-?else-case last_stmt', ('fn$-else-case last_stmt',), ignore),
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
	('prim', ('STR',), NodeStr),
	('prim', ('BOOL',), NodeBool),
	('prim', ('(', ')'), ignore),
	('prim', ('(', 'expr', ')'), ignore),
	('prim', ('(', 'stmt', ')'), ignore),
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
		syntax = Syntax[str, str, Any].Build([Production(*p) for p in SYNTAX_RULES], TERMINALS.__contains__)
		parser = syntax.BuildLR1Parser()
		write('===== LR(1) Parser State =====')
		write(parser)
		write()
		
		ast = parser.Parse(scanner.Tokenize(source_code))
		write('===== Reproduced Source Code (May not be 100%% correct) =====')
		# write('\n'.join(ast.Accept(ToOLangCode('    '))))
		write()
		write(ast)
