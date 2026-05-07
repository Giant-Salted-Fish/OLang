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
'[^(if|prim)]$+pref'
source_code = '''
if a b
'''
SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable[..., Any]]] = [
	('S', ('all',), lambda x: x),
	
	('if', ('IF', 'all', '(if|if$-case|match|prim)', 'ELSE', '(if|prim)'), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	('if$-else', ('IF', 'all', '(if|if$-else|prim)'), lambda IF, cond, true_br: NodeIfElse(cond, true_br, NodeCompound())),
	('if$-else', ('IF', 'all', '(if|if$-case|match|prim)', 'ELSE', 'if$-else'), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	('if$-case', ('IF', 'all', '(if|if$-case|match|prim)', 'ELSE', '(if$-case|match)'), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	('if$-else-case', ('IF', 'all', '(if$-case|if$-else-case|match|match$-else)'), lambda IF, cond, true_br: NodeIfElse(cond, true_br, NodeCompound())),
	('if$-else-case', ('IF', 'all', '(if|if$-case|match|prim)', 'ELSE', '(if$-else-case|match$-else)'), lambda IF, cond, true_br, ELSE, false_br: NodeIfElse(cond, true_br, false_br)),
	
	('match', ('MATCH', '(if|if$-case|match|prim)'), lambda MATCH, expr: ('match', expr, ())),
	('match', ('MATCH', '(if|if$-else|prim)', 'case..'), lambda MATCH, expr, xs: ('match', expr, xs)),
	('case..', ('CASE', 'all', '(if|if$-else|prim)', 'case..'), lambda CASE, pattern, expr, xs: (('case', pattern, expr), *xs)),
	('case..', ('CASE', 'all', '(if|if$-case|match|prim)'), lambda CASE, pattern, expr: (('case', pattern, expr),)),
	
	('match$-else', ('MATCH', '(if$-else|if$-else-case|match$-else)'), lambda MATCH, expr: ('match', expr, ())),
	('match$-else', ('MATCH', '(if|if$-else|prim)', '..case$-else'), lambda MATCH, expr, xs: ('match', expr, ())),
	('..case$-else', ('CASE', 'all', '(if|if$-else|prim)', '..case$-else'),lambda CASE, pattern, expr, xs: (('case', pattern, expr), *xs)),
	('..case$-else', ('CASE', 'all', '(if$-else|if$-else-case|match$-else)'), lambda CASE, pattern, expr: (('case', pattern, expr),)),
	
	
	
	('all', ('if',), lambda x: x),
	('all', ('if$-else',), lambda x: x),
	('all', ('if$-case',), lambda x: x),
	('all', ('if$-else-case',), lambda x: x),
	('all', ('match',), lambda x: x),
	('all', ('match$-else',), lambda x: x),
	('all', ('prim',), lambda x: x),
	
	('(if|if$-case|match|prim)', ('if',), lambda x: x),
	('(if|if$-case|match|prim)', ('if$-case',), lambda x: x),
	('(if|if$-case|match|prim)', ('match',), lambda x: x),
	('(if|if$-case|match|prim)', ('prim',), lambda x: x),
	
	('(if|prim)', ('if',), lambda x: x),
	('(if|prim)', ('prim',), lambda x: x),
	
	('(if|if$-else|prim)', ('if',), lambda x: x),
	('(if|if$-else|prim)', ('if$-else',), lambda x: x),
	('(if|if$-else|prim)', ('prim',), lambda x: x),
	
	('(if$-case|match)', ('if$-case',), lambda x: x),
	('(if$-case|match)', ('match',), lambda x: x),
	
	('(if$-case|if$-else-case|match|match$-else)', ('if$-case',), lambda x: x),
	('(if$-case|if$-else-case|match|match$-else)', ('if$-else-case',), lambda x: x),
	('(if$-case|if$-else-case|match|match$-else)', ('match',), lambda x: x),
	('(if$-case|if$-else-case|match|match$-else)', ('match$-else',), lambda x: x),
	
	('(if$-else-case|match$-else)', ('if$-else-case',), lambda x: x),
	('(if$-else-case|match$-else)', ('match$-else',), lambda x: x),
	
	('(if$-else|if$-else-case|match$-else)', ('if$-else',), lambda x: x),
	('(if$-else|if$-else-case|match$-else)', ('if$-else-case',), lambda x: x),
	('(if$-else|if$-else-case|match$-else)', ('match$-else',), lambda x: x),
	
	('prim', ('IDENT',), NodeLabel),
	('prim', ('INT',), NodeInt),
]
SYNTAX_RULES = [
	('S', ('prefix*', 'if'), ignore),
	('S', ('prefix*', 'if$-else'), ignore),
	('S', ('prefix*', 'if$-case'), ignore),
	('S', ('prefix*', 'if$-else-case'), ignore),
	('S', ('prefix*', 'match'), ignore),
	('S', ('prefix*', 'match$-else'), ignore),
	('S', ('prefixed',), ignore),
	('S', ('(prefix* #[x])?', 'postfixed',), ignore),
	
	
	('if', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'else'), ignore),
	('if', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'else'), ignore),
	('if', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'else'), ignore),
	('if', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'else'), ignore),
	('else', ('ELSE', 'prefix*', 'if'), ignore),
	('else', ('ELSE', '(prefix* #[x])?', 'postfixed'), ignore),
	
	('if$-else', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed'), ignore),
	('if$-else', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else)'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-else)'), ignore),
	('if$-else', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('if$-else prefix*', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('if$-else prefix*', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*'), ignore),
	('if$-else prefix*', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('if$-else prefix*', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*'), ignore),
	('if$-else prefix*', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	('if$-else prefix*', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	('if$-else prefix*', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	('if$-else prefix*', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	
	('if$-else (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	
	
	('if$-case', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	
	('if$-case prefix*', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match) prefix*'), ignore),
	('if$-case prefix*', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-case|match) prefix*'), ignore),
	('if$-case prefix*', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match) prefix*'), ignore),
	('if$-case prefix*', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-case|match) prefix*'), ignore),
	
	('if$-case (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match) (prefix* #[x])?'), ignore),
	('if$-case (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-case|match) (prefix* #[x])?'), ignore),
	('if$-case (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match) (prefix* #[x])?'), ignore),
	('if$-case (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-case|match) (prefix* #[x])?'), ignore),
	
	
	('if$-else-case', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('IF', 'prefix*', '[^postfixed] prefix*', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	
	('if$-else-case prefix*', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if$-case|if$-else-case|match|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('IF', 'prefix*', '[^postfixed] prefix*', '(if$-case|if$-else-case|match|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) prefix*'), ignore),
	
	('if$-else-case (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] prefix*', '(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('IF', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	
	
	('match', ('MATCH', '(prefix* #[x])?', 'postfixed'), ignore),
	('match', ('MATCH', 'prefix*', '(if|if$-case|match)'), ignore),
	('match', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..'), ignore),
	('match', ('MATCH', 'prefix*', '(if|if$-else) prefix*', 'case..'), ignore),
	('case..', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..'), ignore),
	('case..', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*', 'case..'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'case..'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*', 'case..'), ignore),
	('case..', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed'), ignore),
	('case..', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match)'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match)'), ignore),
	
	('match prefix*', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('match prefix*', ('MATCH', 'prefix*', '(if|if$-case|match) prefix*'), ignore),
	('match prefix*', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case.. prefix*'), ignore),
	('match prefix*', ('MATCH', 'prefix*', '(if|if$-else) prefix*', 'case.. prefix*'), ignore),
	('case.. prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case.. prefix*'), ignore),
	('case.. prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*', 'case.. prefix*'), ignore),
	('case.. prefix*', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'case.. prefix*'), ignore),
	('case.. prefix*', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*', 'case.. prefix*'), ignore),
	('case.. prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('case.. prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) prefix*'), ignore),
	('case.. prefix*', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('case.. prefix*', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) prefix*'), ignore),
	
	('match (prefix* #[x])?', ('MATCH', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('MATCH', 'prefix*', '(if|if$-case|match) (prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case.. (prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('MATCH', 'prefix*', '(if|if$-else) prefix*', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-case|match) (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-case|match) (prefix* #[x])?'), ignore),
	
	
	('match$-else', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	('match$-else', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else'), ignore),
	('match$-else', ('MATCH', 'prefix*', '(if|if$-else) prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	('case..$-else', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	
	('match$-else prefix*', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else) prefix*'), ignore),
	('match$-else prefix*', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else prefix*'), ignore),
	('match$-else prefix*', ('MATCH', 'prefix*', '(if|if$-else) prefix*', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if$-else|if$-else-case|match$-else) prefix*'), ignore),
	('case..$-else prefix*', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if$-else|if$-else-case|match$-else) prefix*'), ignore),
	
	('match$-else (prefix* #[x])?', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('match$-else (prefix* #[x])?', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else (prefix* #[x])?'), ignore),
	('match$-else (prefix* #[x])?', ('MATCH', 'prefix*', '(if|if$-else) prefix*', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if|if$-else) prefix*', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?', 'postfixed', 'prefix*', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if|if$-else) prefix*', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*', '(if$-else|if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('CASE', 'prefix*', '[^postfixed] prefix*', '(if$-else|if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	
	
	('(if$-else|if$-else-case|match$-else)', ('if$-else',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('[^postfixed] prefix*', ('if', 'prefix*'), ignore),
	('[^postfixed] prefix*', ('if$-else prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-case prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-else-case prefix*',), ignore),
	('[^postfixed] prefix*', ('match prefix*',), ignore),
	('[^postfixed] prefix*', ('match$-else prefix*',), ignore),
	
	('(if|if$-else)', ('if',), ignore),
	('(if|if$-else)', ('if$-else',), ignore),
	
	('(if|if$-case|match)', ('if',), ignore),
	('(if|if$-case|match)', ('if$-case',), ignore),
	('(if|if$-case|match)', ('match',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else)', ('if$-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('if$-else-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|match) prefix*', ('if$-case prefix*',), ignore),
	('(if$-case|match) prefix*', ('match prefix*',), ignore),
	
	('(if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('if$-case prefix*',), ignore),
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('if$-else-case prefix*',), ignore),
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('match prefix*',), ignore),
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('match$-else prefix*',), ignore),
	
	('(if$-case|match)', ('if$-case',), ignore),
	('(if$-case|match)', ('match',), ignore),
	
	('(if$-else|if$-else-case|match$-else) prefix*', ('if$-else prefix*',), ignore),
	('(if$-else|if$-else-case|match$-else) prefix*', ('if$-else-case prefix*',), ignore),
	('(if$-else|if$-else-case|match$-else) prefix*', ('match$-else prefix*',), ignore),
	
	('(if$-else-case|match$-else) prefix*', ('if$-else-case prefix*',), ignore),
	('(if$-else-case|match$-else) prefix*', ('match$-else prefix*',), ignore),
	
	('[^postfixed]', ('if',), ignore),
	('[^postfixed]', ('if$-else',), ignore),
	('[^postfixed]', ('if$-case',), ignore),
	('[^postfixed]', ('if$-else-case',), ignore),
	('[^postfixed]', ('match',), ignore),
	('[^postfixed]', ('match$-else',), ignore),
	
	('(if|if$-case|match) prefix*', ('if', 'prefix*'), ignore),
	('(if|if$-case|match) prefix*', ('if$-case prefix*',), ignore),
	('(if|if$-case|match) prefix*', ('match prefix*',), ignore),
	
	('(if|if$-else) prefix*', ('if', 'prefix*'), ignore),
	('(if|if$-else) prefix*', ('if$-else prefix*',), ignore),
	
	('[^postfixed] (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if|if$-else) (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('(if|if$-else) (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if|if$-case|match) (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('(if|if$-case|match) (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('(if|if$-case|match) (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	('(if$-else-case|match$-else) (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('(if$-else-case|match$-else) (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if$-else|if$-else-case|match$-else) (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('(if$-else|if$-else-case|match$-else) (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('(if$-else|if$-else-case|match$-else) (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if$-case|match) (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('(if$-case|match) (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	
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
SYNTAX_RULES = [
	('S', ('prefix*', 'if'), ignore),
	('S', ('prefix*', 'if$-else'), ignore),
	('S', ('prefix*', 'if$-case'), ignore),
	('S', ('prefix*', 'if$-else-case'), ignore),
	('S', ('prefix*', 'match'), ignore),
	('S', ('prefix*', 'match$-else'), ignore),
	('S', ('prefixed',), ignore),
	('S', ('(prefix* #[x])?', 'postfixed',), ignore),
	
	
	('if', ('(IF x x)$else', 'else'), ignore),
	('else', ('ELSE', 'prefix*', 'if'), ignore),
	('else', ('ELSE', '(prefix* #[x])?', 'postfixed'), ignore),
	
	
	('if$-else', ('(IF x)$postfixed', 'postfixed'), ignore),
	('if$-else', ('(IF x)$[^postfixed]', '(if|if$-else)'), ignore),
	('if$-else', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('if$-else prefix*', ('(IF x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('if$-else prefix*', ('(IF x)$[^postfixed]', '(if|if$-else) prefix*'), ignore),
	('if$-else prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	
	('if$-else (prefix* #[x])?', ('(IF x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x)$[^postfixed]', '(if|if$-else) (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	
	
	('if$-case', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	
	('if$-case prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match) prefix*'), ignore),
	
	('if$-case (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match) (prefix* #[x])?'), ignore),
	
	
	('if$-else-case', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	
	('if$-else-case prefix*', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else) prefix*'), ignore),
	
	('if$-else-case (prefix* #[x])?', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	
	
	('match', ('MATCH', '(prefix* #[x])?', 'postfixed'), ignore),
	('match', ('MATCH', 'prefix*', '(if|if$-case|match)'), ignore),
	('match', ('(MATCH x)$case..', 'case..'), ignore),
	('case..', ('(CASE x x)$case..', 'case..'), ignore),
	('case..', ('(CASE x)$postfixed', 'postfixed'), ignore),
	('case..', ('(CASE x)$[^postfixed]', '(if|if$-case|match)'), ignore),
	
	('match prefix*', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('match prefix*', ('MATCH', 'prefix*', '(if|if$-case|match) prefix*'), ignore),
	('match prefix*', ('(MATCH x)$case..', 'case.. prefix*'), ignore),
	('case.. prefix*', ('(CASE x x)$case..', 'case.. prefix*'), ignore),
	('case.. prefix*', ('(CASE x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('case.. prefix*', ('(CASE x)$[^postfixed]', '(if|if$-case|match) prefix*'), ignore),
	
	('match (prefix* #[x])?', ('MATCH', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('MATCH', 'prefix*', '(if|if$-case|match) (prefix* #[x])?'), ignore),
	('match (prefix* #[x])?', ('(MATCH x)$case..', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('(CASE x x)$case..', 'case.. (prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('(CASE x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('case.. (prefix* #[x])?', ('(CASE x)$[^postfixed]', '(if|if$-case|match) (prefix* #[x])?'), ignore),
	
	
	('match$-else', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	('match$-else', ('(MATCH x)$case..', 'case..$-else'), ignore),
	('case..$-else', ('(CASE x x)$case..', 'case..$-else'), ignore),
	('case..$-else', ('(CASE x)$[^postfixed]', '(if$-else|if$-else-case|match$-else)'), ignore),
	
	('match$-else prefix*', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else) prefix*'), ignore),
	('match$-else prefix*', ('(MATCH x)$case..', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('(CASE x x)$case..', 'case..$-else prefix*'), ignore),
	('case..$-else prefix*', ('(CASE x)$[^postfixed]', '(if$-else|if$-else-case|match$-else) prefix*'), ignore),
	
	('match$-else (prefix* #[x])?', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('match$-else (prefix* #[x])?', ('(MATCH x)$case..', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('(CASE x x)$case..', 'case..$-else (prefix* #[x])?'), ignore),
	('case..$-else (prefix* #[x])?', ('(CASE x)$[^postfixed]', '(if$-else|if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	
	
	('(IF x x)$else', ('(IF x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('(IF x x)$else', ('(IF x)$[^postfixed]', '(if|if$-case|match) prefix*'), ignore),
	
	('(IF x)$postfixed', ('IF', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('(IF x)$postfixed', ('IF', 'prefix*', '[^postfixed] (prefix* #[x])?'), ignore),
	
	('(IF x)$[^postfixed]', ('IF', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(IF x)$[^postfixed]', ('IF', 'prefix*', '[^postfixed] prefix*'), ignore),
	
	('(MATCH x)$case..', ('MATCH', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(MATCH x)$case..', ('MATCH', 'prefix*', '(if|if$-else) prefix*'), ignore),
	
	('(CASE x x)$case..', ('(CASE x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('(CASE x x)$case..', ('(CASE x)$[^postfixed]', '(if|if$-else) prefix*'), ignore),
	
	('(CASE x)$postfixed', ('CASE', '(prefix* #[x])?', 'postfixed', '(prefix* #[x])?'), ignore),
	('(CASE x)$postfixed', ('CASE', 'prefix*', '[^postfixed] (prefix* #[x])?'), ignore),
	
	('(CASE x)$[^postfixed]', ('CASE', '(prefix* #[x])?', 'postfixed', 'prefix*'), ignore),
	('(CASE x)$[^postfixed]', ('CASE', 'prefix*', '[^postfixed] prefix*'), ignore),
	
	
	('(if$-else|if$-else-case|match$-else)', ('if$-else',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('[^postfixed] prefix*', ('if', 'prefix*'), ignore),
	('[^postfixed] prefix*', ('if$-else prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-case prefix*',), ignore),
	('[^postfixed] prefix*', ('if$-else-case prefix*',), ignore),
	('[^postfixed] prefix*', ('match prefix*',), ignore),
	('[^postfixed] prefix*', ('match$-else prefix*',), ignore),
	
	('(if|if$-else)', ('if',), ignore),
	('(if|if$-else)', ('if$-else',), ignore),
	
	('(if|if$-case|match)', ('if',), ignore),
	('(if|if$-case|match)', ('if$-case',), ignore),
	('(if|if$-case|match)', ('match',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else)', ('if$-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('if$-else-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|match) prefix*', ('if$-case prefix*',), ignore),
	('(if$-case|match) prefix*', ('match prefix*',), ignore),
	
	('(if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('if$-case prefix*',), ignore),
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('if$-else-case prefix*',), ignore),
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('match prefix*',), ignore),
	('(if$-case|if$-else-case|match|match$-else) prefix*', ('match$-else prefix*',), ignore),
	
	('(if$-case|match)', ('if$-case',), ignore),
	('(if$-case|match)', ('match',), ignore),
	
	('(if$-else|if$-else-case|match$-else) prefix*', ('if$-else prefix*',), ignore),
	('(if$-else|if$-else-case|match$-else) prefix*', ('if$-else-case prefix*',), ignore),
	('(if$-else|if$-else-case|match$-else) prefix*', ('match$-else prefix*',), ignore),
	
	('(if$-else-case|match$-else) prefix*', ('if$-else-case prefix*',), ignore),
	('(if$-else-case|match$-else) prefix*', ('match$-else prefix*',), ignore),
	
	('[^postfixed]', ('if',), ignore),
	('[^postfixed]', ('if$-else',), ignore),
	('[^postfixed]', ('if$-case',), ignore),
	('[^postfixed]', ('if$-else-case',), ignore),
	('[^postfixed]', ('match',), ignore),
	('[^postfixed]', ('match$-else',), ignore),
	
	('(if|if$-case|match) prefix*', ('if', 'prefix*'), ignore),
	('(if|if$-case|match) prefix*', ('if$-case prefix*',), ignore),
	('(if|if$-case|match) prefix*', ('match prefix*',), ignore),
	
	('(if|if$-else) prefix*', ('if', 'prefix*'), ignore),
	('(if|if$-else) prefix*', ('if$-else prefix*',), ignore),
	
	('[^postfixed] (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('[^postfixed] (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if|if$-else) (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('(if|if$-else) (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	('(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if|if$-case|match) (prefix* #[x])?', ('if', '(prefix* #[x])?'), ignore),
	('(if|if$-case|match) (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('(if|if$-case|match) (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	('(if$-else-case|match$-else) (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('(if$-else-case|match$-else) (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if$-else|if$-else-case|match$-else) (prefix* #[x])?', ('if$-else (prefix* #[x])?',), ignore),
	('(if$-else|if$-else-case|match$-else) (prefix* #[x])?', ('if$-else-case (prefix* #[x])?',), ignore),
	('(if$-else|if$-else-case|match$-else) (prefix* #[x])?', ('match$-else (prefix* #[x])?',), ignore),
	
	('(if$-case|match) (prefix* #[x])?', ('if$-case (prefix* #[x])?',), ignore),
	('(if$-case|match) (prefix* #[x])?', ('match (prefix* #[x])?',), ignore),
	
	
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
SYNTAX_RULES2 = [
	('S', ('stmt_lst',), ignore),
	
	('stmt_lst', ('norm_stmt', 'stmt_lst'), ignore),
	('stmt_lst', ('last_stmt',), ignore),
	
	('norm_stmt', ('prefix*', 'if'), ignore),
	('norm_stmt', ('prefix*', 'norm_ifx'), ignore),
	('norm_stmt', ('prefixed', ';'), ignore),
	('norm_stmt', (';',), ignore),
	
	('last_stmt', ('prefix*', 'last_ifx'), ignore),
	('last_stmt', ('prefixed',), ignore),
	('last_stmt', (), ignore),
	
	
	('norm_ifx', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'norm_stmt'), ignore),
	('norm_ifx', ('IF', '(prefix* if|postfixed)', 'prefix*', 'norm_ifx'), ignore),
	('norm_ifx', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'norm_stmt'), ignore),
	('norm_ifx', ('IF', 'prefix*', 'ifx', 'norm_ifx'), ignore),
	('norm_ifx', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'prefix*', 'ELSE', 'prefix*', 'norm_ifx'), ignore),
	('norm_ifx', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'prefix*', 'ELSE', 'prefix*', 'norm_ifx'), ignore),
	
	('last_ifx', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'last_stmt'), ignore),
	('last_ifx', ('IF', '(prefix* if|postfixed)', 'prefix*', 'last_ifx'), ignore),
	('last_ifx', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'last_stmt'), ignore),
	('last_ifx', ('IF', 'prefix*', 'ifx', 'last_ifx'), ignore),
	('last_ifx', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'prefix*', 'ELSE', 'prefix*', 'last_ifx'), ignore),
	('last_ifx', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'prefix*', 'ELSE', 'prefix*', 'last_ifx'), ignore),
	
	
	('if', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'prefix*', 'ELSE', '(prefix* if|postfixed)'), ignore),
	('if', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'prefix*', 'ELSE', '(prefix* if|postfixed)'), ignore),
	
	('if$-else', ('IF', '(prefix* if|postfixed)', '(prefix* (if|if$-else)|postfixed)'), ignore),
	('if$-else', ('IF', 'prefix*', '(ifx (if|if$-else)|if$-else postfixed)'), ignore),
	('if$-else', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('ifx', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'prefix*'), ignore),
	('ifx', ('IF', '(prefix* if|postfixed)', 'prefix*', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'prefix*'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', 'ifx'), ignore),
	('ifx', ('IF', '(prefix* if|postfixed)', '(prefix* if|postfixed)', 'prefix*', 'ELSE', 'prefix*', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', '(ifx if|if$-else postfixed)', 'prefix*', 'ELSE', 'prefix*', 'ifx'), ignore),
	
	
	('(prefix* if|postfixed)', ('prefix*', 'if'), ignore),
	('(prefix* if|postfixed)', ('postfixed',), ignore),
	
	('(ifx if|if$-else postfixed)', ('ifx', 'if'), ignore),
	('(ifx if|if$-else postfixed)', ('if$-else', 'postfixed'), ignore),
	
	('(prefix* (if|if$-else)|postfixed)', ('prefix*', 'if'), ignore),
	('(prefix* (if|if$-else)|postfixed)', ('prefix*', 'if$-else'), ignore),
	('(prefix* (if|if$-else)|postfixed)', ('postfixed',), ignore),
	
	('(ifx (if|if$-else)|if$-else postfixed)', ('ifx', 'if'), ignore),
	('(ifx (if|if$-else)|if$-else postfixed)', ('ifx', 'if$-else'), ignore),
	('(ifx (if|if$-else)|if$-else postfixed)', ('if$-else', 'postfixed',), ignore),
	
	
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
