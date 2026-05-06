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
	('S', ('postfixed',), ignore),
	
	
	('if', ('IF', 'postfixed', 'postfixed', 'prefix*', 'else'), ignore),
	('if', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'else'), ignore),
	('if', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'else'), ignore),
	('if', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'else'), ignore),
	('else', ('ELSE', 'prefix*', 'if'), ignore),
	('else', ('ELSE', 'postfixed'), ignore),
	
	('if$-else', ('IF', 'postfixed', 'postfixed'), ignore),
	('if$-else', ('IF', 'postfixed', 'prefix*', '(if|if$-else)'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed]', 'postfixed'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-else)'), ignore),
	('if$-else', ('IF', 'postfixed', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('if$-else+pref', ('IF', 'postfixed', 'postfixed', 'prefix*'), ignore),
	('if$-else+pref', ('IF', 'postfixed', 'prefix*', '(if|if$-else)$+pref'), ignore),
	('if$-else+pref', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*'), ignore),
	('if$-else+pref', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-else)$+pref'), ignore),
	('if$-else+pref', ('IF', 'postfixed', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else+pref'), ignore),
	('if$-else+pref', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', 'if$-else+pref'), ignore),
	('if$-else+pref', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'ELSE', 'prefix*', 'if$-else+pref'), ignore),
	('if$-else+pref', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', 'if$-else+pref'), ignore),
	
	
	('if$-case', ('IF', 'postfixed', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	
	('if$-case+pref', ('IF', 'postfixed', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match)$+pref'), ignore),
	('if$-case+pref', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-case|match)$+pref'), ignore),
	('if$-case+pref', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-case|match)$+pref'), ignore),
	('if$-case+pref', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-case|match)$+pref'), ignore),
	
	
	('if$-else-case', ('IF', 'postfixed', 'prefix*', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('IF', 'prefix*', '[^postfixed]$+pref', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('IF', 'postfixed', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	
	('if$-else-case+pref', ('IF', 'postfixed', 'prefix*', '(if$-case|if$-else-case|match|match$-else)$+pref'), ignore),
	('if$-else-case+pref', ('IF', 'prefix*', '[^postfixed]$+pref', '(if$-case|if$-else-case|match|match$-else)$+pref'), ignore),
	('if$-else-case+pref', ('IF', 'postfixed', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)$+pref'), ignore),
	('if$-else-case+pref', ('IF', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-else-case|match$-else)$+pref'), ignore),
	('if$-else-case+pref', ('IF', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'ELSE', 'prefix*', '(if$-else-case|match$-else)$+pref'), ignore),
	('if$-else-case+pref', ('IF', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref', 'ELSE', 'prefix*', '(if$-else-case|match$-else)$+pref'), ignore),
	
	
	('match', ('MATCH', 'postfixed'), ignore),
	('match', ('MATCH', 'prefix*', '(if|if$-case|match)'), ignore),
	('match', ('MATCH', 'postfixed', 'prefix*', 'case..'), ignore),
	('match', ('MATCH', 'prefix*', '(if|if$-else)$+pref', 'case..'), ignore),
	('case..', ('CASE', 'postfixed', 'postfixed', 'prefix*', 'case..'), ignore),
	('case..', ('CASE', 'postfixed', 'prefix*', '(if|if$-else)$+pref', 'case..'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'case..'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if|if$-else)$+pref', 'case..'), ignore),
	('case..', ('CASE', 'postfixed', 'postfixed'), ignore),
	('case..', ('CASE', 'postfixed', 'prefix*', '(if|if$-case|match)'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed]', 'postfixed'), ignore),
	('case..', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)'), ignore),
	
	('match$+pref', ('MATCH', 'postfixed', 'prefix*'), ignore),
	('match$+pref', ('MATCH', 'prefix*', '(if|if$-case|match)$+pref'), ignore),
	('match$+pref', ('MATCH', 'postfixed', 'prefix*', 'case..$+pref'), ignore),
	('match$+pref', ('MATCH', 'prefix*', '(if|if$-else)$+pref', 'case..$+pref'), ignore),
	('case..$+pref', ('CASE', 'postfixed', 'postfixed', 'prefix*', 'case..$+pref'), ignore),
	('case..$+pref', ('CASE', 'postfixed', 'prefix*', '(if|if$-else)$+pref', 'case..$+pref'), ignore),
	('case..$+pref', ('CASE', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'case..$+pref'), ignore),
	('case..$+pref', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if|if$-else)$+pref', 'case..$+pref'), ignore),
	('case..$+pref', ('CASE', 'postfixed', 'postfixed', 'prefix*'), ignore),
	('case..$+pref', ('CASE', 'postfixed', 'prefix*', '(if|if$-case|match)$+pref'), ignore),
	('case..$+pref', ('CASE', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*'), ignore),
	('case..$+pref', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if|if$-case|match)$+pref'), ignore),
	
	
	('match$-else', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	('match$-else', ('MATCH', 'postfixed', 'prefix*', 'case..$-else'), ignore),
	('match$-else', ('MATCH', 'prefix*', '(if|if$-else)$+pref', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'postfixed', 'postfixed', 'prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'postfixed', 'prefix*', '(if|if$-else)$+pref', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if|if$-else)$+pref', 'case..$-else'), ignore),
	('case..$-else', ('CASE', 'postfixed', 'prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	('case..$-else', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if$-else|if$-else-case|match$-else)'), ignore),
	
	('match$-else+pref', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else)$+pref'), ignore),
	('match$-else+pref', ('MATCH', 'postfixed', 'prefix*', 'case..$-else+pref'), ignore),
	('match$-else+pref', ('MATCH', 'prefix*', '(if|if$-else)$+pref', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('CASE', 'postfixed', 'postfixed', 'prefix*', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('CASE', 'postfixed', 'prefix*', '(if|if$-else)$+pref', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('CASE', 'prefix*', '[^postfixed]', 'postfixed', 'prefix*', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if|if$-else)$+pref', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('CASE', 'postfixed', 'prefix*', '(if$-else|if$-else-case|match$-else)$+pref'), ignore),
	('case..$-else+pref', ('CASE', 'prefix*', '[^postfixed]$+pref', '(if$-else|if$-else-case|match$-else)$+pref'), ignore),
	
	
	('(if$-else|if$-else-case|match$-else)', ('if$-else',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('[^postfixed]$+pref', ('if', 'prefix*'), ignore),
	('[^postfixed]$+pref', ('if$-else+pref',), ignore),
	('[^postfixed]$+pref', ('if$-case+pref',), ignore),
	('[^postfixed]$+pref', ('if$-else-case+pref',), ignore),
	('[^postfixed]$+pref', ('match$+pref',), ignore),
	('[^postfixed]$+pref', ('match$-else+pref',), ignore),
	
	('(if|if$-else)', ('if',), ignore),
	('(if|if$-else)', ('if$-else',), ignore),
	
	('(if|if$-case|match)', ('if',), ignore),
	('(if|if$-case|match)', ('if$-case',), ignore),
	('(if|if$-case|match)', ('match',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else)', ('if$-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('if$-else-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|match)$+pref', ('if$-case+pref',), ignore),
	('(if$-case|match)$+pref', ('match$+pref',), ignore),
	
	('(if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('if$-case+pref',), ignore),
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('if$-else-case+pref',), ignore),
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('match$+pref',), ignore),
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('match$-else+pref',), ignore),
	
	('(if$-case|match)', ('if$-case',), ignore),
	('(if$-case|match)', ('match',), ignore),
	
	('(if$-else|if$-else-case|match$-else)$+pref', ('if$-else+pref',), ignore),
	('(if$-else|if$-else-case|match$-else)$+pref', ('if$-else-case+pref',), ignore),
	('(if$-else|if$-else-case|match$-else)$+pref', ('match$-else+pref',), ignore),
	
	('(if$-else-case|match$-else)$+pref', ('if$-else-case+pref',), ignore),
	('(if$-else-case|match$-else)$+pref', ('match$-else+pref',), ignore),
	
	('[^postfixed]', ('if',), ignore),
	('[^postfixed]', ('if$-else',), ignore),
	('[^postfixed]', ('if$-case',), ignore),
	('[^postfixed]', ('if$-else-case',), ignore),
	('[^postfixed]', ('match',), ignore),
	('[^postfixed]', ('match$-else',), ignore),
	
	('(if|if$-case|match)$+pref', ('if', 'prefix*'), ignore),
	('(if|if$-case|match)$+pref', ('if$-case+pref',), ignore),
	('(if|if$-case|match)$+pref', ('match$+pref',), ignore),
	
	('(if|if$-else)$+pref', ('if', 'prefix*'), ignore),
	('(if|if$-else)$+pref', ('if$-else+pref',), ignore),
	
	
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
	('S', ('postfixed',), ignore),
	
	
	('if', ('if_x_x$else', 'else'), ignore),
	('else', ('ELSE', 'prefix*', 'if'), ignore),
	('else', ('ELSE', 'postfixed'), ignore),
	
	
	('if$-else', ('if_x$postfixed', 'postfixed'), ignore),
	('if$-else', ('if_x$[^postfixed]', '(if|if$-else)'), ignore),
	('if$-else', ('if_x_x$else', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('if$-else+pref', ('if_x$postfixed', 'postfixed', 'prefix*'), ignore),
	('if$-else+pref', ('if_x$[^postfixed]', '(if|if$-else)$+pref'), ignore),
	('if$-else+pref', ('if_x_x$else', 'ELSE', 'prefix*', 'if$-else+pref'), ignore),
	
	
	('if$-case', ('if_x_x$else', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	
	('if$-case+pref', ('if_x_x$else', 'ELSE', 'prefix*', '(if$-case|match)$+pref'), ignore),
	
	
	('if$-else-case', ('if_x$[^postfixed]', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('if_x_x$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	
	('if$-else-case+pref', ('if_x$[^postfixed]', '(if$-case|if$-else-case|match|match$-else)$+pref'), ignore),
	('if$-else-case+pref', ('if_x_x$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else)$+pref'), ignore),
	
	
	('match', ('MATCH', 'postfixed'), ignore),
	('match', ('MATCH', 'prefix*', '(if|if$-case|match)'), ignore),
	('match', ('match_x$case..', 'case..'), ignore),
	('case..', ('case_x_x$case..', 'case..'), ignore),
	('case..', ('case_x$postfixed', 'postfixed'), ignore),
	('case..', ('case_x$[^postfixed]', '(if|if$-case|match)'), ignore),
	
	('match$+pref', ('MATCH', 'postfixed', 'prefix*'), ignore),
	('match$+pref', ('MATCH', 'prefix*', '(if|if$-case|match)$+pref'), ignore),
	('match$+pref', ('match_x$case..', 'case..$+pref'), ignore),
	('case..$+pref', ('case_x_x$case..', 'case..$+pref'), ignore),
	('case..$+pref', ('case_x$postfixed', 'postfixed', 'prefix*'), ignore),
	('case..$+pref', ('case_x$[^postfixed]', '(if|if$-case|match)$+pref'), ignore),
	
	
	('match$-else', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else)'), ignore),
	('match$-else', ('match_x$case..', 'case..$-else'), ignore),
	('case..$-else', ('case_x_x$case..', 'case..$-else'), ignore),
	('case..$-else', ('case_x$[^postfixed]', '(if$-else|if$-else-case|match$-else)'), ignore),
	
	('match$-else+pref', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else)$+pref'), ignore),
	('match$-else+pref', ('match_x$case..', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('case_x_x$case..', 'case..$-else+pref'), ignore),
	('case..$-else+pref', ('case_x$[^postfixed]', '(if$-else|if$-else-case|match$-else)$+pref'), ignore),
	
	
	('if_x_x$else', ('if_x$postfixed', 'postfixed', 'prefix*'), ignore),
	('if_x_x$else', ('if_x$[^postfixed]', '(if|if$-case|match)$+pref'), ignore),
	
	('if_x$postfixed', ('IF', 'postfixed'), ignore),
	('if_x$postfixed', ('IF', 'prefix*', '[^postfixed]'), ignore),
	
	('if_x$[^postfixed]', ('IF', 'postfixed', 'prefix*'), ignore),
	('if_x$[^postfixed]', ('IF', 'prefix*', '[^postfixed]$+pref'), ignore),
	
	('match_x$case..', ('MATCH', 'postfixed', 'prefix*'), ignore),
	('match_x$case..', ('MATCH', 'prefix*', '(if|if$-else)$+pref'), ignore),
	
	('case_x_x$case..', ('case_x$postfixed', 'postfixed', 'prefix*'), ignore),
	('case_x_x$case..', ('case_x$[^postfixed]', '(if|if$-else)$+pref'), ignore),
	
	('case_x$postfixed', ('CASE', 'postfixed'), ignore),
	('case_x$postfixed', ('CASE', 'prefix*', '[^postfixed]'), ignore),
	
	('case_x$[^postfixed]', ('CASE', 'postfixed', 'prefix*'), ignore),
	('case_x$[^postfixed]', ('CASE', 'prefix*', '[^postfixed]$+pref'), ignore),
	
	
	('(if$-else|if$-else-case|match$-else)', ('if$-else',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else|if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('[^postfixed]$+pref', ('if', 'prefix*'), ignore),
	('[^postfixed]$+pref', ('if$-else+pref',), ignore),
	('[^postfixed]$+pref', ('if$-case+pref',), ignore),
	('[^postfixed]$+pref', ('if$-else-case+pref',), ignore),
	('[^postfixed]$+pref', ('match$+pref',), ignore),
	('[^postfixed]$+pref', ('match$-else+pref',), ignore),
	
	('(if|if$-else)', ('if',), ignore),
	('(if|if$-else)', ('if$-else',), ignore),
	
	('(if|if$-case|match)', ('if',), ignore),
	('(if|if$-case|match)', ('if$-case',), ignore),
	('(if|if$-case|match)', ('match',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else)', ('if$-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('if$-else-case',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match',), ignore),
	('(if$-case|if$-else-case|match|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|match)$+pref', ('if$-case+pref',), ignore),
	('(if$-case|match)$+pref', ('match$+pref',), ignore),
	
	('(if$-else-case|match$-else)', ('if$-else-case',), ignore),
	('(if$-else-case|match$-else)', ('match$-else',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('if$-case+pref',), ignore),
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('if$-else-case+pref',), ignore),
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('match$+pref',), ignore),
	('(if$-case|if$-else-case|match|match$-else)$+pref', ('match$-else+pref',), ignore),
	
	('(if$-case|match)', ('if$-case',), ignore),
	('(if$-case|match)', ('match',), ignore),
	
	('(if$-else|if$-else-case|match$-else)$+pref', ('if$-else+pref',), ignore),
	('(if$-else|if$-else-case|match$-else)$+pref', ('if$-else-case+pref',), ignore),
	('(if$-else|if$-else-case|match$-else)$+pref', ('match$-else+pref',), ignore),
	
	('(if$-else-case|match$-else)$+pref', ('if$-else-case+pref',), ignore),
	('(if$-else-case|match$-else)$+pref', ('match$-else+pref',), ignore),
	
	('[^postfixed]', ('if',), ignore),
	('[^postfixed]', ('if$-else',), ignore),
	('[^postfixed]', ('if$-case',), ignore),
	('[^postfixed]', ('if$-else-case',), ignore),
	('[^postfixed]', ('match',), ignore),
	('[^postfixed]', ('match$-else',), ignore),
	
	('(if|if$-case|match)$+pref', ('if', 'prefix*'), ignore),
	('(if|if$-case|match)$+pref', ('if$-case+pref',), ignore),
	('(if|if$-case|match)$+pref', ('match$+pref',), ignore),
	
	('(if|if$-else)$+pref', ('if', 'prefix*'), ignore),
	('(if|if$-else)$+pref', ('if$-else+pref',), ignore),
	
	
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
