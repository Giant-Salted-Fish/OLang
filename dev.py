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
	('norm_stmt', ('union', ';'), ignore),
	('norm_stmt', (';',), ignore),
	
	('last_stmt', ('prefix*', 'if$-else last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'if$-else-case last_stmt'), ignore),
	('last_stmt', ('prefix*', 'match last_stmt'), ignore),
	('last_stmt', ('prefix*', 'match$-else last_stmt'), ignore),
	('last_stmt', ('union',), ignore),
	('last_stmt', (), ignore),
	
	
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
	('if$-else', ('(IF x)$[^postfixed]', '(if|if$-else)'), ignore),
	('if$-else', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('if$-else prefix*', ('(IF x)$postfixed', 'postfixed', 'prefix*'), ignore),
	('if$-else prefix*', ('(IF x)$[^postfixed]', '(if|if$-else) prefix*'), ignore),
	('if$-else prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else prefix*'), ignore),
	
	('if$-else (prefix* #[x])?', ('(IF x)$postfixed', 'postfixed', '(prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x)$[^postfixed]', '(if|if$-else) (prefix* #[x])?'), ignore),
	('if$-else (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else (prefix* #[x])?'), ignore),
	
	('if$-else norm_stmt', ('(IF x)$postfixed', 'postfixed', 'norm_stmt'), ignore),
	('if$-else norm_stmt', ('(IF x)$[^postfixed]', '(if|if$-else) norm_stmt'), ignore),
	('if$-else norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else norm_stmt'), ignore),
	
	('if$-else last_stmt', ('(IF x)$postfixed', 'postfixed', 'last_stmt'), ignore),
	('if$-else last_stmt', ('(IF x)$[^postfixed]', '(if|if$-else) last_stmt'), ignore),
	('if$-else last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', 'if$-else last_stmt'), ignore),
	
	
	('if$-case', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match)'), ignore),
	('if$-case prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match) prefix*'), ignore),
	('if$-case (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match) (prefix* #[x])?'), ignore),
	('if$-case norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match) norm_stmt'), ignore),
	('if$-case last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-case|match) last_stmt'), ignore),
	
	
	('if$-else-case', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else)'), ignore),
	('if$-else-case', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else)'), ignore),
	('if$-else-case prefix*', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else) prefix*'), ignore),
	('if$-else-case prefix*', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else) prefix*'), ignore),
	('if$-else-case (prefix* #[x])?', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case (prefix* #[x])?', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else) (prefix* #[x])?'), ignore),
	('if$-else-case norm_stmt', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else) norm_stmt'), ignore),
	('if$-else-case norm_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else) norm_stmt'), ignore),
	('if$-else-case last_stmt', ('(IF x)$[^postfixed]', '(if$-case|if$-else-case|match|match$-else) last_stmt'), ignore),
	('if$-else-case last_stmt', ('(IF x x)$else', 'ELSE', 'prefix*', '(if$-else-case|match$-else) last_stmt'), ignore),
	
	
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
	
	('match norm_stmt', ('MATCH', '(prefix* #[x])?', 'postfixed', 'norm_stmt'), ignore),
	('match norm_stmt', ('MATCH', 'prefix*', '(if|if$-case|match) norm_stmt'), ignore),
	('match norm_stmt', ('(MATCH x)$case..', 'case.. norm_stmt'), ignore),
	('case.. norm_stmt', ('(CASE x x)$case..', 'case.. norm_stmt'), ignore),
	('case.. norm_stmt', ('(CASE x)$postfixed', 'postfixed', 'norm_stmt'), ignore),
	('case.. norm_stmt', ('(CASE x)$[^postfixed]', '(if|if$-case|match) norm_stmt'), ignore),
	
	('match last_stmt', ('MATCH', '(prefix* #[x])?', 'postfixed', 'last_stmt'), ignore),
	('match last_stmt', ('MATCH', 'prefix*', '(if|if$-case|match) last_stmt'), ignore),
	('match last_stmt', ('(MATCH x)$case..', 'case.. last_stmt'), ignore),
	('case.. last_stmt', ('(CASE x x)$case..', 'case.. last_stmt'), ignore),
	('case.. last_stmt', ('(CASE x)$postfixed', 'postfixed', 'last_stmt'), ignore),
	('case.. last_stmt', ('(CASE x)$[^postfixed]', '(if|if$-case|match) last_stmt'), ignore),
	
	
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
	
	('match$-else norm_stmt', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else) norm_stmt'), ignore),
	('match$-else norm_stmt', ('(MATCH x)$case..', 'case..$-else norm_stmt'), ignore),
	('case..$-else norm_stmt', ('(CASE x x)$case..', 'case..$-else norm_stmt'), ignore),
	('case..$-else norm_stmt', ('(CASE x)$[^postfixed]', '(if$-else|if$-else-case|match$-else) norm_stmt'), ignore),
	
	('match$-else last_stmt', ('MATCH', 'prefix*', '(if$-else|if$-else-case|match$-else) last_stmt'), ignore),
	('match$-else last_stmt', ('(MATCH x)$case..', 'case..$-else last_stmt'), ignore),
	('case..$-else last_stmt', ('(CASE x x)$case..', 'case..$-else last_stmt'), ignore),
	('case..$-else last_stmt', ('(CASE x)$[^postfixed]', '(if$-else|if$-else-case|match$-else) last_stmt'), ignore),
	
	
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
	
	('(if|if$-else) norm_stmt', ('if', 'norm_stmt'), ignore),
	('(if|if$-else) norm_stmt', ('if$-else norm_stmt',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else) norm_stmt', ('if$-case norm_stmt',), ignore),
	('(if$-case|if$-else-case|match|match$-else) norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('(if$-case|if$-else-case|match|match$-else) norm_stmt', ('match norm_stmt',), ignore),
	('(if$-case|if$-else-case|match|match$-else) norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('(if$-else|if$-else-case|match$-else) norm_stmt', ('if$-else norm_stmt',), ignore),
	('(if$-else|if$-else-case|match$-else) norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('(if$-else|if$-else-case|match$-else) norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('(if|if$-case|match) norm_stmt', ('if', 'norm_stmt'), ignore),
	('(if|if$-case|match) norm_stmt', ('if$-case norm_stmt',), ignore),
	('(if|if$-case|match) norm_stmt', ('match norm_stmt',), ignore),
	
	('(if$-case|match) norm_stmt', ('if$-case norm_stmt',), ignore),
	('(if$-case|match) norm_stmt', ('match norm_stmt',), ignore),
	
	('(if$-else-case|match$-else) norm_stmt', ('if$-else-case norm_stmt',), ignore),
	('(if$-else-case|match$-else) norm_stmt', ('match$-else norm_stmt',), ignore),
	
	('(if|if$-else) last_stmt', ('if', 'last_stmt'), ignore),
	('(if|if$-else) last_stmt', ('if$-else last_stmt',), ignore),
	
	('(if$-case|if$-else-case|match|match$-else) last_stmt', ('if$-case last_stmt',), ignore),
	('(if$-case|if$-else-case|match|match$-else) last_stmt', ('if$-else-case last_stmt',), ignore),
	('(if$-case|if$-else-case|match|match$-else) last_stmt', ('match last_stmt',), ignore),
	('(if$-case|if$-else-case|match|match$-else) last_stmt', ('match$-else last_stmt',), ignore),
	
	('(if$-else|if$-else-case|match$-else) last_stmt', ('if$-else last_stmt',), ignore),
	('(if$-else|if$-else-case|match$-else) last_stmt', ('if$-else-case last_stmt',), ignore),
	('(if$-else|if$-else-case|match$-else) last_stmt', ('match$-else last_stmt',), ignore),
	
	('(if|if$-case|match) last_stmt', ('if', 'last_stmt'), ignore),
	('(if|if$-case|match) last_stmt', ('if$-case last_stmt',), ignore),
	('(if|if$-case|match) last_stmt', ('match last_stmt',), ignore),
	
	('(if$-case|match) last_stmt', ('if$-case last_stmt',), ignore),
	('(if$-case|match) last_stmt', ('match last_stmt',), ignore),
	
	('(if$-else-case|match$-else) last_stmt', ('if$-else-case last_stmt',), ignore),
	('(if$-else-case|match$-else) last_stmt', ('match$-else last_stmt',), ignore),
	
	
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
