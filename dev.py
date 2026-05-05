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
@a @b @c if a b
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
	
	('if', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', '(if|prim)'), ignore),
	('if', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', '(if|prim)'), ignore),
	
	('if$-else', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|if$-else|prim)'), ignore),
	('if$-else', ('IF', 'prefix*', 'ifx', '(if|if$-else|prim)'), ignore),
	('if$-else', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*'), ignore),
	('ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'ifx'), ignore),
	
	('(if|prim)', ('if',), ignore),
	('(if|prim)', ('prim',), ignore),
	
	('(if|if$-else|prim)', ('if',), ignore),
	('(if|if$-else|prim)', ('if$-else',), ignore),
	('(if|if$-else|prim)', ('prim',), ignore),
	
	('prefix*', ('prefix*', '@', 'prim'), ignore),
	('prefix*', (), ignore),
	
	('prim', ('INT',), NodeInt),
	('prim', ('IDENT',), NodeInt),
]
SYNTAX_RULES = [
	('S', ('prefix*', 'if'), ignore),
	('S', ('prefix*', 'if$-else'), ignore),
	
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
	('S', ('last_stmt',), ignore),
	
	# ('stmt_lst', ('norm_stmt', 'stmt_lst'), ignore),
	# ('stmt_lst', ('last_stmt',), ignore),
	
	# ('norm_stmt', ('prefix*', 'norm_ctrl'), ignore),
	# ('norm_stmt', ('prefixed', ';'), ignore),
	# ('norm_stmt', (';',), ignore),
	
	('last_stmt', ('prefix*', 'last_ifx'), ignore),
	('last_stmt', ('prefix*', 'if'), ignore),
	('last_stmt', ('prefixed',), ignore),
	('last_stmt', (), ignore),
	
	('last_ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'last_stmt'), ignore),
	('last_ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', 'last_ifx'), ignore),
	('last_ifx', ('IF', 'prefix*', 'ifx', '(if|prim)', 'last_stmt'), ignore),
	('last_ifx', ('IF', 'prefix*', 'ifx', 'last_ifx'), ignore),
	('last_ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'last_ifx'), ignore),
	('last_ifx', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'last_ifx'), ignore),
	
	
	('if', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', '(if|prim)'), ignore),
	('if', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', '(if|prim)'), ignore),
	
	('if$-else', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|if$-else|prim)'), ignore),
	('if$-else', ('IF', 'prefix*', 'ifx', '(if|if$-else|prim)'), ignore),
	('if$-else', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	('if$-else', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'if$-else'), ignore),
	
	('ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*'), ignore),
	('ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', '(if|prim)', 'prefix*', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'ifx'), ignore),
	('ifx', ('IF', 'prefix*', 'ifx', '(if|prim)', 'prefix*', 'ELSE', 'prefix*', 'ifx'), ignore),
	
	('(if|prim)', ('if',), ignore),
	('(if|prim)', ('prim',), ignore),
	
	('(if|if$-else|prim)', ('if',), ignore),
	('(if|if$-else|prim)', ('if$-else',), ignore),
	('(if|if$-else|prim)', ('prim',), ignore),
	
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
