from parser import Production
from lang_ast import (
	NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeCall, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeMatchCase, NodeNamedTuple, NodeNamedStruct
)

import typing
if typing.TYPE_CHECKING:
	from collections.abc import Callable
	from typing import Any
	from lang_ast import Node
	from scanner import Token


TOKEN_TYPES = [
	('TMPLT', r'template'),
	('CONTINUE', r'continue'),
	('STRUCT', r'struct'),
	('RETURN', r'return'),
	('MATCH', r'match'),
	('BREAK', r'break'),
	('TUPLE', r'tuple'),
	('WHILE', r'while'),
	('ELSE', r'else'),
	('CASE', r'case'),
	('LET', r'let'),
	('FOR', r'for'),
	('IF', r'if'),
	('FN', r'fn'),
	
	('BOOL', r'TRUE|FALSE|true|false'),
	('INT', r'[0-9]\d*'),
	('IDENT', r'[a-zA-Z_\$][a-zA-Z0-9_\$]*'),
	('STR', r'\"(?:[^\'\\]|\\[tnr])*\"'),  # See https://github.com/antlr/grammars-v4/blob/master/java/java8/Java8Lexer.g4
	
	('#[', r'#\['),
	('.{', r'\.\{'),
	('.[', r'\.\['),
	('.(', r'\.\('),
	('{', r'\{'),
	('}', r'\}'),
	('[', r'\['),
	(']', r'\]'),
	('(', r'\('),
	(')', r'\)'),
	
	(';', r';'),
	('=', r'='),
	('|', r'\|'),
	(',', r','),
	('||', r'\|\|'),
	('&&', r'&&'),
	('==', r'=='),
	('!=', r'!='),
	('<=', r'<='),
	('>=', r'>='),
	('<', r'<'),
	('>', r'>'),
	('+', r'\+'),
	('-', r'-'),
	('*', r'\*'),
	('/', r'/'),
	('%', r'%'),
	('&', r'&'),
	('!', r'!'),
	('~', r'~'),
	('#', r'#'),
	('@', r'@'),
	(':', r':'),
	('->', r'->'),
	('.', r'\.'),
	
	# TODO: Maybe forbid mult-line comment
	('COMMENT', r'//.*'),
]

TERMINALS = set(t for t, _ in TOKEN_TYPES)

def Identity[T](x: T) -> T:
	return x
SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable[..., Any]]] = [
	('S', ('stmt_lst',), lambda lst: NodeStruct(*lst)),
	
	# stmt_lst: norm_stmt* last_stmt
	('stmt_lst', ('norm_stmt', 'stmt_lst'), lambda x, xs: (*x, *xs)),
	('stmt_lst', ('last_stmt',), Identity),
]
def norm_stmt1(attr: tuple[Node, ...], x: Node):
	return x.AppendPrefix(*attr),
def norm_stmt2(attr: tuple[Node, ...], x_xs: tuple[Node, tuple[Node, ...]]):
	return x_xs[0].AppendPrefix(*attr), *x_xs[1]
SYNTAX_RULES += [
	# norm_stmt: def
	#          | norm_ctrl
	#          | (stmt|assign)? ;
	('norm_stmt', ('prefix*', 'fn_def'), norm_stmt1),
	('norm_stmt', ('prefix*', 'fn_def$-else norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'fn_def$-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'fn_def$-else-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'named_def'), lambda attr, x: (x.AppendPrefix(*attr),)),
	('norm_stmt', ('prefix*', 'named_def$-else norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'named_def$-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'named_def$-else-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'if'), lambda attr, x: (x.AppendPrefix(*attr),)),
	('norm_stmt', ('prefix*', 'if$-else norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'if$-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'if$-else-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'for'), lambda attr, x: (x.AppendPrefix(*attr),)),
	('norm_stmt', ('prefix*', 'for$-else norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'for$-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'for$-else-case norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'match norm_stmt'), norm_stmt2),
	('norm_stmt', ('prefix*', 'match$-else norm_stmt'), norm_stmt2),
	('norm_stmt', ('stmt', ';'), lambda x, SEMI: (x,)),
	('norm_stmt', ('assign', ';'), lambda x, SEMI: (x,)),
	('norm_stmt', (';',), lambda SEMI: ()),
	
	# last_stmt: last_ctrl
	#          | (stmt|assign)?
	('last_stmt', ('prefix*', 'fn_def$-else last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'fn_def$-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'fn_def$-else-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'named_def$-else last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'named_def$-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'named_def$-else-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'if$-else last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'if$-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'if$-else-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'for$-else last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'for$-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'for$-else-case last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'match last_stmt'), norm_stmt2),
	('last_stmt', ('prefix*', 'match$-else last_stmt'), norm_stmt2),
	('last_stmt', ('stmt',), lambda x: (x,)),
	('last_stmt', ('assign',), lambda x: (x,)),
	('last_stmt', (), lambda: ()),
	
	# stmt: prefix* RETURN expr
	#     | prefix* BREAK expr?
	#     | prefix* CONTINUE
	('stmt', ('prefix*', 'RETURN', 'expr'), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	('stmt', ('prefix*', 'BREAK', 'expr'), lambda attr, BREAK, x: NodeBreak(x).AppendPrefix(*attr)),
	('stmt', ('prefix*', 'BREAK'), lambda attr, BREAK: NodeBreak(NodeTuple()).AppendPrefix(*attr)),
	('stmt', ('prefix*', 'CONTINUE'), lambda attr, CONT: NodeContinue().AppendPrefix(*attr)),
	
	# expr: (assign|..assign)
	('expr', ('assign',), Identity),
	('expr', ('..assign',), Identity),
	
	# assign: (let|union) = assign
	#       | (let|union)
	('assign', ('(let|union)', '=', 'expr'), lambda var, EQ, val: NodeAssign(var, val)),
	('assign', ('(let|union)',), Identity),
	('..assign', ('..union', '=', 'expr'), lambda var, EQ, val: NodeAssign(var, val)),
	('..assign', ('..union',), Identity),
	('(let|union)', ('prefix*', 'LET', '(union|..union)'), lambda attr, LET, label: NodeDecl(label).AppendPrefix(*attr)),
	('(let|union)', ('union',), Identity),
	('(union|..union)', ('union',), Identity),
	('(union|..union)', ('..union',), Identity),
	
	# union: tuple? (| tuple?)*
	('union', ('tuple',), Identity),
	('union', ('tuple', '|', 'union..'), lambda x, PIPE, xs: NodeUnion(x, *xs)),
	('union', ('|', 'union..'), lambda PIPE, xs: NodeUnion(*xs)),
	('..union', ('..tuple',), Identity),
	('..union', ('..tuple', '|', 'union..'), lambda x, PIPE, xs: NodeUnion(x, *xs)),
	('union..', ('union..', '|', '(tuple|..tuple)'), lambda xs, PIPE, x: (*xs, x)),
	('union..', ('union..', '|'), lambda xs, PIPE: xs),
	('union..', ('(tuple|..tuple)',), lambda x: (x,)),
	('union..', (), lambda: ()),
	('(tuple|..tuple)', ('tuple',), Identity),
	('(tuple|..tuple)', ('..tuple',), Identity),
	
	# tuple: suffixed? (, suffixed?)*
	('tuple', ('suffixed',), Identity),
	('tuple', ('suffixed', ',', 'tuple..'), lambda x, COMMA, xs: NodeTuple(x, *xs)),
	('tuple', (',', 'tuple..',), lambda COMMA, xs: NodeTuple(*xs)),
	('..tuple', ('..suffixed',), Identity),
	('..tuple', ('..suffixed', ',', 'tuple..'), lambda x, COMMA, xs: NodeTuple(x, *xs)),
	('tuple..', ('tuple..', ',', '(suffixed|..suffixed)'), lambda xs, COMMA, x: (*xs, x)),
	('tuple..', ('tuple..', ','), lambda xs, COMMA: xs),
	('tuple..', ('(suffixed|..suffixed)',), lambda x: (x,)),
	('tuple..', (), lambda: ()),
	('(suffixed|..suffixed)', ('suffixed',), Identity),
	('(suffixed|..suffixed)', ('..suffixed',), Identity),
	
	# suffixed: lmbd suffix*
	('suffixed', ('lmbd', 'suffix*'), lambda x, hint: x.AppendSuffix(*hint)),
	('..suffixed', ('..lmbd', 'suffix*'), lambda x, hint: x.AppendSuffix(*hint)),
	('suffix*', ('suffix*', ':', '(lmbd|..lmbd)'), lambda xs, COLON, x: (*xs, x)),
	('suffix*', (), lambda: ()),
	
	# lmbd: or -> lmbd
	#     | or
	('lmbd', ('or', '->', '(lmbd|..lmbd)'), lambda param, ARROW, body: NodeFunc(param, EnsureCompound(body))),
	('lmbd', ('or',), Identity),
	('..lmbd', ('..or', '->', '(lmbd|..lmbd)'), lambda param, ARROW, body: NodeFunc(param, EnsureCompound(body))),
	('..lmbd', ('..or',), Identity),
	('(lmbd|..lmbd)', ('lmbd',), Identity),
	('(lmbd|..lmbd)', ('..lmbd',), Identity),
	
	# or: or || and
	#   | and
	('or', ('or', '||', '(and|..and)'), lambda lhs, OR, rhs: NodeLogicalOp(OR, lhs, rhs)),
	('or', ('and',), Identity),
	('..or', ('..or', '||', '(and|..and)'), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	('..or', ('..and',), Identity),
	('(and|..and)', ('and',), Identity),
	('(and|..and)', ('..and',), Identity),
	
	# and: and && eq
	#    | eq
	('and', ('and', '&&', '(eq|..eq)'), lambda lhs, OP, rhs: NodeLogicalOp(OP, lhs, rhs)),
	('and', ('eq',), Identity),
	('..and', ('..and', '&&', '(eq|..eq)'), lambda lhs, OP, rhs: NodeBinaryOp(OP, lhs, rhs)),
	('..and', ('..eq',), Identity),
	('(eq|..eq)', ('eq',), Identity),
	('(eq|..eq)', ('..eq',), Identity),
]
def binary_op(lhs: Node, OP: Token, rhs: Node):
	return NodeBinaryOp(OP, lhs, rhs)
SYNTAX_RULES += [
	# eq: eq (==|!=) rel
	#   | rel
	('eq', ('eq', '(==|!=)', '(rel|..rel)'), binary_op),
	('eq', ('rel',), Identity),
	('..eq', ('..eq', '(==|!=)', '(rel|..rel)'), binary_op),
	('..eq', ('..rel',), Identity),
	('(==|!=)', ('==',), Identity),
	('(==|!=)', ('!=',), Identity),
	('(rel|..rel)', ('rel',), Identity),
	('(rel|..rel)', ('..rel',), Identity),
	
	# rel: rel (<=|>=|<|>) add
	#    | add
	('rel', ('rel', '(<=|>=|<|>)', '(add|..add)'), binary_op),
	('rel', ('add',), Identity),
	('..rel', ('..rel', '(<=|>=|<|>)', '(add|..add)'), binary_op),
	('..rel', ('..add',), Identity),
	('(<=|>=|<|>)', ('>=',), Identity),
	('(<=|>=|<|>)', ('<=',), Identity),
	('(<=|>=|<|>)', ('>',), Identity),
	('(<=|>=|<|>)', ('<',), Identity),
	('(add|..add)', ('add',), Identity),
	('(add|..add)', ('..add',), Identity),
	
	# add: add (+|-) mul
	#    | mul
	('add', ('add', '(+|-)', '(mul|..mul)'), binary_op),
	('add', ('mul',), Identity),
	('..add', ('..add', '(+|-)', '(mul|..mul)'), binary_op),
	('..add', ('..mul',), Identity),
	('(+|-)', ('+',), Identity),
	('(+|-)', ('-',), Identity),
	('(mul|..mul)', ('mul',), Identity),
	('(mul|..mul)', ('..mul',), Identity),
	
	# mul: mul (*|/|%) unary
	#    | unary
	('mul', ('mul', '(*|/|%)', '(unary|def|prefix* ctrl_flow)'), binary_op),
	('mul', ('unary',), Identity),
	('..mul', ('..mul', '(*|/|%)', '(unary|def|prefix* ctrl_flow)'), binary_op),
	('..mul', ('prefix*', 'def',), lambda attr, x: x.AppendPrefix(*attr)),
	('..mul', ('prefix*', 'ctrl_flow'), lambda attr, x: x.AppendPrefix(*attr)),
	('(*|/|%)', ('*',), Identity),
	('(*|/|%)', ('/',), Identity),
	('(*|/|%)', ('%',), Identity),
	('(unary|def|prefix* ctrl_flow)', ('unary',), Identity),
	('(unary|def|prefix* ctrl_flow)', ('prefix*', 'def'), lambda attr, x: x.AppendPrefix(*attr)),
	('(unary|def|prefix* ctrl_flow)', ('prefix*', 'ctrl_flow'), lambda attr, x: x.AppendPrefix(*attr)),
	
	# unary: (+|-|!|&|*) (unary|def|prefix* ctrl_flow)
	#      | (prefix* #[_])? call
	#      | prefix* fn_like
	#      | prefixed
	('unary', ('(+|-|!|&|*)', '(unary|def|prefix* ctrl_flow)'), lambda OP, val: NodeUnaryOp(OP, val)),
	('unary', ('(prefix* #[_])?', 'call'), lambda attr, x: x.AppendPrefix(*attr)),
	('unary', ('prefix*', 'fn_like'), lambda attr, x: x.AppendPrefix(*attr)),
	('unary', ('prefixed',), Identity),
	('(+|-|!|&|*)', ('+',), Identity),
	('(+|-|!|&|*)', ('-',), Identity),
	('(+|-|!|&|*)', ('!',), Identity),
	('(+|-|!|&|*)', ('&',), Identity),
	('(+|-|!|&|*)', ('*',), Identity),
	
	# call: postfixed+ arg
	('call', ('postfixed+', 'prefix*', '#[', '(stmt|expr)', ']', 'call'), lambda func, attr_xs, HASH, LBR, attr, RBR, arg: NodeCall(func, arg).AppendPrefix(*attr_xs, attr)),
	('call', ('postfixed+',), Identity),
	('call', ('postfixed+', 'prefix*', 'def'), lambda func, attr, arg: NodeCall(func, arg).AppendPrefix(*attr)),
	('call', ('postfixed+', 'prefix*', '[^postfixed]'), lambda func, attr, arg: NodeCall(func, arg).AppendPrefix(*attr)),
	('call', ('postfixed+', 'prefixed'), lambda func, arg: NodeCall(func, arg)),
	
	# def: fn
	#    | named
	('def', ('fn_def',), Identity),
	('def', ('fn_def$-else',), Identity),
	('def', ('fn_def$-case',), Identity),
	('def', ('fn_def$-else-case',), Identity),
	('def', ('named_def',), Identity),
	('def', ('named_def$-else',), Identity),
	('def', ('named_def$-case',), Identity),
	('def', ('named_def$-else-case',), Identity),
]
def fn2ctor(t: Token):
	return NodeFunc if t.GetType() == 'FN' else NodeTemplate
def fn_def1(fn_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], loop: Node):
	var = NodeDecl(fn_x_x_attr[1])
	ctor = fn2ctor(fn_x_x_attr[0])
	func = ctor(fn_x_x_attr[2], EnsureCompound(loop).AppendPrefix(*fn_x_x_attr[3]))
	return NodeAssign(var, func)
def fn_def2(fn_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], x_follow: tuple[Node, tuple[Node, ...]]):
	var = NodeDecl(fn_x_x_attr[1])
	ctor = fn2ctor(fn_x_x_attr[0])
	func = ctor(fn_x_x_attr[2], EnsureCompound(x_follow[0]).AppendPrefix(*fn_x_x_attr[3]))
	return NodeAssign(var, func), x_follow[1]
SYNTAX_RULES += [
	# fn_def: (FN|TMPLT) x x x (ELSE x)?
	('fn_def', ('(FN|TMPLT) _ _ (prefix* #[_])?', 'postfixed'), fn_def1),
	('fn_def', ('(FN|TMPLT) _ _ prefix*', 'x'), fn_def1),
	
	('fn_def$-else', ('(FN|TMPLT) _ _ prefix*', 'x$-else'), fn_def1),
	('fn_def$-else norm_stmt', ('(FN|TMPLT) _ _ prefix*', 'x$-else norm_stmt'), fn_def2),
	('fn_def$-else last_stmt', ('(FN|TMPLT) _ _ prefix*', 'x$-else last_stmt'), fn_def2),
	
	('fn_def$-case', ('(FN|TMPLT) _ _ prefix*', 'x$-case'), fn_def1),
	('fn_def$-case norm_stmt', ('(FN|TMPLT) _ _ prefix*', 'x$-case norm_stmt'), fn_def2),
	('fn_def$-case last_stmt', ('(FN|TMPLT) _ _ prefix*', 'x$-case last_stmt'), fn_def2),
	
	('fn_def$-else-case', ('(FN|TMPLT) _ _ prefix*', 'x$-else-case'), fn_def1),
	('fn_def$-else-case norm_stmt', ('(FN|TMPLT) _ _ prefix*', 'x$-else-case norm_stmt'), fn_def2),
	('fn_def$-else-case last_stmt', ('(FN|TMPLT) _ _ prefix*', 'x$-else-case last_stmt'), fn_def2),
]
def named2ctor(t: Token):
	return NodeNamedTuple if t.GetType() == 'TUPLE' else NodeNamedStruct
def named_def1(named_x_attr: tuple[Token, Node, tuple[Node, ...]], body: Node):
	var = NodeDecl(named_x_attr[1])
	ctor = named2ctor(named_x_attr[0])
	named = ctor(body.AppendPrefix(*named_x_attr[2]))
	return NodeAssign(var, named)
def named_def2(named_x_attr: tuple[Token, Node, tuple[Node, ...]], x_follow: tuple[Node, tuple[Node, ...]]):
	var = NodeDecl(named_x_attr[1])
	ctor = named2ctor(named_x_attr[0])
	named = ctor(x_follow[0].AppendPrefix(*named_x_attr[2]))
	return NodeAssign(var, named), x_follow[1]
SYNTAX_RULES += [
	# named_def: (TUPLE|STRUCT) x x
	('named_def', ('(TUPLE|STRUCT) _ (prefix* #[_])?', 'postfixed'), named_def1),
	('named_def', ('(TUPLE|STRUCT) _ prefix*', 'x'), named_def1),
	
	('named_def$-else', ('(TUPLE|STRUCT) _ prefix*', 'x$-else'), named_def1),
	('named_def$-else norm_stmt', ('(TUPLE|STRUCT) _ prefix*', 'x$-else norm_stmt'), named_def2),
	('named_def$-else last_stmt', ('(TUPLE|STRUCT) _ prefix*', 'x$-else last_stmt'), named_def2),
	
	('named_def$-case', ('(TUPLE|STRUCT) _ prefix*', 'x$-case'), named_def1),
	('named_def$-case norm_stmt', ('(TUPLE|STRUCT) _ prefix*', 'x$-case norm_stmt'), named_def2),
	('named_def$-case last_stmt', ('(TUPLE|STRUCT) _ prefix*', 'x$-case last_stmt'), named_def2),
	
	('named_def$-else-case', ('(TUPLE|STRUCT) _ prefix*', 'x$-else-case'), named_def1),
	('named_def$-else-case norm_stmt', ('(TUPLE|STRUCT) _ prefix*', 'x$-else-case norm_stmt'), named_def2),
	('named_def$-else-case last_stmt', ('(TUPLE|STRUCT) _ prefix*', 'x$-else-case last_stmt'), named_def2),
]
def fn1(fn_x_attr: tuple[Token, Node, tuple[Node, ...]], body: Node):
	ctor = fn2ctor(fn_x_attr[0])
	return ctor(fn_x_attr[1], body.AppendPrefix(*fn_x_attr[2]))
def fn2(fn_x_attr: tuple[Token, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	ctor = fn2ctor(fn_x_attr[0])
	return ctor(fn_x_attr[1], x_attr[0].AppendPrefix(*fn_x_attr[2])), x_attr[1]
SYNTAX_RULES += [
	('fn', ('(FN|TMPLT) _ (prefix* #[_])?', 'postfixed'), fn1),
	('fn', ('(FN|TMPLT) _ prefix*', 'x'), fn1),
	
	
	('fn$-else', ('(FN|TMPLT) _ prefix*', 'x$-else'), fn1),
	('fn$-else prefix*', ('(FN|TMPLT) _ prefix*', 'x$-else prefix*'), fn2),
	('fn$-else (prefix* #[_])?', ('(FN|TMPLT) _ prefix*', 'x$-else (prefix* #[_])?'), fn2),
	('fn$-else norm_stmt', ('(FN|TMPLT) _ prefix*', 'x$-else norm_stmt'), fn2),
	('fn$-else last_stmt', ('(FN|TMPLT) _ prefix*', 'x$-else last_stmt'), fn2),
	
	
	('fn$-case', ('(FN|TMPLT) _ prefix*', 'x$-case'), fn1),
	('fn$-case prefix*', ('(FN|TMPLT) _ prefix*', 'x$-case prefix*'), fn2),
	('fn$-case (prefix* #[_])?', ('(FN|TMPLT) _ prefix*', 'x$-case (prefix* #[_])?'), fn2),
	('fn$-case norm_stmt', ('(FN|TMPLT) _ prefix*', 'x$-case norm_stmt'), fn2),
	('fn$-case last_stmt', ('(FN|TMPLT) _ prefix*', 'x$-case last_stmt'), fn2),
	
	
	('fn$-else-case', ('(FN|TMPLT) _ prefix*', 'x$-else-case'), fn1),
	('fn$-else-case prefix*', ('(FN|TMPLT) _ prefix*', 'x$-else-case prefix*'), fn2),
	('fn$-else-case (prefix* #[_])?', ('(FN|TMPLT) _ prefix*', 'x$-else-case (prefix* #[_])?'), fn2),
	('fn$-else-case norm_stmt', ('(FN|TMPLT) _ prefix*', 'x$-else-case norm_stmt'), fn2),
	('fn$-else-case last_stmt', ('(FN|TMPLT) _ prefix*', 'x$-else-case last_stmt'), fn2),
]
def named1(named: Token, attr: tuple[Node, ...], body: Node):
	ctor = named2ctor(named)
	return ctor(body.AppendPrefix(*attr))
def named2(named: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	ctor = named2ctor(named)
	return ctor(x_attr[0].AppendPrefix(*attr)), x_attr[1]
SYNTAX_RULES += [
	('named', ('(TUPLE|STRUCT)', '(prefix* #[_])?', 'postfixed'), named1),
	('named', ('(TUPLE|STRUCT)', 'prefix*', 'x'), named1),
	
	
	('named$-else', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else'), named1),
	('named$-else prefix*', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else prefix*'), named2),
	('named$-else (prefix* #[_])?', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else (prefix* #[_])?'), named2),
	('named$-else norm_stmt', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else norm_stmt'), named2),
	('named$-else last_stmt', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else last_stmt'), named2),
	
	
	('named$-case', ('(TUPLE|STRUCT)', 'prefix*', 'x$-case'), named1),
	('named$-case prefix*', ('(TUPLE|STRUCT)', 'prefix*', 'x$-case prefix*'), named2),
	('named$-case (prefix* #[_])?', ('(TUPLE|STRUCT)', 'prefix*', 'x$-case (prefix* #[_])?'), named2),
	('named$-case norm_stmt', ('(TUPLE|STRUCT)', 'prefix*', 'x$-case norm_stmt'), named2),
	('named$-case last_stmt', ('(TUPLE|STRUCT)', 'prefix*', 'x$-case last_stmt'), named2),
	
	
	('named$-else-case', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else-case'), named1),
	('named$-else-case prefix*', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else-case prefix*'), named2),
	('named$-else-case (prefix* #[_])?', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else-case (prefix* #[_])?'), named2),
	('named$-else-case norm_stmt', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else-case norm_stmt'), named2),
	('named$-else-case last_stmt', ('(TUPLE|STRUCT)', 'prefix*', 'x$-else-case last_stmt'), named2),
]
def if2ctor(t: Token):
	return NodeIfElse if t.GetType() == 'IF' else NodeWhileElse
def if1(if_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], otherwise: Node):
	ctor = if2ctor(if_x_x_attr[0])
	return ctor(*if_x_x_attr[1:3], otherwise.AppendPrefix(*if_x_x_attr[3]))
def else1(ELSE: Token, attr: tuple[Node, ...], false_br: Node):
	return EnsureCompound(false_br.AppendPrefix(*attr))
def if2(if_x_attr: tuple[Token, Node, tuple[Node, ...]], true_br: Node):
	ctor = if2ctor(if_x_attr[0])
	return ctor(if_x_attr[1], true_br.AppendPrefix(*if_x_attr[2]), NodeCompound())
def if3(if_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], ELSE: Token, attr: tuple[Node, ...], false_br: Node):
	return if1(if_x_x_attr, EnsureCompound(false_br.AppendPrefix(*attr)))
def if4(if_x_attr: tuple[Token, Node, tuple[Node, ...]], true_br: Node, attr: tuple[Node, ...]):
	return if2(if_x_attr, true_br), attr
def if5(if_x_attr: tuple[Token, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return if4(if_x_attr, *x_attr)
def if6(if_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], ELSE: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return if3(if_x_x_attr, ELSE, attr, x_attr[0]), x_attr[1]
SYNTAX_RULES += [
	('if', ('(IF|WHILE) _ _ prefix*', 'else'), if1),
	('else', ('ELSE', 'prefix*', 'x'), else1),
	('else', ('ELSE', '(prefix* #[_])?', 'postfixed'), else1),
	
	
	('if$-else', ('(IF|WHILE) _ (prefix* #[_])?', 'postfixed'), if2),
	('if$-else', ('(IF|WHILE) _ prefix*', 'x$-?else'), if2),
	('if$-else', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else'), if3),
	
	('if$-else prefix*', ('(IF|WHILE) _ (prefix* #[_])?', 'postfixed', 'prefix*'), if4),
	('if$-else prefix*', ('(IF|WHILE) _ prefix*', 'x$-?else prefix*'), if5),
	('if$-else prefix*', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else prefix*'), if6),
	
	('if$-else (prefix* #[_])?', ('(IF|WHILE) _ (prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), if4),
	('if$-else (prefix* #[_])?', ('(IF|WHILE) _ prefix*', 'x$-?else (prefix* #[_])?'), if5),
	('if$-else (prefix* #[_])?', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else (prefix* #[_])?'), if6),
	
	('if$-else norm_stmt', ('(IF|WHILE) _ (prefix* #[_])?', 'postfixed', 'norm_stmt'), if4),
	('if$-else norm_stmt', ('(IF|WHILE) _ prefix*', 'x$-?else norm_stmt'), if5),
	('if$-else norm_stmt', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else norm_stmt'), if6),
	
	('if$-else last_stmt', ('(IF|WHILE) _ (prefix* #[_])?', 'postfixed', 'last_stmt'), if4),
	('if$-else last_stmt', ('(IF|WHILE) _ prefix*', 'x$-?else last_stmt'), if5),
	('if$-else last_stmt', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else last_stmt'), if6),
	
	
	('if$-case', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-case'), if3),
	('if$-case prefix*', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-case prefix*'), if6),
	('if$-case (prefix* #[_])?', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-case (prefix* #[_])?'), if6),
	('if$-case norm_stmt', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-case norm_stmt'), if6),
	('if$-case last_stmt', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-case last_stmt'), if6),
	
	
	('if$-else-case', ('(IF|WHILE) _ prefix*', 'x$-?else-case'), if2),
	('if$-else-case', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case'), if3),
	('if$-else-case prefix*', ('(IF|WHILE) _ prefix*', 'x$-?else-case prefix*'), if5),
	('if$-else-case prefix*', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case prefix*'), if6),
	('if$-else-case (prefix* #[_])?', ('(IF|WHILE) _ prefix*', 'x$-?else-case (prefix* #[_])?'), if5),
	('if$-else-case (prefix* #[_])?', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case (prefix* #[_])?'), if6),
	('if$-else-case norm_stmt', ('(IF|WHILE) _ prefix*', 'x$-?else-case norm_stmt'), if5),
	('if$-else-case norm_stmt', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case norm_stmt'), if6),
	('if$-else-case last_stmt', ('(IF|WHILE) _ prefix*', 'x$-?else-case last_stmt'), if5),
	('if$-else-case last_stmt', ('(IF|WHILE) _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case last_stmt'), if6),
]
def for1(for_x_x_x_attr: tuple[Token, Node, Node, Node, tuple[Node, ...]], otherwise: Node):
	return NodeForElse(*for_x_x_x_attr[1:4], otherwise.AppendPrefix(*for_x_x_x_attr[4]))
def for2(for_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], body: Node):
	return NodeForElse(*for_x_x_attr[1:3], EnsureCompound(body.AppendPrefix(*for_x_x_attr[3])), NodeCompound()),
def for3(for_x_x_x_attr: tuple[Token, Node, Node, Node, tuple[Node, ...]], ELSE: Token, attr: tuple[Node, ...], false_br: Node):
	return for1(for_x_x_x_attr, EnsureCompound(false_br.AppendPrefix(*attr)))
def for4(for_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], body: Node, attr: tuple[Node, ...]):
	return for2(for_x_x_attr, body), attr
def for5(for_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return for4(for_x_x_attr, *x_attr)
def for6(for_x_x_x_attr: tuple[Token, Node, Node, Node, tuple[Node, ...]], ELSE: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return for3(for_x_x_x_attr, ELSE, attr, x_attr[0]), x_attr[1]
SYNTAX_RULES += [
	('for', ('FOR _ _ _ prefix*', 'else',), for1),
	
	
	('for$-else', ('FOR _ _ (prefix* #[_])?', 'postfixed'), for2),
	('for$-else', ('FOR _ _ prefix*', 'x$-?else'), for2),
	('for$-else', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else'), for3),
	
	('for$-else prefix*', ('FOR _ _ (prefix* #[_])?', 'postfixed', 'prefix*'), for4),
	('for$-else prefix*', ('FOR _ _ prefix*', 'x$-?else prefix*'), for5),
	('for$-else prefix*', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else prefix*'), for6),
	
	('for$-else (prefix* #[_])?', ('FOR _ _ (prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), for4),
	('for$-else (prefix* #[_])?', ('FOR _ _ prefix*', 'x$-?else (prefix* #[_])?'), for5),
	('for$-else (prefix* #[_])?', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else (prefix* #[_])?'), for6),
	
	('for$-else norm_stmt', ('FOR _ _ (prefix* #[_])?', 'postfixed', 'norm_stmt'), for4),
	('for$-else norm_stmt', ('FOR _ _ prefix*', 'x$-?else norm_stmt'), for5),
	('for$-else norm_stmt', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else norm_stmt'), for6),
	
	('for$-else last_stmt', ('FOR _ _ (prefix* #[_])?', 'postfixed', 'last_stmt'), for4),
	('for$-else last_stmt', ('FOR _ _ prefix*', 'x$-?else last_stmt'), for5),
	('for$-else last_stmt', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else last_stmt'), for6),
	
	
	('for$-case', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-case'), for3),
	('for$-case prefix*', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-case prefix*'), for6),
	('for$-case (prefix* #[_])?', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-case (prefix* #[_])?'), for6),
	('for$-case norm_stmt', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-case norm_stmt'), for6),
	('for$-case last_stmt', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-case last_stmt'), for6),
	
	
	('for$-else-case', ('FOR _ _ prefix*', 'x$-?else-case'), for2),
	('for$-else-case', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case'), for3),
	('for$-else-case prefix*', ('FOR _ _ prefix*', 'x$-?else-case prefix*'), for5),
	('for$-else-case prefix*', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case prefix*'), for6),
	('for$-else-case (prefix* #[_])?', ('FOR _ _ prefix*', 'x$-?else-case (prefix* #[_])?'), for5),
	('for$-else-case (prefix* #[_])?', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case (prefix* #[_])?'), for6),
	('for$-else-case norm_stmt', ('FOR _ _ prefix*', 'x$-?else-case norm_stmt'), for5),
	('for$-else-case norm_stmt', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case norm_stmt'), for6),
	('for$-else-case last_stmt', ('FOR _ _ prefix*', 'x$-?else-case last_stmt'), for5),
	('for$-else-case last_stmt', ('FOR _ _ _ prefix*', 'ELSE', 'prefix*', 'x$-else-case last_stmt'), for6),
]
def match1(MATCH: Token, attr: tuple[Node, ...], val: Node):
	return NodeMatchCase(val.AppendPrefix(*attr), ())
def match2(match_x_attr: tuple[Token, Node, tuple[Node, ...]], xs: tuple[tuple[Node, Node], ...]):
	return NodeMatchCase(match_x_attr[1], ((xs[0][0].AppendPrefix(*match_x_attr[2]), xs[0][1]), *xs[1:]))
def case1(case_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], xs: tuple[tuple[Node, Node], ...]):
	return case_x_x_attr[1:3], (xs[0][0].AppendPrefix(*case_x_x_attr[3]), xs[0][1]), *xs[1:]
def case2(case_x_attr: tuple[Token, Node, tuple[Node, ...]], body: Node):
	return case_x_attr[1], EnsureCompound(body.AppendPrefix(*case_x_attr[2])),
def match3(MATCH: Token, attr: tuple[Node, ...], val: Node, attr2: tuple[Node, ...]):
	return match1(MATCH, attr, val), attr2
def match4(MATCH: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return match3(MATCH, attr, *x_attr)
def match5(match_x_attr: tuple[Token, Node, tuple[Node, ...]], xs_attr: tuple[tuple[tuple[Node, Node], ...], tuple[Node, ...]]):
	return match2(match_x_attr, xs_attr[0]), xs_attr[1]
def case3(case_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], xs_attr: tuple[tuple[tuple[Node, Node], ...], tuple[Node, ...]]):
	return case1(case_x_x_attr, xs_attr[0]), xs_attr[1]
def case4(case_x_attr: tuple[Token, Node, tuple[Node, ...]], body: Node, attr: tuple[Node, ...]):
	return case2(case_x_attr, body), attr
def case5(case_x_attr: tuple[Token, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return case4(case_x_attr, *x_attr)
SYNTAX_RULES += [
	('match', ('MATCH', '(prefix* #[_])?', 'postfixed'), match1),
	('match', ('MATCH', 'prefix*', 'x$-?case'), match1),
	('match', ('MATCH _ prefix*', 'case..'), match2),
	('case..', ('CASE _ _ prefix*', 'case..'), case1),
	('case..', ('CASE _ (prefix* #[_])?', 'postfixed'), case2),
	('case..', ('CASE _ prefix*', 'x$-?case'), case2),
	
	('match prefix*', ('MATCH', '(prefix* #[_])?', 'postfixed', 'prefix*'), match3),
	('match prefix*', ('MATCH', 'prefix*', 'x$-?case prefix*'), match4),
	('match prefix*', ('MATCH _ prefix*', 'case.. prefix*'), match5),
	('case.. prefix*', ('CASE _ _ prefix*', 'case.. prefix*'), case3),
	('case.. prefix*', ('CASE _ (prefix* #[_])?', 'postfixed', 'prefix*'), case4),
	('case.. prefix*', ('CASE _ prefix*', 'x$-?case prefix*'), case5),
	
	('match (prefix* #[_])?', ('MATCH', '(prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), match3),
	('match (prefix* #[_])?', ('MATCH', 'prefix*', 'x$-?case (prefix* #[_])?'), match4),
	('match (prefix* #[_])?', ('MATCH _ prefix*', 'case.. (prefix* #[_])?'), match5),
	('case.. (prefix* #[_])?', ('CASE _ _ prefix*', 'case.. (prefix* #[_])?'), case3),
	('case.. (prefix* #[_])?', ('CASE _ (prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), case4),
	('case.. (prefix* #[_])?', ('CASE _ prefix*', 'x$-?case (prefix* #[_])?'), case5),
	
	('match norm_stmt', ('MATCH', '(prefix* #[_])?', 'postfixed', 'norm_stmt'), match3),
	('match norm_stmt', ('MATCH', 'prefix*', 'x$-?case norm_stmt'), match4),
	('match norm_stmt', ('MATCH _ prefix*', 'case.. norm_stmt'), match5),
	('case.. norm_stmt', ('CASE _ _ prefix*', 'case.. norm_stmt'), case3),
	('case.. norm_stmt', ('CASE _ (prefix* #[_])?', 'postfixed', 'norm_stmt'), case4),
	('case.. norm_stmt', ('CASE _ prefix*', 'x$-?case norm_stmt'), case5),
	
	('match last_stmt', ('MATCH', '(prefix* #[_])?', 'postfixed', 'last_stmt'), match3),
	('match last_stmt', ('MATCH', 'prefix*', 'x$-?case last_stmt'), match4),
	('match last_stmt', ('MATCH _ prefix*', 'case.. last_stmt'), match5),
	('case.. last_stmt', ('CASE _ _ prefix*', 'case.. last_stmt'), case3),
	('case.. last_stmt', ('CASE _ (prefix* #[_])?', 'postfixed', 'last_stmt'), case4),
	('case.. last_stmt', ('CASE _ prefix*', 'x$-?case last_stmt'), case5),
	
	
	('match$-else', ('MATCH', 'prefix*', 'x$-else-?case'), match1),
	('match$-else', ('MATCH _ prefix*', 'case..$-else'), match2),
	('case..$-else', ('CASE _ _ prefix*', 'case..$-else'), case1),
	('case..$-else', ('CASE _ prefix*', 'x$-else-?case'), case2),
	
	('match$-else prefix*', ('MATCH', 'prefix*', 'x$-else-?case prefix*'), match4),
	('match$-else prefix*', ('MATCH _ prefix*', 'case..$-else prefix*'), match5),
	('case..$-else prefix*', ('CASE _ _ prefix*', 'case..$-else prefix*'), case3),
	('case..$-else prefix*', ('CASE _ prefix*', 'x$-else-?case prefix*'), case5),
	
	('match$-else (prefix* #[_])?', ('MATCH', 'prefix*', 'x$-else-?case (prefix* #[_])?'), match4),
	('match$-else (prefix* #[_])?', ('MATCH _ prefix*', 'case..$-else (prefix* #[_])?'), match5),
	('case..$-else (prefix* #[_])?', ('CASE _ _ prefix*', 'case..$-else (prefix* #[_])?'), case3),
	('case..$-else (prefix* #[_])?', ('CASE _ prefix*', 'x$-else-?case (prefix* #[_])?'), case5),
	
	('match$-else norm_stmt', ('MATCH', 'prefix*', 'x$-else-?case norm_stmt'), match4),
	('match$-else norm_stmt', ('MATCH _ prefix*', 'case..$-else norm_stmt'), match5),
	('case..$-else norm_stmt', ('CASE _ _ prefix*', 'case..$-else norm_stmt'), case3),
	('case..$-else norm_stmt', ('CASE _ prefix*', 'x$-else-?case norm_stmt'), case5),
	
	('match$-else last_stmt', ('MATCH', 'prefix*', 'x$-else-?case last_stmt'), match4),
	('match$-else last_stmt', ('MATCH _ prefix*', 'case..$-else last_stmt'), match5),
	('case..$-else last_stmt', ('CASE _ _ prefix*', 'case..$-else last_stmt'), case3),
	('case..$-else last_stmt', ('CASE _ prefix*', 'x$-else-?case last_stmt'), case5),
]
def fn_x_x_pref1(fn_x_attr: tuple[Token, Node, tuple[Node, ...]], param: Node, attr: tuple[Node, ...]):
	return *fn_x_attr[:2], param.AppendPrefix(*fn_x_attr[2]), attr
def fn_x_x_pref2(fn_x_attr: tuple[Token, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return fn_x_x_pref1(fn_x_attr, *x_attr)
def fn_x_pref1(fn: Token, attr: tuple[Node, ...], param: Node, attr2: tuple[Node, ...]):
	return fn, param.AppendPrefix(*attr), attr2
def fn_x_pref2(fn: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return fn_x_pref1(fn, attr, *x_attr)
SYNTAX_RULES += [
	('(FN|TMPLT) _ _ (prefix* #[_])?', ('(FN|TMPLT) _ (prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), fn_x_x_pref1),
	('(FN|TMPLT) _ _ (prefix* #[_])?', ('(FN|TMPLT) _ prefix*', '[^postfixed] (prefix* #[_])?'), fn_x_x_pref2),
	
	('(FN|TMPLT) _ _ prefix*', ('(FN|TMPLT) _ (prefix* #[_])?', 'postfixed', 'prefix*'), fn_x_x_pref1),
	('(FN|TMPLT) _ _ prefix*', ('(FN|TMPLT) _ prefix*', '[^postfixed] prefix*'), fn_x_x_pref2),
	
	
	('(FN|TMPLT) _ (prefix* #[_])?', ('(FN|TMPLT)', '(prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), fn_x_pref1),
	('(FN|TMPLT) _ (prefix* #[_])?', ('(FN|TMPLT)', 'prefix*', '[^postfixed] (prefix* #[_])?'), fn_x_pref2),
	
	('(FN|TMPLT) _ prefix*', ('(FN|TMPLT)', '(prefix* #[_])?', 'postfixed', 'prefix*'), fn_x_pref1),
	('(FN|TMPLT) _ prefix*', ('(FN|TMPLT)', 'prefix*', '[^postfixed] prefix*'), fn_x_pref2),
	
	('(FN|TMPLT)', ('FN',), Identity),
	('(FN|TMPLT)', ('TMPLT',), Identity),
]
def named_x_pref1(named: Token, attr: tuple[Node, ...], body: Node, attr2: tuple[Node, ...]):
	return named, body.AppendPrefix(*attr), attr2
def named_x_pref2(named: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return named_x_pref1(named, attr, *x_attr)
SYNTAX_RULES += [
	('(TUPLE|STRUCT) _ (prefix* #[_])?', ('(TUPLE|STRUCT)', '(prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), named_x_pref1),
	('(TUPLE|STRUCT) _ (prefix* #[_])?', ('(TUPLE|STRUCT)', 'prefix*', '[^postfixed] (prefix* #[_])?'), named_x_pref2),
	
	('(TUPLE|STRUCT) _ prefix*', ('(TUPLE|STRUCT)', '(prefix* #[_])?', 'postfixed', 'prefix*'), named_x_pref1),
	('(TUPLE|STRUCT) _ prefix*', ('(TUPLE|STRUCT)', 'prefix*', '[^postfixed] prefix*'), named_x_pref2),
	
	('(TUPLE|STRUCT)', ('TUPLE',), Identity),
	('(TUPLE|STRUCT)', ('STRUCT',), Identity),
]
def if_x_x_pref1(if_x_pref: tuple[Token, Node, tuple[Node, ...]], true_br: Node, attr: tuple[Node, ...]):
	return *if_x_pref[:2], EnsureCompound(true_br.AppendPrefix(*if_x_pref[2])), attr
def if_x_x_pref2(if_x_pref: tuple[Token, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return if_x_x_pref1(if_x_pref, *x_attr)
def if_x_pref1(IF: Token, attr: tuple[Node, ...], cond: Node, attr2: tuple[Node, ...]):
	return IF, cond.AppendPrefix(*attr), attr2
def if_x_pref2(IF: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return if_x_pref1(IF, attr, *x_attr)
SYNTAX_RULES += [
	('(IF|WHILE) _ _ prefix*', ('(IF|WHILE) _ (prefix* #[_])?', 'postfixed', 'prefix*'), if_x_x_pref1),
	('(IF|WHILE) _ _ prefix*', ('(IF|WHILE) _ prefix*', 'x$-?case prefix*'), if_x_x_pref2),
	
	('(IF|WHILE) _ (prefix* #[_])?', ('(IF|WHILE)', '(prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), if_x_pref1),
	('(IF|WHILE) _ (prefix* #[_])?', ('(IF|WHILE)', 'prefix*', '[^postfixed] (prefix* #[_])?'), if_x_pref2),
	
	('(IF|WHILE) _ prefix*', ('(IF|WHILE)', '(prefix* #[_])?', 'postfixed', 'prefix*'), if_x_pref1),
	('(IF|WHILE) _ prefix*', ('(IF|WHILE)', 'prefix*', '[^postfixed] prefix*'), if_x_pref2),
	
	('(IF|WHILE)', ('IF',), Identity),
	('(IF|WHILE)', ('WHILE',), Identity),
]
def for_x_x_x_pref1(for_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], x: Node, attr: tuple[Node, ...]):
	return *for_x_x_attr[:3], EnsureCompound(x.AppendPrefix(*for_x_x_attr[3])), attr
def for_x_x_x_pref2(for_x_x_attr: tuple[Token, Node, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return for_x_x_x_pref1(for_x_x_attr, *x_attr)
def for_x_x_pref1(for_x_attr: tuple[Token, Node, tuple[Node, ...]], var: Node, attr: tuple[Node, ...]):
	return *for_x_attr[:2], var.AppendPrefix(*for_x_attr[2]), attr
def for_x_x_pref2(for_x_attr: tuple[Token, Node, tuple[Node, ...]], x_attr: tuple[Node, tuple[Node, ...]]):
	return for_x_x_pref1(for_x_attr, *x_attr)
def for_x_pref1(FOR: Token, attr: tuple[Node, ...], itr: Node, attr2: tuple[Node, ...]):
	return FOR, itr.AppendPrefix(*attr), attr2
def for_x_pref2(FOR: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return for_x_pref1(FOR, attr, *x_attr)
SYNTAX_RULES += [
	('FOR _ _ _ prefix*', ('FOR _ _ (prefix* #[_])?', 'postfixed', 'prefix*'), for_x_x_x_pref1),
	('FOR _ _ _ prefix*', ('FOR _ _ prefix*', 'x$-?case prefix*'), for_x_x_x_pref2),
	
	('FOR _ _ (prefix* #[_])?', ('FOR _ (prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), for_x_x_pref1),
	('FOR _ _ (prefix* #[_])?', ('FOR _ prefix*', '[^postfixed] (prefix* #[_])?'), for_x_x_pref2),
	
	('FOR _ _ prefix*', ('FOR _ (prefix* #[_])?', 'postfixed', 'prefix*'), for_x_x_pref1),
	('FOR _ _ prefix*', ('FOR _ prefix*', '[^postfixed] prefix*'), for_x_x_pref2),
	
	('FOR _ (prefix* #[_])?', ('FOR', '(prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), for_x_pref1),
	('FOR _ (prefix* #[_])?', ('FOR', 'prefix*', '[^postfixed] (prefix* #[_])?'), for_x_pref2),
	
	('FOR _ prefix*', ('FOR', '(prefix* #[_])?', 'postfixed', 'prefix*'), for_x_pref1),
	('FOR _ prefix*', ('FOR', 'prefix*', '[^postfixed] prefix*'), for_x_pref2),
]
def match_x_pref1(MATCH: Token, attr: tuple[Node, ...], val: Node, attr2: tuple[Node, ...]):
	return MATCH, val.AppendPrefix(*attr), attr2
def match_x_pref2(MATCH: Token, attr: tuple[Node, ...], x_attr: tuple[Node, tuple[Node, ...]]):
	return match_x_pref1(MATCH, attr, *x_attr)
SYNTAX_RULES += [
	('MATCH _ prefix*', ('MATCH', '(prefix* #[_])?', 'postfixed', 'prefix*'), match_x_pref1),
	('MATCH _ prefix*', ('MATCH', 'prefix*', 'x$-?else prefix*'), match_x_pref2),
	
	('CASE _ _ prefix*', ('CASE _ (prefix* #[_])?', 'postfixed', 'prefix*'), match_x_pref1),
	('CASE _ _ prefix*', ('CASE _ prefix*', 'x$-?else prefix*'), match_x_pref2),
	
	('CASE _ (prefix* #[_])?', ('CASE', '(prefix* #[_])?', 'postfixed', '(prefix* #[_])?'), match_x_pref1),
	('CASE _ (prefix* #[_])?', ('CASE', 'prefix*', '[^postfixed] (prefix* #[_])?'), match_x_pref2),
	
	('CASE _ prefix*', ('CASE', '(prefix* #[_])?', 'postfixed', 'prefix*'), match_x_pref1),
	('CASE _ prefix*', ('CASE', 'prefix*', '[^postfixed] prefix*'), match_x_pref2),
]
def x1(x: Node, follow: tuple[Node, ...]):
	return x, follow
SYNTAX_RULES += [
	('[^postfixed]', ('fn_like',), Identity),
	('[^postfixed]', ('ctrl_flow',), Identity),
	
	('fn_like', ('fn',), Identity),
	('fn_like', ('fn$-else',), Identity),
	('fn_like', ('fn$-case',), Identity),
	('fn_like', ('fn$-else-case',), Identity),
	('fn_like', ('named',), Identity),
	('fn_like', ('named$-else',), Identity),
	('fn_like', ('named$-case',), Identity),
	('fn_like', ('named$-else-case',), Identity),
	('fn_like', ('scoped',), Identity),
	
	('ctrl_flow', ('if',), Identity),
	('ctrl_flow', ('if$-else',), Identity),
	('ctrl_flow', ('if$-case',), Identity),
	('ctrl_flow', ('if$-else-case',), Identity),
	('ctrl_flow', ('for',), Identity),
	('ctrl_flow', ('for$-else',), Identity),
	('ctrl_flow', ('for$-case',), Identity),
	('ctrl_flow', ('for$-else-case',), Identity),
	('ctrl_flow', ('match',), Identity),
	('ctrl_flow', ('match$-else',), Identity),
	
	('[^postfixed] prefix*', ('fn', 'prefix*'), x1),
	('[^postfixed] prefix*', ('fn$-else prefix*',), Identity),
	('[^postfixed] prefix*', ('fn$-case prefix*',), Identity),
	('[^postfixed] prefix*', ('fn$-else-case prefix*',), Identity),
	('[^postfixed] prefix*', ('named', 'prefix*'), x1),
	('[^postfixed] prefix*', ('named$-else prefix*',), Identity),
	('[^postfixed] prefix*', ('named$-case prefix*',), Identity),
	('[^postfixed] prefix*', ('named$-else-case prefix*',), Identity),
	('[^postfixed] prefix*', ('if', 'prefix*'), x1),
	('[^postfixed] prefix*', ('if$-else prefix*',), Identity),
	('[^postfixed] prefix*', ('if$-case prefix*',), Identity),
	('[^postfixed] prefix*', ('if$-else-case prefix*',), Identity),
	('[^postfixed] prefix*', ('for', 'prefix*'), x1),
	('[^postfixed] prefix*', ('for$-else prefix*',), Identity),
	('[^postfixed] prefix*', ('for$-case prefix*',), Identity),
	('[^postfixed] prefix*', ('for$-else-case prefix*',), Identity),
	('[^postfixed] prefix*', ('match prefix*',), Identity),
	('[^postfixed] prefix*', ('match$-else prefix*',), Identity),
	('[^postfixed] prefix*', ('scoped', 'prefix*'), x1),
	
	('[^postfixed] (prefix* #[_])?', ('fn', '(prefix* #[_])?'), x1),
	('[^postfixed] (prefix* #[_])?', ('fn$-else (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('fn$-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('fn$-else-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('named', '(prefix* #[_])?'), x1),
	('[^postfixed] (prefix* #[_])?', ('named$-else (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('named$-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('named$-else-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('if', '(prefix* #[_])?'), x1),
	('[^postfixed] (prefix* #[_])?', ('if$-else (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('if$-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('if$-else-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('for', '(prefix* #[_])?'), x1),
	('[^postfixed] (prefix* #[_])?', ('for$-else (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('for$-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('for$-else-case (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('match (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('match$-else (prefix* #[_])?',), Identity),
	('[^postfixed] (prefix* #[_])?', ('scoped', '(prefix* #[_])?'), x1),
	
	('x', ('fn',), Identity),
	('x', ('named',), Identity),
	('x', ('if',), Identity),
	('x', ('for',), Identity),
	('x', ('scoped',), Identity),
	
	('x$-else', ('fn$-else',), Identity),
	('x$-else', ('named$-else',), Identity),
	('x$-else', ('if$-else',), Identity),
	('x$-else', ('for$-else',), Identity),
	
	('x$-else prefix*', ('fn$-else prefix*',), Identity),
	('x$-else prefix*', ('named$-else prefix*',), Identity),
	('x$-else prefix*', ('if$-else prefix*',), Identity),
	('x$-else prefix*', ('for$-else prefix*',), Identity),
	
	('x$-else (prefix* #[_])?', ('fn$-else (prefix* #[_])?',), Identity),
	('x$-else (prefix* #[_])?', ('named$-else (prefix* #[_])?',), Identity),
	('x$-else (prefix* #[_])?', ('if$-else (prefix* #[_])?',), Identity),
	('x$-else (prefix* #[_])?', ('for$-else (prefix* #[_])?',), Identity),
	
	('x$-else norm_stmt', ('fn$-else norm_stmt',), Identity),
	('x$-else norm_stmt', ('named$-else norm_stmt',), Identity),
	('x$-else norm_stmt', ('if$-else norm_stmt',), Identity),
	('x$-else norm_stmt', ('for$-else norm_stmt',), Identity),
	
	('x$-else last_stmt', ('fn$-else last_stmt',), Identity),
	('x$-else last_stmt', ('named$-else last_stmt',), Identity),
	('x$-else last_stmt', ('if$-else last_stmt',), Identity),
	('x$-else last_stmt', ('for$-else last_stmt',), Identity),
	
	('x$-?else', ('fn',), Identity),
	('x$-?else', ('fn$-else',), Identity),
	('x$-?else', ('named',), Identity),
	('x$-?else', ('named$-else',), Identity),
	('x$-?else', ('if',), Identity),
	('x$-?else', ('if$-else',), Identity),
	('x$-?else', ('for',), Identity),
	('x$-?else', ('for$-else',), Identity),
	('x$-?else', ('scoped',), Identity),
	
	('x$-?else prefix*', ('fn', 'prefix*'), x1),
	('x$-?else prefix*', ('fn$-else prefix*',), Identity),
	('x$-?else prefix*', ('named', 'prefix*'), x1),
	('x$-?else prefix*', ('named$-else prefix*',), Identity),
	('x$-?else prefix*', ('if', 'prefix*'), x1),
	('x$-?else prefix*', ('if$-else prefix*',), Identity),
	('x$-?else prefix*', ('for', 'prefix*'), x1),
	('x$-?else prefix*', ('for$-else prefix*',), Identity),
	('x$-?else prefix*', ('scoped', 'prefix*'), x1),
	
	('x$-?else (prefix* #[_])?', ('fn', '(prefix* #[_])?'), x1),
	('x$-?else (prefix* #[_])?', ('fn$-else (prefix* #[_])?',), Identity),
	('x$-?else (prefix* #[_])?', ('named', '(prefix* #[_])?'), x1),
	('x$-?else (prefix* #[_])?', ('named$-else (prefix* #[_])?',), Identity),
	('x$-?else (prefix* #[_])?', ('if', '(prefix* #[_])?'), x1),
	('x$-?else (prefix* #[_])?', ('if$-else (prefix* #[_])?',), Identity),
	('x$-?else (prefix* #[_])?', ('for', '(prefix* #[_])?'), x1),
	('x$-?else (prefix* #[_])?', ('for$-else (prefix* #[_])?',), Identity),
	('x$-?else (prefix* #[_])?', ('scoped', '(prefix* #[_])?'), x1),
	
	('x$-?else norm_stmt', ('fn', 'norm_stmt'), x1),
	('x$-?else norm_stmt', ('fn$-else norm_stmt',), Identity),
	('x$-?else norm_stmt', ('named', 'norm_stmt'), x1),
	('x$-?else norm_stmt', ('named$-else norm_stmt',), Identity),
	('x$-?else norm_stmt', ('if', 'norm_stmt'), x1),
	('x$-?else norm_stmt', ('if$-else norm_stmt',), Identity),
	('x$-?else norm_stmt', ('for', 'norm_stmt'), x1),
	('x$-?else norm_stmt', ('for$-else norm_stmt',), Identity),
	('x$-?else norm_stmt', ('scoped', 'norm_stmt'), x1),
	
	('x$-?else last_stmt', ('fn', 'last_stmt'), x1),
	('x$-?else last_stmt', ('fn$-else last_stmt',), Identity),
	('x$-?else last_stmt', ('named', 'last_stmt'), x1),
	('x$-?else last_stmt', ('named$-else last_stmt',), Identity),
	('x$-?else last_stmt', ('if', 'last_stmt'), x1),
	('x$-?else last_stmt', ('if$-else last_stmt',), Identity),
	('x$-?else last_stmt', ('for', 'last_stmt'), x1),
	('x$-?else last_stmt', ('for$-else last_stmt',), Identity),
	('x$-?else last_stmt', ('scoped', 'last_stmt'), x1),
	
	('x$-case', ('fn$-case',), Identity),
	('x$-case', ('named$-case',), Identity),
	('x$-case', ('if$-case',), Identity),
	('x$-case', ('for$-case',), Identity),
	('x$-case', ('match',), Identity),
	
	('x$-case prefix*', ('fn$-case prefix*',), Identity),
	('x$-case prefix*', ('named$-case prefix*',), Identity),
	('x$-case prefix*', ('if$-case prefix*',), Identity),
	('x$-case prefix*', ('for$-case prefix*',), Identity),
	('x$-case prefix*', ('match prefix*',), Identity),
	
	('x$-case (prefix* #[_])?', ('fn$-case (prefix* #[_])?',), Identity),
	('x$-case (prefix* #[_])?', ('named$-case (prefix* #[_])?',), Identity),
	('x$-case (prefix* #[_])?', ('if$-case (prefix* #[_])?',), Identity),
	('x$-case (prefix* #[_])?', ('for$-case (prefix* #[_])?',), Identity),
	('x$-case (prefix* #[_])?', ('match (prefix* #[_])?',), Identity),
	
	('x$-case norm_stmt', ('fn$-case norm_stmt',), Identity),
	('x$-case norm_stmt', ('named$-case norm_stmt',), Identity),
	('x$-case norm_stmt', ('if$-case norm_stmt',), Identity),
	('x$-case norm_stmt', ('for$-case norm_stmt',), Identity),
	('x$-case norm_stmt', ('match norm_stmt',), Identity),
	
	('x$-case last_stmt', ('fn$-case last_stmt',), Identity),
	('x$-case last_stmt', ('named$-case last_stmt',), Identity),
	('x$-case last_stmt', ('if$-case last_stmt',), Identity),
	('x$-case last_stmt', ('for$-case last_stmt',), Identity),
	('x$-case last_stmt', ('match last_stmt',), Identity),
	
	('x$-?case', ('fn',), Identity),
	('x$-?case', ('fn$-case',), Identity),
	('x$-?case', ('named',), Identity),
	('x$-?case', ('named$-case',), Identity),
	('x$-?case', ('if',), Identity),
	('x$-?case', ('if$-case',), Identity),
	('x$-?case', ('for',), Identity),
	('x$-?case', ('for$-case',), Identity),
	('x$-?case', ('match',), Identity),
	('x$-?case', ('scoped',), Identity),
	
	('x$-?case prefix*', ('fn', 'prefix*'), x1),
	('x$-?case prefix*', ('fn$-case prefix*',), Identity),
	('x$-?case prefix*', ('named', 'prefix*'), x1),
	('x$-?case prefix*', ('named$-case prefix*',), Identity),
	('x$-?case prefix*', ('if', 'prefix*'), x1),
	('x$-?case prefix*', ('if$-case prefix*',), Identity),
	('x$-?case prefix*', ('for', 'prefix*'), x1),
	('x$-?case prefix*', ('for$-case prefix*',), Identity),
	('x$-?case prefix*', ('match prefix*',), Identity),
	('x$-?case prefix*', ('scoped', 'prefix*'), x1),
	
	('x$-?case (prefix* #[_])?', ('fn', '(prefix* #[_])?'), x1),
	('x$-?case (prefix* #[_])?', ('fn$-case (prefix* #[_])?',), Identity),
	('x$-?case (prefix* #[_])?', ('named', '(prefix* #[_])?'), x1),
	('x$-?case (prefix* #[_])?', ('named$-case (prefix* #[_])?',), Identity),
	('x$-?case (prefix* #[_])?', ('if', '(prefix* #[_])?'), x1),
	('x$-?case (prefix* #[_])?', ('if$-case (prefix* #[_])?',), Identity),
	('x$-?case (prefix* #[_])?', ('for', '(prefix* #[_])?'), x1),
	('x$-?case (prefix* #[_])?', ('for$-case (prefix* #[_])?',), Identity),
	('x$-?case (prefix* #[_])?', ('match (prefix* #[_])?',), Identity),
	('x$-?case (prefix* #[_])?', ('scoped', '(prefix* #[_])?'), x1),
	
	('x$-?case norm_stmt', ('fn', 'norm_stmt'), x1),
	('x$-?case norm_stmt', ('fn$-case norm_stmt',), Identity),
	('x$-?case norm_stmt', ('named', 'norm_stmt'), x1),
	('x$-?case norm_stmt', ('named$-case norm_stmt',), Identity),
	('x$-?case norm_stmt', ('if', 'norm_stmt'), x1),
	('x$-?case norm_stmt', ('if$-case norm_stmt',), Identity),
	('x$-?case norm_stmt', ('for', 'norm_stmt'), x1),
	('x$-?case norm_stmt', ('for$-case norm_stmt',), Identity),
	('x$-?case norm_stmt', ('match norm_stmt',), Identity),
	('x$-?case norm_stmt', ('scoped', 'norm_stmt'), x1),
	
	('x$-?case last_stmt', ('fn', 'last_stmt'), x1),
	('x$-?case last_stmt', ('fn$-case last_stmt',), Identity),
	('x$-?case last_stmt', ('named', 'last_stmt'), x1),
	('x$-?case last_stmt', ('named$-case last_stmt',), Identity),
	('x$-?case last_stmt', ('if', 'last_stmt'), x1),
	('x$-?case last_stmt', ('if$-case last_stmt',), Identity),
	('x$-?case last_stmt', ('for', 'last_stmt'), x1),
	('x$-?case last_stmt', ('for$-case last_stmt',), Identity),
	('x$-?case last_stmt', ('match last_stmt',), Identity),
	('x$-?case last_stmt', ('scoped', 'last_stmt'), x1),
	
	('x$-else-case', ('fn$-else-case',), Identity),
	('x$-else-case', ('named$-else-case',), Identity),
	('x$-else-case', ('if$-else-case',), Identity),
	('x$-else-case', ('for$-else-case',), Identity),
	('x$-else-case', ('match$-else',), Identity),
	
	('x$-else-case prefix*', ('fn$-else-case prefix*',), Identity),
	('x$-else-case prefix*', ('named$-else-case prefix*',), Identity),
	('x$-else-case prefix*', ('if$-else-case prefix*',), Identity),
	('x$-else-case prefix*', ('for$-else-case prefix*',), Identity),
	('x$-else-case prefix*', ('match$-else prefix*',), Identity),
	
	('x$-else-case (prefix* #[_])?', ('fn$-else-case (prefix* #[_])?',), Identity),
	('x$-else-case (prefix* #[_])?', ('named$-else-case (prefix* #[_])?',), Identity),
	('x$-else-case (prefix* #[_])?', ('if$-else-case (prefix* #[_])?',), Identity),
	('x$-else-case (prefix* #[_])?', ('for$-else-case (prefix* #[_])?',), Identity),
	('x$-else-case (prefix* #[_])?', ('match$-else (prefix* #[_])?',), Identity),
	
	('x$-else-case norm_stmt', ('fn$-else-case norm_stmt',), Identity),
	('x$-else-case norm_stmt', ('named$-else-case norm_stmt',), Identity),
	('x$-else-case norm_stmt', ('if$-else-case norm_stmt',), Identity),
	('x$-else-case norm_stmt', ('for$-else-case norm_stmt',), Identity),
	('x$-else-case norm_stmt', ('match$-else norm_stmt',), Identity),
	
	('x$-else-case last_stmt', ('fn$-else-case last_stmt',), Identity),
	('x$-else-case last_stmt', ('named$-else-case last_stmt',), Identity),
	('x$-else-case last_stmt', ('if$-else-case last_stmt',), Identity),
	('x$-else-case last_stmt', ('for$-else-case last_stmt',), Identity),
	('x$-else-case last_stmt', ('match$-else last_stmt',), Identity),
	
	('x$-else-?case', ('fn$-else',), Identity),
	('x$-else-?case', ('fn$-else-case',), Identity),
	('x$-else-?case', ('named$-else',), Identity),
	('x$-else-?case', ('named$-else-case',), Identity),
	('x$-else-?case', ('if$-else',), Identity),
	('x$-else-?case', ('if$-else-case',), Identity),
	('x$-else-?case', ('for$-else',), Identity),
	('x$-else-?case', ('for$-else-case',), Identity),
	('x$-else-?case', ('match$-else',), Identity),
	
	('x$-else-?case prefix*', ('fn$-else prefix*',), Identity),
	('x$-else-?case prefix*', ('fn$-else-case prefix*',), Identity),
	('x$-else-?case prefix*', ('named$-else prefix*',), Identity),
	('x$-else-?case prefix*', ('named$-else-case prefix*',), Identity),
	('x$-else-?case prefix*', ('if$-else prefix*',), Identity),
	('x$-else-?case prefix*', ('if$-else-case prefix*',), Identity),
	('x$-else-?case prefix*', ('for$-else prefix*',), Identity),
	('x$-else-?case prefix*', ('for$-else-case prefix*',), Identity),
	('x$-else-?case prefix*', ('match$-else prefix*',), Identity),
	
	('x$-else-?case (prefix* #[_])?', ('fn$-else (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('fn$-else-case (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('named$-else (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('named$-else-case (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('if$-else (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('if$-else-case (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('for$-else (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('for$-else-case (prefix* #[_])?',), Identity),
	('x$-else-?case (prefix* #[_])?', ('match$-else (prefix* #[_])?',), Identity),
	
	('x$-else-?case norm_stmt', ('fn$-else norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('fn$-else-case norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('named$-else norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('named$-else-case norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('if$-else norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('if$-else-case norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('for$-else norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('for$-else-case norm_stmt',), Identity),
	('x$-else-?case norm_stmt', ('match$-else norm_stmt',), Identity),
	
	('x$-else-?case last_stmt', ('fn$-else last_stmt',), Identity),
	('x$-else-?case last_stmt', ('fn$-else-case last_stmt',), Identity),
	('x$-else-?case last_stmt', ('named$-else last_stmt',), Identity),
	('x$-else-?case last_stmt', ('named$-else-case last_stmt',), Identity),
	('x$-else-?case last_stmt', ('if$-else last_stmt',), Identity),
	('x$-else-?case last_stmt', ('if$-else-case last_stmt',), Identity),
	('x$-else-?case last_stmt', ('for$-else last_stmt',), Identity),
	('x$-else-?case last_stmt', ('for$-else-case last_stmt',), Identity),
	('x$-else-?case last_stmt', ('match$-else last_stmt',), Identity),
	
	('x$-?else-case', ('fn$-case',), Identity),
	('x$-?else-case', ('fn$-else-case',), Identity),
	('x$-?else-case', ('named$-case',), Identity),
	('x$-?else-case', ('named$-else-case',), Identity),
	('x$-?else-case', ('if$-case',), Identity),
	('x$-?else-case', ('if$-else-case',), Identity),
	('x$-?else-case', ('for$-case',), Identity),
	('x$-?else-case', ('for$-else-case',), Identity),
	('x$-?else-case', ('match',), Identity),
	('x$-?else-case', ('match$-else',), Identity),
	
	('x$-?else-case prefix*', ('fn$-case prefix*',), Identity),
	('x$-?else-case prefix*', ('fn$-else-case prefix*',), Identity),
	('x$-?else-case prefix*', ('named$-case prefix*',), Identity),
	('x$-?else-case prefix*', ('named$-else-case prefix*',), Identity),
	('x$-?else-case prefix*', ('if$-case prefix*',), Identity),
	('x$-?else-case prefix*', ('if$-else-case prefix*',), Identity),
	('x$-?else-case prefix*', ('for$-case prefix*',), Identity),
	('x$-?else-case prefix*', ('for$-else-case prefix*',), Identity),
	('x$-?else-case prefix*', ('match prefix*',), Identity),
	('x$-?else-case prefix*', ('match$-else prefix*',), Identity),
	
	('x$-?else-case (prefix* #[_])?', ('fn$-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('fn$-else-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('named$-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('named$-else-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('if$-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('if$-else-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('for$-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('for$-else-case (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('match (prefix* #[_])?',), Identity),
	('x$-?else-case (prefix* #[_])?', ('match$-else (prefix* #[_])?',), Identity),
	
	('x$-?else-case norm_stmt', ('fn$-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('fn$-else-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('named$-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('named$-else-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('if$-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('if$-else-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('for$-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('for$-else-case norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('match norm_stmt',), Identity),
	('x$-?else-case norm_stmt', ('match$-else norm_stmt',), Identity),
	
	('x$-?else-case last_stmt', ('fn$-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('fn$-else-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('named$-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('named$-else-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('if$-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('if$-else-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('for$-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('for$-else-case last_stmt',), Identity),
	('x$-?else-case last_stmt', ('match last_stmt',), Identity),
	('x$-?else-case last_stmt', ('match$-else last_stmt',), Identity),
]
SYNTAX_RULES += [
	('scoped', ('{', 'stmt_lst', '}', 'postfix*'), lambda LBR, xs, RBR, post: post(NodeCompound(*xs))),
	('scoped', ('.{', 'stmt_lst', '}', 'postfix*'), lambda LBR, xs, RBR, post: post(NodeStruct(*xs))),
]
SYNTAX_RULES += [
	('(prefix* #[_])?', ('prefix*', '#[', '(stmt|expr)', ']'), lambda xs, LBR, x, RBR: (*xs, x)),
	('(prefix* #[_])?', (), lambda: ()),
	
	('prefixed', ('prefix*', '@', 'postfixed+', 'postfixed'), lambda attr_xs, AT, attr, x: x.AppendPrefix(*attr_xs, attr)),
	('prefix*', ('prefix*', '@', 'postfixed+'), lambda xs, AT, x: (*xs, x)),
	('prefix*', ('prefix*', '#[', '(stmt|expr)', ']'), lambda xs, LBR, x, RBR: (*xs, x)),
	('prefix*', (), lambda: ()),
	('postfixed+', ('postfixed+', 'postfixed'), lambda func, arg: NodeCall(func, arg)),
	('postfixed+', ('postfixed',), Identity),
]
SYNTAX_RULES += [
	('postfixed', ('prim', 'postfix*'), lambda x, post: post(x)),
	('postfix*', ('postfix*', 'postfix'), lambda xs, x: lambda node: x(xs(node))),
	('postfix*', (), lambda: Identity),
	('postfix', ('.', 'prim'), lambda DOT, field: lambda node: NodeAccess(node, field)),
	('postfix', ('[', '(stmt|expr)', ']'), lambda LBR, x, RBR: lambda node: NodeIndex(node, x)),
	('postfix', ('#',), lambda HASH: Identity),
]
SYNTAX_RULES += [
	('prim', ('INT',), NodeInt),
	('prim', ('IDENT',), NodeLabel),
	('prim', ('STR',), NodeStr),
	('prim', ('BOOL',), NodeBool),
	('prim', ('(', ')'), lambda LBR, RBR: NodeTuple()),
	('prim', ('(', '(stmt|expr)', ')'), lambda LBR, x, RBR: x),
	('prim', ('.(', 'field_lst', ')'), lambda LBR, xs, RBR: NodeStruct(*xs)),
	('prim', ('.[', 'tuple..', ']'), lambda LBR, xs, RBR: NodeTuple(*xs)),
	('(stmt|expr)', ('stmt',), Identity),
	('(stmt|expr)', ('expr',), Identity),
	
	('field_lst', ('field_lst', '(;|,)', 'field_set'), lambda xs, SEMI, x: (*xs, x)),
	('field_lst', ('field_lst', '(;|,)'), lambda xs, SEMI: xs),
	('field_lst', ('field_set',), lambda x: (x,)),
	('field_lst', (), lambda: ()),
	('field_set', ('(let|suffixed|..suffixed)',), Identity),
	('field_set', ('(let|suffixed|..suffixed)', '=', 'field_set'), lambda var, EQ, expr: NodeAssign(var, expr)),
	('(let|suffixed|..suffixed)', ('prefix*', 'LET', '(suffixed|..suffixed)'), lambda attr, LET, var: NodeDecl(var).AppendPrefix(*attr)),
	('(let|suffixed|..suffixed)', ('(suffixed|..suffixed)',), NodeDecl),
	('(;|,)', (';',), Identity),
	('(;|,)', (',',), Identity),
]
PRODUCTIONS = [Production(*p) for p in SYNTAX_RULES]


def EnsureCompound(node: Node) -> NodeCompound | NodeStruct:
	return node if isinstance(node, (NodeCompound, NodeStruct)) else NodeCompound(node)
