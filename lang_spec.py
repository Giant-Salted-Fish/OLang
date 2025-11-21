from parser import Production
from lang_ast import (
	Node, NodeInt, NodeLabel, NodeStr, NodeBool, NodeCompound, NodeDecl, NodeAssign, NodeFunc,
	NodeTemplate, NodeApply, NodeUnion, NodeTuple, NodeStruct, NodeLogicalOp, NodeBinaryOp,
	NodeUnaryOp, NodeAccess, NodeIndex, NodeReturn, NodeBreak, NodeContinue, NodeIfElse,
	NodeWhileElse, NodeForElse, NodeNamedTuple, NodeNamedStruct
)
from collections.abc import Callable


TOKEN_TYPES = [
	("TMPLT", r"template"),
	("CONTINUE", r"continue"),
	("STRUCT", r"struct"),
	("RETURN", r"return"),
	("INLINE", r"inline"),  # Comment out this to disable inline keyword.
	("BREAK", r"break"),
	("TUPLE", r"tuple"),
	("WHILE", r"while"),
	("THEN", r"then"),
	("ELSE", r"else"),
	("LET", r"let"),
	("FOR", r"for"),
	("IF", r"if"),
	("FN", r"fn"),
	
	("BOOL", r"TRUE|FALSE|true|false"),
	("INT", r"[0-9]\d*"),
	("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	("STR", r"\"(?:[^\"\\]|\\[tnr])*\""),  # See https://github.com/antlr/grammars-v4/blob/master/java/java8/Java8Lexer.g4
	
	(".{", r"\.\{"),
	(".[", r"\.\["),
	(".(", r"\.\("),
	("{", r"\{"),
	("}", r"\}"),
	("[", r"\["),
	("]", r"\]"),
	("(", r"\("),
	(")", r"\)"),
	
	(";", r";"),
	("=", r"="),
	("|", r"\|"),
	(",", r","),
	("||", r"\|\|"),
	("&&", r"&&"),
	("==", r"=="),
	("!=", r"!="),
	("<=", r"<="),
	(">=", r">="),
	("<", r"<"),
	(">", r">"),
	("+", r"\+"),
	("-", r"-"),
	("*", r"\*"),
	("/", r"/"),
	("%", r"%"),
	("&", r"&"),
	("!", r"!"),
	("~", r"~"),
	("#", r"#"),
	("@", r"@"),
	(":", r":"),
	("->", r"->"),
	(".", r"\."),
	
	# TODO: Maybe forbid mult-line comment
	("COMMENT", r"//.*"),
]

TERMINALS = set(t for t, _ in TOKEN_TYPES)

SYNTAX_RULES: list[tuple[str, tuple[str, ...], Callable[..., Node]]] = [
	("S", ("stmt_lst",), lambda lst: NodeStruct(*lst)),
	
	# stmt_lst: norm_stmt* last_stmt
	("stmt_lst", ("norm_stmt", "stmt_lst"), lambda x, lst: (*x, *lst)),
	("stmt_lst", ("last_stmt",), lambda x: x),
	
	# norm_stmt: stmt? ;
	#          | decl
	#          | norm_ctrl
	("norm_stmt", ("stmt", ";"), lambda x, SEMI: (x,)),
	("norm_stmt", (";",), lambda SEMI: ()),
	("norm_stmt", ("decl",), lambda x: (x,)),
	("norm_stmt", ("norm_ctrl",), lambda x: x),
	
	# last_stmt: stmt?
	#          | last_ctrl
	("last_stmt", ("stmt",), lambda x: (x,)),
	("last_stmt", (), lambda: ()),
	("last_stmt", ("last_ctrl",), lambda x: x),
	
	# stmt: assign
	#     | prefix* RETURN expr
	#     | prefix* BREAK expr?
	#     | prefix* CONTINUE
	("stmt", ("assign",), lambda x: x),
	("stmt", ("prefix*", "RETURN", "expr"), lambda attr, RET, x: NodeReturn(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK", "expr"), lambda attr, BREAK, x: NodeBreak(x).AppendPrefix(*attr)),
	("stmt", ("prefix*", "BREAK"), lambda attr, BREAK: NodeBreak(NodeTuple()).AppendPrefix(*attr)),
	("stmt", ("prefix*", "CONTINUE"), lambda attr, CONT: NodeContinue().AppendPrefix(*attr)),
	
	# decl: prefix* (TMPLT|FN) bound bound bound
	#     | prefix* (STRUCT|TUPLE) bound bound
	("decl", ("prefix*", "TMPLT", "bound", "bound", "bound"), lambda attr, TMPLT, label, param, body: NodeAssign(NodeDecl(label), NodeTemplate(param, ensure_compound(body)).AppendPrefix(*attr))),
	("decl", ("prefix*", "FN", "bound", "bound", "bound"), lambda attr, FN, label, param, body: NodeAssign(NodeDecl(label), NodeFunc(param, ensure_compound(body)).AppendPrefix(*attr))),
	("decl", ("prefix*", "STRUCT", "bound", "bound"), lambda attr, STRUCT, label, body: NodeAssign(NodeDecl(label), NodeNamedStruct(body).AppendPrefix(*attr))),
	("decl", ("prefix*", "TUPLE", "bound", "bound"), lambda attr, TUPLE, label, body: NodeAssign(NodeDecl(label), NodeNamedTuple(body).AppendPrefix(*attr))),
	
	# norm_ctrl: prefix* IF bound (prefix* THEN)? bound norm_else
	#          | prefix* WHILE bound bound norm_else
	#          | prefix* FOR bound bound bound norm_else
	("norm_ctrl", ("prefix*", "IF", "bound", "prefix*", "THEN", "bound", "norm_else"), lambda attr, IF, cond, attr2, THEN, expr, x: (NodeIfElse(cond, ensure_compound(expr).AppendPrefix(*attr2), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "IF", "bound", "bound", "norm_else"), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, ensure_compound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "WHILE", "bound", "bound", "norm_else"), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, ensure_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("norm_ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "norm_else"), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, ensure_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	
	# norm_else: prefix* ELSE norm_ctrl
	#          | prefix* ELSE bound norm_stmt
	#          | norm_stmt
	("norm_else", ("prefix*", "ELSE", "norm_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("norm_else", ("prefix*", "ELSE", "bound", "norm_stmt"), lambda attr, ELSE, x, follow: (ensure_compound(x).AppendPrefix(*attr), *follow)),
	("norm_else", ("norm_stmt",), lambda x: (NodeCompound(), *x)),
	
	# last_ctrl: prefix* IF bound (prefix* THEN)? bound last_else
	#          | prefix* WHILE bound bound last_else
	#          | prefix* FOR bound bound bound last_else
	("last_ctrl", ("prefix*", "IF", "bound", "prefix*", "THEN", "bound", "last_else"), lambda attr, IF, cond, attr2, THEN, expr, x: (NodeIfElse(cond, ensure_compound(expr).AppendPrefix(*attr2), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "IF", "bound", "bound", "last_else"), lambda attr, IF, cond, expr, x: (NodeIfElse(cond, ensure_compound(expr), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "WHILE", "bound", "bound", "last_else"), lambda attr, WHILE, cond, loop, x: (NodeWhileElse(cond, ensure_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	("last_ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "last_else"), lambda attr, FOR, itr, var, loop, x: (NodeForElse(itr, var, ensure_compound(loop), x[0]).AppendPrefix(*attr), *x[1:])),
	
	# last_else: prefix* ELSE last_ctrl
	#          | prefix* ELSE bound last_stmt
	#          | last_stmt
	("last_else", ("prefix*", "ELSE", "last_ctrl"), lambda attr, ELSE, x: (x[0].AppendPrefix(*attr), *x[1:])),
	("last_else", ("prefix*", "ELSE", "bound", "last_stmt"), lambda attr, ELSE, x, follow: (ensure_compound(x).AppendPrefix(*attr), *follow)),
	("last_else", ("last_stmt",), lambda x: (NodeCompound(), *x)),
	
	# expr: assign
	#     | ctrl_assign
	("expr", ("assign",), lambda x: x),
	("expr", ("ctrl_assign",), lambda x: x),
	
	# assign: let (= (let|ctrl_union))*
	("assign", ("let", "assign.."), lambda var, expr: NodeAssign(var, expr) if expr else var),
	("assign..", ("=", "(let|ctrl_union)", "assign.."), lambda EQ, var, expr: NodeAssign(var, expr) if expr else var),
	("assign..", (), lambda: None),
	("(let|ctrl_union)", ("let",), lambda x: x),
	("(let|ctrl_union)", ("ctrl_union",), lambda x: x),
	
	# let: prefix* LET (union|ctrl_union)
	#    | union
	("let", ("prefix*", "LET", "(union|ctrl_union)"), lambda attr, LET, var: NodeDecl(var).AppendPrefix(*attr)),
	("let", ("union",), lambda x: x),
	("(union|ctrl_union)", ("union",), lambda x: x),
	("(union|ctrl_union)", ("ctrl_union",), lambda x: x),
	
	# ctrl_assign: ctrl_union (= (union|ctrl_union))*
	("ctrl_assign", ("ctrl_union", "assign.."), lambda var, EQ, expr: NodeAssign(var, expr) if expr else var),
	
	# union: tuple (| (tuple|ctrl_tuple))* |?
	("union", ("tuple", "union.."), lambda x, lst: x if lst is None else NodeUnion(x, *lst)),
	("union..", ("|", "(tuple|ctrl_tuple)", "union.."), lambda PIPE, x, lst: (x, *(lst or ()))),
	("union..", ("|",), lambda PIPE: ()),
	("union..", (), lambda: None),
	("(tuple|ctrl_tuple)", ("tuple",), lambda x: x),
	("(tuple|ctrl_tuple)", ("ctrl_tuple",), lambda x: x),
	
	# ctrl_union: ctrl_tuple (| (tuple|ctrl_tuple))* |?
	("ctrl_union", ("ctrl_tuple", "union.."), lambda x, lst: x if lst is None else NodeUnion(x, *lst)),
	
	# tuple: suffixed (, (suffixed|ctrl_suffixed))* ,?
	("tuple", ("suffixed", "tuple.."), lambda x, lst: x if lst is None else NodeTuple(x, *lst)),
	("tuple..", (",", "(suffixed|ctrl_suffixed)", "tuple.."), lambda COMMA, x, lst: (x, *(lst or ()))),
	("tuple..", (",",), lambda COMMA: ()),
	("tuple..", (), lambda: None),
	("(suffixed|ctrl_suffixed)", ("suffixed",), lambda x: x),
	("(suffixed|ctrl_suffixed)", ("ctrl_suffixed",), lambda x: x),
	
	# ctrl_tuple: ctrl_suffixed (, (suffixed|ctrl_suffixed))* ,?
	("ctrl_tuple", ("ctrl_suffixed", "tuple.."), lambda x, lst: x if lst is None else NodeTuple(x, *lst)),
	
	# suffixed: lmbd suffix*
	("suffixed", ("lmbd", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	("suffix*", ("suffix*", ":", "lmbd"), lambda lst, COLON, attr: (*lst, attr)),
	("suffix*", (), lambda: ()),
	
	# ctrl_suffixed: ctrl_lmbd suffix*
	("ctrl_suffixed", ("ctrl_lmbd", "suffix*"), lambda x, attr: x.AppendSuffix(*attr)),
	
	# lmbd: or (-> (or|ctrl_or))*
	("lmbd", ("or", "lmbd.."), lambda param, body: NodeFunc(param, ensure_compound(body)) if body else param),
	("lmbd..", ("->", "(or|ctrl_or)", "lmbd.."), lambda ARROW, param, body: NodeFunc(param, ensure_compound(body)) if body else param),
	("lmbd..", (), lambda: None),
	("(or|ctrl_or)", ("or",), lambda x: x),
	("(or|ctrl_or)", ("ctrl_or",), lambda x: x),
	
	# ctrl_lmbd: ctrl_or (-> (or|ctrl_or))*
	("ctrl_lmbd", ("ctrl_or", "lmbd.."), lambda param, body: NodeFunc(param, ensure_compound(body)) if body else param),
	
	# or: and (|| (and|ctrl_and))*
	("or", ("or", "||", "(and|ctrl_and)"), lambda left, OR, right: NodeLogicalOp(OR, left, right)),
	("or", ("and",), lambda x: x),
	("(and|ctrl_and)", ("and",), lambda x: x),
	("(and|ctrl_and)", ("ctrl_and",), lambda x: x),
	
	# ctrl_or: ctrl_and (|| (and|ctrl_and))*
	("ctrl_or", ("ctrl_or", "||", "(and|ctrl_and)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("ctrl_or", ("ctrl_and",), lambda x: x),
	
	# and: eq (&& (eq|ctrl_eq))*
	("and", ("and", "&&", "(eq|ctrl_eq)"), lambda left, AND, right: NodeLogicalOp(AND, left, right)),
	("and", ("eq",), lambda x: x),
	("(eq|ctrl_eq)", ("eq",), lambda x: x),
	("(eq|ctrl_eq)", ("ctrl_eq",), lambda x: x),
	
	# ctrl_and: ctrl_eq (&& (eq|ctrl_eq))*
	("ctrl_and", ("ctrl_and", "&&", "(eq|ctrl_eq)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("ctrl_and", ("ctrl_eq",), lambda x: x),
	
	# eq: rel ((==|!=) (rel|ctrl_rel))*
	("eq", ("eq", "(==|!=)", "(rel|ctrl_rel)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("eq", ("rel",), lambda x: x),
	("(==|!=)", ("==",), lambda EQ: EQ),
	("(==|!=)", ("!=",), lambda NEQ: NEQ),
	("(rel|ctrl_rel)", ("rel",), lambda x: x),
	("(rel|ctrl_rel)", ("ctrl_rel",), lambda x: x),
	
	# ctrl_eq: ctrl_rel ((==|!=) (rel|ctrl_rel))*
	("ctrl_eq", ("ctrl_eq", "(==|!=)", "(rel|ctrl_rel)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("ctrl_eq", ("ctrl_rel",), lambda x: x),
	
	# rel: add ((<=|>=|<|>) (add|ctrl_add))*
	("rel", ("rel", "(<=|>=|<|>)", "(add|ctrl_add)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("rel", ("add",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda GTE: GTE),
	("(<=|>=|<|>)", ("<=",), lambda LSE: LSE),
	("(<=|>=|<|>)", (">",), lambda GT: GT),
	("(<=|>=|<|>)", ("<",), lambda LS: LS),
	("(add|ctrl_add)", ("add",), lambda x: x),
	("(add|ctrl_add)", ("ctrl_add",), lambda x: x),
	
	# ctrl_rel: ctrl_add ((<=|>=|<|>) (add|ctrl_add))*
	("ctrl_rel", ("ctrl_rel", "(<=|>=|<|>)", "(add|ctrl_add)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("ctrl_rel", ("ctrl_add",), lambda x: x),
	
	# add: mul ((+|-) (mul|ctrl_mul))*
	("add", ("add", "(+|-)", "(mul|ctrl_mul)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("add", ("mul",), lambda x: x),
	("(+|-)", ("+",), lambda PLUS: PLUS),
	("(+|-)", ("-",), lambda MINUS: MINUS),
	("(mul|ctrl_mul)", ("mul",), lambda x: x),
	("(mul|ctrl_mul)", ("ctrl_mul",), lambda x: x),
	
	# ctrl_add: ctrl_mul ((+|-) (mul|ctrl_mul))*
	("ctrl_add", ("ctrl_add", "(+|-)", "(mul|ctrl_mul)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("ctrl_add", ("ctrl_mul",), lambda x: x),
	
	# mul: unary ((*|/|%) (unary|ctrl))*
	("mul", ("mul", "(*|/|%)", "(unary|ctrl)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("mul", ("unary",), lambda x: x),
	("(*|/|%)", ("*",), lambda MUL: MUL),
	("(*|/|%)", ("/",), lambda DIV: DIV),
	("(*|/|%)", ("%",), lambda MOD: MOD),
	("(unary|ctrl)", ("unary",), lambda x: x),
	("(unary|ctrl)", ("ctrl",), lambda x: x),
	
	# ctrl_mul: ctrl ((*|/|%) (unary|ctrl))*
	("ctrl_mul", ("ctrl_mul", "(*|/|%)", "(unary|ctrl)"), lambda left, OP, right: NodeBinaryOp(OP, left, right)),
	("ctrl_mul", ("ctrl",), lambda x: x),
	
	# unary: (+|-|!|&|*)* (call|bound|prefix)
	#      | (+|-|!|&|*)+ ctrl
	("unary", ("(+|-|!|&|*)", "unary"), lambda op, val: NodeUnaryOp(op, val)),
	("unary", ("(+|-|!|&|*)", "ctrl"), lambda op, val: NodeUnaryOp(op, val)),
	("unary", ("call",), lambda x: x),
	("unary", ("inline_ctrl",), lambda x: x),
	("unary", ("prefixed",), lambda x: x),
	("(+|-|!|&|*)", ("+",), lambda PLUS: PLUS),
	("(+|-|!|&|*)", ("-",), lambda MINUS: MINUS),
	("(+|-|!|&|*)", ("!",), lambda NOT: NOT),
	("(+|-|!|&|*)", ("&",), lambda AMPERSAND: AMPERSAND),
	("(+|-|!|&|*)", ("*",), lambda MUL: MUL),
	
	# call: post+ (ctrl|bound|prefixed)
	("call", ("post+", "ctrl"), lambda func, arg: NodeApply(func, arg)),
	("call", ("post+", "inline_ctrl"), lambda func, arg: NodeApply(func, arg)),
	("call", ("post+", "prefixed"), lambda func, arg: NodeApply(func, arg)),
	
	# ctrl: prefix* IF bound (prefix* THEN)? bound else?
	#     | prefix* WHILE bound bound else?
	#     | prefix* FOR bound bound bound else?
	#     | decl
	("ctrl", ("prefix*", "IF", "bound", "prefix*", "THEN", "bound", "else"), lambda attr, IF, cond, attr2, THEN, expr, otherwise: NodeIfElse(cond, ensure_compound(expr).AppendPrefix(*attr2), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "IF", "bound", "bound", "else"), lambda attr, IF, cond, expr, otherwise: NodeIfElse(cond, ensure_compound(expr), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "WHILE", "bound", "bound", "else"), lambda attr, WHILE, cond, loop, otherwise: NodeWhileElse(cond, ensure_compound(loop), otherwise).AppendPrefix(*attr)),
	("ctrl", ("prefix*", "FOR", "bound", "bound", "bound", "else"), lambda attr, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, ensure_compound(loop), otherwise).AppendPrefix(*attr)),
	("ctrl", ("decl",), lambda x: NodeCompound(x)),
	
	# else: prefix* ELSE (ctrl|bound)
	("else", ("prefix*", "ELSE", "ctrl"), lambda attr, ELSE, x: x.AppendPrefix(*attr)),
	("else", ("prefix*", "ELSE", "bound"), lambda attr, ELSE, x: ensure_compound(x).AppendPrefix(*attr)),
	("else", (), lambda: NodeCompound()),
	
	# inline_ctrl: prefix* INLINE (IF|WHILE) bound bound else?
	#            | prefix* INLINE FOR bound bound bound else?
	#            | bound
	("inline_ctrl", ("prefix*", "INLINE", "IF", "bound", "bound", "else"), lambda attr, INLINE, IF, cond, expr, otherwise: NodeIfElse(cond, ensure_compound(expr), otherwise).AppendPrefix(*attr)),
	("inline_ctrl", ("prefix*", "INLINE", "WHILE", "bound", "bound", "else"), lambda attr, INLINE, WHILE, cond, loop, otherwise: NodeWhileElse(cond, ensure_compound(loop), otherwise).AppendPrefix(*attr)),
	("inline_ctrl", ("prefix*", "INLINE", "FOR", "bound", "bound", "bound", "else"), lambda attr, INLINE, FOR, itr, var, loop, otherwise: NodeForElse(itr, var, ensure_compound(loop), otherwise).AppendPrefix(*attr)),
	("inline_ctrl", ("bound",), lambda x: x),
	
	# It is named "bound" even if its length can go infinite, as it has a clear terminator that indicates the end of this element.
	# bound: prefix* (TMPLT|FN) bound bound
	#      | prefix* (STRUCT|TUPLE) bound
	#      | prefix* { stmt_lst }
	#      | prefix* .{ stmt_lst }
	#      | post
	("bound", ("prefix*", "TMPLT", "bound", "bound"), lambda attr, TMPLT, param, body: NodeTemplate(param, ensure_compound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "FN", "bound", "bound"), lambda attr, FN, param, body: NodeFunc(param, ensure_compound(body)).AppendPrefix(*attr)),
	("bound", ("prefix*", "STRUCT", "bound"), lambda attr, STRCT, body: NodeNamedStruct(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "TUPLE", "bound"), lambda attr, TUPLE, body: NodeNamedTuple(body).AppendPrefix(*attr)),
	("bound", ("prefix*", "{", "stmt_lst", "}", "postfix*"), lambda attr, LCB, lst, RCB, post: post(NodeCompound(*lst)).AppendPrefix(*attr)),
	("bound", ("prefix*", ".{", "stmt_lst", "}", "postfix*"), lambda attr, LCB, lst, RCB, post: post(NodeStruct(*lst)).AppendPrefix(*attr)),
	("bound", ("post",), lambda x: x[1](x[0])),
	
	# prefixed: prefix* @ post+ post
	("prefixed", ("prefix*", "@", "post+", "post"), lambda attr_lst, AT, attr, x: x[1](x[0]).AppendPrefix(*attr_lst, attr)),
	("prefix*", ("prefix*", "@", "post+"), lambda lst, AT, attr: (*lst, attr)),
	("prefix*", (), lambda: ()),
	("post+", ("post+", "post"), lambda head, x: x[1](NodeApply(head, x[0]))),
	("post+", ("post",), lambda x: x[1](x[0])),
	
	# post: prim postfix*
	("post", ("prim", "postfix*"), lambda x, post: (x, post)),
	("postfix*", ("postfix*", "postfix"), lambda head, post: lambda node: post(head(node))),
	("postfix*", (), lambda: lambda x: x),
	("postfix", (".", "prim"), lambda DOT, label: lambda node: NodeAccess(node, label)),
	("postfix", ("[", "stmt", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("[", "decl", "]"), lambda LBR, idx, RBR: lambda node: NodeIndex(node, idx)),
	("postfix", ("#",), lambda HASH: lambda node: node),
	
	# prim: (INT|LABEL|STR)
	#     | ( )
	#     | ( | )
	#     | ( expr )
	#     | ( decl )
	#     | .( field_lst )
	#     | .[ element_lst ]
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("STR",), NodeStr),
	("prim", ("BOOL",), NodeBool),
	("prim", ("(", ")"), lambda LPR, RPR: NodeTuple()),
	("prim", ("(", "|", ")"), lambda LPR, PIPE, RPR: NodeUnion()),
	("prim", ("(", "stmt", ")"), lambda LPR, x, RPR: x),
	("prim", ("(", "decl", ")"), lambda LPR, x, RPR: x),
	("prim", (".(", "field_lst", ")"), lambda LPR, x, RPR: NodeStruct(*x)),
	("prim", (".[", "element_lst", "]"), lambda LPR, x, RPR: NodeTuple(*x)),
	
	# element_lst: element , element_lst
	#            | element?
	("element_lst", ("(suffixed|ctrl_suffixed)", ",", "element_lst"), lambda e, COMMA, lst: (e, *lst)),
	("element_lst", ("(suffixed|ctrl_suffixed)",), lambda e: (e,)),
	("element_lst", (), lambda: ()),
	
	# field_lst: field_set (;|,) field_lst
	#          | field_set?
	("field_lst", ("field_set", "(;|,)", "field_lst"), lambda x, SEMI, lst: (x, *lst)),
	("field_lst", ("field_set",), lambda x: (x,)),
	("field_lst", (), lambda: ()),
	("field_set", ("prefix*", "LET", "suffixed", "=", "suffixed"), lambda attr, LET, label, EQ, expr: NodeAssign(NodeDecl(label).AppendPrefix(*attr), expr)),
	("field_set", ("prefix*", "LET", "suffixed"), lambda attr, LET, label: NodeDecl(label).AppendPrefix(*attr)),
	("field_set", ("suffixed", "=", "suffixed"), lambda label, EQ, expr: NodeAssign(NodeDecl(label), expr)),
	("field_set", ("suffixed",), lambda x: x),
	("(;|,)", (";",), lambda SEMI: SEMI),
	("(;|,)", (",",), lambda COMMA: COMMA),
]
PRODUCTIONS = [Production(*p) for p in SYNTAX_RULES]


def ensure_compound(node: Node):
	return node if isinstance(node, NodeCompound) else NodeCompound(node)
