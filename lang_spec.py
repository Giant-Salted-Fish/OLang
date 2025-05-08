from parser import Production
from lang_ast import *


TOKEN_TYPES = [
	("RETURN", r"return"),
	("DEF", r"def"),
	("LET", r"let"),
	
	("INT", r"[1-9]\d*"),
	("ID", r"[a-zA-Z_\$][a-zA-Z0-9_\$]*"),
	
	("{", r"\{"),
	("}", r"\}"),
	("[", r"\["),
	("]", r"\]"),
	("(", r"\("),
	(")", r"\)"),
	
	(";", r";"),
	("=", r"="),
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
	("!", r"!"),
	("~", r"~"),
	("@", r"@"),
	(":", r":"),
	("->", r"->"),
	(".", r"\."),
	(",", r","),
	
	# TODO: Handle mult-line comment
	("COMMENT", r"//.*"),
]

TERMINALS = set(t for t, _ in TOKEN_TYPES)

SYNTAX_RULES = [
	("S", ("stmt*",), lambda x: x),
	("stmt*", ("stmt+",), lambda cmpd: cmpd),
	("stmt*", (), lambda: NodeCompound()),
	("stmt+", ("stmt+", "stmt"), lambda cmpd, stmt: cmpd.Append(stmt)),
	("stmt+", ("stmt",), lambda stmt: NodeCompound(stmt)),
	
	# stmt: (!expr|expr) ;
	#     | LET expr = (!expr|expr) ;
	#     | DEF ID prim { stmt* } ;?
	#     | expr = (!expr|expr) ;
	#     | RETURN (!expr|expr) ;
	("stmt", ("(!expr|expr)", ";"), lambda expr, SEMI: expr),
	("stmt", ("LET", "expr", "=", "(!expr|expr)", ";"), lambda LET, var, EQ, expr, SEMI: NodeDecl(var, expr)),
	("stmt", ("DEF", "ID", "prim", "{", "stmt*", "}", ";?"), lambda DEF, ID, param, LPR, cmpd, RPR, SEMI: NodeDecl(NodeLabel(ID), NodeCallable(param, cmpd))),
	("stmt", ("expr", "=", "(!expr|expr)", ";"), lambda var, EQ, expr, SEMI: NodeAssign(var, expr)),
	("stmt", ("RETURN", "(!expr|expr)", ";"), lambda RET, expr, SEMI: expr),
	("(!expr|expr)", ("!expr",), lambda x: x),
	("(!expr|expr)", ("expr",), lambda x: x),
	(";?", (";",), lambda SEMI: SEMI),
	(";?", (), lambda: None),
	
	# expr: prim suffix*
	("expr", ("prim", "suffix*"), lambda x, suffix: x.Annotate(*suffix)),
	("suffix*", ("suffix+",), lambda suffix: suffix),
	("suffix*", (), lambda: ()),
	("suffix+", ("suffix+", "suffix",), lambda lst, attr: (*lst, attr)),
	("suffix+", ("suffix",), lambda attr: (attr,)),
	
	# !expr: (!lmbd|!or) suffix*
	("!expr", ("(!lmbd|!or)", "suffix*"), lambda x, suffix: x.Annotate(*suffix)),
	("(!lmbd|!or)", ("!lmbd",), lambda x: x),
	("(!lmbd|!or)", ("!or",), lambda x: x),
	
	# suffix: : (!lmbd|!or|prim)
	("suffix", (":", "(!lmbd|!or|prim)",), lambda COLON, attr: attr),
	("(!lmbd|!or|prim)", ("!lmbd",), lambda x: x),
	("(!lmbd|!or|prim)", ("!or",), lambda x: x),
	("(!lmbd|!or|prim)", ("prim",), lambda x: x),
	
	# !lmbd: prim -> !lmbd | prim -> (!or|prim)
	("!lmbd", ("prim", "->", "!lmbd"), lambda param, ARROW, body: NodeCallable(param, body)),
	("!lmbd", ("prim", "->", "(!or|prim)"), lambda param, ARROW, body: NodeCallable(param, body)),
	
	# !or: (!or|prim) || (!and|prim) | !and
	("!or", ("(!or|prim)", "||", "(!and|prim)"), lambda left, OR, right: NodeBinaryOp(OR, left, right)),
	("!or", ("!and",), lambda x: x),
	("(!or|prim)", ("!or",), lambda x: x),
	("(!or|prim)", ("prim",), lambda x: x),
	
	# !and: (!and|prim) && (!eq|prim) | !eq
	("!and", ("(!and|prim)", "&&", "(!eq|prim)"), lambda left, AND, right: NodeBinaryOp(AND, left, right)),
	("!and", ("!eq",), lambda x: x),
	("(!and|prim)", ("!and",), lambda x: x),
	("(!and|prim)", ("prim",), lambda x: x),
	
	# !eq: (!eq|prim) (==|!=) (!rel|prim) | !rel
	("!eq", ("(!eq|prim)", "(==|!=)", "(!rel|prim)"), lambda left, EQ, right: NodeBinaryOp(EQ, left, right)),
	("!eq", ("!rel",), lambda x: x),
	("(!eq|prim)", ("!eq",), lambda x: x),
	("(!eq|prim)", ("prim",), lambda x: x),
	("(==|!=)", ("==",), lambda eq: eq),
	("(==|!=)", ("!=",), lambda neq: neq),
	
	# !rel: (!rel|prim) (<=|>=|<|>) (!add|prim) | !add
	("!rel", ("(!rel|prim)", "(<=|>=|<|>)", "(!add|prim)"), lambda left, REL, right: NodeBinaryOp(REL, left, right)),
	("!rel", ("!add",), lambda x: x),
	("(!rel|prim)", ("!rel",), lambda x: x),
	("(!rel|prim)", ("prim",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda gte: gte),
	("(<=|>=|<|>)", ("<=",), lambda lse: lse),
	("(<=|>=|<|>)", (">",), lambda gt: gt),
	("(<=|>=|<|>)", ("<",), lambda ls: ls),
	
	# !add: (!add|prim) (+|-) (!mul|prim) | !mul
	("!add", ("(!add|prim)", "(+|-)", "(!mul|prim)"), lambda left, ADD, right: NodeBinaryOp(ADD, left, right)),
	("!add", ("!mul",), lambda x: x),
	("(!add|prim)", ("!add",), lambda x: x),
	("(!add|prim)", ("prim",), lambda x: x),
	("(+|-)", ("+",), lambda plus: plus),
	("(+|-)", ("-",), lambda minus: minus),
	
	# !mul: (!mul|prim) (*|/|%) (!unary|prim) | !unary
	("!mul", ("(!mul|prim)", "(*|/|%)", "(!unary|prim)"), lambda left, MUL, right: NodeBinaryOp(MUL, left, right)),
	("!mul", ("!unary",), lambda x: x),
	("(!mul|prim)", ("!mul",), lambda x: x),
	("(!mul|prim)", ("prim",), lambda x: x),
	("(*|/|%)", ("*",), lambda mul: mul),
	("(*|/|%)", ("/",), lambda div: div),
	("(*|/|%)", ("%",), lambda mod: mod),
	
	# !unary: (+|-|!|~) (!unary|prim) | !deref
	("!unary", ("(+|-|!|~)", "(!unary|prim)"), lambda PRE, unary: NodeUnaryOp(PRE, unary)),
	("!unary", ("!deref",), lambda x: x),
	("(!unary|prim)", ("!unary",), lambda x: x),
	("(!unary|prim)", ("prim",), lambda x: x),
	("(+|-|!|~)", ("+",), lambda plus: plus),
	("(+|-|!|~)", ("-",), lambda minus: minus),
	("(+|-|!|~)", ("!",), lambda not_: not_),
	("(+|-|!|~)", ("~",), lambda bit_not: bit_not),
	
	# !deref: (!deref|prim) . ID | !app
	("!deref", ("(!deref|prim)", ".", "ID"), lambda x, DOT, ID: NodeDeref(x, ID)),
	("!deref", ("!app",), lambda x: x),
	("(!deref|prim)", ("!deref",), lambda x: x),
	("(!deref|prim)", ("prim",), lambda x: x),
	
	# !app: (!deref|prim) (!prim|prim) | !prim
	("!app", ("(!deref|prim)", "(!prim|prim)"), NodeApply),
	("!app", ("!prim",), lambda x: x),
	("(!prim|prim)", ("!prim",), lambda x: x),
	("(!prim|prim)", ("prim",), lambda x: x),
	
	# prim: INT | ID | ( expr ) | tup
	("prim", ("INT",), NodeInt),
	("prim", ("ID",), NodeLabel),
	("prim", ("(", "expr", ")"), lambda LPR, x, RPR: x),
	("prim", ("tup",), lambda x: x),
	
	# !prim: ( !expr ) | !tup | { stmt+ }
	("!prim", ("(", "!expr", ")"), lambda LPR, expr, RPR: expr),
	("!prim", ("!tup",), lambda x: x),
	("prim", ("{", "stmt+", "}",), lambda LPR, cmpd, RPR: cmpd),
	
	# tup: ( ) | ( expr , ) | ( (expr ,)+ expr ,? )
	("tup", ("(", ")",), lambda LPR, RPR: NodeTuple()),
	("tup", ("(", "expr", ",", ")"), lambda LPR, x, RPR: NodeTuple(x)),
	("tup", ("(", "(expr ,)+", "expr", ",?", ")"), lambda LPR, tup, x, COMMA, RPR: tup.Append(x)),
	("(expr ,)+", ("(expr ,)+", "expr", ","), lambda tup, x, COMMA: tup.Append(x)),
	("(expr ,)+", ("expr", ","), lambda x, COMMA: NodeTuple(x)),
	(",?", (",",), lambda COMMA: COMMA),
	(",?", (), lambda: None),
	
	# !tup: ( !expr , ) | ( !expr, (, (!expr|expr))+ ,? ) | ( (expr ,)+ !expr (, (!expr|expr))* ,? )
	("!tup", ("(", "!expr", ",", ")"), lambda LPR, x, comma, rpr: NodeTuple(x)),
	("!tup", ("(", "!expr", "(, (!expr|expr))+", ",?", ")"), lambda LPR, x, tup, COMMA, RPR: NodeTuple(x).Merge(tup)),
	("!tup", ("(", "(expr ,)+", "!expr" ,"(, (!expr|expr))*", ",?", ")"), lambda LPR, tup, x, cont, COMMA, RPR: tup.Append(x).Merge(cont)),
	("(, (!expr|expr))*", ("(, (!expr|expr))+",), lambda x: x),
	("(, (!expr|expr))*", (), lambda: NodeTuple()),
	("(, (!expr|expr))+", ("(, (!expr|expr))+", ",", "(!expr|expr)"), lambda tup, COMMA, x: tup.Append(x)),
	("(, (!expr|expr))+", (",", "(!expr|expr)"), lambda COMMA, x: x),
]
SYNTAX_RULES = [Production(*p) for p in SYNTAX_RULES]
