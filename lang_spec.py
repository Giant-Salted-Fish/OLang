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
	
	# expr: prefixed suffix*
	("expr", ("prefixed", "suffix*"), lambda x, suffix: x.Annotate(*suffix)),
	("suffix*", ("suffix+",), lambda suffix: suffix),
	("suffix*", (), lambda: ()),
	("suffix+", ("suffix+", "suffix",), lambda lst, attr: (*lst, attr)),
	("suffix+", ("suffix",), lambda attr: (attr,)),
	
	# prefixed: @ ((!prim|prim) (. ID)*)+ prefixed | prim
	("prefixed", ("@", "((!prim|prim) (. ID)*)+", "prefixed"), lambda AT, lst, x: x.Annotate(chain_call_then_deref(*lst))),
	("prefixed", ("prim",), lambda x: x),
	("((!prim|prim) (. ID)*)+", ("((!prim|prim) (. ID)*)+", "((!prim|prim) (. ID)*)"), lambda lst, x: (*lst, x)),
	("((!prim|prim) (. ID)*)+", ("((!prim|prim) (. ID)*)",), lambda x: (x,)),
	("((!prim|prim) (. ID)*)", ("(!prim|prim)", "(. ID)*"), lambda x, lst: (x, *lst)),
	("(. ID)*", ("(. ID)+",), lambda lst: lst),
	("(. ID)*", (), lambda: ()),
	("(. ID)+", ("(. ID)+", "(. ID)"), lambda lst, x: (*lst, x)),
	("(. ID)+", ("(. ID)",), lambda x: (x,)),
	("(. ID)", (".", "ID"), lambda DOT, ID: ID),
	
	# !expr: (!lmbd|!or) suffix*
	("!expr", ("(!lmbd|!or)", "suffix*"), lambda x, suffix: x.Annotate(*suffix)),
	("(!lmbd|!or)", ("!lmbd",), lambda x: x),
	("(!lmbd|!or)", ("!or",), lambda x: x),
	
	# suffix: : (!lmbd|!or|prefixed)
	("suffix", (":", "(!lmbd|!or|prefixed)",), lambda COLON, attr: attr),
	("(!lmbd|!or|prefixed)", ("!lmbd",), lambda x: x),
	("(!lmbd|!or|prefixed)", ("!or",), lambda x: x),
	("(!lmbd|!or|prefixed)", ("prefixed",), lambda x: x),
	
	# !lmbd: prefixed -> !lmbd | prefixed -> (!or|prefixed)
	("!lmbd", ("prefixed", "->", "!lmbd"), lambda param, ARROW, body: NodeCallable(param, body)),
	("!lmbd", ("prefixed", "->", "(!or|prefixed)"), lambda param, ARROW, body: NodeCallable(param, body)),
	
	# !or: (!or|prefixed) || (!and|prefixed) | !and
	("!or", ("(!or|prefixed)", "||", "(!and|prefixed)"), lambda left, OR, right: NodeBinaryOp(OR, left, right)),
	("!or", ("!and",), lambda x: x),
	("(!or|prefixed)", ("!or",), lambda x: x),
	("(!or|prefixed)", ("prefixed",), lambda x: x),
	
	# !and: (!and|prefixed) && (!eq|prefixed) | !eq
	("!and", ("(!and|prefixed)", "&&", "(!eq|prefixed)"), lambda left, AND, right: NodeBinaryOp(AND, left, right)),
	("!and", ("!eq",), lambda x: x),
	("(!and|prefixed)", ("!and",), lambda x: x),
	("(!and|prefixed)", ("prefixed",), lambda x: x),
	
	# !eq: (!eq|prefixed) (==|!=) (!rel|prefixed) | !rel
	("!eq", ("(!eq|prefixed)", "(==|!=)", "(!rel|prefixed)"), lambda left, EQ, right: NodeBinaryOp(EQ, left, right)),
	("!eq", ("!rel",), lambda x: x),
	("(!eq|prefixed)", ("!eq",), lambda x: x),
	("(!eq|prefixed)", ("prefixed",), lambda x: x),
	("(==|!=)", ("==",), lambda eq: eq),
	("(==|!=)", ("!=",), lambda neq: neq),
	
	# !rel: (!rel|prefixed) (<=|>=|<|>) (!add|prefixed) | !add
	("!rel", ("(!rel|prefixed)", "(<=|>=|<|>)", "(!add|prefixed)"), lambda left, REL, right: NodeBinaryOp(REL, left, right)),
	("!rel", ("!add",), lambda x: x),
	("(!rel|prefixed)", ("!rel",), lambda x: x),
	("(!rel|prefixed)", ("prefixed",), lambda x: x),
	("(<=|>=|<|>)", (">=",), lambda gte: gte),
	("(<=|>=|<|>)", ("<=",), lambda lse: lse),
	("(<=|>=|<|>)", (">",), lambda gt: gt),
	("(<=|>=|<|>)", ("<",), lambda ls: ls),
	
	# !add: (!add|prefixed) (+|-) (!mul|prefixed) | !mul
	("!add", ("(!add|prefixed)", "(+|-)", "(!mul|prefixed)"), lambda left, ADD, right: NodeBinaryOp(ADD, left, right)),
	("!add", ("!mul",), lambda x: x),
	("(!add|prefixed)", ("!add",), lambda x: x),
	("(!add|prefixed)", ("prefixed",), lambda x: x),
	("(+|-)", ("+",), lambda plus: plus),
	("(+|-)", ("-",), lambda minus: minus),
	
	# !mul: (!mul|prefixed) (*|/|%) (!unary|prefixed) | !unary
	("!mul", ("(!mul|prefixed)", "(*|/|%)", "(!unary|prefixed)"), lambda left, MUL, right: NodeBinaryOp(MUL, left, right)),
	("!mul", ("!unary",), lambda x: x),
	("(!mul|prefixed)", ("!mul",), lambda x: x),
	("(!mul|prefixed)", ("prefixed",), lambda x: x),
	("(*|/|%)", ("*",), lambda mul: mul),
	("(*|/|%)", ("/",), lambda div: div),
	("(*|/|%)", ("%",), lambda mod: mod),
	
	# !unary: (+|-|!|~) (!unary|prefixed) | (!prefixed|!deref)
	("!unary", ("(+|-|!|~)", "(!unary|prefixed)"), lambda PRE, unary: NodeUnaryOp(PRE, unary)),
	("!unary", ("(!prefixed|!deref)",), lambda x: x),
	("(!unary|prefixed)", ("!unary",), lambda x: x),
	("(!unary|prefixed)", ("prefixed",), lambda x: x),
	("(!prefixed|!deref)", ("!prefixed",), lambda x: x),
	("(!prefixed|!deref)", ("!deref",), lambda x: x),
	("(+|-|!|~)", ("+",), lambda plus: plus),
	("(+|-|!|~)", ("-",), lambda minus: minus),
	("(+|-|!|~)", ("!",), lambda not_: not_),
	("(+|-|!|~)", ("~",), lambda bit_not: bit_not),
	
	# !prefixed: @ ((!prim|prim) (. ID)*)+ !prefixed | ((!prim|prim) (. ID)*)+ !prim | ((!prim|prim) (. ID)*)+ ((!prim|prim) (. ID)+)
	("!prefixed", ("@", "((!prim|prim) (. ID)*)+", "!prefixed"), lambda AT, lst, x: x.Annotate(chain_call_then_deref(*lst))),
	("!prefixed", ("@", "((!prim|prim) (. ID)*)+", "!prim"), lambda AT, lst, x: x.Annotate(chain_call_then_deref(*lst))),
	("!prefixed", ("@", "((!prim|prim) (. ID)*)+", "((!prim|prim) (. ID)+)"), lambda AT, lst, xs: chain_deref(xs[0], *xs[1:]).Annotate(chain_call_then_deref(*lst))),
	("((!prim|prim) (. ID)+)", ("(!prim|prim)", "(. ID)+"), lambda x, lst: (x, *lst)),
	
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
