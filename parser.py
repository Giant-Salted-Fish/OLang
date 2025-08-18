from scaner import Token
from typing import Iterable, Iterator, Collection, TypeVar, Callable, NamedTuple, Sequence, Any


T = TypeVar("T")


class Production[T](NamedTuple):
	lhs: str
	rhs: tuple[str | T, ...]
	action: Callable
	
	def __str__(self):
		return f"{self.lhs} -> {' '.join(str(s) for s in self.rhs)}"


class Item[T](NamedTuple):
	production: Production[T]
	dot_pos: int
	lookaheads: frozenset[T | None]


class Syntax[T]:
	def __init__(
		self,
		start_symbol: str,
		symbol_2_production: dict[str, Collection[Production[T]]],
		is_terminal: Callable[[str | T], bool],
		first_sets: dict[str, set[T]],
		epsilons: Collection[str]
	):
		self._start_symbol = start_symbol
		self._symbol_2_production = symbol_2_production
		self._is_terminal = is_terminal
		self._first_sets = first_sets
		self._epsilons = epsilons
	
	def GetStartSymbol(self):
		return self._start_symbol
	
	def GetProductionsOf(self, lhs: str):
		return self._symbol_2_production[lhs]
	
	def AsTerminal(self, symbol: str | T) -> T | None:
		if self._is_terminal(symbol):
			# If T can be str, we need a way to distinguish between terminals and non-terminals.
			# We rely on is_terminal to do this, regardless of type.
			return symbol  # type: ignore
		else:
			return None
	
	def AsNonTerminal(self, symbol: str | T) -> str | None:
		if self._is_terminal(symbol):
			return None
		else:
			assert isinstance(symbol, str)
			return symbol
	
	def GetFirstSet(self, symbol: str):
		return self._first_sets[symbol]
	
	def CanProduceEpsilon(self, symbol: str):
		return symbol in self._epsilons
	
	def BuildInitialState(self):
		return self.ExpandState([(prod, 0, (None,)) for prod in self.GetProductionsOf(self._start_symbol)])
	
	def ExpandState(self, items: Iterable[tuple[Production[T], int, Iterable[T | None]]]):
		new_state: dict[tuple[Production[T], int], set[T | None]] = {}
		def expand_item(production: Production[T], dot_pos: int, lookahead: set[T | None]):
			if (production, dot_pos) in new_state:
				prev_lookahead = new_state[production, dot_pos]
				if prev_lookahead.issuperset(lookahead):
					return  # Already expanded
				
				prev_lookahead |= lookahead
				lookahead = prev_lookahead
			else:
				new_state[production, dot_pos] = lookahead
			
			if dot_pos == len(production.rhs):
				return
			
			next_symbol = self.AsNonTerminal(production.rhs[dot_pos])
			if next_symbol is None:
				return
			
			new_lookahead: set[T|None] = set()
			for i in range(dot_pos + 1, len(production.rhs)):
				s = production.rhs[i]
				if following := self.AsTerminal(s):
					new_lookahead.add(following)
					break
				
				following = self.AsNonTerminal(s)
				assert following is not None
				new_lookahead |= self.GetFirstSet(following)
				if not self.CanProduceEpsilon(following):
					break
			else:
				new_lookahead |= lookahead
			
			next_prods = self.GetProductionsOf(next_symbol)
			for p, lkahd in zip(next_prods, (new_lookahead,) + tuple(new_lookahead.copy() for _ in range(len(next_prods) - 1))):
				expand_item(p, 0, lkahd)
		
		for production, dot_pos, lookaheads in items:
			expand_item(production, dot_pos, set(lookaheads))
		assert len(new_state) > 0
		
		return frozenset(
			Item(item, dot_pos, frozenset(lookahead))
			for (item, dot_pos), lookahead in new_state.items()
		)
	
	def BuildLR1Parser(self):
		state_table: dict[frozenset[Item[T]], int] = {}
		shift_table: dict[tuple[int, T | None], int] = {}
		reduce_table: dict[tuple[int, T | None], Production[T]] = {}
		goto_table: dict[tuple[int, str], int] = {}
		def explore(state: frozenset[Item[T]]) -> int:
			if sid := state_table.get(state):
				return sid
			
			sid = len(state_table)
			state_table[state] = sid
			
			working_set = set(it for it in state if it.dot_pos < len(it.production.rhs))
			while working_set:
				item = working_set.pop()
				next_symbol = item.production.rhs[item.dot_pos]
				items = [
					it for it in working_set
					if it.production.rhs[it.dot_pos] == next_symbol
				]
				working_set.difference_update(items)
				items.append(item)
				
				new_items = [(it.production, it.dot_pos + 1, it.lookaheads) for it in items]
				next_state = self.ExpandState(new_items)
				next_sid = explore(next_state)
				if following := self.AsTerminal(next_symbol):
					shift_table[sid, following] = next_sid
				else:
					assert isinstance(next_symbol, str)
					goto_table[sid, next_symbol] = next_sid
			
			working_set = set(it for it in state if it.dot_pos == len(it.production.rhs))
			while working_set:
				item = working_set.pop()
				for terminal in item.lookaheads:
					if (sid, terminal) in shift_table:
						print(f"Potential shift-reduce conflict: {terminal}")
						continue
					
					if (sid, terminal) in reduce_table:
						raise Exception(f"Detect reduce-reduce conflict: {terminal}")
					
					reduce_table[sid, terminal] = item.production
			return sid
		
		explore(self.BuildInitialState())
		state_tbl: list = [None] * len(state_table)
		for state, sid in state_table.items():
			state_tbl[sid] = state
		return Parser(self, state_tbl, shift_table, reduce_table, goto_table)
	
	def BruteLR1Parse(self, token_stream: Iterator[Token[T]]):
		state_stack = [self.BuildInitialState()]
		action_stack: list[Any] = []
		accept = False
		for token in token_stream:
			if accept:
				raise Exception("Unexpected end of input")
			
			# TODO: This is a hack
			if token.GetType() == "COMMENT":
				continue
			
			while True:
				state = state_stack[-1]
				
				new_items = tuple(
					(it.production, it.dot_pos + 1, it.lookaheads)
					for it in state
					if it.dot_pos < len(it.production.rhs) and it.production.rhs[it.dot_pos] == token.GetType()
				)
				if len(new_items) > 0:
					new_state = self.ExpandState(new_items)
					state_stack.append(new_state)
					action_stack.append(token)
					break
				
				# Try to reduce
				productions = tuple(
					production
					for production, dot_pos, lookahead in state
					if dot_pos == len(production.rhs) and token.GetType() in lookahead
				)
				if len(productions) == 0:
					raise Exception("No solution")
				
				if len(productions) > 1:
					raise Exception("Ambiguous")
				
				reduce_rule = productions[0]
				split_idx = len(action_stack) - len(reduce_rule.rhs)
				action_result = reduce_rule.action(*action_stack[split_idx:])
				action_stack = action_stack[:split_idx]
				action_stack.append(action_result)
				
				state_stack = state_stack[:split_idx + 1]
				if token.GetType() is None and reduce_rule.lhs == self._start_symbol:
					accept = True
					break
				
				state = state_stack[-1]
				new_items = tuple(
					(it.production, it.dot_pos + 1, it.lookaheads)
					for it in state
					if it.dot_pos < len(it.production.rhs) and it.production.rhs[it.dot_pos] == reduce_rule.lhs
				)
				new_state = self.ExpandState(new_items)
				state_stack.append(new_state)
		
		if not accept:
			raise Exception("Not enough tokens")
		return action_stack[-1]
	
	@classmethod
	def Build(cls, production_rules: Sequence[Production[T]], is_terminal: Callable[[str | T], bool]):
		symbol_2_production = {}
		for prod in production_rules:
			symbol_2_production.setdefault(prod.lhs, []).append(prod)
		
		epsilons = set()
		while True:
			size = len(epsilons)
			for productions in symbol_2_production.values():
				for prod in productions:
					if prod.rhs == () or all(s in epsilons for s in prod.rhs):
						epsilons.add(prod.lhs)
			if size == len(epsilons):
				break
		
		first_sets = {lhs: set() for lhs in symbol_2_production.keys()}
		while True:
			size = sum(len(s) for s in first_sets.values())
			for productions in symbol_2_production.values():
				for prod in productions:
					for symbol in prod.rhs:
						if is_terminal(symbol):
							first_sets[prod.lhs].add(symbol)
							break
						
						first_sets[prod.lhs] |= first_sets[symbol]
						if symbol not in epsilons:
							break
			if size == sum(len(s) for s in first_sets.values()):
				break
		epsilons = epsilons
		first_sets = first_sets
		
		return cls(
			production_rules[0].lhs,
			symbol_2_production,
			is_terminal,
			first_sets,
			epsilons
		)


class Parser[T]:
	def __init__(
		self,
		syntax: Syntax,
		state_table: Sequence[frozenset[Item[T]]],
		shift_table: dict[tuple[int, T | None], int],
		reduce_table: dict[tuple[int, T | None], Production[T]],
		goto_table: dict[tuple[int, str], int]
	):
		self._syntax = syntax
		self._state_table = state_table
		self._shift_table = shift_table
		self._reduce_table = reduce_table
		self._goto_table = goto_table
	
	def __str__(self):
		return f"{self.__class__.__name__}(state_table_size={len(self._state_table)}, shift_table_size={len(self._shift_table)}, reduce_table_size={len(self._reduce_table)}, goto_table_size={len(self._goto_table)})"
	
	def Parse(self, token_stream: Iterator[Token[T]]):
		state_stack = [0]
		action_stack: list[Any] = []
		accept = False
		for token in token_stream:
			if accept:
				raise Exception("Too much tokens")
			
			# TODO: This is a hack
			if token.GetType() == "COMMENT":
				continue
			
			while True:
				key = (state_stack[-1], token.GetType())
				
				if next_state := self._shift_table.get(key):
					state_stack.append(next_state)
					action_stack.append(token)
					break
				
				if prod := self._reduce_table.get(key):
					split_idx = len(action_stack) - len(prod.rhs)
					action_result = prod.action(*action_stack[split_idx:])
					action_stack = action_stack[:split_idx]
					action_stack.append(action_result)
					
					state_stack = state_stack[:split_idx + 1]
					if token.GetType() is None and prod.lhs == self._syntax.GetStartSymbol():
						accept = True
						break
					
					key = (state_stack[-1], prod.lhs)
					next_state = self._goto_table[key]
					state_stack.append(next_state)
				else:
					raise Exception("No solution")
		
		if not accept:
			raise Exception("Not enough tokens")
		return action_stack[-1]
	
	def Serialize(self, line_writer: Callable[[str], None]):
		pass
	
	@classmethod
	def Deserialize(cls, line_reader: Iterator[str]):
		pass
