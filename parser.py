from scaner import Token
from typing import Iterable, Iterator, Collection, TypeVar, Callable, NamedTuple


T = TypeVar("T")


class Production[T](NamedTuple):
	lhs: str
	rhs: tuple[str | T, ...]
	
	def __repr__(self):
		return f"{self.lhs} -> {' '.join(str(s) for s in self.rhs)}"


class State[T](NamedTuple):
	items: tuple[tuple[Production[T], int, tuple[str | T | None]], ...]
	
	def __iter__(self):
		return iter(self.items)
	
	def __len__(self):
		return len(self.items)
	
	def __getitem__(self, item):
		return self.items[item]
	
	def __contains__(self, item):
		return item in self.items


class Syntax[T]:
	def __init__(
		self,
		symbol_2_production: dict[str, Collection[Production[T]]],
		production_2_action: dict[Production[T], Callable[[tuple], any]],
		is_terminal: Callable[[str | T], bool],
		first_sets: dict[str, Collection[T]],
		epsilons: Collection[str]
	):
		self._symbol_2_production = symbol_2_production
		self._production_2_action = production_2_action
		self._is_terminal = is_terminal
		self._first_sets = first_sets
		self._epsilons = epsilons
	
	def GetProductionsOf(self, lhs: str) -> Collection[Production[T]]:
		return self._symbol_2_production[lhs]
	
	def GetActionOf(self, production: Production[T]) -> Callable[[tuple], any]:
		return self._production_2_action[production]
	
	def AsTerminal(self, symbol: str | T) -> T | None:
		return symbol if self._is_terminal(symbol) else None
	
	def AsNonTerminal(self, symbol: str | T) -> str | None:
		return symbol if not self._is_terminal(symbol) else None
	
	def GetFirstSet(self, symbol: str) -> Collection[T]:
		return self._first_sets[symbol]
	
	def CanProduceEpsilon(self, symbol: str) -> bool:
		return symbol in self._epsilons
	
	def InitState(self, start_symbol: str) -> State[T]:
		items = []
		for production in self.GetProductionsOf(start_symbol):
			items.append((production, 0, (None,)),)
		return self.ExpandState(items)
	
	def ExpandState(self, items: Iterable[tuple[Production[T], int, Iterable[str | T | None]]]) -> State[T] | None:
		new_state = {}
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
			
			new_lookahead = set()
			for i in range(dot_pos + 1, len(production.rhs)):
				s = production.rhs[dot_pos + 1]
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
			
			for p in self.GetProductionsOf(next_symbol):
				expand_item(p, 0, new_lookahead)
		
		for production, dot_pos, lookahead in items:
			expand_item(production, dot_pos, set(lookahead))
		
		if len(new_state) == 0:
			return None
		
		return State(tuple((item, dot_pos, tuple(lookahead)) for (item, dot_pos), lookahead in new_state.items()))
	
	def Parse(self, start_symbol: str, token_stream: Iterator[Token[T]]) -> any:
		state_stack = [self.InitState(start_symbol)]
		action_stack = []
		accept = False
		for token in token_stream:
			while True:
				state = state_stack[-1]
				
				new_items = tuple(
					(production, dot_pos + 1, lookahead)
					for production, dot_pos, lookahead in state
					if dot_pos < len(production.rhs) and production.rhs[dot_pos] == token.GetType()
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
					print("No solution")
					return None
				
				if len(productions) > 1:
					print("Ambiguous")
					return None
				
				reduce_rule = productions[0]
				rhs_len = len(reduce_rule.rhs)
				action_result = self.GetActionOf(reduce_rule)(*action_stack[-rhs_len:])
				action_stack = action_stack[:-rhs_len]
				action_stack.append(action_result)
				
				state_stack = state_stack[:-rhs_len]
				if token.GetType() is None and reduce_rule.lhs == start_symbol:
					accept = True
					break
				
				state = state_stack[-1]
				new_items = tuple(
					(production, dot_pos + 1, lookahead)
					for production, dot_pos, lookahead in state
					if dot_pos < len(production.rhs) and production.rhs[dot_pos] == reduce_rule.lhs
				)
				new_state = self.ExpandState(new_items)
				state_stack.append(new_state)
		return action_stack[-1] if accept else None


def build_syntax(
	production_rules: Iterable[tuple[Production[T], Callable[[tuple], any]]],
	is_terminal: Callable[[str | T], bool]
):
	symbol_2_production = {}
	production_2_action = {}
	for production, action in production_rules:
		symbol_2_production.setdefault(production.lhs, []).append(production)
		production_2_action[production] = action
	
	epsilons = set()
	while True:
		size = len(epsilons)
		for productions in symbol_2_production.values():
			for lhs, rhs in productions:
				if rhs == () or all(s in epsilons for s in rhs):
					epsilons.add(lhs)
		if size == len(epsilons):
			break
	
	first_sets = {lhs: set() for lhs in symbol_2_production.keys()}
	while True:
		size = sum(len(s) for s in first_sets.values())
		for productions in symbol_2_production.values():
			for lhs, rhs in productions:
				for symbol in rhs:
					if is_terminal(symbol):
						first_sets[lhs].add(symbol)
						break
					
					first_sets[lhs] |= first_sets[symbol]
					if symbol not in epsilons:
						break
		if size == sum(len(s) for s in first_sets.values()):
			break
	epsilons = epsilons
	first_sets = first_sets
	
	return Syntax(
		symbol_2_production,
		production_2_action,
		is_terminal,
		first_sets,
		epsilons
	)
