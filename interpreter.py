from typing import TypeVar, Callable


T = TypeVar("T")


class EvaluationContext:
	def __init__(self, parent_lookup=lambda _: None):
		self._parent_lookup = parent_lookup
		self._symbol_table = {}
		self._stack = []
	
	def Lookup(self, symbol: str) -> any:
		return self._symbol_table[symbol] or self._parent_lookup(symbol)
	
	def Declare(self, symbol: str, value: T, destructor: Callable[[T], None] = lambda _: None):
		self._stack.append(symbol)
		self._symbol_table[symbol] = (value, destructor)
	
	def Pop(self, symbol: str):
		self._stack.remove(symbol)
		return self._symbol_table.pop(symbol)[0]
	
	def Release(self, symbol: str):
		self._stack.remove(symbol)
		value, destructor = self._symbol_table.pop(symbol)
		destructor(value)
	
	def PushScope(self):
		return EvaluationContext(self.Lookup)
	
	def PopScope(self):
		while self._stack:
			symbol = self._stack.pop()
			self.Release(symbol)
