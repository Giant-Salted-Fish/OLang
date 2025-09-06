from typing import Callable, Any
from enum import Enum, auto, unique


class EvaluationContext:
	def __init__(self, parent: "EvaluationContext | None" = None):
		self._parent = parent
		self._symbol_table: dict[str, tuple[Any, Callable]] = {}
		self._stack: list[str] = []
	
	def Lookup(self, symbol: str) -> tuple[Any, Callable] | None:
		return self._symbol_table.get(symbol) or (self._parent and self._parent.Lookup(symbol))
	
	def Push[T](self, symbol: str, value: T, destructor: Callable[[T], None]):
		self._stack.append(symbol)
		self._symbol_table[symbol] = (value, destructor)
	
	def Pop[T](self, symbol: str) -> tuple[T, Callable[[T], None]]:
		self._stack.remove(symbol)
		return self._symbol_table.pop(symbol)
	
	def Update[T](self, symbol: str, value: T, destructor: Callable[[T], None]):
		self.Release(symbol)
		self.Push(symbol, value, destructor)
	
	def Release(self, symbol: str):
		self._stack.remove(symbol)
		value, destructor = self._symbol_table.pop(symbol)
		destructor(value)
	
	def PushScope(self):
		return EvaluationContext(self)
	
	def PopScope(self):
		for symbol in self._stack:
			value, destructor = self._symbol_table.pop(symbol)
			destructor(value)
		self._stack.clear()
		return self._parent


@unique
class ControlState(Enum):
	PASS = auto()
	BREAK_LOOP = auto()
	CONT_LOOP = auto()
	RETURN = auto()
