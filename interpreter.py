from typing import Callable, Any
from enum import Enum, auto, unique


class EvaluationContext:
	def __init__(self, parent: "EvaluationContext | None"):
		self._parent = parent
		self._symbol_table: dict[str, tuple[Any, Callable]] = {}
		self._stack: list[str] = []
	
	def Lookup(self, symbol: str) -> Any:
		if symbol in self._symbol_table:
			return self._symbol_table[symbol]
		
		assert self._parent is not None, f"No variable named {{{symbol}}} to lookup"
		return self._parent.Lookup(symbol)
	
	def Push(self, symbol: str, value: Any):
		self._stack.append(symbol)
		self._symbol_table[symbol] = value
	
	def Pop(self, symbol: str) -> Any:
		self._stack.remove(symbol)
		return self._symbol_table.pop(symbol)
	
	def Update(self, symbol: str, value: Any):
		if symbol in self._symbol_table:
			self._symbol_table[symbol] = value
		else:
			assert self._parent is not None, f"No variable named {{{symbol}}} to update"
			self._parent.Update(symbol, value)


@unique
class ControlState(Enum):
	PASS = auto()
	BREAK_LOOP = auto()
	CONT_LOOP = auto()
	RETURN = auto()
