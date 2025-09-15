from typing import Any
from enum import Enum, auto, unique


class EvaluationContext:
	def __init__(self, parent: "EvaluationContext | None", local_stack: list[str], local_table: dict[str, Any]):
		self._parent = parent
		self._local_stack = local_stack
		self._local_table = local_table
	
	@classmethod
	def New(cls):
		return cls(None, [], {})
	
	@classmethod
	def Nest(cls, parent: "EvaluationContext"):
		return cls(parent, [], {})
	
	def Resolve(self, symbol: str) -> Any:
		if symbol in self._local_table:
			return self._local_table[symbol]
		
		assert self._parent is not None, f"No variable named \"{symbol}\" to lookup"
		return self._parent.Resolve(symbol)
	
	def Push(self, symbol: str, value: Any):
		assert symbol not in self._local_table
		self._local_stack.append(symbol)
		self._local_table[symbol] = value
	
	def Pop(self, symbol: str) -> Any:
		self._local_stack.remove(symbol)
		return self._local_table.pop(symbol)
	
	def Update(self, symbol: str, value: Any):
		if symbol in self._local_table:
			self._local_table[symbol] = value
		else:
			assert self._parent is not None, f"No variable named \"{symbol}\" to update"
			self._parent.Update(symbol, value)
	
	def GetLocals(self):
		return self._local_table


@unique
class ControlState(Enum):
	PASS = auto()
	BREAK_LOOP = auto()
	CONT_LOOP = auto()
	RETURN = auto()
