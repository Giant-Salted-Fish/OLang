

class EvaluationContext:
	def __init__(self):
		self._symbol_table = {}
	
	def Lookup(self, symbol: str) -> any:
		return self._symbol_table[symbol]
	
	def PushVar(self, symbol: str, value: any):
		self._symbol_table[symbol] = value
	
	def PopVar(self, symbol: str):
		return self._symbol_table.pop(symbol)
	
	def PushScope(self):
		raise NotImplementedError
	
	def PopScope(self):
		raise NotImplementedError
