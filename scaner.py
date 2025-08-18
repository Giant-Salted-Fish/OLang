from typing import Iterable, Sequence, Iterator, TypeVar, Callable
import re


T = TypeVar("T")


class Token[T]:
	def __init__(self, type_: T | None, lines: Sequence[str], line_num: int, col_idx: int, token_len: int):
		self._type = type_
		self._lines = lines
		self._line_num = line_num
		self._col_idx = col_idx
		self._token_len = token_len
	
	def GetType(self):
		return self._type
	
	def GetValue(self):
		return self._lines[self._line_num][self._col_idx:self._col_idx + self._token_len]
	
	def GetLineNum(self):
		return self._line_num + 1
	
	def GetColumnNum(self):
		return self._col_idx + 1
	
	def __str__(self):
		return f"Token(type={repr(self.GetType())}, value={repr(self.GetValue())}, at=({self.GetLineNum()}, {self.GetColumnNum()}))"


class Scaner[T]:
	def __init__(
		self,
		match_lst: Iterable[tuple[T, Callable[[str, int], int]]],
		match_space: Callable[[str, int], int]
	):
		self._match_lst = match_lst
		self._match_space = match_space
	
	def Tokenize(self, src: str) -> Iterator[Token[T]]:
		line_num = 0
		col_idx = 0
		lines = src.splitlines()
		while line_num < len(lines):
			line = lines[line_num]
			col_idx = 0
			while True:
				col_idx += self._match_space(line, col_idx)
				if col_idx >= len(line):
					break
				
				match_len = 0
				token_type = None
				for type_, matcher in self._match_lst:
					mlen = matcher(line, col_idx)
					if mlen > match_len:
						match_len = mlen
						token_type = type_
				if match_len == 0:
					raise ValueError(f"Invalid token at line={line_num + 1}, column={col_idx + 1}")
				
				yield Token(token_type, lines, line_num, col_idx, match_len)
				
				col_idx += match_len
			line_num += 1
		yield Token(None, lines, line_num - 1, col_idx, 0)
	
	@classmethod
	def Build(cls, types: Iterable[tuple[T, str]]):
		def build_matcher(pattern: str):
			r = re.compile(pattern)
			def match(s: str, offset: int):
				m = r.match(s, offset)
				if m is None:
					return 0
				return m.end() - offset
			return match
		match_lst = tuple((type_, build_matcher(pattern)) for type_, pattern in types)
		match_space = build_matcher(r"\s*")
		return Scaner(match_lst, match_space)
