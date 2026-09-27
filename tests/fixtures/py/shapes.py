"""Fixture for steer's Python gate and anchors: the shapes note 12 lists."""
import functools
from typing import overload

MAX_SIZE = 10
DEFAULTS: dict = {"a": 1, "b": [1, 2, 3]}


def top_level(a, b=2, *args, **kwargs):
    """A plain function."""
    total = a + b
    return total


def multi_line_signature(
    first: int,
    second: str = "x",
    *,
    flag: bool = False,
) -> str:
    return f"{first}{second}{flag}"


@functools.singledispatch
def render(value):
    return str(value)


@render.register
def _(value: int):
    return "int"


@render.register
def _(value: str):
    return "str"


@render.register(list)
def _(value):
    return "list"


class First:
    limit = 3

    def __init__(self, name):
        self.name = name

    @property
    def size(self):
        return self._size

    @size.setter
    def size(self, value):
        self._size = value

    @staticmethod
    def make(name):
        return First(name)

    async def fetch(self, url):
        async with self.session.get(url) as resp:
            return await resp.text()


class Second(First):
    def __init__(self, name, extra):
        super().__init__(name)
        self.extra = extra

    def describe(self):
        def helper(x):
            return x * 2

        return helper(len(self.name))


class Third:
    def __init__(self):
        self.items = [x for x in range(3) if x % 2 == 0]

    @overload
    def get(self, key: int) -> int: ...

    @overload
    def get(self, key: str) -> str: ...

    def get(self, key):
        return key


def classify(command):
    match command.split():
        case ["go", direction]:
            return direction
        case [action]:
            return action
        case _:
            return None


if (n := len(DEFAULTS)) > 1:
    SQUARED = lambda v: v * v
