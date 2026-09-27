"""T62 fixture: exercised by the Python extractor's conformance test. Covers a same-file exact call,
a cross-file declared call resolved through an import, a class hierarchy with an override reached via
`super()` (receiver_hint="base") and `self.` (receiver_hint="self"), a decorator, `__all__`-based
exported?, an explicit `# steer: entry` marker, the `__main__` guard as a has_statements? signal, and a
helper reachable from nothing (so a later reachability pass has something real to call dead)."""
from helper import double, triple

__all__ = ["area"]


# steer: entry
def area(shape):
    return compute_area(shape)


def compute_area(shape):
    return double(shape.w)


class Animal:
    def speak(self):
        return "..."


class Dog(Animal):
    def speak(self):
        return super().speak() + self.bark()

    def bark(self):
        return "woof"


def unused_helper(x):
    return triple(x)


if __name__ == "__main__":
    area(None)
