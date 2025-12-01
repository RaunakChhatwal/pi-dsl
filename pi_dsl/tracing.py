from dataclasses import dataclass
from typing import Optional
from . import bindings
from .env import *

@dataclass
class Trace:
    func: str
    args: list[str]
    events: list[str]
    result: Optional[str]

@dataclass
class TraceTree:
    trace: Trace
    children: list['TraceTree']
    size: int

    def __str__(self):
        def truncate(string: str) -> str:
            return string.split("\n")[0] + "..." if "\n" in string else string

        string = f"{self.trace.func}({", ".join(map(truncate, self.trace.args))})"
        if result := self.trace.result:
            string += f" -> {truncate(result)}"
        if children := self.children:
            string += f" [{len(children)} children, {self.size} total nodes]"
        return string

    __repr__ = __str__

def from_bindings(traces: list[bindings.Trace]) -> list[TraceTree]:
    stack: list[TraceTree] = []
    trees: list[TraceTree] = []

    for trace in traces:
        match trace.kind:
            case bindings.Trace.KIND_INVOC:
                func, args = trace.get_invoc()
                tree = TraceTree(Trace(str(func), [str(arg) for arg in args.get()], [], None), [], 1)
                
                if stack:
                    stack[-1].children.append(tree)
                    # TODO: replace with post order traversal
                    for ancestor in stack:
                        ancestor.size += 1
                else:
                    trees.append(tree)
                stack.append(tree)

            case bindings.Trace.KIND_EVENT:
                stack[-1].trace.events.append(str(trace.get_event()))

            case bindings.Trace.KIND_RESULT:
                stack[-1].trace.result = str(trace.get_result())
                stack.pop()

    return trees

def trace_type_check(entries: list[Entry]) -> tuple[Optional[str], list[TraceTree]]:
    entry_bindings = [entry.entry_binding() for entry in entries]
    (error, trace_bindings) = \
        bindings.trace_type_check(List[bindings.Entry](*entry_bindings)).get()
    traces = from_bindings(trace_bindings.get())
    match error.kind:
        case Maybe.KIND_NOTHING:
            return (None, traces)
        case Maybe.KIND_JUST:
            return (str(error), traces)