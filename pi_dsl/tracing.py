from __future__ import annotations

from dataclasses import dataclass
from typing import Optional
from . import bindings

@dataclass
class Trace:
    func: str
    args: list[str]
    events: list[str]
    result: Optional[str]


def truncate(string: str) -> str:
    truncated = False
    if "\n" in string:
        string = string.split("\n")[0]
        truncated = True

    if len(string) > 35:
        string = string[:35]
        truncated = True

    if truncated:
        string += "..."

    return string

@dataclass
class TraceTree:
    trace: Trace
    children: list['TraceTree']
    size: int

    def __str__(self):
        args = ", ".join([f"`{truncate(arg)}`" for arg in self.trace.args])
        string = f"{self.trace.func}({args})"
        if result := self.trace.result:
            string += f" -> {truncate(result)}"
        if children := self.children:
            string += f" [{len(children)} children, {self.size} total nodes]"
        return string

    def stack_trace(self, only_pending: bool=True) -> list[TraceTree]:
        traces: list[TraceTree] = []
        curr = self
        while (only_pending and curr.trace.result is None) or not only_pending:
            traces.append(curr)
            if len(curr.children) > 0:
                curr = curr.children[-1]
            else:
                break

        return traces

    __repr__ = __str__

def from_bindings(traces: list[bindings.Trace]) -> list[TraceTree]:
    stack: list[TraceTree] = []
    trees: list[TraceTree] = []

    for trace in traces:
        match trace.kind:
            case bindings.Trace.KIND_INVOC:
                pass

            case bindings.Trace.KIND_EVENT:
                stack[-1].trace.events.append(str(trace.get_event()))
                continue

            case bindings.Trace.KIND_RESULT:
                stack[-1].trace.result = str(trace.get_result())
                stack.pop()
                continue

        
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

    return trees