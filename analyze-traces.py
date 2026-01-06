from __future__ import annotations

import sys
from textual.app import App
from textual.containers import Vertical, VerticalScroll
from textual.widgets import Footer, Header, Static, Tree
from textual.widgets.tree import TreeNode
from pi_dsl.tracing import TraceTree


class TraceTui(App[None]):
    BINDINGS = [
        ("q", "quit", "Quit"),
        ("enter", "expand_children", "Children"),
        ("c", "expand_children", "Children"),
        ("s", "expand_stack", "Stack"),
        ("x", "collapse", "Collapse"),
    ]

    def __init__(self, traces: list[TraceTree]):
        super().__init__()
        self.traces = traces
        self.trace_tree = Tree[TraceTree]("Trace roots")
        self.trace_tree.styles.width = 60
        self.trace_tree.styles.min_width = 40
        self.details = Static("", expand=True)

    async def on_mount(self):
        header = Header()
        footer = Footer()
        body = Vertical()
        bottom = VerticalScroll()
        self.trace_tree.styles.width = "100%"
        self.trace_tree.styles.height = "60%"
        bottom.styles.height = "40%"
        await self.mount(header, body, footer)
        await body.mount(self.trace_tree, bottom)
        await bottom.mount(self.details)

        self.trace_tree.root.expand()
        for i, t in enumerate(self.traces):
            self.trace_tree.root.add(f"{i}: {t}", data=t)
        self.trace_tree.focus()

    def on_tree_node_highlighted(self, event: Tree.NodeHighlighted[TraceTree]):
        data = event.node.data
        if data is None:
            self.details.update("")
        else:
            self.details.update(self._render_details(data))

    def action_expand_children(self) -> None:
        node = self.trace_tree.cursor_node
        assert node is not None and node.data is not None
        self._populate_children(node, node.data)
        node.expand()

    def action_expand_stack(self) -> None:
        node = self.trace_tree.cursor_node
        assert node is not None and node.data is not None
        self._populate_stack(node, node.data)
        node.expand()

    def action_collapse(self) -> None:
        node = self.trace_tree.cursor_node
        assert node is not None
        node.collapse()
        node.remove_children()

    def _populate_children(self, node: TreeNode[TraceTree], trace: TraceTree):
        node.remove_children()
        for i, child in enumerate(trace.children):
            node.add(f"{i}: {child}", data=child)

    def _populate_stack(self, node: TreeNode[TraceTree], trace: TraceTree):
        node.remove_children()
        frames = trace.stack_trace(only_pending=False)
        for i, frame in enumerate(frames):
            node.add(f"{i}: {frame}", data=frame)

    def _render_details(self, trace: TraceTree) -> str:
        t = trace.trace
        args = "\n".join(t.args)
        events = "\n".join(t.events)
        result = t.result or ""

        return "\n".join(
            [
                f"func: {t.func}",
                f"children: {len(trace.children)}",
                f"size: {trace.size}",
                "",
                "args:",
                args,
                "",
                "events:",
                events,
                "",
                "result:",
                result,
            ]
        )

def main() -> None:
    from pi_dsl.env import PiDslError

    try:
        __import__(sys.argv[1])
        raise Exception("No error encountered")
    except PiDslError as error:
        TraceTui(error.traces).run()

if __name__ == "__main__":
    main()
