import collections
import sys
import typing
import re

import numpy as np
import numpy.typing as npt

from dataclasses import dataclass

INPUT_FORMAT = re.compile(
    r'Valve (\w+) has flow rate=(\d+); '
    r'tunnels? leads? to valves? (.*)'
)

ValveId = int


@dataclass(frozen=True)
class Valve:
    name: str
    flow_rate: int
    destinations: list[str]


def read_input() -> typing.Iterator[Valve]:
    for line in sys.stdin:
        if matches := INPUT_FORMAT.match(line):
            valve, flow_rate, destinations = matches.groups()
            destinations = [d.strip() for d in destinations.split(',')]
            yield Valve(valve, int(flow_rate), destinations)
        else:
            raise ValueError(f'Invalid input line: `{line}`')


class ValveGraph:
    def __init__(self, valves: list[Valve]):
        self.valves = valves

        valve_count = len(valves)
        self.graph = np.zeros((valve_count, valve_count), dtype=np.uint)
        for valve_index, valve in enumerate(valves):
            for destination in valve.destinations:
                dest_index = self.get_valve_index(destination)
                self[valve_index, dest_index] = 1

    def __getitem__(self, index: tuple[ValveId, ValveId]) -> int:
        return int(self.graph[index])

    def __setitem__(self, index: tuple[ValveId, ValveId], distance: int):
        self.graph[index] = distance
        # TODO Also set for other direction
        # self.graph[index[1], index[0]] = distance

    def get_valve_index(self, valve_name: str) -> ValveId:
        for i, v in enumerate(self.valves):
            if v.name == valve_name:
                return i
        raise KeyError(f'Invalid valve name: {valve_name}')

    def get_flow_rate(self, valve: ValveId) -> int:
        return self.valves[valve].flow_rate

    def get_destinations(self, valve: ValveId) -> typing.Iterable[ValveId]:
        """Get immediate destinations"""
        return (
            destination for destination in self.nodes()
            if self[valve, destination] == 1
        )

    @property
    def node_count(self) -> int:
        return len(self.valves)

    def nodes(self) -> typing.Iterable[ValveId]:
        return range(self.node_count)


def breadth_first_span(graph: ValveGraph, starting_node: ValveId):
    # Breadth-first search to all other nodes
    visited = np.zeros(graph.node_count, dtype=np.bool_)
    visited[starting_node] = True
    queue = collections.deque((starting_node,), maxlen=graph.node_count)
    while len(queue) > 0:
        src = queue.popleft()
        for dst in graph.get_destinations(src):
            if visited[dst]:
                continue
            visited[dst] = True
            queue.append(dst)
            if graph[starting_node, dst] == 0:
                graph[starting_node, dst] = \
                    graph[starting_node, src] + graph[src, dst]


def compute_full_connectivity(graph: ValveGraph):
    # Compute full connectivity
    for node in graph.nodes():
        breadth_first_span(graph, node)


def process_input() -> ValveGraph:
    valves = list(read_input())
    graph = ValveGraph(valves)
    compute_full_connectivity(graph)
    return graph


PathData = dict[int, int]  # Mapping from bitmap to score


def compute_all_paths(
    graph: ValveGraph, nodes: list[ValveId], start: ValveId, maxtime: int
) -> PathData:
    path_scores: dict[int, int] = {}

    def recurse(
        node: ValveId, visited: int, score: int, remaining_time: int
    ):
        path_scores[visited] = max(path_scores.get(visited, 0), score)
        for next_valve in nodes:
            if visited & (1 << next_valve):
                continue
            time = graph[node, next_valve] + 1  # Travel + open
            new_remaining = remaining_time - time
            if new_remaining <= 0:
                continue

            pressure_release = (remaining_time - time) \
                * graph.get_flow_rate(next_valve)
            recurse(
                next_valve,
                visited | (1 << next_valve),
                score + pressure_release,
                remaining_time - time,
            )

    recurse(start, 0, 0, maxtime)
    return path_scores


TOTAL_TIME = 30


def main():
    graph = process_input()
    important_valves = [
        node_id for node_id in graph.nodes()
        if graph.get_flow_rate(node_id) > 0
    ]
    best = max(compute_all_paths(
        graph, important_valves,
        start=graph.get_valve_index('AA'), maxtime=TOTAL_TIME
    ).values())

    print(f'{best=}')


if __name__ == '__main__':
    main()
