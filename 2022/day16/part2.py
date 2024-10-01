import itertools
import typing

from dataclasses import dataclass

import numpy as np
import numpy.typing as npt

from part1 import compute_all_paths, process_input

TOTAL_TIME = 26


def main():
    graph = process_input()
    important_valves = [
        node_id for node_id in graph.nodes()
        if graph.get_flow_rate(node_id) > 0
    ]
    all_paths = compute_all_paths(
        graph, important_valves,
        start=graph.get_valve_index('AA'), maxtime=TOTAL_TIME
    )

    best = max(my_score + el_score
               for my_path, my_score in all_paths.items()
               for el_path, el_score in all_paths.items()
               if not (my_path & el_path)
               )

    print(f'{best=}')


if __name__ == '__main__':
    main()
