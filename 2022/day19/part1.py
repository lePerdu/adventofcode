import collections
import enum
import itertools
import re
import sys
import typing

from dataclasses import dataclass

import numpy as np
import numpy.typing as npt

np.seterr(divide='ignore', invalid='ignore')


class Resource(enum.IntEnum):
    ORE = 0
    CLAY = 1
    OBSIDIAN = 2
    GEODE = 3


@dataclass(frozen=True)
class Blueprint:
    blueprint_id: int
    requires: np.ndarray[tuple[int, int], np.dtype[np.int_]]

    def __post_init__(self):
        self.requires.setflags(write=False)


@dataclass(frozen=True)
class Resources:
    stores: np.ndarray[tuple[int], np.dtype[np.int_]]
    robots: np.ndarray[tuple[int], np.dtype[np.int_]]

    def __post_init__(self):
        self.stores.setflags(write=False)
        self.robots.setflags(write=False)

    def __hash__(self) -> int:
        return hash((tuple(self.stores), tuple(self.robots)))

    def __eq__(self, other: 'Resources') -> bool:
        return (
            np.array_equal(self.stores, other.stores) and
            np.array_equal(self.robots, other.robots)
        )

    def strictly_greater(self, other: 'Resources') -> bool:
        return bool(
            np.all(self.stores > other.stores) and
            np.all(self.robots > other.robots)
        )


PATTERN = re.compile(
    r'Blueprint (\d+):\s*'
    r'Each ore robot costs (\d+) ore.\s*'
    r'Each clay robot costs (\d+) ore.\s*'
    r'Each obsidian robot costs (\d+) ore and (\d+) clay.\s*'
    r'Each geode robot costs (\d+) ore and (\d+) obsidian.\s*'
)


def read_input() -> typing.Iterator[Blueprint]:
    all_input = sys.stdin.read()
    for found in PATTERN.finditer(all_input):
        blueprint_id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs = \
            (int(g) for g in found.groups())
        yield Blueprint(
            blueprint_id,
            np.array((
                (ore_ore, 0, 0, 0),
                (clay_ore, 0, 0, 0),
                (obs_ore, obs_clay, 0, 0),
                (geo_ore, 0, geo_obs, 0),
            ), dtype=np.int_),
        )


def time_until_able(
    resources: Resources, blueprint: Blueprint, robot_type: Resource
) -> typing.Optional[int]:
    remaining = np.maximum(
        blueprint.requires[robot_type] - resources.stores, 0)
    minutes_remaining = remaining / resources.robots
    if np.any(np.isinf(minutes_remaining)):
        return None
    max_minutes = np.nanmax(minutes_remaining)
    return int(np.ceil(max_minutes))


@dataclass(frozen=True)
class Action:
    wait_time: int
    robot_type: Resource


@dataclass
class State:
    # Action taken
    action: Action
    # Resources after action
    resources: Resources


def print_history(history: list[State], maxtime: int = 24):
    elapsed_time = 0
    for step in history:
        print(f'== Minute {elapsed_time} ==')
        if step.action.wait_time > 0:
            print(f'Wait {step.action.wait_time} minutes')
        elapsed_time += step.action.wait_time + 1
        print(f'Build {step.action.robot_type.name} robot')
        print('Resources:', step.resources.stores)
        print('Robots:', step.resources.robots)
        print()


def get_possible_actions(
    resources: Resources, blueprint: Blueprint
) -> typing.Iterator[Action]:
    for robot_type in Resource:
        wait_time = time_until_able(resources, blueprint, robot_type)
        if wait_time is not None:
            yield Action(wait_time, robot_type)


Result = int


class Cache:
    def __init__(self, maxtime: int):
        self.times_cache: list[dict[Resources, Result]] = \
            [{} for _ in range(maxtime+1)]
        self.hits = 0

    def __len__(self) -> int:
        return sum(len(t) for t in self.times_cache)

    def get(self, key: tuple[int, Resources]) -> typing.Optional[Result]:
        times = self.times_cache[key[0]]
        result = times.get(key[1])
        if result is not None:
            self.hits += 1
        return result

    def update(self, key: tuple[int, Resources], new: Result):
        times = self.times_cache[key[0]]
        times[key[1]] = new

    def has_better(self, key: tuple[int, Resources]) -> bool:
        times = self.times_cache[key[0]]
        return any(r.strictly_greater(key[1]) for r in times.keys())


cache: Cache
best = 0


def calc_blueprint_score(
    resources: Resources, blueprint: Blueprint, maxtime: int
) -> Result:
    # Score if we did nothing but wait
    result: int = (
        resources.stores[Resource.GEODE] +
        resources.robots[Resource.GEODE] * maxtime
        # , history,
    )
    if maxtime <= 1:
        global best
        if result > best:
            best = result
            print('New best:', result)
        #     print_history(result)
        #     print('Score:', best)
        #     print()
        #     print()
        # No need to add this to the cache since checking time is quicker then
        # hashing
        return result

    cache_key = (maxtime, resources)
    existing_cache = cache.get(cache_key)
    if existing_cache is not None:
        return existing_cache

    # if cache.has_better(cache_key):
    #     # "Abort" by returning something which will be worse
    #     return 0

    for action in get_possible_actions(resources, blueprint):
        if action.wait_time + 1 >= maxtime:
            continue

        stores = (
            resources.stores
            + resources.robots * (action.wait_time + 1)
            - blueprint.requires[action.robot_type]
        )

        robots = resources.robots.copy()
        robots[action.robot_type] += 1

        branch = calc_blueprint_score(
            Resources(stores, robots),
            blueprint,
            maxtime - action.wait_time - 1,
            # history + [State(action, branch_resources)],
        )
        if branch > result:
            result = branch

    cache.update(cache_key, result)
    return result


def calc_blueprint_quality(blueprint: Blueprint, maxtime) -> int:
    init_resources = Resources(
        np.zeros(4, dtype=np.int_),
        np.array((1, 0, 0, 0), dtype=np.int_),
    )

    result = calc_blueprint_score(init_resources, blueprint, maxtime)

    return blueprint.blueprint_id * result  # [0], result[1]


TIME = 24


def main():
    blueprints = read_input()

    total = 0
    for b in blueprints:
        # Reset cache
        global cache, best
        best = 0
        cache = Cache(TIME)
        quality = calc_blueprint_quality(b, TIME)
        print(f'Quality for {b.blueprint_id}: {quality}')
        total += quality
        print(f'Cache size: {len(cache)}. Hit {cache.hits} times')

    print(total)


if __name__ == '__main__':
    main()
