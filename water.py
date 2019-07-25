#!/usr/bin/env python3

from enum import Enum
import numpy as np
import re

class Ground(Enum):
    CLAY = 0
    SAND = 1
    SPRING = 2
    FLOWING_WATER = 3
    STILL_WATER = 4
    UNINITIALIZED = 5

    def render(self):
        render_dict = {
               Ground.CLAY : '#',
               Ground.SAND : '.',
               Ground.SPRING : '+',
               Ground.FLOWING_WATER : '|',
               Ground.STILL_WATER : '~',
               Ground.UNINITIALIZED: 'x',
                }
        return render_dict[self]

class Block:
    def __init__(self, ground_type : Ground):
        self.ground_type = ground_type

    def is_conductive(self):
        conductive_grounds = {
                Ground.CLAY : False,
                Ground.SAND : True,
                Ground.SPRING: True,
                Ground.STILL_WATER: False,
                Ground.FLOWING_WATER: True,
                }
        return conductive_grounds[self.ground_type]

class Search:
    # clay_veins LIKE [{'x': 12, 'y': 3}]
    def __init__(self, clay_veins : list):
        # sort by y, then x
        self.sorted_clay_veins = sorted(clay_veins, key=lambda dic:(dic['y'], dic['x']))
        self.x_max = max(self.sorted_clay_veins, key = lambda dic:(dic['x']))['x']
        self.x_min = min(self.sorted_clay_veins, key = lambda dic:(dic['x']))['x']
        self.y_max = self.sorted_clay_veins[-1]['y']
        self.y_min = self.sorted_clay_veins[0]['y']
        print('[Search#__init__]: sorted clay veins')
        print(self.sorted_clay_veins)
        self.clay_map = np.full((1 + self.y_max - self.y_min, 1 + self.x_max - self.x_min), fill_value=Block(Ground.UNINITIALIZED))

    def at(self, x,y):
        return self.clay_map[x - self.x_min, y - self.y_min]

    def render(self):
        def block_render(block: Block):
            return block.ground_type.render()
        vec_block_render = np.vectorize(block_render)
        #print(vec_block_render(self.clay_map))
        return vec_block_render(self.clay_map)

class SearchBuilder:
    regexp_x_first = re.compile('x=(?P<x_num>[0-9]+),\sy=(?P<y_first>[0-9]+)\.\.(?P<y_second>[0-9]+)')
    regexp_y_first = re.compile('y=(?P<x_first>[0-9]+)\.\.(?P<x_second>[0-9]+),(?P<y_num>[0-9])')
    
    def __init__(self):
        self.veins = [] # LIKE [{'x':123, 'y':12}]

    # list of strings for parsing
    def build_search(self, clay_veins_strings : list):
        for line in clay_veins_strings:
            res_x_first = re.match(SearchBuilder.regexp_x_first, line)
            res_y_first = re.match(SearchBuilder.regexp_y_first, line)
            if res_x_first is not None:
                x = int(res_x_first.group('x_num'))
                r = range(int(res_x_first.group('y_first')), int(res_x_first.group('y_second')))
                for y in r:
                    self.veins.append({'x': x, 'y': y})
            elif res_y_first is not None:
                y = int(res_y_first.group('y_num'))
                r = range(int(res_y_first.group('x_first')), int(res_y_first.group('x_second')))
                for x in r:
                    self.veins.append({'x': x, 'y': y})

        return Search(self.veins)


if __name__ == '__main__':
    sb = SearchBuilder()
    s = sb.build_search(
        [
            'x=495, y=2..7',
            'y=7, x=495..501',
            'x=501, y=3..7',
            'x=498, y=2..4',
            'x=506, y=1..2',
            'x=498, y=10..13',
            'x=504, y=10..13',
            'y=13, x=498..504',
        ]
    )
    print(s.at(506, 1).ground_type.render())
    s.render()

