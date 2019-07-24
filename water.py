#!/usr/bin/env python3

from enum import Enum
import numpy as np

class Ground(Enum):
    CLAY = 0
    SAND = 1
    SPRING = 2
    FLOWING_WATER = 3
    STILL_WATER = 4

    def render(self):
        render_dict = {
               Ground.CLAY : '#',
               Ground.SAND : '.',
               Ground.SPRING : '+',
               Ground.FLOWING_WATER : '|',
               Ground.STILL_WATER : '~',
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
        self.x_max = self.sorted_clay_veins[-1]['x']
        self.x_min = self.sorted_clay_veins[0]['x']
        self.y_max = self.sorted_clay_veins[-1]['y']
        self.y_min = self.sorted_clay_veins[0]['y']
        self.clay_map = np.empty((1 + self.y_max - self.y_min, 1 + self.x_max - self.x_min))
        print(self.clay_map)

if __name__ == '__main__':
    b = Block(Ground.SAND)
    s = Search(
                [
                    {'x': 0, 'y':0},
                    {'x': 1, 'y':3}
                ]
            )
    print(b.is_conductive())

