#!/usr/bin/env python3

from enum import Enum

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

if __name__ == '__main__':
    b = Block(Ground.SAND)
    print(b.is_conductive())

