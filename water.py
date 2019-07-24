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

class Block():
    def __init__(self):
        pass

if __name__ == '__main__':
    g = Ground.CLAY
    print(g.render())

