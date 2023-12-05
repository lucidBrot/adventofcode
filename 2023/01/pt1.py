#!/usr/bin/env python3
import re

def unnumber(nr:str):
    return {
            "one": 1,
            "two": 2,
            "three": 3,
            "four": 4,
            "five": 5,
            "six": 6,
            "seven": 7,
            "eight": 8,
            "nine": 9,
            }.get(nr, None) or int(nr)

def get_calibration_nr(line:str):
    # combine the first and last digit
    list_ = list(re.findall('(\d|one|two|three|four|five|six|seven|eight|nine)', line))
    print(f"{list_=}")
    nr1 = unnumber(list_[0])
    #nr2 = unnumber(list_[-1])
    
    # handle edge case that is neither in the example nor in the specs:
    # When the last word that is considered a digit starts with a character that matches, e.g. "sevenine", i am ignoring the "ine" part. which makes perfect sense but seems to be not what they wanted.
    # So...
    rev_str = ''.join(reversed(')d\|one|two|three|four|five|six|seven|eight|nine('))
    reversed_list_ = list(re.findall(rev_str, ''.join(reversed(line))))
    nr2 = unnumber(''.join(reversed(reversed_list_[0])))

    nr = int(str(nr1) + str(nr2))
    print(f"{line} -> {nr}")
    return nr


def main():
    with open("input.txt", "r") as f:
    #with open("example.txt", "r") as f:
        total = sum(get_calibration_nr(line) for line in f.readlines())

    print("pt1 total: {}".format(total))
    pass

if __name__ == "__main__":
    main()
