#!/usr/bin/bash
# open a terminal that stays open for user usage
mintty /bin/bash -lc 'cd /cygdrive/n/Files/projects/adventofcode;exec bash'
# open explorer
cygstart /cygdrive/n/Files/projects/adventofcode
# close current terminal
exit

