#!/usr/bin/bash
tr , '\n' < 'cable1.line' > 'cable1.txt'
tr , '\n' < 'cable2.line' > 'cable2.txt'
cobc -x -o pt1.exe pt1.cob && ./pt1.exe

