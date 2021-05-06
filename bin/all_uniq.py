#!/usr/bin/env python3
import fileinput

lines = set()

for line in fileinput.input():
    if line in lines:
        continue

    lines.add(line)
    print(line.rstrip('\n'))
