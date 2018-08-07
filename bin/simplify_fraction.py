#!/usr/bin/env python3

import sys

# Euclid's recursive formula
def euclid(first, second):
    remainder = first % second;
    return second if remainder == 0 else euclid(second, remainder)

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('Usage: simplify_fraction.py first_num second_num')
        exit(1)

    first  = int(sys.argv[1])
    second = int(sys.argv[2])

    gcd = euclid(first, second)
    print('GCD: {}'.format(gcd))
    print('Simplified: {} {}'.format(int(first / gcd), int(second / gcd)))
