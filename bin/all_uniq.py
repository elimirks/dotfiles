#!/usr/bin/env python3
import argparse
import fileinput
import sys

def only_uniq():
    lines = set()
    
    for line in sys.stdin:
        if not line in lines:
            lines.add(line)
            print(line.rstrip('\n'))

def uniq_counts():
    lines = dict()

    for line in sys.stdin:
        if line in lines:
            lines[line] += 1
        else:
            lines[line] = 1

    for line,count in lines.items():
        print('{:>10} {}'.format(count, line.rstrip('\n')))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Count unique lines')
    parser.add_argument('-c', '--count', dest='count', action='store_const',
                        const=True, default=False,
                        help='Display counts of duplicate lines')

    args = parser.parse_args()

    if args.count:
        uniq_counts()
    else:
        only_uniq()
    
