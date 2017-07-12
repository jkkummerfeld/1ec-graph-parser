#!/usr/bin/env python3

from __future__ import print_function

import argparse
import sys

parser = argparse.ArgumentParser(description='Add lines indicating sentence IDs, in the correct format (reads from standard in, writes to standard out).')
parser.add_argument('start', help='Starting number', default=1, nargs='?')
args = parser.parse_args()

num = int(args.start)

for line in sys.stdin:
    print("# SentID", num)
    print(line[:-1])
    num += 1
