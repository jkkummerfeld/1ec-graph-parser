#!/usr/bin/env python3

from __future__ import print_function

import argparse
import sys

parser = argparse.ArgumentParser(description="Replace parses with 'NO_TAG' in output with a single NP (head final).")
args = parser.parse_args()

cur = []
has_no_tag = False
for line in sys.stdin:
    cur.append(line[:-1])
    if 'NO_TAG' in line:
        has_no_tag = True

    if len(line.strip()) == 0:
        if has_no_tag:
            count = 0
            for i in range(len(cur)):
                if cur[i].strip().startswith("#") or len(cur[i].strip()) == 0:
                    continue
                count += 1
            for i in range(len(cur)):
                fields = cur[i].split()
                if len(fields) == 0 or fields[0] == "#":
                    pass
                elif i < len(cur) - 2:
                    fields[3] = "_"
                    fields[4] = str(count)
                    fields[5] = "NP_0"
                    cur[i] = ' '.join(fields)
                else:
                    fields[3] = "NP"
                    fields[4] = "0"
                    fields[5] = "ROOT_0"
                    cur[i] = ' '.join(fields)
                    break

        print("\n".join(cur))
        has_no_tag = False
        cur = []
