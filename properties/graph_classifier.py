#!/usr/bin/env python3

from __future__ import print_function

import argparse
import sys

def read_arcs(src):
    arcs = []
    info = {}
    while True:
        line = src.readline()
        if len(line) == 0:
            if len(arcs) > 0:
                yield (arcs, info)
            return
        elif len(line.strip()) == 0:
            yield (arcs, info)
            arcs = []
            info = {}
        elif line[0] != '#':
            fields = line.strip().split()
            child = int(fields[0])
            for part in fields[6::6]:
                parent = int(part)
                arcs.append((child, parent))
        else:
            content = line
            name = line.strip().split()[1]
            if name in info:
                info[name] += "\n"+ content
            else:
                info[name] = content

def is_projective(arcs):
    for arc0 in arcs:
        for arc1 in arcs:
            if arc0[0] < arc1[0] < arc0[1] < arc1[1]:
                return False
            if arc1[0] < arc0[0] < arc1[1] < arc0[1]:
                return False
    return True

def has_self_arc(arcs):
    for arc in arcs:
        if arc[0] == arc[1]:
            return True
    return False

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Find out information for graphs: (1) projective? (2) are self-arcs present?')
    args = parser.parse_args()

    for arcs, info in read_arcs(sys.stdin):
        if is_projective(arcs):
            print("Projective", info['Sentence'])
        if has_self_arc(arcs):
            print("Has self arc", info['Sentence'])
