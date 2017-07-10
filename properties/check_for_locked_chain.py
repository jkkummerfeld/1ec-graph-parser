#!/usr/bin/env python3

from __future__ import print_function

import argparse
import sys

def check_edges(edges):
    # Reduce to crossing edges
    crossed = set()
    for edge0 in edges:
        for edge1 in edges:
            if edge0[0] < edge1[0] < edge0[1] < edge1[1]:
                crossed.add(edge0)
                crossed.add(edge1)

    if len(crossed) < 5:
        return False

    for big_left in crossed:
        auto_fail = False

        # Is it crossed correctly? (two edges, same end, outside on right)
        inside = set()
        outside = set()
        potential_big_right = set()
        for edge in crossed:
            if edge[0] < big_left[0] < edge[1] < big_left[1]:
                auto_fail = True
                break
            if big_left[0] < edge[0] < big_left[1] < edge[1]:
                potential_big_right.add(edge)
                inside.add(edge[0])
                outside.add(edge[1])
        if len(potential_big_right) != 2 or len(outside) != 1:
            auto_fail = True
        if auto_fail:
            continue

        # Find the big right
        big_right = None
        little_right = None
        for edge in potential_big_right:
            if big_right is None:
                big_right = edge
                little_right = edge
            elif big_right[0] > edge[0]:
                big_right = edge
            else:
                little_right = edge

        # Check big right is correct
        inside = set()
        outside = set()
        potential_big_left = set()
        for edge in crossed:
            if edge[0] < big_right[0] < edge[1] < big_right[1]:
                potential_big_left.add(edge)
                inside.add(edge[1])
                outside.add(edge[0])
            if big_right[0] < edge[0] < big_right[1] < edge[1]:
                auto_fail = True
                break
        if len(potential_big_left) != 2 or len(outside) != 1:
            auto_fail = True
        if auto_fail:
            continue

        # Find the little left
        little_left = None
        for edge in potential_big_left:
            if little_left is None:
                little_left = edge
            elif little_left[1] > edge[1]:
                little_left = edge

        # Now we have big and little on both sides
        cur = little_left
        next_left = big_right[0]
        while cur != little_right:
            ncur = None
            for edge in crossed:
                if edge[0] == next_left:
                    if cur[0] < edge[0] < cur[1] < edge[1]:
                        ncur = edge
            if ncur is None:
                auto_fail = True
                break
            next_left = cur[1]
            cur = ncur

        if auto_fail:
            continue

        return True
    return False

def add_pair(num0, num1, pairs):
    pairs.append((min(num0, num1), max(num0, num1)))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Check if a set of parses contain a Locked Chain (expects my shp format).')
    args = parser.parse_args()

    cur = []
    sentence = ""
    for line in sys.stdin:
        if len(line.strip()) == 0:
            if len(cur) > 0:
                if check_edges(cur):
                    print(sentence)
            cur = []
            sentence = ""
        elif line[0] not in '#\n':
            parts = line.strip().split()
            sentence += " "+ parts[1]
            start = int(parts[0])
            add_pair(start, int(parts[4]), cur)
            for num in range(6, len(parts), 6):
                add_pair(start, int(parts[num]), cur)

