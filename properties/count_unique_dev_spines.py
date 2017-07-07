#!/usr/bin/env python3

from __future__ import print_function

import argparse
import string
import sys

def read(filename):
    sent = []
    spines = []
    for line in open(filename):
        line = line.strip()
        if line.startswith("# Sentence"):
            spines.append([])
            sent = line.strip().split()[2:]
        elif len(line) > 0 and line[0] != '#':
            fields = line.split()
            num = int(fields[0])
            word = fields[1]
            pos = fields[2]
            spine = fields[3]
            spines[-1].append((word, pos, spine))
    return spines

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Calculate how many spines in the dev set are novel.')
    parser.add_argument('train', help='Training data in SHP format.')
    parser.add_argument('dev', help='Development data in SHP format.')
    args = parser.parse_args()

    train_spines = read(args.train)
    word_set = set()
    pos_set = set()
    spine_set = set()
    for spines in train_spines:
        for spine in spines:
            word_set.add(spine)
            pos_set.add((spine[1], spine[2]))
            spine_set.add(spine[2])

    results = {
        'Dev sentences with all seen: (word, POS, spine)': 0,
        'Dev sentences with all seen: (POS, spine)': 0,
        'Dev sentences with all seen: spine': 0,
        'Train spines (word, POS, spine)': len(word_set),
        'Train spines (POS, spine)': len(pos_set),
        'Train spines spine': len(spine_set),
        'Dev spines new (word, POS, spine)': 0,
        'Dev spines new (POS, spine)': 0,
        'Dev spines new spine': 0,
        'Dev spines': 0
    }
    sentences = 0
    for spines in read(args.dev):
        sentences += 1
        all_wpresent = True
        all_ppresent = True
        all_cpresent = True
        for spine in spines:
          results['Dev spines'] += 1
          if spine not in word_set:
              results['Dev spines new (word, POS, spine)'] += 1
              all_wpresent = False
          if (spine[1], spine[2]) not in pos_set:
              results['Dev spines new (POS, spine)'] += 1
              all_ppresent = False
          if spine[2] not in spine_set:
              results['Dev spines new spine'] += 1
              all_cpresent = False
        if all_wpresent:
            results['Dev sentences with all seen: (word, POS, spine)'] += 1
        if all_ppresent:
            results['Dev sentences with all seen: (POS, spine)'] += 1
        if all_cpresent:
            results['Dev sentences with all seen: spine'] += 1

    for key in results:
        if key.startswith("Dev sentences"):
            print("{} {} {:.1f}%".format(key, results[key], results[key] * 100 / sentences))
        else:
            print(key, results[key])
