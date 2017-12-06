#!/usr/bin/env python3

from __future__ import print_function

import sys
import argparse

parser = argparse.ArgumentParser(description='Converts input from standard shp to format for the tagger.')
args = parser.parse_args()

words = []
spines = []
for line in sys.stdin:
    if len(line.strip()) == 0:
        if len(words) > 0:
            print("{} ||| {}".format(' '.join(words).lower(), ' '.join(spines)))
            words = []
            spines = []
    elif line[0] == '#':
        continue
    else:
        parts = line.strip().split()
        words.append(parts[1])
        spine = parts[3] + ";"
        if len(parts) > 6:
            for i in range(6, len(parts), 6):
                if parts[i] == parts[0]:
                    spine += "{}:{}:{}:{}:{};".format(parts[i+1], parts[i+2], parts[i+3], parts[i+4], parts[i+5])
        spines.append(spine)

if len(words) > 0:
    print("{} ||| {}".format(' '.join(words).lower(), ' '.join(spines)))

###1 Bob     NNP NP                   2 S_0    4 NP_0 T NP_0 F *PRO*
###2 tried   VBD VP_S                 0 ROOT_0
###3 to      TO  _                    4 VP_1
###4 wash    VB  VP_VP_(NP_(*PRO*))_S 2 VP_0
###5 himself PRP NP                   4 VP_0
