#!/usr/bin/env python3

import sys

def read_parse(src):
  summaries = []
  unlab_summaries = []
  while True:
    line = src.readline()
    if line == '':
      return (None, None)
    elif line.strip() == '':
      break
    if line[0] != '#':
      parts = line.strip().split()
      spine = parts[3]
      edge = ' '.join([parts[0], parts[4], parts[5]])
      unlab_edge = parts[0] +" "+ parts[4]
      traces = []
      unlab_traces = []
      for i in range(6, len(parts), 6):
        traces.append(parts[0] +' '+ ' '.join(parts[i:i+6]))
        unlab_traces.append(parts[0] +" "+ parts[i])
      summaries.append((spine, edge, traces))
      unlab_summaries.append(("_", unlab_edge, unlab_traces))
  return (summaries, unlab_summaries)

def results(prefix, gcounts, acounts, matches):
  for word in matches:
    g = gcounts[word]
    m = matches[word]
    a = acounts[word]
    p = 0.0
    if a > 0:
      p = 100.0 * m / a
    r = 0.0
    if g > 0:
      r = 100.0 * m / g
    f = 0.0
    if (p+r) > 0:
      f = 2 * p * r / (p + r)
    print("{} {}  {:>4} {:>4} {:>4}  {:.1f} {:.1f} {:.1f}".format(prefix, word, m, g, a, p, r, f))

gold = open(sys.argv[1])
auto = open(sys.argv[2])

gcounts = { 'spine': 0, 'edge': 0, 'trace': 0 }
matches = { 'spine': 0, 'edge': 0, 'trace': 0 }
acounts = { 'spine': 0, 'edge': 0, 'trace': 0 }
total = 0
gcounts_u = { 'edge': 0, 'trace': 0 }
matches_u = { 'edge': 0, 'trace': 0 }
acounts_u = { 'edge': 0, 'trace': 0 }
while True:
  gparse, gparse_u = read_parse(gold)
  aparse, aparse_u = read_parse(auto)
  if gparse is None or aparse is None:
    if gparse is not None or aparse is not None:
      print("Failed on gold {} auto {}".format(gparse, aparse))
    break

  total += 1
  for i in range(len(gparse)):
    gcounts['spine'] += 1
    gcounts['edge'] += 1
    for trace in gparse[i][2]:
      gcounts['trace'] += 1
    if len(aparse) > i:
      if aparse[i][0] == gparse[i][0]:
        matches['spine'] += 1
      if aparse[i][1] == gparse[i][1]:
        matches['edge'] += 1
      for trace in gparse[i][2]:
        if trace in aparse[i][2]:
          matches['trace'] += 1
  for i in range(len(aparse)):
    acounts['spine'] += 1
    acounts['edge'] += 1
    for trace in aparse[i][2]:
      acounts['trace'] += 1

  for i in range(len(gparse_u)):
    gcounts_u['edge'] += 1
    for trace in gparse_u[i][2]:
      gcounts_u['trace'] += 1
    if len(aparse_u) > i:
      if aparse_u[i][1] == gparse_u[i][1]:
        matches_u['edge'] += 1
      for trace in gparse_u[i][2]:
        if trace in aparse_u[i][2]:
          matches_u['trace'] += 1
  for i in range(len(aparse_u)):
    acounts_u['edge'] += 1
    for trace in aparse_u[i][2]:
      acounts_u['trace'] += 1

results('labeled', gcounts, acounts, matches)
results('unlabeled', gcounts_u, acounts_u, matches_u)
print("Processed", total)
