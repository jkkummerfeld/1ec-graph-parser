#!/usr/bin/env python

import sys
from collections import defaultdict
import heapq

marginals = [[[]]]
triplet_map = []
text = []
marginals_file = sys.argv[1]
triplet_map_file = sys.argv[2]
text_file = sys.argv[3]

# The/DT bill/NN intends/VBZ to/TO restrict/VB the/DT RTC/NNP to/TO Treasury/NNP borrowings/NNS only/RB ,/, unless/IN the/DT agency/NN receives/VBZ specific/JJ congressional/JJ authorization/NN ./.
for line in open(text_file):
  text.append([])
  for part in line.strip().split():
    parts = part.split('/')
    word = "/".join(parts[:-1])
    pos = parts[-1]
    text[-1].append((word, pos))

for line in open(triplet_map_file):
  line = line.strip().split()
  num = int(line[0])
  triple = line[1].split("+")
  assert len(triplet_map) == num
  triplet_map.append(triple)
  # triplet is:
  # parent, head, dependant

triplet_map.append(("ROOT", "ROOT", "ROOT"))

smallest = (-1, None)
counts = []
scores = [defaultdict(lambda: [])]
for line in open(marginals_file):
  line = line.strip()
  if line == "":
    if len(counts) > 2:
      av = sum(counts)# / float(len(counts))
      if smallest[1] is None or av < smallest[0]:
        smallest = (av, marginals[-1])
    counts = []
    marginals.append([[(0, -1, 1.0)]])
    scores.append(defaultdict(lambda: []))
  else:
    fields = line.split()
    sentence = int(fields[0])
    token = int(fields[1])
    assert len(marginals) == sentence + 1
    mlist = []
    marginals[sentence].append([])
    marginals[sentence][token] = mlist
    counts.append(0)
    for i in range(2, len(fields), 3):
      head = int(fields[i])
      label = int(fields[i + 1])
      prob = float(fields[i + 2])
      mlist.append((head, label, prob))
      scores[sentence][head, token].append((label, prob))
      counts[-1] += 1

def align_text(raw, ann):
  '''Performs a uniform cost search to align annotations with raw data'''
  # States are (raw_index, ann_index) tuples
  start = (-1, -1)
  goal = (len(raw) - 1, len(ann) - 1)

  # Start with nothing aligned
  best = None
  expanded = set()
  # Search states are (cost, state, prev_search_state)
  fringe = [(0, start, None)]

  # Search
  while len(fringe) > 0:
    cur = heapq.heappop(fringe)
    if cur[1] in expanded:
      continue
    expanded.add(cur[1])
    if cur[1] == goal:
      best = cur
      break

    # Consider match
    npos = (cur[1][0] + 1, cur[1][1] + 1)
    if npos[0] < len(raw) and npos[1] < len(ann):
      cost = cur[0]
      if raw[npos[0]] == ann[npos[1]]:
        heapq.heappush(fringe, (cost, npos, cur))
    # Consider insert
    npos = (cur[1][0] + 1, cur[1][1])
    if npos[0] < len(raw):
      cost = cur[0] + 1
      heapq.heappush(fringe, (cost, npos, cur))
    # Consider skip
    npos = (cur[1][0], cur[1][1] + 1)
    if npos[1] < len(ann):
      cost = cur[0] + 1
      heapq.heappush(fringe, (cost, npos, cur))

  # Extract the points where annotations are present
  ans = []
  while best is not None:
    prev = best[2]
    if prev is None:
      break
    if prev[1][0] != best[1][0]:
      ans.append(raw[best[1][0]])
    else:
      ans.append(ann[best[1][1]])
    best = prev
  return ans[::-1]

class Chart:
  def __init__(self, sent, scores):
    self.chart = defaultdict(lambda: (0.0, None))
    # Entries are:
    # (start, end, connection, has root?)
    # 0 No connection
    # 1   --->
    # -1  <---
    self.sent = sent
    self.scores = scores
    for i in range(len(sent) - 1):
      self.chart[i, i+1, 0, False] = (1.0, None)

  def get_best_arc(self, cur, span, left_head, prob):
    left, right = span if left_head else (span[1], span[0])
    for label, lprob in self.scores[left, right]:
      nprob = lprob * prob
      if nprob > cur[0]:
        cur = (nprob, label)
    return cur

  def arcs(self, span):
    prob, source = self.chart[span[0], span[1], 0, False]
    cur = self.chart[span[0], span[1], 1, span[0] == 0]
    self.chart[span[0], span[1], 1, span[0] == 0] = self.get_best_arc(cur, span, True, prob)
    if span[0] != 0:
      cur = self.chart[span[0], span[1], -1, False]
      self.chart[span[0], span[1], -1, False] = self.get_best_arc(cur, span, False, prob)

  def update_binary_best(self, cur, left, split, right, lstate, rstate, with_root):
    left_state = self.chart[left, split, lstate, with_root]
    right_state = self.chart[split, right, rstate, False]
    prob = left_state[0] * right_state[0]
    if prob > cur[0]:
      return (prob, (split, lstate, rstate))
    else:
      return cur

  def binaries(self, span):
    best0 = self.chart[span[0], span[1], 0, False]
    best1 = self.chart[span[0], span[1], 1, False]
    best1n = self.chart[span[0], span[1], -1, False]
    best1t = self.chart[span[0], span[1], 1, True]
    for split in range(span[0] + 1, span[1]):
      if split == span[0] + 1 or split == span[1] - 1:
        best0 = self.update_binary_best(best0, span[0], split, span[1], 0, -1, False)
      best0 = self.update_binary_best(best0, span[0], split, span[1], 1, 0, False)
      best1 = self.update_binary_best(best1, span[0], split, span[1], 1, 1, False)
      best1n = self.update_binary_best(best1n, span[0], split, span[1], -1, -1, False)
      best1t = self.update_binary_best(best1t, span[0], split, span[1], 1, 1, True)
    self.chart[span[0], span[1], 0, False] = best0
    self.chart[span[0], span[1], 1, False] = best1
    self.chart[span[0], span[1], -1, False] = best1n
    self.chart[span[0], span[1], 1, True] = best1t

  def __repr__(self):
    ans = ""
    for cell in self.chart:
      ans += str(cell) +" "+ str(self.chart[cell]) +"\n"
    return ans

  def extract_parse(self, span=None, depth=0):
    if span is None:
      span = (0, len(self.sent) - 1, 1, True)
    prev = self.chart[span]
    spines = {}
    edges = {}
    if prev[1] is None:
      spines[span[0]] = []
      spines[span[1]] = []
    elif isinstance(prev[1], int):
      spines, edges = self.extract_parse((span[0], span[1], 0, False), depth + 1)
      psym, hsym, csym = triplet_map[prev[1]]
      if span[2] == 1:
        if csym != '*' and csym not in spines[span[1]]:
          spines[span[1]].append(csym)
        if hsym != '*' and hsym not in spines[span[0]]:
          spines[span[0]].append(hsym)
        if psym != '*' and psym not in spines[span[0]]:
          spines[span[0]].append(psym)
        edges[span[1]] = [span[0], psym, csym]
      elif span[2] == -1:
        if csym != '*' and csym not in spines[span[0]]:
          spines[span[0]].append(csym)
        if hsym != '*' and hsym not in spines[span[1]]:
          spines[span[1]].append(hsym)
        if psym != '*' and psym not in spines[span[1]]:
          spines[span[1]].append(psym)
        edges[span[0]] = [span[1], psym, csym]
    elif isinstance(prev[1], tuple):
      lspines, edges = self.extract_parse((span[0], prev[1][0], prev[1][1], span[3]), depth + 1)
      rspines, redges = self.extract_parse((prev[1][0], span[1], prev[1][2], False), depth + 1)
      for edge in redges:
        edges[edge] = redges[edge]
      for spine in lspines:
        if spine != prev[1][0]:
          spines[spine] = lspines[spine]
      for spine in rspines:
        if spine != prev[1][0]:
          spines[spine] = rspines[spine]
      left = lspines[prev[1][0]]
      right = rspines[prev[1][0]]
      spines[prev[1][0]] = align_text(left, right)
    return spines, edges

for sentence, score_set, words in zip(marginals, scores, text):
  # Extract optimal set of dependencies
  chart = Chart(sentence, score_set)
  for length in range(1, len(sentence)):
    for start in range(0, len(sentence)):
      end = start + length
      if end < len(sentence):
        if length > 1:
          chart.binaries((start, end))
        chart.arcs((start, end))
  spines, edges = chart.extract_parse()
  print "# Sent", " ".join([w[0] for w in words])
  for i in range(1, len(spines)):
    if i != 0:
      to_print = "* "
      if spines[i] == []:
        to_print += "_"
      else:
        to_print += "_".join(spines[i])
      if edges[i][0] == 0:
        to_print += "_ROOT | 0 _ ROOT_0 _"
      else:
        to_print += " | %s %s_0 %s_0 _" %  (edges[i][0], edges[i][1], edges[i][2])
      to_print = "".join(words[i-1][1].join(to_print.split("*")).split("_CC"))
      print "%d %s" % (i, words[i-1][0]), to_print
  print
