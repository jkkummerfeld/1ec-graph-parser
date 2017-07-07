#!/usr/bin/env python

import sys

ptb_nonterminals = set(['S', 'SBAR', 'SBARQ', 'SINV', 'SQ', 'ADJP', 'ADVP',
'CONJP', 'FRAG', 'INTJ', 'LST', 'NAC', 'NP', 'NX', 'PP', 'PRN', 'PRT', 'QP',
'RRC', 'UCP', 'VP', 'WHADJP', 'WHADVP', 'WHNP', 'WHPP', 'X', 'NML', "ADJP_CC",
"ADVP_CC", "FRAG_CC", "INTJ_CC", "NA_CCC", "NP_CC", "NX_CC", "PP_CC", "PRN_CC",
"PRT_CC", "QP_CC", "RR_CCC", "SBAR_CC", "SBARQ_CC", "S_CC", "SINV_CC", "SQ_CC",
"UCP_CC", "VP_CC", "WHADJP_CC", "WHADVP_CC", "WHNP_CC", "X_CC", "RRC_CC"])

def convCC(label):
  if label.endswith('CC'):
    return label[:-2] + '_CC'
  else:
    return label

def process(lines):
  for fields in lines:
    src = convCC(fields[5].split('_')[0])
    if src not in ptb_nonterminals:
      src = "*"
    target = convCC(fields[4].split('_')[0])
    prev = '*'
    count = 0
    target_count = int(fields[4].split('_')[-1])
    for symbol in lines[int(fields[3])-1][-1].split("+"):
      if symbol == target:
        count += 1
        if count > target_count:
          break
      prev = symbol
    if target == 'ROOT':
      prev = 'ROOT'
###    print "%-3s %-20s %-7s %-3s %-15s %s" % (fields[0], fields[1], fields[2], fields[3], target +"+"+ prev +"+"+ src, fields[-1])
    print "%-3s %-20s %-7s %-3s %s" % (fields[0], fields[1], fields[2], fields[3], target +"+"+ prev +"+"+ src)

cur = []
for line in sys.stdin:
  if len(line.strip()) == 0:
    if len(cur) > 0:
      process(cur)
    cur = []
    print
  elif line[0] != '#':
    fields = line.strip().split()
    num = fields[0]
    word = fields[1]
    pos = fields[2]
    spine = fields[3]
    if spine == "_":
      spine = "*"
    else:
      spine = "*+"+ '+'.join([convCC(l) for l in spine.split("_")])
    target = fields[5]
    tsym = fields[6]
    ssym = fields[7]
    cur.append([num, word, pos, target, tsym, ssym, spine])
###  else:
###    print line.strip()
