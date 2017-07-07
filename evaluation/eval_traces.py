#!/usr/bin/env python

from __future__ import print_function

import sys, string

from nlp_util import pstree, nlp_eval, treebanks, parse_errors, init, render_tree

options = {
  # 'option_word': ((valid options or type), default, "Long description"),
  "config_file": [str, None, # TODO
    "A file containing option settings.  If options other than this are"
    "specified, they will override settings in the file"],
  # Input
  "gold": [str, "-", # TODO - part
    "The file containing gold trees, if '-', stdin is used"],
  "test": [str, "-", # TODO - part
    "The file containing system produced trees, if '-', stdin is used"],
  "gold_input": [('ptb', 'ontonotes'), 'ptb',
    "Input format for the gold file: PTB (single or multiple lines per parse),"
    "OntoNotes (one file in all cases)"],
  "test_input": [('ptb', 'ontonotes'), 'ptb',
    "Input format for the test file: PTB (single or multiple lines per parse),"
    "OntoNotes (one file in all cases)"],
  # Scoring modification
  "labelled_score": [bool, True, # TODO
    "Labeled or unlabelled score"],
  "include_POS_in_score": [bool, False,
    "Include POS tags in overall score"],
  "include_unparsed_in_score": [bool, True,
    "Include missed sentences in overall score"],
  "averaging": [('macro', 'micro'), 'macro', # TODO
    "How to calculate the overall scores, with a macro average (score for sums"
    "of counts) or micro average (average of scores for each count)"],
  "null-only": [bool, "--nulls" in sys.argv,
    "Whether to only score the null itself"],
  # Tree modification
  "remove_trivial_unaries": [bool, False,
    "Remove unaries that go from a label to iself,"
    "e.g. (NP (NP (NNP it))) has one"],
  "remove_function_labels": [bool, True,
    "Remove function labels, e.g. NP-TMP, remove the -TMP part"],
  "homogenise_top_label": [bool, True,
    "Homogenise the top labels, so all are ROOT"],
  "labels_to_remove": [[str],
###    ["TOP", "ROOT", "S1"],
    ['#', "''", ",", ".", ":", "``"],
    "Remove nodes with the given labels, keep subtrees, but remove"
    "parents that now have a span of size 0"],
  "words_to_remove": [[str], [],
    "Remove nodes with the given words, and do as for labels"],
  "equivalent_labels": [[(str, str)], [("ADVP", "PRT")],
    "Labels to treat as equivalent"],
  "equivalent_words": [[(str, str)], [],
    "Words to treat as equivalent"],
}

def get_reference(text, sep='-'):
  parts = []
  cur = []
  for i in range(len(text) -1, -1, -1):
      cur.insert(0, text[i])
      if text[i] in '-=' and len(cur) > 0:
          parts.append(''.join(cur))
          cur = []
  if text == '0':
    return None
  for part in parts:
    if len(part) > 0 and part[0] == sep:
      return part[1:]
  return None

def get_traces(node, mapping=None):
  if mapping is None:
    mapping = (
      {}, # num from [NP]-num mapping to the parse node
      {}, # num from [NP]=num mapping to a list of parse nodes
      {}, # num from (-NONE- [*]-num) to the parse node
      [], # list of parse nodes like (-NONE- [no num])
    )

  # Recurse
  for subnode in node.subtrees:
    get_traces(subnode, mapping)

  plabel = node.label

  # 0 - num from [NP]-num mapping to the parse node
  if '-' in plabel and get_reference(plabel) is not None:
    num = get_reference(plabel)
    over_null = False
    if node.wordspan[0] == node.wordspan[1]:
      over_null = True
    mapping[0][num] = (node, over_null)

  # 1 - num from [NP]=num mapping to a list of parse nodes
  if '=' in plabel and get_reference(plabel, '=') is not None:
    num = get_reference(plabel, '=')
    if num not in mapping[1]:
      mapping[1][num] = []
    mapping[1][num].append(node)

  # 2 - num from (-NONE- [*]-num) to the parse node, e.g. inner node of:
  #       (NP (-NONE- *-1))
  #       (NP-SBJ-3 (-NONE- *T*-5))
  if plabel == '-NONE-' and get_reference(node.word) is not None:
    num = get_reference(node.word)
    if num not in mapping[2]:
      mapping[2][num] = []
    mapping[2][num].append(node)

  # 3 - list of parse nodes like (-NONE- [no num]), e.g. inner node of:
  #        (WHNP (-NONE- *))
  #        (WHADVP-1 (-NONE- 0))
  if plabel == '-NONE-' and get_reference(node.word) is None:
    mapping[3].append(node)

  return mapping

def get_nonzero_span(node):
  if node.wordspan[0] == node.wordspan[1]:
    return get_nonzero_span(node.parent)
  else:
    return node.wordspan

def mapping_to_items(mapping):
  # 0 - num from [NP]-num mapping to the parse node
  # 1 - num from [NP]=num mapping to a list of parse nodes
  # 2 - num from (-NONE- [*]-num) to the parse node
  # 3 - list of parse nodes like (-NONE- [no num])

  items = []

  #TODO: Don't rely on provided position, get the left end of the next non-null

  # Add items without coindexation
  for node in mapping[3]:
    label = ""
    if node.parent.wordspan[0] == node.parent.wordspan[1]:
      label = node.parent.label.split('-')[0]
    items.append((
      'no-link_'+ node.word.split('-')[0] +"_"+ label,
      get_nonzero_span(node)[0]
    ))

  # Add items with gapping
  if not options['null-only'][1]:
    for num in mapping[1]:
      ref = mapping[0][num][0]
      for node in mapping[1][num]:
        items.append((
          "gap",
          get_nonzero_span(node),
          node.label.split('=')[0],
          get_nonzero_span(ref),
          ref.label.split('-')[0]
        ))

  # Add items with coindexation
  for num in mapping[2]:
    for node in mapping[2][num]:
      ref = mapping[0][num]
###      while ref[1]:
###        child = ref[0].subtrees[0]
###        ref_num = get_reference(child.word)
###        if ref_num is None:
###          break
###        ref = mapping[0][ref_num]
      ref = ref[0]

      if options['null-only'][1]:
        label = ""
        if node.parent.wordspan[0] == node.parent.wordspan[1]:
          label = node.parent.label.split('-')[0]
        items.append((
          'no-link_'+ node.word.split('-')[0] +"_"+ label,
          get_nonzero_span(node)[0]
        ))
      else:
        items.append((
          node.word.split('-')[0] +"_"+ ref.label.split('-')[0] +"_"+ node.parent.label.split("-")[0],
          get_nonzero_span(node)[0],
          get_nonzero_span(ref)
        ))

  return items


# Provide current execution info
out = sys.stdout
init.header(sys.argv, out)


# Handle options
test_in = None
gold_in = None
options['gold'][1] = sys.argv[1]
options['test'][1] = sys.argv[2]

# Print list of options in use
for option in options:
  print("# {: <28} : {}".format(option, str(options[option][1])))

# Set up reading
test_in = open(options['test'][1])
test_tree_reader = treebanks.ptb_read_tree
if options["test_input"][1] == 'ontonotes':
  test_tree_reader = treebanks.conll_read_tree

gold_in = open(options['gold'][1])
gold_tree_reader = treebanks.ptb_read_tree
if options["gold_input"][1] == 'ontonotes':
  gold_tree_reader = treebanks.conll_read_tree

# Process sentences
scores = []
sent_id = 0
match_by_type = {}
gold_by_type = {}
test_by_type = {}
while True:
  sent_id += 1

  # Read trees
  test_tree = test_tree_reader(test_in, True, True, True, True)
  gold_tree = gold_tree_reader(gold_in, True, True, True, True)
  if test_tree is None or gold_tree is None:
    break
  if test_tree == "Empty":
    test_tree = None

  # Coverage error
  gwords = len(gold_tree.word_yield().split())
  if test_tree is None:
    print("Gold", gold_tree)
    print("Test", "none")
    gold_traces = get_traces(gold_tree)
    gold_items = mapping_to_items(gold_traces)
    print(0, 0, len(gold_items))
    continue

  # Modify as per options
  if options["remove_function_labels"][1]:
    treebanks.remove_function_tags(test_tree)
    treebanks.remove_function_tags(gold_tree)
###  if options["homogenise_top_label"][1]:
###    test_tree = treebanks.homogenise_tree(test_tree)
###    gold_tree = treebanks.homogenise_tree(gold_tree)
  if len(options['labels_to_remove'][1]) > 0:
    treebanks.remove_nodes(test_tree, lambda(n): n.label in options['labels_to_remove'][1], True, True)
    treebanks.remove_nodes(gold_tree, lambda(n): n.label in options['labels_to_remove'][1], True, True)
###  if len(options['words_to_remove'][1]) > 0:
###    treebanks.remove_nodes(test_tree, lambda(n): n.word in options['words_to_remove'][1], True, True)
###    treebanks.remove_nodes(gold_tree, lambda(n): n.word in options['words_to_remove'][1], True, True)
  if len(options['equivalent_labels'][1]) > 0:
    for tree in [gold_tree, test_tree]:
      for node in tree:
        for pair in options['equivalent_labels'][1]:
          if node.label in pair:
            node.label = pair[0]
  if len(options['equivalent_words'][1]) > 0:
    for tree in [gold_tree, test_tree]:
      for node in tree:
        for pair in options['equivalent_words'][1]:
          if node.word in pair:
            node.word = pair[0]
###  if options['remove_trivial_unaries'][1]:
###    treebanks.remove_trivial_unaries(test_tree)
###    treebanks.remove_trivial_unaries(gold_tree)

  gold_tree.calculate_spans()
  test_tree.calculate_spans()

  print("Gold", gold_tree)
  print("Test", test_tree)

  # Score and report
  gold_traces = get_traces(gold_tree)
  gold_items = mapping_to_items(gold_traces)

  test_traces = get_traces(test_tree)
  print(test_traces)
  test_items = mapping_to_items(test_traces)

###  print("Comparison:", render_tree.text_coloured_errors(test_tree, gold_tree))

  match = 0
  to_print = []
  has_items = (len(gold_items) + len(test_items) ) > 0
  all_match = True
  for item in gold_items:
    if item in test_items:
      match += 1
      to_print.append("  Match "+ str(item))
      if item[0] not in match_by_type:
        match_by_type[item[0]] = 0
      match_by_type[item[0]] += 1
  for item in gold_items:
    if item[0] not in gold_by_type:
      gold_by_type[item[0]] = 0
    gold_by_type[item[0]] += 1
    if item not in test_items:
      to_print.append("  Missing gold " + str(item))
      all_match = False
  for item in test_items:
    if item[0] not in test_by_type:
      test_by_type[item[0]] = 0
    test_by_type[item[0]] += 1
    if item not in gold_items:
      to_print.append("  Extra test "+ str(item))
      all_match = False
###  if not all_match:
###  if has_items:
###    print("Gold", render_tree.text_tree(gold_tree, False, True))
###    print("Test", render_tree.text_tree(test_tree, False, True))
###  else:
###    print("Gold", gold_tree)
###    print("Test", test_tree)
  print("\n".join(to_print))
  print(match, len(test_items), len(gold_items))

done = set()
total_match = 0
total_gold = 0
total_test = 0
to_print = []
for sym in match_by_type:
  done.add(sym)
  match = match_by_type[sym]
  gold = 0
  test = 0
  if sym in gold_by_type:
    gold = gold_by_type[sym]
  if sym in test_by_type:
    test = test_by_type[sym]
  to_print.append((gold, sym, match, gold, test, 100.0 * match / test, 100.0 * match / gold))
  total_match += match
  total_gold += gold
  total_test += test
for sym in gold_by_type:
  if sym not in done:
    done.add(sym)
    gold = gold_by_type[sym]
    test = 0
    if sym in test_by_type:
      test = test_by_type[sym]
    to_print.append((gold, sym, 0, gold, test, 0.0, 0.0))
    total_gold += gold
    total_test += test
for sym in test_by_type:
  if sym not in done:
    done.add(sym)
    if sym != "empty*T*":
      test = test_by_type[sym]
      to_print.append((0, sym, 0, 0, test, 0.0, 0.0))
      total_test += test

if total_gold > 0 and total_test > 0:
    to_print.append((total_gold, "all", total_match, total_gold, total_test, total_match * 100.0 / total_test, total_match * 100.0 / total_gold))

to_print.sort()
for gold, sym, match, gold, test, precision, recall in to_print:
  f = 0
  if precision + recall > 0:
      f = 2 * precision * recall / (precision + recall)
  print(sym, match, gold, test, precision, recall, f)
