#!/usr/bin/env python

import string

from pstree import *

# TODO: Handle malformed input with trees that have random stuff instead of symbols
# For chinese I found:
### leaf nodes split across lines:
###     (blah
###        ))
### lone tags:
###       CP (IP...

# At the moment the generator can't handle blank line indicating a
# failed parse.

ptb_nonterminals = set(['S', 'SBAR', 'SBARQ', 'SINV', 'SQ', 'ADJP', 'ADVP',
'CONJP', 'FRAG', 'INTJ', 'LST', 'NAC', 'NP', 'NX', 'PP', 'PRN', 'PRT', 'QP',
'RRC', 'UCP', 'VP', 'WHADJP', 'WHADVP', 'WHNP', 'WHPP', 'X', 'NML'])
ontonotes_nonterminals = set(['ADJP', 'ADP', 'ADVP', 'CLP', 'CONJP', 'CP',
'DFL', 'DNP', 'DP', 'DVP', 'EDITED', 'FLR', 'FRAG', 'INC', 'INTJ', 'IP', 'LCP',
'LST', 'META', 'NAC', 'NML', 'NP', 'NX', 'PP', 'PRN', 'PRT', 'QP', 'RRC', 'S',
'SBAR', 'SBARQ', 'SINV', 'SQ', 'TOP', 'UCP', 'VCD', 'VCP', 'VNV', 'VP', 'VPT',
'VRD', 'VSB', 'WHADJP', 'WHADVP', 'WHNP', 'WHPP'])
ontonores_pos = set(["$", "''", ",", ":", "ABBREV", "AD", "ADD", "ADJ", "ADV",
"AFX", "AS", "BA", "CC", "CD", "CODE", "CONJ", "CONNEC", "CS", "CV", "CVSUFF",
"DEC", "DEG", "DEM", "DER", "DET", "DEV", "DIALECT", "DT", "EMPHATIC", "ETC",
"EX", "FOCUS", "FOREIGN", "FUT", "FW", "GRAMMAR", "HYPH", "IJ", "IN", "INTERJ",
"INTERROG", "IV", "IV1P", "IV1S", "IV2D", "IV2FP", "IV2FS", "IV2MP", "IV2MS",
"IV3FD", "IV3FP", "IV3FS", "IV3MD", "IV3MP", "IV3MS", "IVSUFF", "JJ", "JJR",
"JJS", "JUS", "LATIN", "LB", "LC", "LS", "M", "MD", "MSP", "NEG", "NFP", "NN",
"NNP", "NNPS", "NNS", "NOUN", "NR", "NT", "OD", "ON", "P", "PART", "PDT", "PN",
"POS", "POSS", "PREP", "PRON", "PRP", "PRP$", "PSEUDO", "PU", "PUNC", "PV",
"PVSUFF", "RB", "RBR", "RBS", "RC", "REL", "RESTRIC", "RP", "SB", "SP", "SUB",
"SYM", "TO", "TYPO", "UH", "URL", "VA", "VB", "VBD", "VBG", "VBN", "VBP",
"VBZ", "VC", "VE", "VERB", "VOC", "VV", "WDT", "WP", "WP$", "WRB", "X", "XX",
"``"])

# TODO: Consider extending this to other bracket types too
word_to_word_mapping = {
  '{': '-LCB-',
  '}': '-RCB-'
}
word_to_POS_mapping = {
  '--': ':',
  '-': ':',
  ';': ':',
  ':': ':',
  '-LRB-': '-LRB-',
  '-RRB-': '-RRB-',
  '-LCB-': '-LRB-',
  '-RCB-': '-RRB-',
  '{': '-LRB-',
  '}': '-RRB-',
}
bugfix_word_to_POS = {
  'Wa': 'NNP'
}
def ptb_cleaning(tree, in_place=True):
  '''Clean up some bugs/odd things in the PTB, and standardise punctuation.'''
  if not in_place:
    tree = tree.clone()
  for node in tree:
    # In a small number of cases multiple POS tags were assigned
    if '|' in node.label:
      if 'ADVP' in node.label:
        node.label = 'ADVP'
      else:
        node.label = node.label.split('|')[0]
    # Fix some issues with variation in output, and one error in the treebank
    # for a word with a punctuation POS
    # TODO: Look into the POS replacement leading to incorrect tagging for some
    # punctuation
    if node.word in word_to_word_mapping:
      node.word = word_to_word_mapping[node.word]
    if node.word in word_to_POS_mapping:
      node.label = word_to_POS_mapping[node.word]
    if node.word in bugfix_word_to_POS:
      node.label = bugfix_word_to_POS[node.word]
  return tree

def remove_trivial_unaries(tree, in_place=True):
  '''Collapse A-over-A unary productions.

  >>> tree = tree_from_text("(ROOT (S (S (PP (PP (PP (IN By) (NP (CD 1997))))))))")
  >>> otree = remove_trivial_unaries(tree, False)
  >>> print otree
  (ROOT (S (PP (IN By) (NP (CD 1997)))))
  >>> print tree
  (ROOT (S (S (PP (PP (PP (IN By) (NP (CD 1997))))))))
  >>> remove_trivial_unaries(tree)
  (ROOT (S (PP (IN By) (NP (CD 1997)))))
  '''
  if in_place:
    if len(tree.subtrees) == 1 and tree.label == tree.subtrees[0].label:
      tree.subtrees = tree.subtrees[0].subtrees
      for subtree in tree.subtrees:
        subtree.parent = tree
      remove_trivial_unaries(tree, True)
    else:
      for subtree in tree.subtrees:
        remove_trivial_unaries(subtree, True)
  else:
    if len(tree.subtrees) == 1 and tree.label == tree.subtrees[0].label:
      return remove_trivial_unaries(tree.subtrees[0], False)
    subtrees = [remove_trivial_unaries(subtree, False) for subtree in tree.subtrees]
    tree = PSTree(tree.word, tree.label, tree.span, None, subtrees)
    for subtree in subtrees:
      subtree.parent = tree
  return tree

def remove_nodes(tree, filter_func, in_place=True, preserve_subtrees=False, init_call=True, left=-1):
  if filter_func(tree) and not preserve_subtrees:
    return None
  if left == -1:
    left = tree.span[0]
  oleft = left
  subtrees = []
  for subtree in tree.subtrees:
    ans = remove_nodes(subtree, filter_func, in_place, preserve_subtrees, False, left)
    if ans is not None:
      if type(ans) == type([]):
        subtrees += ans
      else:
        subtrees.append(ans)
      left = subtrees[-1].span[1]
  if len(subtrees) == 0 and (not tree.is_terminal()):
    return None
  if filter_func(tree) and preserve_subtrees:
    if len(subtrees) == 0:
      return None
    return subtrees
  if in_place:
    tree.subtrees = subtrees
    for subtree in subtrees:
      subtree.parent = tree
  else:
    tree = PSTree(tree.word, tree.label, tree.span, None, subtrees)
  if tree.is_terminal():
    left += 1
  tree.span = (oleft, left)
  return tree

def is_possessive(parse):
  if parse.label != 'NP':
    return False
  if len(parse.subtrees) < 2:
    return False
  if parse.subtrees[-1].label == 'POS' or parse.subtrees[-1].word in {"'s", "'S"}:
    return True
  return False

def is_flat_NP(parse):
  flat_NP = parse.label.startswith('NP') or parse.label.startswith('QP')
  for subtree in parse.subtrees:
    allowed = is_possessive(subtree) or subtree.is_terminal() or subtree.label in ['QP', 'ADJP', 'ADVP', 'S', 'PRN', 'PP', 'NAC', 'SBAR', 'NX', 'CONJP']
    if not allowed:
      flat_NP = False
  return flat_NP

def insert_CC_node(parse, start, end):
  siblings = parse.subtrees[start:end]
  span = (siblings[0].span[0], siblings[-1].span[1])
  label = parse.label.split('=')[0]
  if label[-1] in string.digits:
    label = '-'.join(label.split('-')[:-1])
  nlabel = label
  if len(label) <= 2 or (not label.startswith("CC")):
    nlabel = "CC"+ label
  nnode = PSTree(None, nlabel, span, parse, siblings)
  parse.subtrees[start] = nnode
  for j in xrange(len(siblings) - 1):
    parse.subtrees.pop(start + 1)

def contains_conjunction(parse):
  seen_non_null = False
  for i, subparse in enumerate(parse.subtrees):
    if seen_non_null and subparse.is_conjunction():
      return True
    if subparse.wordspan[1] != subparse.wordspan[0] and not (parse.subtrees[i].is_conjunction() or parse.subtrees[i].is_punct()):
      seen_non_null = True
  return False

def binarise_coordination(parse, method, in_place=True):
  if method == 'k':
    if not parse.label.startswith("CC"):
      # Collins only places the conjunction and it's neighbour under the new node,
      # not a complete right branching, e.g.  from above:
      #     (UCP smaller (UCP_CC and (ADVP therefore)) (ADJP more affected))

      # Whenever a child is a CC, take it and the next child, put them under a
      # new node with a label taken from the parent

      if not is_flat_NP(parse):
        i = 1
        while i < len(parse.subtrees) - 1:
          if parse.subtrees[i].is_conjunction() and (not parse.subtrees[i+1].is_conjunction()):
            prev_punc = parse.subtrees[i-1].is_punct()
            end = i + 1
            while parse.subtrees[end].is_punct() and end < len(parse.subtrees):
              end += 1
            if end < len(parse.subtrees):
              end += 1
            next_punc = parse.subtrees[end - 1].is_punct()
            if not ((i == 1 and prev_punc) or next_punc):
              insert_CC_node(parse, i, end)
          i += 1
  elif 'j' in method and '0' not in method:
    # To check if this is a conjunction, look at the children, find a pattern of"
    #  _ , _ , ... CC / CONJP _
    # Note:
    #  - The length of items may vary e.g. 
    #     (VP baking and eating (NP cookies))
    #     (UCP smaller and (ADVP therefore) (ADJP more affected))
    #     (UCP highest quality and (ADJP most reasonably priced))
    #  - The _'s may vary in type, e.g. (UCP (ADJP big) and (VP growing))
    #  - Oxford comma (or similar uses) should be left high (to match punctuation use elsewhere)

    # My version
    # Binarise, with each part extending to include up to the next item of the same type
    #   Only 'sell' here:
    #     (VP (VB buy) (CC and) (VB sell) (NP (NNS futures) ...
    #   The PP and what follows here:
    #     (NP (NP (JJ nuclear) (NN power) ) (CC and) (PP-LOC (IN in) (NP (DT some) (NNS cases) )) (, ,) (NP ...
    #   Not inside base NPs or QPs
    #   Not for cases of a CC at the start of a phrase (ie must be a conjunction of things)
    # Opions:
    #   Should punctuation attach to the left or right?
    #   Binaries with larger chunks on the left or right?

    # 0 (A  ,  B  ,  C  and  D)  
    # 1 (A (, B (, C (and D))))
    # 2 (A , (B , (C and D)))
    # 3 (A (, B) (, C) (and D))
    # 4 ((((A ,) B ,) C and) D)
    # 5 (((A , B) , C) and D)
    # 6 ((A ,) (B ,) (C and) D)

    if not is_flat_NP(parse):
      # Check if it contains a conjunction
      done = False
      while contains_conjunction(parse) and not done:
        i = 1
        done = True
        while i < len(parse.subtrees) - 1:
          if parse.subtrees[i].is_conjunction() or parse.subtrees[i].is_punct():
            done = False
            next_conjunct = len(parse.subtrees)
            for option in range(i, len(parse.subtrees)):
              if not (parse.subtrees[option].is_conjunction() or parse.subtrees[option].is_punct()):
                next_conjunct = option
                break

            if '1' in method:
              # (A (, B (, C (and D))))
              insert_CC_node(parse, i, len(parse.subtrees))
              break
            elif '2' in method:
              # (A , (B , (C and D)))
              if next_conjunct < len(parse.subtrees) - 1:
                insert_CC_node(parse, next_conjunct, len(parse.subtrees))
                break
              done = True
            elif '3' in method:
              # (A (, B) (, C) (and D))
              j = i + 1
              # Consume any interveningg punctuation
              while j < len(parse.subtrees) and (parse.subtrees[j].is_conjunction() or parse.subtrees[j].is_punct()):
                j += 1
              # Consume all items until the next punctuation or conjunction
              while j < len(parse.subtrees) and (not (parse.subtrees[j].is_conjunction() or parse.subtrees[j].is_punct())):
                j += 1
              insert_CC_node(parse, i, j)
            elif '4' in method:
              # ((((A ,) B ,) C and) D)
              if next_conjunct < len(parse.subtrees):
                insert_CC_node(parse, 0, next_conjunct)
                break
              done = True
            elif '5' in method:
              # (((A , B) , C) and D)
              if next_conjunct < len(parse.subtrees) - 1:
                insert_CC_node(parse, 0, next_conjunct + 1)
                break
              done = True
          i += 1

        if '6' in method:
          i = len(parse.subtrees) - 1
          while i >= 0:
            if parse.subtrees[i].is_conjunction() or parse.subtrees[i].is_punct():
              # ((A ,) (B ,) (C and) D)
              j = i - 1
              # Consume any interveningg punctuation
              while j >= 0 and (parse.subtrees[j].is_conjunction() or parse.subtrees[j].is_punct()):
                j -= 1
              # Consume all items until the next punctuation or conjunction
              while j >= 0 and (not (parse.subtrees[j].is_conjunction() or parse.subtrees[j].is_punct())):
                j -= 1
              if j < 0:
                j = 0
              if abs(i - j) > 0 and (i != len(parse.subtrees) - 1 or j != 0):
                insert_CC_node(parse, j, i + 1)
              i = j
            i -= 1
          done = True

  if in_place:
    for subtree in parse.subtrees:
      binarise_coordination(subtree, method, in_place)
  else:
    raise Exception

def redirect_gapping(parse):
  traces = resolve_traces(parse)
  next_num = 1
  for num in traces[0]:
    if int(num) >= next_num:
      next_num = int(num) + 1

  for num in traces[1]:
    # Change the goal
    if num in traces[0]:
      target, over_null = traces[0][num]
      if num not in traces[2]:
        target.label = ''.join(target.label.split('-{}'.format(num)))

      ntarget = target.parent
      new_num = get_reference(ntarget.label)
      if new_num is None:
        if num in traces[2]:
          new_num = next_num
          next_num += 1
        else:
          new_num = num
        ntarget.label += "-{}".format(new_num)

      if new_num != num:
        for node in traces[1][num]:
          # Change all to point to new location
          core = ''.join(node.label.split('={}'.format(num)))
          node.label = core + "={}".format(new_num)

def follow_chain_in_mapping(mapping, num, trace_group=2):
  if num not in mapping[0]:
    return (None, None)
  ref = mapping[0][num]
  chained = False
###  if trace_group == 2:
###    while ref[1]:
###      ref_num = None
###      for child in ref[0].subtrees:
###        if child.label == TRACE_LABEL:
###          ref_num = get_reference(child.word)
###          if ref_num is not None:
###            break
###      if ref_num is None or ref_num not in mapping[0]:
###        break
###      ref = mapping[0][ref_num]
###      chained = True
  return (chained, ref[0])

def resolve_traces(node, mapping=None):
  if mapping is None:
    mapping = (
      {}, # num from [NP]-num mapping to the parse node
      {}, # num from [NP]=num mapping to a list of parse nodes
      {}, # num from (-NONE- [*]-num) to a list of parse nodes
      [], # list of parse nodes like (-NONE- [no num])
    )

  # Recurse
  for subnode in node.subtrees:
    resolve_traces(subnode, mapping)

  plabel = node.label

  # 0 - num from [NP]-num mapping to the parse node
  num = get_reference(plabel, '-')
  if num is not None:
    over_null = False
    if node.wordspan[0] == node.wordspan[1]:
      over_null = True
    mapping[0][num] = (node, over_null)

  # 1 - num from [NP]=num mapping to a list of parse nodes
  num = get_reference(plabel, '=')
  if num is not None:
    if num not in mapping[1]:
      mapping[1][num] = []
    mapping[1][num].append(node)

  # 2 - num from (-NONE- [*]-num) to a list of parse nodes, e.g. inner node of:
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

def remove_traces(tree, in_place=True):
  '''Adjust the tree to remove traces.

  >>> tree = tree_from_text("(ROOT (S (PP (IN By) (NP (CD 1997))) (, ,) (NP (NP (ADJP (RB almost) (DT all)) (VBG remaining) (NNS uses)) (PP (IN of) (NP (JJ cancer-causing) (NN asbestos)))) (VP (MD will) (VP (VB be) (VP (VBN outlawed) (NP (-NONE- *-6))))) (. .)))")
  >>> ntree = remove_traces(tree, False)
  >>> otree = remove_traces(tree, True)
  >>> otree == tree
  True
  >>> ntree == tree
  False
  >>> print ntree
  (ROOT (S (PP (IN By) (NP (CD 1997))) (, ,) (NP (NP (ADJP (RB almost) (DT all)) (VBG remaining) (NNS uses)) (PP (IN of) (NP (JJ cancer-causing) (NN asbestos)))) (VP (MD will) (VP (VB be) (VP (VBN outlawed)))) (. .)))
  '''
  return remove_nodes(tree, PSTree.is_trace, in_place)

def split_label_type_and_function(label):
  parts = label.split('=')
  if len(label) > 0 and label[0] != '-':
    cur = parts
    parts = []
    for part in cur:
      parts += part.split('-')
  return parts

def remove_coindexation_from_label(label):
  if len(label) > 0 and label[-1] in string.digits:
    while len(label) > 0 and label[-1] in string.digits or label[-1] in '-=':
      label = label[:-1]
  return label

def remove_coindexation(tree, in_place=True):
  '''Adjust the tree to remove coindexation info.'''
  label = remove_coindexation_from_label(tree.label)
  if in_place:
    for subtree in tree.subtrees:
      remove_coindexation(subtree, True)
    tree.label = label
  else:
    subtrees = [remove_coindexation(subtree, False) for subtree in tree.subtrees]
    tree = PSTree(tree.word, label, tree.span, None, subtrees)
    for subtree in subtrees:
      subtree.parent = tree
  return tree

def remove_function_tags(tree, in_place=True):
  '''Adjust the tree to remove function tags on labels.

  >>> tree = tree_from_text("(ROOT (S (NP-SBJ (NNP Ms.) (NNP Haag)) (VP (VBZ plays) (NP (NNP Elianti))) (. .)))")
  >>> remove_function_tags(tree, False)
  (ROOT (S (NP (NNP Ms.) (NNP Haag)) (VP (VBZ plays) (NP (NNP Elianti))) (. .)))

  # don't remove brackets
  >>> tree = tree_from_text("(ROOT (S (NP-SBJ (`` ``) (NP-TTL (NNP Funny) (NNP Business)) ('' '') (PRN (-LRB- -LRB-) (NP (NNP Soho)) (, ,) (NP (CD 228) (NNS pages)) (, ,) (NP ($ $) (CD 17.95)) (-RRB- -RRB-)) (PP (IN by) (NP (NNP Gary) (NNP Katzenstein)))) (VP (VBZ is) (NP-PRD (NP (NN anything)) (PP (RB but)))) (. .)))")
  >>> remove_function_tags(tree)
  (ROOT (S (NP (`` ``) (NP (NNP Funny) (NNP Business)) ('' '') (PRN (-LRB- -LRB-) (NP (NNP Soho)) (, ,) (NP (CD 228) (NNS pages)) (, ,) (NP ($ $) (CD 17.95)) (-RRB- -RRB-)) (PP (IN by) (NP (NNP Gary) (NNP Katzenstein)))) (VP (VBZ is) (NP (NP (NN anything)) (PP (RB but)))) (. .)))
  '''
  label = tree.label
  if len(label) > 0 and label[0] != '-':
    num = None
    to_add = []
    cur = []
    cur_all_digits = True
    for i in range(len(label) -1, -1, -1):
        cur.insert(0, label[i])
        if label[i] in '-=' and len(cur) > 0:
            if cur_all_digits:
                to_add.insert(0, ''.join(cur))
            cur_all_digits = True
            cur = []
        elif label[i] not in string.digits:
            cur_all_digits = False
    label = ''.join(cur) + ''.join(to_add)

  if in_place:
    for subtree in tree.subtrees:
      remove_function_tags(subtree, True)
    tree.label = label
  else:
    subtrees = [remove_function_tags(subtree, False) for subtree in tree.subtrees]
    tree = PSTree(tree.word, label, tree.span, None, subtrees)
    for subtree in subtrees:
      subtree.parent = tree
  return tree

# Applies rules to strip out the parts of the tree that are not used in the
# standard evalb evaluation
def apply_collins_rules(tree, in_place=True):
  '''Adjust the tree to remove parts not evaluated by the standard evalb
  config.

  # cutting punctuation and -X parts of labels
  >>> tree = tree_from_text("(ROOT (S (NP-SBJ (NNP Ms.) (NNP Haag) ) (VP (VBZ plays) (NP (NNP Elianti) )) (. .) ))")
  >>> apply_collins_rules(tree)
  (ROOT (S (NP (NNP Ms.) (NNP Haag)) (VP (VBZ plays) (NP (NNP Elianti)))))
  >>> print tree.word_yield()
  Ms. Haag plays Elianti

  # cutting nulls
  >>> tree = tree_from_text("(ROOT (S (PP-TMP (IN By) (NP (CD 1997))) (, ,) (NP-SBJ-6 (NP (ADJP (RB almost) (DT all)) (VBG remaining) (NNS uses)) (PP (IN of) (NP (JJ cancer-causing) (NN asbestos)))) (VP (MD will) (VP (VB be) (VP (VBN outlawed) (NP (-NONE- *-6))))) (. .)))")
  >>> apply_collins_rules(tree)
  (ROOT (S (PP (IN By) (NP (CD 1997))) (NP (NP (ADJP (RB almost) (DT all)) (VBG remaining) (NNS uses)) (PP (IN of) (NP (JJ cancer-causing) (NN asbestos)))) (VP (MD will) (VP (VB be) (VP (VBN outlawed))))))

  # changing PRT to ADVP
  >>> tree = tree_from_text("(ROOT (S (NP-SBJ-41 (DT That) (NN fund)) (VP (VBD was) (VP (VBN put) (NP (-NONE- *-41)) (PRT (RP together)) (PP (IN by) (NP-LGS (NP (NNP Blackstone) (NNP Group)) (, ,) (NP (DT a) (NNP New) (NNP York) (NN investment) (NN bank)))))) (. .)))")
  >>> apply_collins_rules(tree)
  (ROOT (S (NP (DT That) (NN fund)) (VP (VBD was) (VP (VBN put) (ADVP (RP together)) (PP (IN by) (NP (NP (NNP Blackstone) (NNP Group)) (NP (DT a) (NNP New) (NNP York) (NN investment) (NN bank))))))))

  # not removing brackets
  >>> tree = tree_from_text("(ROOT (S (NP-SBJ (`` ``) (NP-TTL (NNP Funny) (NNP Business)) ('' '') (PRN (-LRB- -LRB-) (NP (NNP Soho)) (, ,) (NP (CD 228) (NNS pages)) (, ,) (NP ($ $) (CD 17.95) (-NONE- *U*)) (-RRB- -RRB-)) (PP (IN by) (NP (NNP Gary) (NNP Katzenstein)))) (VP (VBZ is) (NP-PRD (NP (NN anything)) (PP (RB but) (NP (-NONE- *?*))))) (. .)))")
  >>> apply_collins_rules(tree)
  (ROOT (S (NP (NP (NNP Funny) (NNP Business)) (PRN (-LRB- -LRB-) (NP (NNP Soho)) (NP (CD 228) (NNS pages)) (NP ($ $) (CD 17.95)) (-RRB- -RRB-)) (PP (IN by) (NP (NNP Gary) (NNP Katzenstein)))) (VP (VBZ is) (NP (NP (NN anything)) (PP (RB but))))))
  '''
  tree = tree if in_place else tree.clone()
  remove_traces(tree, True)
  remove_function_tags(tree, True)
  ptb_cleaning(tree, True)

  # Remove Puncturation
### words_to_ignore = set(["'","`","''","``","--",":",";","-",",",".","...",".","?","!"])
  labels_to_ignore = ["-NONE-",",",":","``","''","."]
  remove_nodes(tree, lambda(t): t.label in labels_to_ignore, True)

  # Set all PRTs to be ADVPs
  POS_to_convert = {'PRT': 'ADVP'}
  for node in tree:
    if node.label in POS_to_convert:
      node.label = POS_to_convert[node.label]
  
  tree.calculate_spans()
  return tree

def homogenise_tree(tree, tag_set=ptb_nonterminals):
  '''Change the top of the tree to be of a consistent form.

  >>> tree = tree_from_text("( (S (NP (NNP Example))))", True)
  >>> homogenise_tree(tree)
  (ROOT (S (NP (NNP Example))))
  >>> tree = tree_from_text("( (ROOT (S (NP (NNP Example))) ) )", True)
  >>> homogenise_tree(tree)
  (ROOT (S (NP (NNP Example))))
  >>> tree = tree_from_text("(S1 (S (NP (NNP Example))))")
  >>> homogenise_tree(tree)
  (ROOT (S (NP (NNP Example))))
  '''
  orig = tree
  tree = tree.root()
  if tree.label != 'ROOT':
    while split_label_type_and_function(tree.label)[0] not in tag_set:
      if len(tree.subtrees) > 1:
        break
      elif tree.is_terminal():
        raise Exception("Tree has no labels in the tag set\n%s" % orig.__repr__())
      tree = tree.subtrees[0]
    if split_label_type_and_function(tree.label)[0] not in tag_set:
      tree.label = 'ROOT'
    else:
      root = PSTree(None, 'ROOT', tree.span, None, [])
      root.subtrees.append(tree)
      tree.parent = root
      tree = root
  return tree

def ptb_read_tree(source, return_empty=False, allow_empty_labels=False, allow_empty_words=False, blank_line_coverage=False):
  '''Read a single tree from the given PTB file.

  The function reads a character at a time, stopping as soon as a tree can be
  constructed, so multiple trees on a sinlge line are manageable.
  
  >>> from StringIO import StringIO
  >>> file_text = """(ROOT (S
  ...   (NP-SBJ (NNP Scotty) )
  ...   (VP (VBD did) (RB not)
  ...     (VP (VB go)
  ...       (ADVP (RB back) )
  ...       (PP (TO to)
  ...         (NP (NN school) ))))
  ...   (. .) ))"""
  >>> in_file = StringIO(file_text)
  >>> ptb_read_tree(in_file)
  (ROOT (S (NP-SBJ (NNP Scotty)) (VP (VBD did) (RB not) (VP (VB go) (ADVP (RB back)) (PP (TO to) (NP (NN school))))) (. .)))'''
  cur_text = ''
  depth = 0
  while True:
    char = source.read(1)
    if char == '':
      return None
    if char == '\n' and cur_text == ' ' and blank_line_coverage:
      return "Empty"
    if char in '\n\t':
      char = ' '
    cur_text += char
    if char == '(':
      depth += 1
    elif char == ')':
      depth -= 1
    if depth == 0:
      if '()' in cur_text:
        if return_empty:
          return "Empty"
        cur_text = ''
        continue
      if '(' in cur_text:
        break

  tree = tree_from_text(cur_text, allow_empty_labels, allow_empty_words)
  ptb_cleaning(tree)
  return tree

def shp_read_tree(source, return_empty=False, allow_empty_labels=False, allow_empty_words=False, blank_line_coverage=False):
  '''Read a single tree from the given file of split head grammar parses.
  
  >>> from StringIO import StringIO
  >>> file_text = """# Parse  (ROOT
  ... # Parse (ROOT
  ... # Parse   (S
  ... # Parse     (NP-SBJ-1 (DT A) (NN record) (NN date) )
  ... # Parse     (VP (VBZ has) (RB n't)
  ... # Parse       (VP (VBN been)
  ... # Parse         (VP (VBN set)
  ... # Parse           (NP (-NONE- *-1) ))))
  ... # Parse     (. .) ))
  ... # SentID 8
  ... # Sentence   A record date has n't been set .
  ... # Tokens     1 A  2 record  3 date  4 has  5 n't  6 been  7 set  8 .
  ... # Identity   1 (0, 3) NP-1 False
  ... # Reference  1 (7, 7) *-1
  ... # Graph type  proj graph no-cycle no-cycle-rev-edges no-len1-cycle has-double
  ... 1 A      DT  _                   3 NP-SBJ_0
  ... 2 record NN  _                   3 NP-SBJ_0
  ... 3 date   NN  NP-SBJ              7 S_0    7 NP_0 T NP-SBJ_0 F *
  ... 4 has    VBZ _                   7 VP_2
  ... 5 n't    RB  _                   7 VP_2
  ... 6 been   VBN _                   7 VP_1
  ... 7 set    VBN (NP_(*))_VP_VP_VP_S 0 ROOT_0
  ... 8 .      .   _                   7 S_0
  ... 
  ... """
  >>> in_file = StringIO(file_text)
  >>> shp_read_tree(in_file)
  (ROOT (S (NP-SBJ-1 (DT A) (NN record) (NN date)) (VP (VBZ has) (RB n't) (VP (VBN been) (VP (VBN set) (NP (-NONE- *-1))))) (. .)))'''
  cur_text = []
  while True:
    line = source.readline()
    if line == '':
      return None
    line = line.strip()
    if len(line) == 0:
      if len(cur_text) > 0:
        # Check for failure cases, with *_0
        is_fine = True
        for line in cur_text:
          if "*_0" in line:
            is_fine = False
        if is_fine:
          break
        else:
          return "Empty"
      else:
        continue
    if line[0] == '#':
      continue
    assert line[0] in string.digits
    cur_text.append(line)

  tree = tree_from_shp(cur_text)
  ptb_cleaning(tree)
  return tree

def conll_read_tree(source, return_empty=False, allow_empty_labels=False, allow_empty_words=False, blank_line_coverage=False):
  '''Read a single tree from the given OntoNotes data file.
  
  >>> from StringIO import StringIO
  >>> file_text = """#begin document (nw/wsj/00/wsj_0020)
  ... nw/wsj/00/wsj_0020          0          0       They        PRP (TOP(S(NP*)          -          -          -          -          * (ARG1*)          *        (0)
  ... nw/wsj/00/wsj_0020          0          1       will         MD      (VP*          -          -          -          -          * (ARGM-M OD*)          *          -
  ... nw/wsj/00/wsj_0020          0          2     remain         VB      (VP*     remain         01          1          -          *       ( V*)          *          -
  ... nw/wsj/00/wsj_0020          0          3         on         IN      (PP*          -          -          -          -          *     (AR G3*          *          -
  ... nw/wsj/00/wsj_0020          0          4          a         DT  (NP(NP*          -          -          -          -          * *     (ARG2*          -
  ... nw/wsj/00/wsj_0020          0          5      lower        JJR     (NML*          -          -          -          -          * *          *          -
  ... nw/wsj/00/wsj_0020          0          6          -       HYPH          *          -          -          -          -          * *          *          -
  ... nw/wsj/00/wsj_0020          0          7   priority         NN         *)          -          -          -          -          * *          *          -
  ... nw/wsj/00/wsj_0020          0          8       list         NN         *)          -          -          1          -          * *         *)          -
  ... nw/wsj/00/wsj_0020          0          9       that        WDT (SBAR(WHNP*)          -          -          -          -          * *          *          -
  ... nw/wsj/00/wsj_0020          0         10   includes        VBZ   (S(VP*          -          -          1          -          * *       (V*)          -
  ... nw/wsj/00/wsj_0020          0         11         17         CD      (NP*          -          -          -          - (CARDINAL) *     (ARG1*        (10
  ... nw/wsj/00/wsj_0020          0         12      other         JJ          *          -          -          -          -          * *          *          -
  ... nw/wsj/00/wsj_0020          0         13  countries        NNS  *))))))))          -          -          3          -          * *)         *)        10)
  ... nw/wsj/00/wsj_0020          0         14          .          .        *))          -          -          -          -          * *          *          -
  ... 
  ... """
  >>> in_file = StringIO(file_text)
  >>> tree = conll_read_tree(in_file)
  >>> print tree
  (TOP (S (NP (PRP They)) (VP (MD will) (VP (VB remain) (PP (IN on) (NP (NP (DT a) (NML (JJR lower) (HYPH -) (NN priority)) (NN list)) (SBAR (WHNP (WDT that)) (S (VP (VBZ includes) (NP (CD 17) (JJ other) (NNS countries))))))))) (. .)))'''
  cur_text = []
  while True:
    line = source.readline()
    # Check if we are out of input
    if line == '':
      return None
    # strip whitespace and see if this is then end of the parse
    line = line.strip()
    if line == '':
      break
    cur_text.append(line)
  
  text = ''
  for line in cur_text:
    if len(line) == 0 or line[0] == '#':
      continue
    line = line.split()
    word = line[3]
    pos = line[4]
    tree = line[5]
    tree = ' ('.join(tree.split('(')).split('*')
    text += '%s(%s %s)%s' % (tree[0], pos, word, tree[1])
  return tree_from_text(text)

def generate_trees(source, tree_reader=ptb_read_tree, max_sents=-1, return_empty=False, allow_empty_labels=False, allow_empty_words=False):
  '''Read trees from the given file (opening the file if only a string is given).
  
  >>> from StringIO import StringIO
  >>> file_text = """(ROOT (S
  ...   (NP-SBJ (NNP Scotty) )
  ...   (VP (VBD did) (RB not)
  ...     (VP (VB go)
  ...       (ADVP (RB back) )
  ...       (PP (TO to)
  ...         (NP (NN school) ))))
  ...   (. .) ))
  ...
  ... (ROOT (S 
  ...     (NP-SBJ (DT The) (NN bandit) )
  ...     (VP (VBZ laughs) 
  ...       (PP (IN in) 
  ...         (NP (PRP$ his) (NN face) )))
  ...     (. .) ))"""
  >>> in_file = StringIO(file_text)
  >>> for tree in generate_trees(in_file):
  ...   print tree
  (ROOT (S (NP-SBJ (NNP Scotty)) (VP (VBD did) (RB not) (VP (VB go) (ADVP (RB back)) (PP (TO to) (NP (NN school))))) (. .)))
  (ROOT (S (NP-SBJ (DT The) (NN bandit)) (VP (VBZ laughs) (PP (IN in) (NP (PRP$ his) (NN face)))) (. .)))'''
  if type(source) == type(''):
    source = open(source)
  count = 0
  while True:
    tree = tree_reader(source, return_empty, allow_empty_labels, allow_empty_words)
    if tree == "Empty":
      yield None
      continue
    if tree is None:
      return
    tree.set_unique_id()
    yield tree
    count += 1
    if count >= max_sents > 0:
      return

def read_trees(source, tree_reader=ptb_read_tree, max_sents=-1, return_empty=False):
  return [tree for tree in generate_trees(source, tree_reader, max_sents, return_empty)]

if __name__ == '__main__':
  print "Running doctest"
  import doctest
  doctest.testmod()

