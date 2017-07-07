#!/usr/bin/env python

import sys, re

import treebanks

#TODO: Handle other langauges

log = True
log = False

jkk_mapping_table = {
  "ADJP": ("left", "NNS QP NN $ ADVP JJ VBN VBG ADJP JJR NP JJS DT FW RBR RBS SBAR RB".split()),
  "ADVP": ("right", "RB RBR RBS FW ADVP TO CD JJR JJ IN NP JJS NN".split()),
  "CONJP": ("right", "CC RB IN".split()),
  "FRAG": ("right", []),
  "INTJ": ("left", []),
  "LST": ("right", "LS :".split()),
  "NAC": ("left", "NN NNS NNP NNPS NP NAC EX $ CD QP PRP VBG JJ JJS JJR ADJP FW".split()),
  "NP": ("left", "POS NN NNP NNPS NNS NX JJR CD JJ JJS RB QP NP".split()),
  "NX": ("right", "POS NN NNP NNPS NNS NX JJR CD JJ JJS RB QP NP".split()),
  "PP": ("right", "IN TO VBG VBN RP FW".split()),
###  "PRN": ("left", []), # pre March 2
  "PRN": ("left", "S VP".split()), # March 2b
###  "PRN": ("left", ["S", "VP", re.compile('^[A-Z]')]), # post thesis
  "PRT": ("right", "RP"),
  "QP": ("left", "$ IN NNS NN JJ RB DT CD NCD QP JJR JJS".split()),
  "RRC": ("right", "VP NP ADVP ADJP PP".split()),
  "S": ("left", "TO IN PRN VP S SBAR ADJP UCP NP ADVP SINV".split()),
  "SBAR": ("left", "S WHNP WHPP WHADVP WHADJP IN DT SQ SINV SBAR FRAG".split()),
  "SBARQ": ("left", "SQ S SINV SBARQ FRAG".split()),
  "SINV": ("left", "VBZ VBD VBP VB MD VP S SINV ADJP NP".split()),
###  "SQ": ("left", "VBZ VBD VBP VB MD VP SQ".split()), # pre March 2
  "SQ": ("left", "VP VBZ VBD VBP VB MD SQ".split()), # March 2b
  "UCP": ("right", []),
###  "VP": ("left", "VP MD VB VBG VBP VBD VBN VBZ TO ADJP NN NNS NP IN".split()), # pre March 2
###  "VP": ("left", "VP VBZ S MD VB VBG VBP VBD VBN TO ADJP NN NNS NP IN".split()), # March 2b
  "VP": ("left", "VP VBZ VB VBG VBP VBD VBN MD S aux TO ADJP NN NNS NP IN".split()), # March 23
  "WHADJP": ("left", "CC WRB JJ ADJP".split()),
  "WHADVP": ("right", "CC WRB".split()),
###  "WHNP": ("left", "WDT WP WP$ WHADJP WHPP WHNP".split()), # pre March 2
  "WHNP": ("left", "WDT WP WP$ WHADJP WHPP S WHNP".split()), # March 2b
  "WHPP": ("right", "IN TO FW".split()),
  "X": ("right", []),
  'META': ('right', []),
  "CCADJP": ("left", ['CC', 'CONJP']),
  "CCADVP": ("left", ['CC', 'CONJP']),
  "CCFRAG": ("left", ['CC', 'CONJP']),
  "CCINTJ": ("left", ['CC', 'CONJP']),
  "CCNAC": ("left", ['CC', 'CONJP']),
  "CCNP": ("left", ['CC', 'CONJP']),
  "CCNX": ("left", ['CC', 'CONJP']),
  "CCPP": ("left", ['CC', 'CONJP']),
  "CCPRN": ("left", ['CC', 'CONJP']),
  "CCPRT": ("left", ['CC', 'CONJP']),
  "CCQP": ("left", ['CC', 'CONJP']),
  "CCRRC": ("left", ['CC', 'CONJP']),
  "CCSBAR": ("left", ['CC', 'CONJP']),
  "CCSBARQ": ("left", ['CC', 'CONJP']),
  "CCS": ("left", ['CC', 'CONJP']),
  "CCSINV": ("left", ['CC', 'CONJP']),
  "CCSQ": ("left", ['CC', 'CONJP']),
  "CCUCP": ("left", ['CC', 'CONJP']),
  "CCVP": ("left", ['CC', 'CONJP']),
  "CCWHADJP": ("left", ['CC', 'CONJP']),
  "CCWHADVP": ("left", ['CC', 'CONJP']),
  "CCWHNP": ("left", ['CC', 'CONJP']),
  "CCX": ("left", ['CC', 'CONJP']),
}
special_cases = {
  "aux": {"'d", "be", "am", "are", "'s", "is", "was", "were", "being", "been", "can", "could", "do", "does", "did", "have", "has", "had", "having", "may", "might", "must", "need", "ought", "shall", "should", "will", "would"}
}


# Collins 2008 (from script provided by xavier)
collins_mapping_table = {
  "ADJP": ("left", "NNS QP NN $ ADVP JJ VBN VBG ADJP JJR NP JJS DT FW RBR RBS SBAR RB".split()),
  "ADVP": ("right", "RB RBR RBS FW ADVP TO CD JJR JJ IN NP JJS NN".split()),
  "CONJP": ("right", "CC RB IN".split()),
  "FRAG": ("right", []),
  "INTJ": ("left", []),
  "LST": ("right", "LS :".split()),
  "NAC": ("left", "NN NNS NNP NNPS NP NAC EX $ CD QP PRP VBG JJ JJS JJR ADJP FW".split()),
  "NP": ("left", "POS NN NNP NNPS NNS NX JJR CD JJ JJS RB QP NP".split()),
  "NX": ("right", "POS NN NNP NNPS NNS NX JJR CD JJ JJS RB QP NP".split()),
  "PP": ("right", "IN TO VBG VBN RP FW".split()),
  "PRN": ("left", []),
  "PRT": ("right", "RP"),
  "QP": ("left", "$ IN NNS NN JJ RB DT CD NCD QP JJR JJS".split()),
  "RRC": ("right", "VP NP ADVP ADJP PP".split()),
  "S": ("left", "TO IN VP S SBAR ADJP UCP NP ADVP SINV".split()),
  "SBAR": ("left", "WHNP WHPP WHADVP WHADJP IN DT S SQ SINV SBAR FRAG".split()),
  "SBARQ": ("left", "SQ S SINV SBARQ FRAG".split()),
  "SINV": ("left", "VBZ VBD VBP VB MD VP S SINV ADJP NP".split()),
  "SQ": ("left", "VBZ VBD VBP VB MD VP SQ".split()),
  "UCP": ("right", []),
  "VP": ("left", "TO VBD VBN MD VBZ VB VBG VBP VP ADJP NN NNS NP IN".split()),
  "WHADJP": ("left", "CC WRB JJ ADJP".split()),
  "WHADVP": ("right", "CC WRB".split()),
  "WHNP": ("left", "WDT WP WP$ WHADJP WHPP WHNP".split()),
  "WHPP": ("right", "IN TO FW".split()),
  "X": ("right", []),
  # My additions:
  'META': ('right', []),
  "CCADJP": ("left", ['CC', 'CONJP']),
  "CCADVP": ("left", ['CC', 'CONJP']),
  "CCFRAG": ("left", ['CC', 'CONJP']),
  "CCINTJ": ("left", ['CC', 'CONJP']),
  "CCNAC": ("left", ['CC', 'CONJP']),
  "CCNP": ("left", ['CC', 'CONJP']),
  "CCNX": ("left", ['CC', 'CONJP']),
  "CCPP": ("left", ['CC', 'CONJP']),
  "CCPRN": ("left", ['CC', 'CONJP']),
  "CCPRT": ("left", ['CC', 'CONJP']),
  "CCQP": ("left", ['CC', 'CONJP']),
  "CCRRC": ("left", ['CC', 'CONJP']),
  "CCSBAR": ("left", ['CC', 'CONJP']),
  "CCSBARQ": ("left", ['CC', 'CONJP']),
  "CCS": ("left", ['CC', 'CONJP']),
  "CCSINV": ("left", ['CC', 'CONJP']),
  "CCSQ": ("left", ['CC', 'CONJP']),
  "CCUCP": ("left", ['CC', 'CONJP']),
  "CCVP": ("left", ['CC', 'CONJP']),
  "CCWHADJP": ("left", ['CC', 'CONJP']),
  "CCWHADVP": ("left", ['CC', 'CONJP']),
  "CCWHNP": ("left", ['CC', 'CONJP']),
  "CCX": ("left", ['CC', 'CONJP']),
}

# Based on Johansson and Nugues
non_punct_re = re.compile('[A-Z].*')
pennconverter_mapping_table = {
  'ADJP': ( 'right', ['NNS', 'QP', 'NN', '$', 'ADVP', 'JJ', 'VBN', 'VBG', 'ADJP', 'JJR', 'NP', 'JJS', 'DT', 'FW', 'RBR', 'RBS', 'SBAR', 'RB']),
  'ADVP': ( 'left', ['RB', 'RBR', 'RBS', 'FW', 'ADVP', 'TO', 'CD', 'JJR', 'JJ', 'IN', 'NP', 'JJS', 'NN']),
  'CONJP': ( 'left', ['CC', 'RB', 'IN']),
  'FRAG': ( 'left', [re.compile('(NN.*)|(NP)'), re.compile('W.*'), 'SBAR', re.compile('(PP)|(IN)'), re.compile('(ADJP)|(JJ)'), 'ADVP', 'RB']),
  'INTJ': ( 'right', []),
  'LST': ( 'left', ['LS', ':']),
  'NAC': ( 'right', [re.compile('NN.*'), 'NP', 'NAC', 'EX', '$', 'CD', 'QP', 'PRP', 'VBG', 'JJ', 'JJS', 'JJR', 'ADJP', 'FW']),
  'NP': ( 'right', [re.compile('(NN*)|(NX)'), 'JJR', 'CD', 'JJ', 'JJS', 'RB', 'QP', re.compile('NP$'), 'NP']),
  'NX': ( 'right', [re.compile('(NN*)|(NX)'), 'JJR', 'CD', 'JJ', 'JJS', 'RB', 'QP', re.compile('NP$'), 'NP']),
  'PRN': ( 'left', [non_punct_re]),
  'PRT': ( 'left', ['RP']),
  'QP': ( 'right', ['$', 'IN', 'NNS', 'NN', 'JJ', 'RB', 'DT', 'CD', 'NCD', 'QP', 'JJR', 'JJS']),
  'RRC': ( 'left', ['VP', 'NP', 'ADVP', 'ADJP', 'PP']),
  'S': ( 'right', ['VP', re.compile('.*-PRD'), 'S', 'SBAR', 'ADJP', 'UCP', 'NP']),
  'SBAR': ( 'right', ['S', 'SQ', 'SINV', 'SBAR', 'FRAG', 'IN', 'DT']),
  'SBARQ': ( 'right', ['SQ', 'S', 'SINV', 'SBARQ', 'FRAG']),
  'SINV': ( 'right', ['VBZ', 'VBD', 'VBP', 'VB', 'MD', 'VP', re.compile('.*-PRD'), 'S', 'SINV', 'ADJP', 'NP']),
  'SQ': ( 'right', ['VBZ', 'VBD', 'VBP', 'VB', 'MD', re.compile('.*-PRD'), 'VP', 'SQ']),
  'UCP': ( 'left', []),
  'VP': ( 'left', ['VBD', 'VBN', 'MD', 'VBZ', 'VB', 'VBG', 'VBP', 'VP', re.compile('.*-PRD'), 'ADJP', 'NN', 'NNS', 'NP']),
  'WHADJP': ( 'right', ['CC', 'WRB', 'JJ', 'ADJP']),
  'WHADVP': ( 'left', ['CC', 'WRB']),
  'WHNP': ( 'right', [re.compile('NN.*'), 'WDT', 'WP', 'WP$', 'WHADJP', 'WHPP', 'WHNP']),
  'X': ( 'left', []),
  # Added by me:
  'META': ('right', [])
}
# TODO: Handle the various flags defined for pennconverter

# For TiGer, based on "Treebank-Based Grammar Acquisition for German", PhD Thesis, Ines Rehbein
tiger_mapping_table = {
  'AA': ('right', ['ADJD', 'PIS', 'PIAT', 'ADV', 'ADJA']),
  'AP': ('right', ['ADJA', 'ADJD', 'CARD', 'ART', 'PIAT', 'NN', 'PIS', 'ADV', 'PDAT', 'VVPP', 'PTKNEG', 'PWAT', 'TRUNC']),
  'AVP': ('right', ['ADV', 'PTKNEG', 'PROAV', 'PWAV', 'ADJD', 'PWAT', 'PIS', 'PTKA', 'PIAT', 'APPR', 'KOUS', 'PTKANT', 'KON', 'KOUS', 'NN']),
  'CAC': ('right', ['KON']),
  'CAP': ('right', ['KON', 'APPR', 'ADV']),
  'CAVP': ('right', ['KON', 'APPR']),
  'CCP': ('right', ['KON']),
  'CH': ('right', ['NN', 'NE', 'FM', 'CARD', 'XY', 'KON', 'ADV', 'ITJ']),
  'CNP': ('right', ['KON']),
  'CO': ('right', ['KON', 'APPR', 'ADV', 'KOKOM', 'PROAV']),
  'CPP': ('right', ['KON', 'ADV']),
  'CS': ('right', ['KON', 'ADV']),
  'CVP': ('right', ['KON']),
  'CVZ': ('right', ['KON']),
  'DL': ('right', ['NE', 'NN', 'KON', 'ADV', 'NP', 'PP', 'PN', 'CNP', 'S', 'CS']),
  'ISU': ('left', ['ADV', 'APPR', 'KON', 'PIS']),
  'MTA': ('right', ['ADJA', 'NE', 'NN']),
  'NM': ('right', ['NN', 'CARD', 'ADJA']),
  'NP': ('left', ['NN', 'NE', 'PPER', 'FM', 'PIS', 'PDS', 'PWS', 'PRELS', 'PRF', 'PPOSS', 'CH', 'CNP', 'NP', 'PIAT', 'PN', 'CARD', 'AP', 'ADJA', 'ART']),
  'PN': ('right', ['NE', 'NNE', 'NN', 'NP', 'CNP']),
  'PP': ('left', ['KOKOM', 'APPR', 'APPRART', 'APPO', 'PROAV', 'APZR', 'KOUS', 'NE', 'FM', 'PDS']),
  'QL': ('right', ['CARD']),
  'S': ('left', ['VAFIN', 'VMFIN', 'VVFIN', 'VVIMP', 'VAIMP', 'VVPP', 'VAINF', 'VMINF', 'VVFIN', 'VVIZU']),
  'VP': ('left', ['VVPP', 'VVINF', 'VAINF', 'VMINF', 'VAPP', 'VMPP', 'VVIZU', 'VVFIN', 'VMFIN', 'VZ', 'CVZ', 'CVP', 'ADJD', 'TRUNC', 'PP']),
  'VZ': ('right', ['VVINF', 'VMINF', 'VAINF', 'ADJA', 'VVIZU']),
  "MPN": ('right', []),
}

def without_func(label):
  return treebanks.split_label_type_and_function(label)[0]

def add_head(head_map, tree, head):
  if log: print "Added", tree.span, tree, head
  tree_repr = (tree.span, tree.label)
  head_map[tree_repr] = head

def get_head(head_map, tree, amend_for_trace=False):
  if not amend_for_trace:
    tree_repr = (tree.span, tree.label)
    if tree_repr in head_map:
      return head_map[tree_repr]
  tree_repr = (tree.wordspan, treebanks.remove_coindexation(tree, False).label)
  if tree_repr in head_map:
    return head_map[tree_repr]
  tree_repr = (tree.wordspan, without_func(tree.label))
  if tree_repr in head_map:
    return head_map[tree_repr]
  return None

def get_signature(head_map, tree):
  tree_repr = (tree.span, tree.label)
  if tree_repr in head_map:
    return tree_repr
  tree_repr = (tree.wordspan, treebanks.remove_coindexation(tree, False).label)
  if tree_repr in head_map:
    return tree_repr
  tree_repr = (tree.wordspan, without_func(tree.label))
  if tree_repr in head_map:
    return tree_repr
  return None

def add_if_match(tree, options, head_map, reverse=False):
  for i in xrange(len(tree.subtrees)):
    if reverse:
      i = len(tree.subtrees) - i - 1
    subtree = tree.subtrees[i]
    if without_func(subtree.label) in options:
      add_head(head_map, tree, get_head(head_map, subtree))
      return True
  return False

def collins_NP(tree, head_map):
  #TODO:todo handle NML properly
  all_NP_or_comma = True
  first_NP = None
  for i in xrange(len(tree.subtrees)):
    subtree = tree.subtrees[i]
    if without_func(subtree.label) not in {'CCNP', 'NP', ','}:
      all_NP_or_comma = False
    elif subtree.label != ',' and first_NP is None:
      first_NP = subtree
  if all_NP_or_comma:
    add_head(head_map, tree, get_head(head_map, first_NP))
    return

  if (not tree.subtrees[-1].is_trace) and get_head(head_map, tree.subtrees[-1])[2] == 'POS':
    add_head(head_map, tree, get_head(head_map, tree.subtrees[-1]))
    return
  if add_if_match(tree, {'NN', 'NNP', 'NNPS', 'NNS', 'NX', 'POS', 'JJR'}, head_map, True):
    return
  if add_if_match(tree, {'NP', 'NML'}, head_map, False):
    return
  if add_if_match(tree, {'$', 'ADJP', 'PRN'}, head_map, True):
    return
  if add_if_match(tree, {'CD'}, head_map, True):
    return
  if add_if_match(tree, {'JJ', 'JJS', 'RB', 'QP'}, head_map, True):
    return
  # Fallback, no punct
  backup = None
  for i in xrange(len(tree.subtrees)):
    i = len(tree.subtrees) - i - 1
    subtree = tree.subtrees[i]
    if not subtree.is_punct():
      backup = subtree
      if not (len(subtree.label) > 2 and subtree.label.startswith('CC')):
        if log: print "Match NP backup"
        add_head(head_map, tree, get_head(head_map, subtree))
        return
  add_head(head_map, tree, get_head(head_map, backup))

def pennconverter_PP(tree, head_map):
# ( 'left', [('first non-punctuation after preposition)']),
  prep_seen = False
  for subtree in tree.subtrees:
    if without_func(subtree.label) in {'IN', 'TO', 'RP'}:
      prep_seen = True
    elif prep_seen and re.match(non_punct_re, without_func(subtree.label)) is not None:
      add_head(head_map, tree, get_head(head_map, subtree))
      return
  add_head(head_map, tree, get_head(head_map, tree.subtrees[0]))

def pennconverter_is_coord(node):
  if len(node.subtrees) < 2:
    return False

  # If it contains a conjunction other than '[n]either', use that
  for sub in node.subtrees:
    if without_func(sub.label) == 'CONJP':
      return True
    if without_func(sub.label) == 'CC' and sub.word not in {'either', 'neither'}:
      return True

  if without_func(node.label) == 'UCP':
    return True

  commas = False
  for sub in node.subtrees:
    if sub.label in {',', ':'}:
      commas = True
        
  if without_func(node.label) in {"NP", "NX", "NML", "NAC"}:
    # Check for appositives
    if not commas:
      return False
    np_children = 0
    for sub in node.subtrees:
      if 'TMP' in sub.label or 'LOC' in sub.label:
        return False
      if without_func(sub.label) in {"NP', 'NX', 'NML', 'NAC"}:
        if len(sub.subtrees) != 1 or without_func(sub.subtrees[0].label) != 'CD':
          np_children += 1
    if np_children > 2:
      return True
  else:
    label = None
    terminals = False
    nonterminals = 0
    uniform = True
    for sub in node.subtrees:
      if sub.is_terminal():
        if (not sub.is_punct()) and (without_func(sub.label) not in {"RB', 'UH', 'IN', 'CC"}):
          terminals = True
      else:
        nonterminals += 1
        if label is None:
          label = without_func(sub.label)
          if label in {'SINV', 'SQ', 'SBARQ'}:
            label = 'S'
        else:
          if without_func(sub.label) in {'SINV', 'SQ', 'SBARQ'}:
            if label != 'S':
              uniform = False
          elif label != without_func(sub.label):
            uniform = False
        if sub.word_yield(as_list=True)[-1] in {",", ";"}:
          commas = True
    
    if commas and uniform and nonterminals > 1:
      return not terminals

  return False

def find_heads(tree, style, head_map=None):
  mapping = pennconverter_mapping_table
  if style == 'collins':
    mapping = collins_mapping_table
  elif 'jkk' in style:
    mapping = jkk_mapping_table
  if head_map is None:
    head_map = {}
    tree = treebanks.remove_coindexation(tree, False)
  for subtree in tree.subtrees:
    find_heads(subtree, style, head_map)

  if log: print "Head for", tree.span, tree.label

  # A word is it's own head
  if tree.word is not None:
    head = (tree.span, tree.word, tree.label)
    add_head(head_map, tree, head)
    return head_map

  # First handle conjunctions
  coord = pennconverter_is_coord(tree)
  if coord and style == 'pennconverter':
    if not add_if_match(tree, {'CC', 'CONJP'}, head_map, True):
      if not add_if_match(tree, {',', ':'}, head_map, True):
        add_head(head_map, tree, get_head(head_map, tree.subtrees[-1]))
    return head_map
  
  collins_coord = False
  for subtree in tree.subtrees:
    if subtree.label.startswith('CC'):
      if len(subtree.label) > 2:
        collins_coord = True
      elif 'jkk' in style and len(tree.subtrees) > 2:
        collins_coord = True
  if len(tree.subtrees) > 2 and without_func(tree.subtrees[0].label) == without_func(tree.label) and tree.subtrees[1].label == 'CC':
    collins_coord = True
  flat_NP = False
  if tree.label == "NP":
    flat_NP = True
    for subtree in tree.subtrees:
      if len(subtree.subtrees) != 0:
        flat_NP = False
  if (not flat_NP) and collins_coord and (style == 'collins' or 'jkk' in style):
    if log: print "doing coord special case"
    # Options:
    # 0 - First non-punct (collins)
    # 1 - First conjunction
    # 2 - First non-punct non-conjunction
    # 3 - Last non-punct
    # 4 - Last conjunction
    # 5 - Last non-punct non-conjunction
    for i in xrange(len(tree.subtrees)):
      subtree = tree.subtrees[i]
      if style == 'collins' or style == 'jkk' or style[-1] == '0':
        if not subtree.is_punct():
          if log: print "Match backup"
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map
      elif style[-1] == '1':
        if subtree.is_conjunction():
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map
      elif style[-1] == '2':
        if not (subtree.is_conjunction() or subtree.is_punct()):
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map
      subtree = tree.subtrees[len(tree.subtrees) - i - 1]
      if style[-1] == '3':
        if not subtree.is_punct():
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map
      elif style[-1] == '4':
        if subtree.is_conjunction():
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map
      elif style[-1] == '5':
        if not (subtree.is_conjunction() or subtree.is_punct()):
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map
    if log: print "coord special case didn't find a head"

  # If the label for this node is not in the table we are either at the bottom,
  # at an NP, or have an error
  base_label = without_func(tree.label)
  if base_label not in mapping or base_label in ['NP', 'NML']:
    if base_label in ['NP', 'NML']:
      if log: print "doing collins NP"
      collins_NP(tree, head_map)
    elif base_label in ['PP', 'WHPP']:
      pennconverter_PP(tree, head_map)
    else:
      add_head(head_map, tree, get_head(head_map, tree.subtrees[-1]))
    return head_map
  
  # Look through and take the first/last occurrence that matches
  info = mapping[base_label]
  for label in info[1]:
    for i in xrange(len(tree.subtrees)):
      if info[0] == 'right':
        i = len(tree.subtrees) - i - 1
      subtree = tree.subtrees[i]
      if isinstance(label, str):
###        if subtree.label == label:
###          if log: print "Match add 1"
###          add_head(head_map, tree, get_head(head_map, subtree))
###          return head_map
        if label in special_cases:
          if subtree.word in special_cases[label]:
            if log: print "Match add 1a"
            add_head(head_map, tree, get_head(head_map, subtree))
            return head_map
        elif without_func(subtree.label) == label:
          if 'aux' not in info[1] or subtree.word not in special_cases['aux']:
            if log: print "Match add 1"
            add_head(head_map, tree, get_head(head_map, subtree))
            return head_map
      else:
        if re.match(label, without_func(subtree.label)) is not None:
          if log: print "Match add 2"
          add_head(head_map, tree, get_head(head_map, subtree))
          return head_map

  # Fallback, no punct
  for i in xrange(len(tree.subtrees)):
    if info[0] == 'right':
      i = len(tree.subtrees) - i - 1
    subtree = tree.subtrees[i]
    if not subtree.is_punct():
      if not (len(subtree.label) > 2 and subtree.label.startswith('CC')):
        if log: print "Match backup"
        add_head(head_map, tree, get_head(head_map, subtree))
        return head_map

  # Final fallback
  if info[0] == 'left':
    if log: print "Fallback add 1"
    add_head(head_map, tree, get_head(head_map, tree.subtrees[0]))
  else:
    if log: print "Fallback add 2"
    add_head(head_map, tree, get_head(head_map, tree.subtrees[-1]))
  return head_map

'''Text from Collins' website:

This file describes the table used to identify head-words in the papers

Three Generative, Lexicalised Models for Statistical Parsing  (ACL/EACL97)
A New Statistical Parser Based on Bigram Lexical Dependencies (ACL96)

There are two parts to this file:

[1] an email from David Magerman describing the head-table used in
    D. Magerman. 1995. Statistical Decision-Tree Models for Parsing.
    {\it Proceedings of the 33rd Annual Meeting of
    the Association for Computational Linguistics}, pages 276-283.

[2] A modified version of David's head-table which I used in my experiments.

Many thanks to David Magerman for allowing me to distribute his table.


[1]

From magerman@bbn.com Thu May 25 13:48 EDT 1995
Posted-Date: Thu, 25 May 1995 13:48:07 -0400
Received-Date: Thu, 25 May 1995 13:48:43 +0500
Message-Id: <199505251748.NAA02892@thane.bbn.com>
To: mcollins@gradient.cis.upenn.edu, robertm@unagi.cis.upenn.edu,
        mitch@linc.cis.upenn.edu
Cc: magerman@bbn.com
Subject: Re: Head words table 
In-Reply-To: Your message of "Thu, 25 May 1995 13:17:14 EDT."
             <9505251717.AA17874@gradient.cis.upenn.edu> 
Date: Thu, 25 May 1995 13:48:07 -0400
From: David Magerman <magerman@bbn.com>
Content-Type: text
Content-Length: 2972


Hi all.  Mike and Robert asked me for the Tree Head Table, so I
thought I'd pass it along to everyone in one shot.  Feel free to
distribute it to whomever at Penn wants it.

Note that it's not complete, and that I've invented a tag (% for the
symbol %) and a label (NP$ for NP's that end in POS).  I also have
some optional mapping mechanisms that: (a) convert to_TO -> to_IN when
in a prepositional phrase and (b) translate (PRT x_RP) -> (ADVP x_RB),
thus mapping away the distinction between particles and adverbs.  I
currently use transformation (b) in my parser, but don't use (a).
These facts may or may not be relevant, depending on how you want to
use this table.

Cheers,
-- David

Tree Head Table
---------------

Instructions:

1. The first column is the non-terminal.  The second column indicates
where you start when you are looking for a head (left is for
head-initial categories, right is for head-final categories).  The
rest of the line is a list of non-terminal and pre-terminal categories
which represent the head rule.

2. ** is a wildcard value.  Any non-terminal with ** in its rule means
that anything can be its head.  So, for a head-initial category, **
means the first word is always the head, and for a head-final
category, ** means the last word is always the head.  In most cases,
** means I didn't investigate good head rules for that category, so it
might be worthwhile to do so yourself.

3. The Tree Head Table is used as follows:

  a. Use tree head rule based on NT category of constituent
  b. For each category X in tree head rule, scan the children of
           the constituent for the first (or last, for head-final)
           occurrence of category X.  If
           X occurs, that child is the head.
        c. If no child matches any category in the list, use the first
           (or last, for head-final) child as the head.

4. I treat the NP category as a special case.  Before consulting the
head rule for NP, I look for the rightmost child with a label
beginning with the letter N.  If one exists, I use that child as the
head.  If no child's tag begins with N, I use the tree head rule.

ADJP  right % QP JJ VBN VBG ADJP $ JJR JJS DT FW **** RBR RBS RB
ADVP  left  RBR RB RBS FW ADVP CD **** JJR JJS JJ
CONJP left  CC RB IN
FRAG  left  **
INTJ  right **
LST left  LS :
NAC right NN NNS NNP NNPS NP NAC EX $ CD QP PRP VBG JJ JJS JJR ADJP FW
NP  right EX $ CD QP PRP VBG JJ JJS JJR ADJP DT FW RB SYM PRP$
NP$ right NN NNS NNP NNPS NP NAC EX $ CD QP PRP VBG JJ JJS JJR ADJP FW SYM
PNP right **
PP  left  IN TO FW
PRN left  **
PRT left  RP
QP  right CD NCD % QP JJ JJR JJS DT
RRC left  VP NP ADVP ADJP PP
S right VP SBAR ADJP UCP NP
SBAR  right S SQ SINV SBAR FRAG X
SBARQ right SQ S SINV SBARQ FRAG X
SINV  right S VP VBZ VBD VBP VB SINV ADJP NP
SQ  right VP VBZ VBD VBP VB MD SQ
UCP left  **
VP  left  VBD VBN MD VBZ TO VB VP VBG VBP ADJP NP
WHADJP  right JJ ADJP
WHADVP  left  WRB
WHNP  right WDT WP WP$ WHADJP WHPP WHNP
WHPP  left  IN TO FW
X left  **


[2]

Here's the head table which I used in my experiments below. The first column
is just the number of fields on that line. Otherwise, the format is the same
as David's.

Ignore the row for NPs -- I use a special set of rules for this. For these
I initially remove ADJPs, QPs, and also NPs which dominate a possesive 
(tagged POS, e.g.  (NP (NP the man 's) telescope ) becomes 
(NP the man 's telescope)). These are recovered as a post-processing stage 
after parsing. The following rules are then used to recover the NP head:

If the last word is tagged POS, return (last-word);

Else search from right to left for the first child which is an NN, NNP, NNPS, NNS, NX, POS, or JJR
                       
Else search from left to right for first child which is an NP

Else search from right to left for the first child which is a $, ADJP or PRN

Else search from right to left for the first child which is a CD

Else search from right to left for the first child which is a JJ, JJS, RB or QP

Else return the last word


20 ADJP 0 NNS QP NN $ ADVP JJ VBN VBG ADJP JJR NP JJS DT FW RBR RBS SBAR RB
15 ADVP 1 RB RBR RBS FW ADVP TO CD JJR JJ IN NP JJS NN
5 CONJP 1 CC RB IN
2 FRAG  1 
2 INTJ  0 
4 LST 1 LS :
19 NAC  0 NN NNS NNP NNPS NP NAC EX $ CD QP PRP VBG JJ JJS JJR ADJP FW
8 PP  1 IN TO VBG VBN RP FW
2 PRN 0 
3 PRT 1 RP
14 QP 0 $ IN NNS NN JJ RB DT CD NCD QP JJR JJS
7 RRC 1 VP NP ADVP ADJP PP
10 S  0 TO IN VP S SBAR ADJP UCP NP
13 SBAR 0 WHNP WHPP WHADVP WHADJP IN DT S SQ SINV SBAR FRAG
7 SBARQ 0 SQ S SINV SBARQ FRAG
12 SINV 0 VBZ VBD VBP VB MD VP S SINV ADJP NP
9 SQ  0 VBZ VBD VBP VB MD VP SQ
2 UCP 1 
15 VP 0 TO VBD VBN MD VBZ VB VBG VBP VP ADJP NN NNS NP
6 WHADJP  0 CC WRB JJ ADJP
4 WHADVP  1 CC WRB
8 WHNP  0 WDT WP WP$ WHADJP WHPP WHNP
5 WHPP  1 IN TO FW'''


if __name__ == "__main__":
  print "Running doctest"
  import doctest
  doctest.testmod()

