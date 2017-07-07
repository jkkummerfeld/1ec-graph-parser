#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set ts=2 sw=2 noet:
'''Various string representations of trees.'''

import string
from collections import defaultdict

import pstree, parse_errors, head_finder, treebanks

# TODO:todo Fix handling of traces throughout
# Handling of unary order
# Sites that render trees
# http://mshang.ca/syntree/
# http://www.yohasebe.com/rsyntaxtree/

def text_words(tree, show_traces=False):
  '''Print just the words in the tree.'''
  text = []
  for node in tree:
    if node.is_terminal():
      if node.is_trace() and not show_traces:
        continue
      text.append(node.word)
  return ' '.join(text)

def text_POS_tagged(tree, show_traces=False):
  '''Print words and part of speech tags in the tree.'''
  text = []
  for node in tree:
    if node.is_terminal():
      if node.is_trace() and not show_traces:
        continue
      text.append(node.word + '|' + node.label)
  return ' '.join(text)

def text_tree(tree, single_line=True, show_traces=False, depth=0, dense=True, newline=True, match_ptb=False, prev_and=False):
  if not show_traces:
    tree = treebanks.remove_traces(tree, False)
    tree = treebanks.remove_coindexation(tree, False)
  ans = ''
  if not single_line and depth > 0:
    if newline or (not dense) or tree.word is None or (match_ptb and (prev_and or tree.word in {'and', '-RRB-', '-RCB-', '-RSB-', '-LRB-', '-LCB-', '-LSB-'})):
      if match_ptb:
        if tree.parent is None or tree.parent.label != 'ROOT':
          ans = '\n' + depth * '  '
      else:
        ans = '\n' + depth * '\t'
    else:
      ans = ' '
  if match_ptb and tree.label == 'ROOT':
    ans += "( "
  else:
    ans += '(' + tree.label
  if tree.word is not None:
    ans += ' ' + tree.word
    newline = True
  else:
    newline = False
  prev_and = False
  for subtree in tree.subtrees:
    if single_line:
      ans += ' '
    ans += text_tree(subtree, single_line, True, depth + 1, dense, newline, match_ptb, prev_and)
    prev_and = (subtree.word == 'and')
    newline = subtree.word is None
  if tree.word is None and dense and tree.subtrees[-1].word is not None and ans[-1] != ' ':
    ans += ' '
  ans += ')'
  return ans

def text_ontonotes(tree, filename='filename', words=None, tree_text=None, depth=0):
  resolve = False
  if words is None:
    resolve = True
    words = []
    tree_text = ''

  if tree.word is None:
    tree_text += '(' + tree.label + '_'
  else:
    words.append((tree.word, tree.label))
    tree_text += '*'

  for subtree in tree.subtrees:
    tree_text = text_ontonotes(subtree, filename, words, tree_text, depth)

  if tree.word is None:
    tree_text += ')'

  if resolve:
    ans = ''
    cpos = 0
    cword = 0
    while cpos < len(tree_text):
      ctext = ''
      while cpos < len(tree_text) and tree_text[cpos] != '*':
        ctext += tree_text[cpos]
        cpos += 1
      ctext += tree_text[cpos]
      cpos += 1
      while cpos < len(tree_text) and tree_text[cpos] == ')':
        ctext += tree_text[cpos]
        cpos += 1
      ans += '%s %9s %9d %9s %9s %9s' % (filename, 0, cword, words[cword][0], words[cword][1], ctext)
      for val in ['-', '-', '-', '-', '*', '*', '*', '*', '*', '*', '-']:
        ans += ' %9s' % val
      ans += '\n'
      cword += 1
    return ans
  else:
    return tree_text

def tex_synttree(tree, other_spans=None, depth=0, compressed=True, span=None):
  if tree.label == '.':
    return ''
  if span is not None and (tree.span[1] <= span[0] or tree.span[0] >= span[1]):
    # TODO:todo will give long skinny trees
    return ''
  correct = True
  if other_spans is not None:
    correct = (tree.label, tree.span[0], tree.span[1]) in other_spans
  else:
    compressed = False
  all_in_subtree = False
  if span is not None:
    for subtree in tree.subtrees:
      if subtree.span[0] <= span[0] and span[1] <= subtree.span[1]:
        all_in_subtree = True

  # Clean the label and word
  label = tree.label
  if '$' in label:
    label = '\$'.join(label.split('$'))
  word = tree.word
  if word is not None:
    word = ''.join(word.split('.'))
    word = '\&'.join(word.split('&'))
    word = '\$'.join(word.split('$'))
    word = '\%'.join(word.split('%'))

  # Make the text
  ans = ''
  if tree.parent is None:
    ans += '\synttree'
    if not all_in_subtree:
      ans += '\n'
  elif not all_in_subtree:
    ans += '\n' + '  ' * depth
  if len(tree.subtrees) == 0:
    ans += '[%s [%s]]' % (label, word)
  else:
    if not all_in_subtree:
      if correct:
        ans += '[%s' % (label)
      else:
        ans += '[\wrongnode{%s}' % (label)
    for subtree in tree.subtrees:
      ans += tex_synttree(subtree, other_spans, depth + 1, compressed, span)
    if not all_in_subtree:
      ans += ']'

  # When compressing we only want errors visible
  if compressed and 'wrongnode' not in ans and tree.word is None:
    words = ''.join(tree.word_yield().split('.'))
    words = '\&'.join(words.split('&'))
    words = '\$'.join(words.split('$'))
    if tree.parent is None:
      ans = '\synttree\n'
    else:
      ans = '\n' + '  ' * depth
    ans += '[%s [.t %s]]' % (label, words)
  return ans

### Sketch of new coloured error design:
### 1. Create tokens for current tree, label each with its span, and whether it
###    is extra (or different in the case of POS)
### 2. Introduce missing brackets, placing them in the token list as
###    appropriate
### 3. Introduce crossing brackets, similarly
def get_init_tokens(tree, mapping, tokens):
  tokens.append(('(' + tree.label, tree.span, False, False, False, False))
  mapping[tree] = [len(tokens) - 1]
  for subtree in tree.subtrees:
    get_init_tokens(subtree, mapping, tokens)
  if tree.is_terminal():
    tokens.append((' ' + tree.word + ')', tree.span, False, False, False, False))
  else:
    tokens.append((')', tree.span, False, False, False, False))
  mapping[tree].append(len(tokens) - 1)
  return tokens

def text_coloured_errors(tree, gold=None, unused=0, single_line=False, unused2=None, unused3=None, compressed='words', POS=True, indent='   '):
  # TODO: Work on ordering of unaries, particularly at the root
  if compressed == True:
    compressed = 'words'
  start_missing = "\033[01;36m"
  start_extra = "\033[01;31m"
  start_crossing = "\033[01;33m"
  end_colour = "\033[00m"

  mapping = {}
  tokens = []
  get_init_tokens(tree, mapping, tokens)

  # Mark extra
  errors = parse_errors.Parse_Error_Set(gold, tree, POS)
  for etype, span, label, node in errors.extra:
    for token_loc in mapping[node]:
      cur = tokens[token_loc]
      tokens[token_loc] = (cur[0], cur[1], True, False, False, False)

  # Mark POS
  for etype, span, label, node, gold_label in errors.POS:
    token_loc = mapping[node][0]
    cur = tokens[token_loc]
    ntext = '(' + start_missing + gold_label + ' ' + start_extra + label + end_colour
    tokens[token_loc] = (ntext, cur[1], False, False, False, True)

  # Insert missing
  for etype, span, label, node in errors.missing:
    for i in range(len(tokens)):
      if tokens[i][1][0] == span[0] and tokens[i][1][1] <= span[1]:
        tokens.insert(i, ('(' + label, span, False, True, False, False))
        break
    last = None
    for i in range(len(tokens)):
      if tokens[i][1][1] == span[1] and tokens[i][1][0] >= span[0]:
        last = i
    assert last is not None
    tokens.insert(last + 1, (')', span, False, True, False, False))

  # Insert crossing
  for etype, span, label, node in errors.crossing:
    for i in range(len(tokens)):
      if tokens[i][1][0] == span[0] and tokens[i][1][1] <= span[1]:
        tokens.insert(i, ('(' + label + ' ', span, False, False, True, False))
        break
    last = None
    for i in range(len(tokens)):
      if tokens[i][1][1] == span[1] and tokens[i][1][0] >= span[0]:
        last = i
    assert last is not None
    tokens.insert(last + 1, (' ' + label + ')', span, False, False, True, False))

  # Compressed
  if compressed == 'none':
    pass
  else:
    i = 0
    while i < len(tokens):
      if '(' in tokens[i][0] and not (tokens[i][2] or tokens[i][3] or tokens[i][4] or tokens[i][5]):
        all_correct = True
        depth = 0
        last = None
        for j in range(i, len(tokens)):
          if tokens[j][2] or tokens[j][3] or tokens[j][4] or tokens[j][5]:
            all_correct = False
            break
          if '(' in tokens[j][0]:
            depth += 1
          if ')' in tokens[j][0]:
            depth -= 1
          if depth == 0:
            last = j
            break
        if all_correct:
          assert last is not None
          text = ''
          if compressed == 'words':
            top_label = ''
            words = []
            for token in tokens[i:last + 1]:
              if '(' in token[0] and top_label == '':
                top_label = token[0][1:]
              elif ')' in token[0] and len(token[0]) > 1:
                words.append(token[0].strip()[:-1])
            text = '(' + top_label + ' ' + ' '.join(words) + ')'
          elif compressed == 'single line':
            for token in tokens[i:last + 1]:
              if '(' in token[0]:
                text += ' '
              text += token[0]
            text = text.strip()
          tokens[i] = (text, tokens[i][1], False, False, False, False)
          for j in range(i+1, last+1):
            tokens.pop(i+1)
      i += 1

  # Combine tokens
  ans = []
  depth = 0
  no_indent = False
  for text, span, extra, missing, crossing, POS_error in tokens:
    begin = ''
    if '(' in text:
      if crossing:
        if not no_indent:
          no_indent = True
          begin = '\n' + depth * indent
          depth += 1
      else:
        if not no_indent:
          begin = '\n' + depth * indent
          depth += 1
        else:
          no_indent = False
    if ')' in text and not crossing:
      depth -= 1
    if extra:
      ans.append(begin + start_extra + text + end_colour)
    elif missing:
      ans.append(begin + start_missing + text + end_colour)
    elif crossing:
      ans.append(begin + start_crossing + text + end_colour)
    else:
      ans.append(begin + text)
  return ''.join(ans)

def label_level(parse, head_map, label=None):
  head = head_finder.get_head(head_map, parse, True)
  if label is None:
    label = treebanks.remove_coindexation_from_label(parse.label)
  count = 0
  done = False
  while not done:
    done = True
    for subparse in parse.subtrees:
      slabel = treebanks.remove_coindexation_from_label(subparse.label)
      if head == head_finder.get_head(head_map, subparse, True):
        done = False
        parse = subparse
        if slabel == label:
          count += 1
        break
  return count

def compress_null_for_spine(parse, symbol_counts, symbol_map, traces):
  if parse.is_terminal():
    num = pstree.get_reference(parse.word)
    if num is None:
      return "("+ parse.word +")"
    else:
      chained, child = treebanks.follow_chain_in_mapping(traces, num)
      if child is None:
        return None
      return "(" + treebanks.remove_coindexation_from_label(parse.word) +")"

  label = treebanks.remove_coindexation_from_label(parse.label)
  num = symbol_counts[label]
  symbol_counts[label] += 1
  symbol_map[parse.unique_id] = "{}_{}".format(label, num)

  parts = []
  for node in parse.subtrees:
    sub = compress_null_for_spine(node, symbol_counts, symbol_map, traces)
    if sub is not None:
      parts.append(sub)

  if len(parts) > 0:
    return "({}_{})".format(label, "_".join(parts))
  else:
    return None

def get_spines(parse, head_map, spines, symbol_map, traces):
  if not parse.is_terminal():
    for subparse in parse.subtrees:
      get_spines(subparse, head_map, spines, symbol_map, traces)
  elif not parse.is_trace():
    # Add spine, happens on the terminal that this spine will be assigned to
    chead = head_finder.get_head(head_map, parse, True)
    chain = []
    cur = parse.parent
    symbol_counts = defaultdict(lambda: 0)
    trace_symbol_counts = defaultdict(lambda: 0)
    # Walk up the parse via parent links, gradually building the spine
    while cur is not None and chead == head_finder.get_head(head_map, cur, True):
      if cur.parent is not None: # Avoid the case of the ROOT
        chain.append('')
        # Add null elements that need to live here.
        # Most complex case:
        # ( (S
        #     (ADJP-TPC-1 (RB Not) (RB likely) )
        #     (, ,)
        #     (NP-SBJ (PRP I) )
        #     (VP (VBP think)
        #       (SBAR (-NONE- 0)
        #         (S
        #           (NP-SBJ (-NONE- *) )
        #           (ADJP-PRD (-NONE- *T*-1) ))))
        #     (. .) ))
        for node in cur.subtrees:
          if node.wordspan[0] == node.wordspan[1]:
            to_add = compress_null_for_spine(node, trace_symbol_counts, symbol_map, traces)
            if to_add is not None:
              chain[-1] += to_add + "_"
        # Add the non-terminal
        label = treebanks.remove_coindexation_from_label(cur.label)
        num = symbol_counts[label]
        symbol_counts[label] += 1
        symbol_map[cur.unique_id] = "{}_{}".format(label, num)
        chain[-1] += label
      cur = cur.parent
    spines.append((parse.wordspan[1], parse.label, chain, parse.word))

def get_edges(parse, edges, spines, head_map, symbol_map):
  for subparse in parse.subtrees:
    get_edges(subparse, edges, spines, head_map, symbol_map)

  if not parse.is_terminal():
    phead = head_finder.get_head(head_map, parse, True)
    # Normal edges, added by looking at which subparses are not the head of this non-terminal
    for subparse in parse.subtrees:
      chead = head_finder.get_head(head_map, subparse, True)
      if phead is not None and chead is not None and phead[0] != chead[0]:
        psym = symbol_map[parse.unique_id]
        csym = subparse.label + "_0"
        if not subparse.is_terminal():
          cysm = symbol_map[subparse.unique_id]
        edges.append((chead[0][1], csym, phead[0][1], psym, "_", False, False))

def check_tree(edges):
  children = set()
  for edge in edges:
    child = int(edge[0])
    if child in children:
      return False
    children.add(child)
  return True

def check_proj(edges):
  for edge1 in edges:
    for edge2 in edges:
      if edge1[0] < edge2[0] < edge1[1] < edge2[1]:
        return False
      if edge2[0] < edge1[0] < edge2[1] < edge1[1]:
        return False
  return True

def check_1ec(edges):
  edge_sets = {}
  for edge1 in edges:
    for edge2 in edges:
      if edge1[0] < edge2[0] < edge1[1] < edge2[1]:
        if edge1 not in edge_sets:
          edge_sets[edge1] = (set(), set())
        edge_sets[edge1][0].add(edge2[0])
        edge_sets[edge1][1].add(edge2[1])
      if edge2[0] < edge1[0] < edge2[1] < edge1[1]:
        if edge1 not in edge_sets:
          edge_sets[edge1] = (set(), set())
        edge_sets[edge1][0].add(edge2[1])
        edge_sets[edge1][1].add(edge2[0])
  ans = True
  for edge in edge_sets:
    edge_set = edge_sets[edge]
    if len(edge_set[0]) > 1 and len(edge_set[1]) > 1:
      ans = False
      print "# 1EC_violation", edge, edge_set
  return ans

def check_cyclic(edges, len1=False, reverse_null=False):
  edge_sets = {}
  for edge in edges:
    start = edge[0] if reverse_null and edge[5] else edge[2]
    end = edge[2] if reverse_null and edge[5] else edge[0]
    if start not in edge_sets:
      edge_sets[start] = set()
    if end not in edge_sets:
      edge_sets[end] = set()
    edge_sets[start].add(end)

  cycle = False
  for start in edge_sets:
    queue = [start]
    done = set()
    while len(queue) > 0:
      cur = queue.pop(0)
      for other in edge_sets[cur]:
        if other == start:
          if len1 or cur != start:
            cycle = True
        if other not in done:
          queue.append(other)
          done.add(other)
  return cycle

def check_self_cycle(edges):
  for edge in edges:
    if edge[0] == edge[2]:
      return True
  return False

def check_double_edge(edges):
  edge_set = set()
  for edge in edges:
    edge = (edge[0], edge[2])
    if edge in edge_set:
      return True
    edge_set.add(edge)
  return False

def shg_format(parse, head_rules, reverse_null_null=False):
  # The format is:
  #  Lines with a # are useful comments
  #  Main lines are a series of space separated fields:
  #    <token number>
  #    <token>
  #    <part of speech>
  #    <spine>
  #    <parent>
  #    <structural parent label as Symbol_Number>
  # {
  #    <parent>
  #    <parent label as Symbol_Number>
  #    <child label as Symbol_Number>
  #    T/F if the child is a null element or not
  #    <trace symbol>
  # }
  #
  # All numbers count from 0 upwards, starting from the bottom of the spine.
  # Symbols for null elements are counted separately.
  parse.calculate_spans()
  traces = treebanks.resolve_traces(parse)
  base_parse = treebanks.remove_traces(parse, False)
  head_map = head_finder.find_heads(base_parse, head_rules)

  ans = []

  # Prefix
  ans += ["# Parse " + line for line in text_tree(parse, False, True).split("\n")]
  words = text_words(parse).split()
  ans.append("# Sentence   "+ " ".join(words))
  ans.append("# Tokens     "+ "  ".join(["{} {}".format(i + 1, w) for i, w in enumerate(words)]))

  # Trace info
  for num in traces[0]:
    node, over_null = traces[0][num]
    ans.append("# Identity   {} {} {} {}".format(num, node.wordspan, node.label, over_null))
  for num in traces[1]:
    for node in traces[1][num]:
      ans.append("# Gapping    {} {} {}".format(num, node.wordspan, node.label))
  for num in traces[2]:
    for node in traces[2][num]:
      ans.append("# Reference  {} {} {}".format(num, node.wordspan, node.word))
  for node in traces[3]:
    ans.append("# Empty      {} {}".format(node.wordspan, node.word))

  # Actual structure

  # Spines
  spines = []
  symbol_map = {}
  get_spines(parse, head_map, spines, symbol_map, traces)

  # Regular Edges
  edges = []
  assert len(parse.subtrees) == 1
  sym = symbol_map[parse.subtrees[0].unique_id]
  head = head_finder.get_head(head_map, parse, True)
  edges.append((head[0][1], sym, 0, "ROOT_0", "_", False, False))
  get_edges(parse, edges, spines, head_map, symbol_map)

  # Traces
  for trace_group in [1, 2]:
    for num in traces[trace_group]:
      # Get the identity location, which will be the child for the edges
      chained, child = treebanks.follow_chain_in_mapping(traces, num, trace_group)
      if child is None:
        ans.append("# Error      {} reference without identity".format(num))
        continue
      csym = symbol_map[child.unique_id]
      child_is_null = (child.wordspan[1] == child.wordspan[0])
      while child.parent is not None and child.wordspan[1] == child.wordspan[0]:
        child = child.parent
      chead = head_finder.get_head(head_map, child, True)
      # Consider each reference
      for node in traces[trace_group][num]:
        trace_type = "="
        if trace_group == 2:
          trace_type = treebanks.remove_coindexation_from_label(node.word)
        if chained:
          trace_type += "_chain"
        parent = node
        if trace_group == 2:
          parent = parent.parent
        psym = symbol_map[parent.unique_id]
        # Get the head location
        parent_is_null = (parent.wordspan[1] == parent.wordspan[0])
        while parent.parent is not None and parent.wordspan[1] == parent.wordspan[0]:
          parent = parent.parent
        phead = head_finder.get_head(head_map, parent, True)
        edges.append((chead[0][1], csym, phead[0][1], psym, trace_type, child_is_null, parent_is_null))

  # Graph properties
  nedges = []
  for edge in edges:
    a = int(edge[0])
    b = int(edge[2])
    nedges.append((min(a, b), max(a, b)))
  graph_type = '# Graph type '
  if check_proj(nedges):
    graph_type += " proj"
  elif check_1ec(nedges):
    graph_type += "  1ec"
  else:
    graph_type += "other"
  graph_type += ' tree' if check_tree(edges) else ' graph'
  graph_type += ' has-cycle' if check_cyclic(edges) else ' no-cycle'
  graph_type += ' has-cycle-rev-edges' if check_cyclic(edges, False, True) else ' no-cycle-rev-edges'
  graph_type += ' has-len1-cycle' if check_self_cycle(edges) else ' no-len1-cycle'
  graph_type += ' has-double' if check_double_edge(edges) else ' no-double'
  ans.append(graph_type)

  # Spines and edges
  spines.sort()
  final_structure = []
  for spine in spines:
    word, POS, chain, token = spine
    chain = '_'.join(chain) if len(chain) > 0 else '_'
    line = [str(word), token, POS, chain]
    to_add = []
    for edge in edges:
      clabel = edge[1]
      parent = edge[2]
      plabel = edge[3]
      trace_info = edge[4]
      child_is_null = str(edge[5])[0]
      parent_is_null = str(edge[6])[0]

      if trace_info == '=' or (reverse_null_null and edge[5] and edge[6]):
        if edge[2] == word:
          part = "{} {} {} {} {} {}".format(edge[0], clabel, child_is_null, plabel, parent_is_null, trace_info)
          to_add.append(part)
      elif edge[0] == word:
        if trace_info == '_':
          line.append(str(parent))
          line.append(plabel)
        else:
          part = "{} {} {} {} {} {}".format(parent, plabel, parent_is_null, clabel, child_is_null, trace_info)
          to_add.append(part)
    to_add.sort()
    final_structure.append(line + to_add)
  max_lengths = defaultdict(lambda: 0)
  for line in final_structure:
    for i, text in enumerate(line):
      max_lengths[i] = max(max_lengths[i], len(text))
  for line in final_structure:
    padded = []
    for i, text in enumerate(line):
      if i < 6:
        if i == 0 or i == 4:
          fmt = "{:>"+ str(max_lengths[i]) +"}"
        else:
          fmt = "{:<"+ str(max_lengths[i]) +"}"
        padded.append(fmt.format(text))
      else:
        padded.append(text)
    ans.append(' '.join(padded))
  ans.append('')
  return "\n".join(ans)

def cut_text_below(text, depth):
  '''Simplify text to only show the top parts of a tree
  >>> print cut_text_below("(ROOT (NP (PRP I)) (VP (VBD ran) (NP (NN home))))", 1)
  (ROOT)
  >>> print cut_text_below("(ROOT (NP (PRP I)) (VP (VBD ran) (NP (NN home))))", 2)
  (ROOT (NP) (VP))
  >>> print cut_text_below("(ROOT (NP (PRP I)) (VP (VBD ran) (NP (NN home))))", 3)
  (ROOT (NP (PRP I)) (VP (VBD ran) (NP)))
  >>> print cut_text_below("(ROOT (NP (PRP I)) (VP (VBD ran) (NP (NN home))))", 20)
  (ROOT (NP (PRP I)) (VP (VBD ran) (NP (NN home))))
  '''

  # Cut lower content
  cdepth = 0
  ntext = ''
  for char in text:
    if char == '(':
      cdepth += 1
    if cdepth <= depth:
      ntext += char
    if char == ')':
      cdepth -= 1

  # Walk back and remove extra whitespace
  text = ntext
  ntext = ''
  ignore = False
  for char in text[::-1]:
    if char == ')':
      ignore = True
      ntext += char
    elif ignore:
      if char != ' ':
        ntext += char
        ignore = False
    else:
      ntext += char
  return ntext[::-1]

if __name__ == '__main__':
  print "Running doctest"
  import doctest
  doctest.testmod()

