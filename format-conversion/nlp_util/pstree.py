#!/usr/bin/env python

import re, string

from collections import defaultdict

DEFAULT_LABEL = 'label_not_set'
TRACE_LABEL = '-NONE-'

class TreeIterator:
  '''Iterator for traversal of a tree.
  
  PSTree uses pre-order traversal by default, but this supports post-order too, e.g.:
  >>> tree = tree_from_text("(ROOT (S (NP-SBJ (NNP Ms.) (NNP Haag) ) (VP (VBZ plays) (NP (NNP Elianti) )) (. .) ))")
  >>> for node in TreeIterator(tree, 'post'):
  ...   print node
  (NNP Ms.)
  (NNP Haag)
  (NP-SBJ (NNP Ms.) (NNP Haag))
  (VBZ plays)
  (NNP Elianti)
  (NP (NNP Elianti))
  (VP (VBZ plays) (NP (NNP Elianti)))
  (. .)
  (S (NP-SBJ (NNP Ms.) (NNP Haag)) (VP (VBZ plays) (NP (NNP Elianti))) (. .))
  (ROOT (S (NP-SBJ (NNP Ms.) (NNP Haag)) (VP (VBZ plays) (NP (NNP Elianti))) (. .)))
  '''
  def __init__(self, tree, order='pre'):
    self.tree = tree
    self.pos = [0]
    self.order = order

  def __iter__(self):
    return self

  def next(self):
    while True:
      if len(self.pos) == 0:
        raise StopIteration

      # For pre-order traversal, return nodes when first reached
      ans = None
      if self.order == 'pre' and self.pos[-1] == 0:
        ans = self.tree

      # Update internal state to point at the next node in the tree
      if self.pos[-1] < len(self.tree.subtrees):
        self.tree = self.tree.subtrees[self.pos[-1]]
        self.pos[-1] += 1
        self.pos.append(0)
      else:
        if self.order == 'post':
          ans = self.tree
        self.tree = self.tree.parent
        self.pos.pop()

      if ans is not None:
        return ans

class PSTree:
  '''Phrase Structure Tree

  >>> tree = tree_from_text("(ROOT (NP (NNP Newspaper)))")
  >>> print tree
  (ROOT (NP (NNP Newspaper)))
  >>> tree = tree_from_text("(ROOT (S (NP-SBJ (NNP Ms.) (NNP Haag) ) (VP (VBZ plays) (NP (NNP Elianti) )) (. .) ))")
  >>> print tree
  (ROOT (S (NP-SBJ (NNP Ms.) (NNP Haag)) (VP (VBZ plays) (NP (NNP Elianti))) (. .)))
  >>> print tree.word_yield()
  Ms. Haag plays Elianti .
  >>> tree = tree_from_text("(ROOT (NFP ...))")
  >>> print tree
  (ROOT (NFP ...))
  >>> tree.word_yield()
  '...'
  >>> tree = tree_from_text("(VP (VBD was) (VP (VBN named) (S (NP-SBJ (-NONE- *-1) ) (NP-PRD (NP (DT a) (JJ nonexecutive) (NN director) ) (PP (IN of) (NP (DT this) (JJ British) (JJ industrial) (NN conglomerate) ))))))")
  >>> print tree
  (VP (VBD was) (VP (VBN named) (S (NP-SBJ (-NONE- *-1)) (NP-PRD (NP (DT a) (JJ nonexecutive) (NN director)) (PP (IN of) (NP (DT this) (JJ British) (JJ industrial) (NN conglomerate)))))))
  >>> tree.word_yield()
  'was named *-1 a nonexecutive director of this British industrial conglomerate'
  '''
  def __init__(self, word=None, label=DEFAULT_LABEL, span=(0, 0), parent=None, subtrees=None):
    self.word = word
    self.label = label
    self.span = span
    self.wordspan = span
    self.parent = parent
    self.unique_id = None
    self.subtrees = []
    if subtrees is not None:
      self.subtrees = subtrees
      for subtree in subtrees:
        subtree.parent = self
  
  def __iter__(self):
    return TreeIterator(self, 'pre')
  
  def clone(self):
    ans = PSTree(self.word, self.label, self.span)
    for subtree in self.subtrees:
      subclone = subtree.clone()
      subclone.parent = ans
      ans.subtrees.append(subclone)
    return ans

  def is_terminal(self):
    '''Check if the tree has no children.'''
    return len(self.subtrees) == 0

  def is_trace(self):
    '''Check if this tree is the end of a trace.'''
    return self.label == TRACE_LABEL

  def is_punct(self):
    if self.label in {"IN", "TO", "RB", "AUX", "DT"} or self.word is None:
      return False
    if self.word in {"!", "#", "'", "''", "*", ",", "-", "--", ".", "...", ":", ";", "=", "?", "@", "\*", "\*\*", "`", "``"}:
      return True
    return False

  def is_conjunction(self):
    return self.label in {'CC', 'CONJP'}

  def root(self):
    '''Follow parents until a node is reached that has no parent.'''
    if self.parent is not None:
      return self.parent.root()
    else:
      return self

  def __repr__(self):
    '''Return a bracket notation style representation of the tree.'''
    # TODO: Shift this to str and add more field info
    ans = '('
    if self.is_trace():
      ans += TRACE_LABEL + ' ' + self.word
    elif self.is_terminal():
      ans += self.label + ' ' + self.word
    else:
      ans += self.label
    for subtree in self.subtrees:
      ans += ' ' + subtree.__repr__()
    ans += ')'
    return ans

  def set_unique_id(self, cur=0):
    '''Set a unique numerical ID for each node.'''
    self.unique_id = cur
    cur += 1
    for subtree in self.subtrees:
      cur = subtree.set_unique_id(cur)
    return cur

  def calculate_spans(self, left=0, wordleft=0):
    '''Update the spans for every node in this tree.'''
    right = left
    wordright = wordleft
    if self.is_terminal():
      if not self.is_trace():
        wordright += 1
      right += 1
    for subtree in self.subtrees:
      right, wordright = subtree.calculate_spans(right, wordright)
    self.span = (left, right)
    self.wordspan = (wordleft, wordright)
    return right, wordright

  def check_consistency(self):
    '''Check that the parents and spans are consistent with the tree
    structure.'''
    ans = True
    if self.is_terminal():
      if self.is_trace() and self.span[0] != self.span[1]:
        print "non-zero span at a terminal trace node"
        ans = False
      elif self.span[0] + 1 != self.span[1]:
        print "span changes by value other than 1 at non-trace terminal node"
        ans = False
    else:
      for i in xrange(len(self.subtrees)):
        subtree = self.subtrees[i]
        if subtree.parent != self:
          print "bad parent link"
          ans = False
        if i > 0 and self.subtrees[i - 1].span[1] != subtree.span[0]:
          print "Subtree spans don't match"
          ans = False
        ans = ans and subtree.check_consistency()
      if self.span != (self.subtrees[0].span[0], self.subtrees[-1].span[1]):
        print "Span doesn't match subtree spans"
        ans = False
    return ans

  def production_list(self, ans=None):
    '''Get a list of productions as:
    (node label, node span, ((subtree1, end1), (subtree2, end2)...))'''
    if ans is None:
      ans = []
    if len(self.subtrees) > 0:
      cur = (self.label, self.span, tuple([(sub.label, sub.span[1]) for sub in self.subtrees]))
      ans.append(cur)
      for sub in self.subtrees:
        sub.production_list(ans)
    return ans

  def word_yield(self, span=None, as_list=False):
    '''Return the set of words at terminal nodes, either as a space separated
    string, or as a list.'''
    if self.is_terminal():
      if span is None or span[0] <= self.span[0] < span[1]:
        if self.word is None:
          return None
        if as_list:
          return [self.word]
        else:
          return self.word
      else:
        return None
    else:
      ans = []
      for subtree in self.subtrees:
        words = subtree.word_yield(span, as_list)
        if words is not None:
          if as_list:
            ans += words
          else:
            ans.append(words)
      if not as_list:
        ans = ' '.join(ans)
      return ans

  def node_dict(self, depth=0, node_dict=None):
    '''Get a dictionary of labelled nodes. Note that we use a dictionary to
    take into consideration unaries like (NP (NP ...))'''
    if node_dict is None:
      node_dict = defaultdict(lambda: [])
    for subtree in self.subtrees:
      subtree.node_dict(depth + 1, node_dict)
    node_dict[(self.label, self.span[0], self.span[1])].append(depth)
    return node_dict

  def get_nodes(self, request='all', start=-1, end=-1, node_list=None):
    '''Get the node(s) that have a given span.  Unspecified endpoints are
    treated as wildcards.  The request can be 'lowest', 'highest', or 'all'.
    For 'all', the list of nodes is in order from the highest first.'''
    if request not in ['highest', 'lowest', 'all']:
      raise Exception("%s is not a valid request" % str(request))
    if request == 'lowest' and start < 0 and end < 0:
      raise Exception("Lowest is not well defined when both ends are wildcards")

    if request == 'all' and node_list is None:
      node_list = []
    if request == 'highest':
      if self.span[0] == start or start < 0:
        if self.span[1] == end or end < 0:
          return self

    for subtree in self.subtrees:
      # Skip subtrees with no overlapping range
      if 0 < end <= subtree.span[0] or subtree.span[1] < start:
        continue
      ans = subtree.get_nodes(request, start, end, node_list)
      if ans is not None and request != 'all':
        return ans

    if self.span[0] == start or start < 0:
      if self.span[1] == end or end < 0:
        if request == 'lowest':
          return self
        elif request == 'all':
          node_list.insert(0, self)
          return node_list
    if request == 'all':
      return node_list
    else:
      return None

  def get_spanning_nodes(self, start, end, node_list=None):
    return_ans = False
    if node_list is None:
      return_ans = True
      node_list = []

    if self.span[0] == start and self.span[1] <= end:
      node_list.append(self)
      start = self.span[1]
    else:
      for subtree in self.subtrees:
        if subtree.span[1] < start:
          continue
        start = subtree.get_spanning_nodes(start, end, node_list)
        if start == end:
          break

    if return_ans:
      if start == end:
        return node_list
      else:
        return None
    else:
      return start

def tree_from_text(text, allow_empty_labels=False, allow_empty_words=False):
  '''Construct a PSTree from the provided string, which is assumed to represent
  a tree with nested round brackets.  Nodes are labeled by the text between the
  open bracket and the next space (possibly an empty string).  Words are the
  text after that space and before the close bracket.'''
  root = None
  cur = None
  pos = 0
  word = ''
  for char in text:
    # Consume random text up to the first '('
    if cur is None:
      if char == '(':
        root = PSTree()
        cur = root
      continue

    if char == '(':
      word = word.strip()
      if cur.label is DEFAULT_LABEL:
        if len(word) == 0 and not allow_empty_labels:
          raise Exception("Empty label found\n%s" % text)
        cur.label = word
        word = ''
      if word != '':
        raise Exception("Stray '%s' while processing\n%s" % (word, text))
      sub = PSTree()
      cur.subtrees.append(sub)
      sub.parent = cur
      cur = sub
    elif char == ')':
      word = word.strip()
      if word != '':
        if len(word) == 0 and not allow_empty_words:
          raise Exception("Empty word found\n%s" % text)
        cur.word = word
        word = ''
        cur.span = (pos, pos + 1)
        pos += 1
      else:
        cur.span = (cur.subtrees[0].span[0], cur.subtrees[-1].span[1])
      cur = cur.parent
    elif char == ' ':
      if cur.label is DEFAULT_LABEL:
        if len(word) == 0 and not allow_empty_labels:
          raise Exception("Empty label found\n%s" % text)
        cur.label = word
        word = ''
      else:
        word += char
    else:
      word += char
  if cur is not None:
    raise Exception("Text did not include complete tree\n%s" % text)
  root.calculate_spans()
  return root

def get_reference(text, sep='-'):
  # Work backwards through it
  cur = []
  for char in text[::-1]:
    if char in string.digits:
      cur.append(char)
    elif char == sep:
      if len(cur) == 0:
        return None
      else:
        return ''.join(cur)
    elif char in {'-', '='}:
      cur = []
  return None

def tree_from_shp(text, allow_empty_labels=False, allow_empty_words=False):
  '''Construct a PSTree from the provided split head grammar parse.'''
  # Create spine defined non-terminals
  nodes = {}
  sent_len = 0
  nulls_to_add = []
  trace_edges = {}
  for line in text:
    # Extract data
    parts = line.strip().split()
    num = int(parts[0])
    sent_len = max(sent_len, num)
    word = parts[1]
    POS = parts[2]
    spine = parts[3]

    tnum = int(parts[4])
    tlabel = parts[5]
    structural_edge = (tnum, tlabel)

    for i in range(6, len(parts), 6):
      tnum = int(parts[i])
      tlabel = parts[i + 1]
      tlabel += "_T" if parts[i + 2] == 'T' else "_F"
      slabel = parts[i + 3]
      slabel += "_T" if parts[i + 4] == 'T' else "_F"
      trace = parts[i + 5]
      if trace.endswith("_chain"):
        trace = trace[:-6]
      edge = (tnum, tlabel, num, slabel, trace)
      if num not in trace_edges:
        trace_edges[num] = []
      trace_edges[num].append(edge)

    # Make spine
    prev_node = PSTree(word, POS, (num - 1, num))
    symbol_count = defaultdict(lambda: 0)
    node_map = {
      POS +"_0_F": prev_node
    }
    nodes[num] = (word, POS, spine, structural_edge, node_map)
    if spine != '_':
      cur = []
      depth = 0
      null_node = []
      for char in spine +"_":
        if char == ')':
          cur.append(")")
          depth -= 1
        elif char == '(':
          cur.append("(")
          depth += 1
        elif char == '_':
          if depth != 0:
            cur.append(" ")
          else:
            # Generate new node
            if '(' not in cur:
              symbol = ''.join(cur)
              node = PSTree(None, symbol, (num - 1, num), None, [prev_node])
              prev_node.parent = node
              count = symbol_count[symbol +"_F"]
              symbol_count[symbol +"_F"] += 1
              node_map["{}_{}_F".format(symbol, count)] = node
              prev_node = node
              if len(null_node) > 0:
                for null in null_node:
                  nulls_to_add.append((null, node))
                null_node = []
            else:
              modified = []
              to_add = []
              for char2 in cur:
                to_add.append(char2)
                if char2 == ')':
                  if not ' ' in to_add and len(to_add) > 1:
                    to_add[0] += '-NONE- '
                  modified.append(''.join(to_add))
                  to_add = []
                elif char2 == '(':
                  modified.append(''.join(to_add[:-1]))
                  to_add = ['(']
              modified = ''.join(modified)
              null = tree_from_text(modified)
              for node in null:
                symbol = node.label
                count = symbol_count[symbol +"_T"]
                symbol_count[symbol +"_T"] += 1
                node_map["{}_{}_T".format(symbol, count)] = node
              null_node.append(null)
            cur = []
        else:
          cur.append(char)

  # Link together with structural edges, ensuring proper nesting
  root = None
  decisions = {}
  for length in xrange(1, len(nodes) + 1):
    for pos in nodes:
      word, POS, spine, structural_edge, node_map = nodes[pos]
      tnum, tlabel = structural_edge
      if abs(pos - tnum) == length:
        # Identify the top of the spine at this position
        top = None
        for name in node_map:
          if node_map[name].parent is None and name.endswith("_F"):
            top = node_map[name]
        # Attach it to the target
        if tnum == 0:
          root = PSTree(None, "ROOT", (0, sent_len), None, [top])
          top.parent = root
        else:
          left = tnum > pos
          target_info = nodes[tnum]
          tlabel += "_F"
          tnode = target_info[4][tlabel]
          # Check that linking to this node won't cause a crossing
          for opos in xrange(min(pos, tnum) + 1, max(pos, tnum)):
              if opos in decisions:
                  onode = decisions[opos]
                  # See if this is above our node
                  pnode = tnode
                  while pnode is not None:
                    if pnode == onode:
                      tnode = onode
                      break
                    pnode = pnode.parent

          top.parent = tnode
          if left:
            tnode.subtrees.insert(0, top)
          else:
            tnode.subtrees.append(top)
          decisions[pos] = tnode

  # TODO: gather stats on the original trees
  for null, node in nulls_to_add:
    pos = len(node.subtrees)
    if null.label.startswith("ADVP"):
      pass
    elif node.label.startswith("SBAR") or null.label.startswith("NP-SBJ"):
      pos = 0
    else:
      npos = 0
      for subnode in node.subtrees:
        npos += 1
        if subnode.label.startswith("VB"):
          pos = npos
        if subnode.label.startswith("LST"):
          pos = npos + 1
          break
        if subnode.label.startswith("VP"):
          pos = max(0, npos - 1)
          break
    if len(node.subtrees) > pos:
      sub_label = node.subtrees[pos].label
      if sub_label.startswith("-L") or sub_label in {",", "LST", "``"}:
        pos += 1
    node.subtrees.insert(pos, null)
    null.parent = node

  root.calculate_spans()

  # Add traces
  max_index = 0
  for num in trace_edges:
    for tnum, tlabel, snum, slabel, trace in trace_edges[num]:
      snode = nodes[snum][4][slabel]
      tnode = nodes[tnum][4][tlabel]
      if tlabel.endswith("_T") and slabel.endswith("_T"):
        tmp = snode
        snode = tnode
        tnode = tmp
      if trace == '=':
        identity = get_reference(tnode.label)
        if identity is None:
          max_index += 1
          identity = max_index
          tnode.label += "-{}".format(identity)
  
        snode.label += "={}".format(identity)
      else:
        identity = get_reference(snode.label)
        if identity is None:
          max_index += 1
          identity = max_index
          snode.label += "-{}".format(identity)
  
        tnode.subtrees[0].word += "-{}".format(identity)

  return root

def clone_and_find(nodes):
  '''Clone the tree these nodes are in and finds the equivalent nodes in the
  new tree.'''
  return_list = True
  if type(nodes) != type([]):
    return_list = False
    nodes = [nodes]

  # Note the paths to the nodes
  paths = []
  for node in nodes:
    paths.append([])
    tree = node
    while tree.parent is not None:
      prev = tree
      tree = tree.parent
      paths[-1].append(tree.subtrees.index(prev))

  # Duplicate and follow the path back to the equivalent node
  ntree = nodes[0].root().clone()
  ans = []
  for path in paths:
    tree = ntree
    for index in path[::-1]:
      tree = tree.subtrees[index]
    ans.append(tree)
  if return_list:
    return ans
  else:
    return ans[0]


if __name__ == '__main__':
  print "Running doctest"
  import doctest
  doctest.testmod()

