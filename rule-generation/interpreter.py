#!/usr/bin/env python3

import sys

from feasible_parents import valid_ijx_states, valid_ij_states

spans = [
  "=._B",
  "=_.B",
  "-_.L",
  "=_.L",
  "-._L",
  "=._L",
  "-_.R",
  "=_.R",
  "-._R",
  "=._R",
  "-._N",
  "-_.N",
  "=._N",
  "=_.N",
  "__.X",
  "_._X",
  "___I",
]

class Span:
  def __init__(self, fields):
    self.text = ' '.join(fields)
    self.span = ''.join(''.join(fields[0].split('{')).split('}'))
    self.span_single = '\\dotuline' in self.span
    self.span_multi = '\\uwave' in self.span
    self.span_dotL = self.span.startswith("\\cdotp")
    self.span_dotR = self.span.endswith("\\cdotp")
    self.span = ''.join(self.span.split('\\cdotp'))
    self.span = ''.join(self.span.split('\\dotuline'))
    self.span = ''.join(self.span.split('\\uwave'))
    self.positions = 2
    self.pos2symbol = {
      'i': fields[1][0],
      'j': fields[1][1]
    }
    self.symbol2pos = {
      fields[1][0]: 'i',
      fields[1][1]: 'j'
    }
    if len(fields[1]) > 2:
      self.pos2symbol['x'] = fields[1][2]
      self.symbol2pos[fields[1][2]] = 'x'
      self.positions += 1
    self.edges = {}
    self.direct_edges = []
    self.edges['i'] = self.add_edges(fields[2], ['ij', 'ix'])
    self.edges['j'] = self.add_edges(fields[3], ['ji', 'jx'])
    if self.positions == 3:
      self.edges['x'] = self.add_edges(fields[4], ['xi', 'xj'])

  def apply_mapping(self, parent_instance):
    ans = []
    for pos in parent_instance:
      pos = pos.lower()
      ans.append(self.pos2symbol.get(pos, pos))
    return ''.join(ans)

  def has_x(self):
    return self.positions == 3

  def add_edges(self, token, direct):
    bar = False
    case = None
    ans = []
    for char in token:
      if char == ' ':
        pass
      elif char in 'I J X K L F . j':
        case = char
        if bar:
          case = '-'+ case
        elif case in 'I J X K L':
          self.direct_edges.append(direct[len(ans)])
        ans.append(case)
      elif char == '{':
        bar = True
      elif char == '}':
        bar = False
    if len(ans) < self.positions - 1 and '-' not in ans[0]:
      ans.append(ans[0])
    return ans

  def __repr__(self):
    span = self.span
    if self.span_dotL:
      span = '.' + span
      if self.span_single:
        span += '-'
      elif self.span_multi:
        span += '='
    elif self.span_dotR:
      span = span + '.'
      if self.span_single:
        span = '-' + span
      elif self.span_multi:
        span = '=' + span
    else:
      if self.span_single:
        span = '-' + span
      elif self.span_multi:
        span = '=' + span
    ans = "Span({}, ".format(span)
    ans += "i:{i}, j:{j}".format_map(self.pos2symbol)
    ans += ", x:{}".format(self.pos2symbol['x']) if 'x' in self.pos2symbol else ""
    ans += ", {}".format(self.edges)
    ans += ", {}".format(self.direct_edges)
    ans += ")"
    return ans

edge_adding = { # 'child parent'
  'ij': (0, [('xi', 'xj'), ('jx', 'ix')]),
  'ix': (1, [('ji', 'jx'), ('xj', 'ij')]),
  'ji': (2, [('xj', 'xi'), ('ix', 'jx')]),
  'jx': (3, [('ij', 'ix'), ('xi', 'ji')]),
  'xi': (4, [('jx', 'ji'), ('ij', 'xj')]),
  'xj': (5, [('ix', 'ij'), ('ji', 'xi')]),
}

class Edge:
  def __init__(self, fields):
    self.parent = fields[1]
    self.child = fields[2]

  def __repr__(self):
    return "Edge(parent:{}, child:{})".format(self.parent, self.child)

  def add_to_parents(self, parents):
    nparents = list(parents)
    info = edge_adding[self.child + self.parent]
    nparents[info[0]] = self.parent.upper()
    for present, create in info[1]:
      present_pos = edge_adding[present][0]
      create_pos = edge_adding[create][0]
      if nparents[present_pos] != 'F' and nparents[create_pos] == 'F':
        nparents[create_pos] = create[1]
    return ''.join(nparents)

def check(goal, items, edges):
  ### Checking point related properties ###

  # Determine the order of points and check that they form a continuous span
  point_seq = {}
  rev_seq = {}
  ordered_seq = []
  for item in items:
    left = item.pos2symbol['i']
    right = item.pos2symbol['j']
    if left in point_seq:
      print("Two items have spans that overlap")
      return False
    point_seq[left] = right
    rev_seq[right] = left
  ends = [point_seq[x] for x in point_seq if point_seq[x] not in point_seq]
  if len(ends) != 1:
    print("No valid endpoint of the span")
    return False
  end = ends[0]
  start = rev_seq[end]
  while start in rev_seq:
    start = rev_seq[start]
  cur = start
  while cur in point_seq:
    ordered_seq.append(cur)
    cur = point_seq[cur]
  ordered_seq.append(end)

  # Check that the endpoints match the rule
  if start != goal.pos2symbol['i'] or end != goal.pos2symbol['j']:
    print("Endpoints of items don't match span of goal")
    return False

  # Check none of the intermediate span points are used in the goal
  goal_x = None
  if goal.has_x():
    goal_x = goal.pos2symbol['x']
    for pos in point_seq.keys():
      if pos == goal_x or goal_x == end:
        print("Goal external point is within the item span range")
        return False

  # Check that any exteral points are either equal to an intermediate point or
  # equal to the goal's external point.
  for item in items:
    if item.has_x():
      x = item.pos2symbol['x']
      if x not in point_seq and x != end:
        if goal_x is None or goal_x != x:
          print("Item's external point does not match anythin else")
          return False

  # For items with a defined external point side, does that position match up
  for item in items:
    if item.span_dotL or item.span_dotR:
      left = item.span_dotL
      x = item.pos2symbol['x']
      if x == goal_x:
        if not (goal.span_dotL or goal.span_dotR):
          continue
        if (left and goal.span_dotL) or (not left and goal.span_dotR):
          continue
        print("Item and goal disagree on positioning of external point")
        return False
      seen_span = False
      for cur in ordered_seq:
        if cur == x:
          if left == seen_span:
            print("External point for item is in position inconsistent with dot")
            return False
        elif cur == item.pos2symbol['i']:
          seen_span = True

  ### Checking parent related properties ###

  # Direct arcs - check the same pair is not used in multiple cases
  direct = set()
  def add_and_check(parent, child):
    if (parent, child) in direct or (child, parent) in direct:
      print("A pair ({}, {}) are connected multiple times".format(parent, child))
      return True
    direct.add((parent, child))
    return False
  for edge in edges:
    if add_and_check(edge.parent, edge.child):
      return False
  for item in items:
    for edge in item.direct_edges:
      if add_and_check(item.pos2symbol[edge[0]], item.pos2symbol[edge[1]]):
        return False

  # Result matches connectivity of parts
  def check_connectivity(symbol, state):
    satisfied = [False for i in state]
    for index, val in enumerate(state):
      if val == '.' or val[0] == '-' or val == 'F':
        satisfied[index] = True
    for item in items:
      if symbol in item.symbol2pos:
        pos = item.symbol2pos[symbol]
        pos_edges = item.edges[pos]
        if state[0] == 'F':
          if pos_edges[0] != 'F':
            print("Connectivity: Goal {} is F, item is {}".format(symbol, pos_edges))
            return True
        elif state[0] == '-F':
          print("Goal connectivity of -F not currently checked")
          return True
        for index, val in enumerate(state):
          for oval in pos_edges:
            if val[0] == '-' and oval == val[1]:
              print("Goal has {} for {}, item has {}".format(val, symbol, oval))
              return True
            elif val == oval:
              satisfied[index] = True
            elif val == 'j':
              if oval == 'J':
                print("Goal has an indirect j edge, but item has a direct edge")
                return True
              elif oval in 'I X L K' or oval == '-F':
                satisfied[index] = True
    for edge in edges:
      if edge.child == symbol:
        parent = edge.parent.upper()
        for index, val in enumerate(state):
          if val[0] == '-' and parent == val[1]:
            print("Goal has {} for {}, edge has {}".format(val, symbol, parent))
            return True
          elif val == parent:
            satisfied[index] = True
    for requirement in satisfied:
      if not requirement:
        print("Connectivity failed {}".format(satisfied))
        return True

  if check_connectivity(goal.pos2symbol['i'], goal.edges['i']):
    return False
  if check_connectivity(goal.pos2symbol['j'], goal.edges['j']):
    return False
  if goal.has_x():
    if check_connectivity(goal.pos2symbol['x'], goal.edges['x']):
      return False

  return True

#####################################################
# Input

rules = []
cur_goal = None
in_eq = False
is_goal = False
for raw_line in sys.stdin:
  line = raw_line.strip()
  cur_items = []
  cur_edges = []
  if line == '\\begin{flalign*}' and 'Interpreter ignore' not in line:
    in_eq = True
  elif line == '\\end{flalign*}':
    in_eq = False
  elif in_eq:
    line = ''.join(line.split())
    line = ' '.join(line.split('\\;'))
    line = ' '.join(line.split('\\quad'))
    is_goal = '\\max' in line
    cur = []
    for token in line.split():
      if '[' in token:
        cur = token.split('[')
      elif len(cur) > 0:
        if ']' in token:
          cur.append(token.split(']')[0])
          # Deal with the [i i+1] and [i+1 j] cases
          if '+' in cur[2]:
            cur[1] += '+'
            cur.pop(2)
          elif '+' in cur[1]:
            cur[2] = '+'+cur[2]
            cur.pop(1)

          if cur[0] == 'A':
            cur_edges.append(Edge(cur))
          else:
            item = Span(cur)
            if is_goal:
              cur_goal = item
            else:
              cur_items.append(item)
          cur = []
        else:
          cur.append(token)
    if len(cur_items) > 0:
      rules.append((cur_goal, cur_items, cur_edges))

#####################################################
# Check

if 'check' in sys.argv:
  for goal, items, edges in rules:
    if not check(goal, items, edges):
      print("INVALID RULE!")
    print(goal)
    for edge in edges:
      print("    "+ str(edge))
    for item in items:
      print("    "+ str(item))
    print()

#####################################################
# Codegen

def check_parents(instance, pattern, num):
###  print("checking", instance, pattern, num)
  if num >= len(pattern):
    return True
  if pattern[0] == '-F':
    return instance != 'FF'

  pattern = pattern[num]
  instance = instance[num]
  if pattern == '.':
    return True
  elif '-' in pattern:
    return instance == 'F' or instance.islower()
  elif pattern == 'F':
    return instance == 'F'
  elif pattern == instance:
    return True
  else:
    return instance.isupper() and instance != 'F'

def span_consistent_with_template(span, item):
  if item.span_dotL and span[1] != '.':
    return False
  if item.span_dotR and span[2] != '.':
    return False
  if item.span_single and span[0] != '-':
    return False
  if item.span_multi and span[0] != '=':
    return False
  if span[-1] not in item.span:
    return False
  return True

def spans_consistent_with_rule(goal, items):
  for parent_span in spans:
    if span_consistent_with_template(parent_span, goal):
      for left_span in spans:
        if span_consistent_with_template(left_span, items[0]):
          if len(items) == 1:
            yield (parent_span, [left_span])
            continue
          for right_span in spans:
            if span_consistent_with_template(right_span, items[-1]):
              if len(items) == 2:
                yield (parent_span, [left_span, right_span])
                continue
              for middle_span in spans:
                if span_consistent_with_template(middle_span, items[1]):
                  yield (parent_span, [left_span, middle_span, right_span])

def states_consistent_with_item(item):
  to_consider = valid_ijx_states
  if 'I' in item.span:
    to_consider = valid_ij_states

  for option in to_consider:
###    print(option, 'for', item)
    parents = option[6]
    if not check_parents(parents[:2], item.edges['i'], 0):
      continue
    if not check_parents(parents[2:4], item.edges['j'], 0):
      continue
    if item.has_x():
      if not check_parents(parents[:2], item.edges['i'], 1):
        continue
      if not check_parents(parents[2:4], item.edges['j'], 1):
        continue
      if not check_parents(parents[4:], item.edges['x'], 0):
        continue
      if not check_parents(parents[4:], item.edges['x'], 1):
        continue
###    print("yes")
    yield option

def states_consistent_with_rule_parts(items):
  for option1 in states_consistent_with_item(items[0]):
    if len(items) == 1:
      yield [option1]
      continue
    for option2 in states_consistent_with_item(items[1]):
      if len(items) == 2:
        yield [option1, option2]
        continue
      for option3 in states_consistent_with_item(items[2]):
        yield [option1, option2, option3]

def get_and_set(symbol_to_id, item):
  if item not in symbol_to_id:
    symbol_to_id[item] = len(symbol_to_id)
  return symbol_to_id[item]

def update_pos_in_mat(adj_mat, child, parent, state):
  if adj_mat[child][parent] == "F" or (state.isupper() and state != "F"):
    adj_mat[child][parent] = state

def make_graph(states, items):
  adj_mat = [ # First index is child, second is parent
    ["F", "F", "F", "F"],
    ["F", "F", "F", "F"],
    ["F", "F", "F", "F"],
    ["F", "F", "F", "F"],
  ]
  symbol_to_id = {}
  for state, item in zip(states, items):
    left = get_and_set(symbol_to_id, item.pos2symbol['i'])
    right = get_and_set(symbol_to_id, item.pos2symbol['j'])
    x = None
    if item.has_x():
      x = get_and_set(symbol_to_id, item.pos2symbol['x'])
      update_pos_in_mat(adj_mat, left, x, state[6][1])
      update_pos_in_mat(adj_mat, right, x, state[6][3])
      update_pos_in_mat(adj_mat, x, left, state[6][4])
      update_pos_in_mat(adj_mat, x, right, state[6][5])
    update_pos_in_mat(adj_mat, left, right, state[6][0])
    update_pos_in_mat(adj_mat, right, left, state[6][2])
###    print(left, right, x, state[6])
###  print(adj_mat)
  return (symbol_to_id, adj_mat)

def check_for_cycles(states, items, symbol_to_id, adj_mat):
  # BFS
  cycle = False
  for start in range(4):
    queue = [start]
    done = set()
    while len(queue) > 0:
      cur = queue.pop()
      done.add(cur)
      for i in range(4):
        if adj_mat[cur][i] != 'F':
          if i not in done:
            queue.append(i)
          if i == start:
            cycle = True

  # Explicitly checking all possible cycles
  if adj_mat[0][0] != 'F' or adj_mat[1][1] != 'F' or adj_mat[2][2] != 'F' or adj_mat[3][3] != 'F':
    if not cycle:
      raise Exception("Cycle finding methods disagree 1 "+ str(adj_mat))
  orders = [
    [0, 1],
    [0, 2],
    [0, 3],
    [1, 2],
    [1, 3],
    [2, 3],
    [0, 1, 2],
    [0, 2, 1],
    [0, 1, 3],
    [0, 3, 1],
    [0, 2, 3],
    [0, 3, 2],
    [1, 2, 3],
    [1, 3, 2],
    [0, 1, 2, 3],
    [0, 1, 3, 2],
    [0, 2, 1, 3],
    [0, 2, 3, 1],
    [0, 3, 1, 2],
    [0, 3, 2, 1]
  ]
  for order in orders:
    if adj_mat[order[-1]][order[0]] != 'F' and adj_mat[order[0]][order[1]] != 'F':
      if len(order) == 2:
        if not cycle:
          raise Exception("Cycle finding methods disagree 2 "+ str(adj_mat))
      elif adj_mat[order[1]][order[2]] != 'F':
        if len(order) == 3:
          if not cycle:
            raise Exception("Cycle finding methods disagree 3 "+ str(adj_mat))
        elif adj_mat[order[2]][order[3]] != 'F':
          if not cycle:
            raise Exception("Cycle finding methods disagree 4 "+ str(adj_mat))
  return not cycle

def check_closing(states, items, symbol_to_id, adj_mat):
  # Make sure that every point being closed off will have a parent that is not
  # being closed off.
  ordered_seq = [items[0].pos2symbol['i']]
  for item in items:
    ordered_seq.append(item.pos2symbol['j'])

  for pos in ordered_seq[1:-1]:
    queue = [symbol_to_id[pos]]
    found_edge = False
    done = set()
    while len(queue) > 0:
      cur = queue.pop()
      done.add(cur)
      for i in range(4):
        if adj_mat[cur][i] != 'F' and i not in done:
          queue.append(i)
    for symbol in symbol_to_id:
      if symbol_to_id[symbol] in done:
        if symbol not in ordered_seq or symbol == ordered_seq[0] or symbol == ordered_seq[-1]:
          found_edge = True
    if not found_edge:
###      print(pos, ordered_seq, done, symbol_to_id[pos], adj_mat)
      return False
  return True

def check_dot_location(items, goal_span, item_spans):
  for item_span, item in zip(item_spans, items):
    if 'x' in item.symbol2pos:
      if item_span[1] != goal_span[1] or item_span[2] != goal_span[2]:
        return False
  return True

def adj_mat_to_parents(adj_mat, symbol_to_id):
  connectivity = [
    [" ", " ", " ", " "],
    [" ", " ", " ", " "],
    [" ", " ", " ", " "],
    [" ", " ", " ", " "],
  ]
  for start in range(4):
    queue = [start]
    done = {start}
    while len(queue) > 0:
      cur = queue.pop()
      for i in range(4):
        if adj_mat[cur][i] != 'F':
          if adj_mat[cur][i].upper() == adj_mat[cur][i]:
            connectivity[cur][i] = "-"
          else:
            connectivity[cur][i] = "_"
          if cur != start:
            connectivity[start][i] = "_"
          if i not in done:
            queue.append(i)
            done.add(i)
  ans = ""
  for pair in ["ij", "ix", "ji", "jx", "xi", "xj"]:
    if 'x' in pair and 'x' not in symbol_to_id:
      ans += "_"
    else:
      pos0 = symbol_to_id[pair[0]]
      pos1 = symbol_to_id[pair[1]]
      if connectivity[pos0][pos1] == " ":
        ans += "F"
      elif connectivity[pos0][pos1] == "_":
        ans += pair[1]
      elif connectivity[pos0][pos1] == "-":
        ans += pair[1].upper()
###  print(" | ".join(["".join(row) for row in connectivity]) +"    "+ ans)
  return ans

def check_goal_parents(goal, parents):
  if not check_parents(parents[:2], goal.edges['i'], 0):
    return False
  if not check_parents(parents[2:4], goal.edges['j'], 0):
    return False
  if goal.has_x():
    if not check_parents(parents[:2], goal.edges['i'], 1):
      return False
    if not check_parents(parents[2:4], goal.edges['j'], 1):
      return False
    if not check_parents(parents[4:], goal.edges['x'], 0):
      return False
    if not check_parents(parents[4:], goal.edges['x'], 1):
      return False
  return True

def check_external(item_spans, states):
  for span, parents in zip(item_spans, states):
    parents = parents[6]
    if "X" in span:
      if parents[1].isupper() and parents[1] != 'F': pass
      elif parents[3].isupper() and parents[3] != 'F': pass
      elif parents[4].isupper() and parents[4] != 'F': pass
      elif parents[5].isupper() and parents[5] != 'F': pass
      else: return False
  return True

def states_consistent_with_rule(goal, items, goal_span, item_spans):
  for states in states_consistent_with_rule_parts(items):
###    print(states)
    symbol_to_id, adj_mat = make_graph(states, items)
    if not check_for_cycles(states, items, symbol_to_id, adj_mat):
###      if len(states) > 1:
###        print("cycle", states[0][6], states[1][6], items[0], items[1])
      continue
    if not check_closing(states, items, symbol_to_id, adj_mat):
###      if len(states) > 1:
###        print("unclosed", states[0][6], states[1][6], items[0], items[1])
      continue
    if not check_dot_location(items, goal_span, item_spans):
###      if len(states) > 1:
###        print("dot", states[0][6], states[1][6], items[0], items[1])
      continue
    if not check_external(item_spans, states):
      continue
    graph_only = False
    for pos in range(4):
      count = len([v for v in adj_mat[pos] if v != 'F'])
      if count > 1:
        graph_only = True
    goal_state = adj_mat_to_parents(adj_mat, symbol_to_id)
    if not check_goal_parents(goal, goal_state):
###      if len(states) > 1:
###        print("parents", states[0][6], states[1][6], items[0], items[1])
      continue
###    if len(states) > 1:
###      print("yes", states[0][6], states[1][6], items[0], items[1])
    yield (states, graph_only, goal_state)

def get_points_for_items(items):
  mapping = {}
  for pos, item in enumerate(items):
    if item.pos2symbol['i'] not in mapping:
      mapping[item.pos2symbol['i']] = 'i'+ str(pos)
    if item.pos2symbol['j'] not in mapping:
      mapping[item.pos2symbol['j']] = 'j'+ str(pos)
  if 'x' not in mapping:
    mapping['x'] = 'xP'

  points = []
  for item in items:
    if '+' in item.pos2symbol['j']:
      points.append('i+')
    elif item.has_x():
      points.append(mapping[item.pos2symbol['x']])
    else:
      points.append("__")
  return points

if 'codegen' in sys.argv:
  # Arcs
  # This generates lists that can be used to check allowable edges.
  # When adding arcs we will do as follows:
  #
  #   For each item in this cell:
  #      Get the relevant option from this list
  #      Add the arcs that are allowed by the list
  #   Repeat for items just created (so we can add a second arc)
  if 'arcs' in sys.argv:
    for span in spans:
      if 'I' in span:
        print('arc | _ ___I F_I___ | F_F___ | p: i | c: j | all | all')
        print('arc | _ ___I J_F___ | F_F___ | p: j | c: i | all | all')
        print('arc | _ ___I F_I___ | F_i___ | p: i | c: j | graph | all')
        print('arc | _ ___I J_F___ | j_F___ | p: j | c: i | graph | all')
      elif 'X' in span:
        if span[1] != '.':
          for state, graph_only in [
              ('F_F___', None),
              ('F_i___', 'xj'),
              ('j_F___', 'xi'),
              ('F_I___', 'xj'),
              ('J_F___', 'xi')
          ]:
            for arc, pos in [('jx', 3), ('ix', 1), ('xi', 4), ('xj', 5)]:
              graph = 'all' if arc != graph_only else 'graph'
              nparents = Edge(' '+ arc[1] + arc[0]).add_to_parents("F".join(state.split("_")))
              print('arc | _ ___I ' + nparents, end=' | ')
              print(state, end=' | ')
              print('p:', arc[1], end=' | ')
              print('c:', arc[0], end=' | ')
              print(graph + ' | 1ec')
      else:
        for goal, items, edges in rules:
          if len(edges) > 0 and span_consistent_with_template(span, items[0]):
            for options in states_consistent_with_rule_parts(items):
              if not check_dot_location(items, span, [span]):
                continue
              option = options[0]
              parents = options[0][6]
              item = items[0]

              goal_parents = edges[0].add_to_parents(parents)

              # Adding this arc must not create a cycle
              ij, ix, jx = option[:3]
              if edges[0].parent == 'i' and edges[0].child == 'j': ij = '->'
              if edges[0].parent == 'i' and edges[0].child == 'x': ix = '->'
              if edges[0].parent == 'j' and edges[0].child == 'i': ij = '<-'
              if edges[0].parent == 'j' and edges[0].child == 'x': jx = '->'
              if edges[0].parent == 'x' and edges[0].child == 'i': ix = '<-'
              if edges[0].parent == 'x' and edges[0].child == 'j': jx = '<-'
              # Check three step cycles
              if ij[1] == jx[1] == '>' and ix[0] == '<':
                continue
              if ij[0] == jx[0] == '<' and ix[1] == '>':
                continue
              # Check two step cycles
              if ('<' in ij and '>' in option[0]) or \
                  ('>' in ij and '<' in option[0]):
                continue
              if ('<' in ix and '>' in option[1]) or \
                  ('>' in ix and '<' in option[1]):
                continue
              if ('<' in jx and '>' in option[2]) or \
                  ('>' in jx and '<' in option[2]):
                continue

              # Also note, does this create a point with multiple parents
              graph_only = 'all'
              if (parents[0] != 'F' and parents[1] != 'F') or \
                  (parents[2] != 'F' and parents[3] != 'F') or \
                  (parents[4] != 'F' and parents[5] != 'F') or \
                  (edges[0].child == 'i' and len(option[3]) > 0) or \
                  (edges[0].child == 'j' and len(option[4]) > 0) or \
                  (edges[0].child == 'x' and len(option[5]) > 0):
                graph_only = 'graph'

              goal_multi = "_"
              if goal.span_single:
                goal_multi = '-'
              elif goal.span_multi:
                goal_multi = '='
              else:
                goal_multi = span[0]

              print('arc', end=' | ')
              print(goal_multi, span, goal_parents, end=' | ')
              print(parents, end=' | ')
              print('p:', edges[0].parent, end=' | ')
              print('c:', edges[0].child, end=' | ')
              print(graph_only + ' | 1ec')

  # Binary and ternary
  if 'combinations' in sys.argv:
    for goal, items, edges in rules:
      if len(edges) == 0:
###        print(goal, '= sum of')
###        for item in items:
###          print("    "+ str(item))
        points = get_points_for_items(items)
        for goal_span, item_spans in spans_consistent_with_rule(goal, items):
###          print(goal_span, item_spans)
          for states, graph_only, goal_state in states_consistent_with_rule(goal, items, goal_span, item_spans):
            graph = 'graph' if graph_only else 'all'
            crossing = 'all'
            for span in item_spans:
              if 'I' not in span:
                crossing = '1ec'
            print('combination', end=' | ')
            print(goal_span +" "+ goal_state, end=' | ')
            print(' '.join(item_spans), end=' | ')
            print(' '.join([state[6] for state in states]), end=' | ')
            print(' '.join(points), end=' | ')
            print(graph, end=' | ')
            print(crossing)
###        print()
