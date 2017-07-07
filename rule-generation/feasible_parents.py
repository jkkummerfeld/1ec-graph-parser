#!/usr/bin/env python3

edge_options = ['<_', '_>', '<-', '->', '__']

valid_ijx_states = []
valid_ij_states = []

# i - j
for ij in edge_options:
    ix = None
    jx = None
    # Determine parentage
    i = set()
    j = set()
    x = set()
    # Direct values
    if ij == '<-': i.add('J')
    elif ij == '->': j.add('I')
    elif ij == '<_': i.add('j')
    elif ij == '_>': j.add('i')

    desc = ''
    if 'j' in i: desc += 'j'
    elif 'J' in i: desc += 'J'
    else: desc += 'F'
    desc += '_'
    if 'i' in j: desc += 'i'
    elif 'I' in j: desc += 'I'
    else: desc += 'F'
    desc += '_'
    desc += '__'

    valid_ij_states.append((ij, ix, jx, i, j, x, desc, False))

# i - j - x
for ij in edge_options:
    for ix in edge_options:
        for jx in edge_options:
            # Indirect values must match
            if ix[0] == '<' and jx[1] == '>' and ij[0] != '<': continue
            if ij[0] == '<' and jx[0] == '<' and ix[0] != '<': continue
            if ij[1] == '>' and ix[0] == '<' and jx[0] != '<': continue
            if jx[0] == '<' and ix[1] == '>' and ij[1] != '>': continue
            if ij[1] == '>' and jx[1] == '>' and ix[1] != '>': continue
            if ij[0] == '<' and ix[1] == '>' and jx[1] != '>': continue

            # Check for a cycle
            if ij[1] == jx[1] == '>' and ix[0] == '<':
                continue
            if ij[0] == jx[0] == '<' and ix[1] == '>':
                continue
            # Determine parentage
            i = set()
            j = set()
            x = set()
            # Direct values
            if ij == '<-': i.add('J')
            elif ij == '->': j.add('I')
            elif ij == '<_': i.add('j')
            elif ij == '_>': j.add('i')
            if ix == '<-': i.add('X')
            elif ix == '->': x.add('I')
            elif ix == '<_': i.add('x')
            elif ix == '_>': x.add('i')
            if jx == '<-': j.add('X')
            elif jx == '->': x.add('J')
            elif jx == '<_': j.add('x')
            elif jx == '_>': x.add('j')

            desc = ''
            if 'j' in i: desc += 'j'
            elif 'J' in i: desc += 'J'
            else: desc += 'F'
            if 'x' in i: desc += 'x'
            elif 'X' in i: desc += 'X'
            else: desc += 'F'
            if 'i' in j: desc += 'i'
            elif 'I' in j: desc += 'I'
            else: desc += 'F'
            if 'x' in j: desc += 'x'
            elif 'X' in j: desc += 'X'
            else: desc += 'F'
            if 'i' in x: desc += 'i'
            elif 'I' in x: desc += 'I'
            else: desc += 'F'
            if 'j' in x: desc += 'j'
            elif 'J' in x: desc += 'J'
            else: desc += 'F'

            graph_only = False
            for s in [i, j, x]:
                if len(s) > 1:
                    graph_only = True

            valid_ijx_states.append((ij, ix, jx, i, j, x, desc, graph_only))

if __name__ == '__main__':
    print("There are only 90 possible valid parent sets (assuming no cycles). This script enumerates them.")
    for option in valid_ij_states:
        print(option)

    for option in valid_ijx_states:
        print(option)
