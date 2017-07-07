#!/usr/bin/env python3

import sys
import string

def map_token(token, pos):
    # Lowercase
    token = token.lower()
    # Alternatives:
    # - Just change the case of the first letter of the first word
    # - Previous, plus leaving it as is if we've seen this capitalised elsewhere

    # Replace numbers
    letters = []
    for letter in token:
        if letter in string.digits:
            if len(letters) > 0 and letters[-1] == '0':
                continue
            else:
                letters.append("0")
        else:
            letters.append(letter)
    token = ''.join(letters)

    # Do the suffix trick?

    return token

for filename in sys.argv[1:]:
    out = open(filename + ".clean", "w")

    for line in open(filename):
        mapping = True
        tokens = []
        for pos, token in enumerate(line.strip().split()):
            if token == "|||" or (not mapping):
                tokens.append(token)
            else:
                tokens.append(map_token(token, pos))
        print(" ".join(tokens), file=out)
    out.close()
