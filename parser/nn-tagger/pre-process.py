#!/usr/bin/env python3

import sys
import string

def map_token(token, pos):
    # Lowercase
    token = token.lower()
    # Alternatives:
    # - Just change the case of the first letter of the first word
    # - Also, leave it as is if we've seen this capitalised elsewhere

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

for line in sys.stdin:
    if line.strip().startswith("# SentID"):
        print(line.strip())
        continue

    mapping = True
    tokens = []
    token_count = 0
    for pos, token in enumerate(line.strip().split()):
        if token == "|||" or (not mapping):
            tokens.append(token)
            mapping = False
        else:
            token_count += 1
            tokens.append(map_token(token, pos))
    
    # Add "_" tags if needed
    assert len(tokens) <= token_count * 2 + 1
    if len(tokens) < token_count * 2 + 1:
        if mapping:
            tokens.append("|||")
        while len(tokens) < token_count * 2 + 1:
            tokens.append("_;")

    print(" ".join(tokens))
