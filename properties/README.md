# Miscellaneous Information and Utilities

### stats.md and trace-type-stats.md

These contain statistics about the training data regarding distributions of spine types and trace types.

### check_for_locked_chain.py

Reads parses in our representation from standard input and prints sentences when the parse contains a locked chain.
Example run:

```Shell
python3 check_for_locked_chain.py < example.shp
This is not a real parse .
```

The example file contains two made up parses, one of which contains a locked chain, and one of which does not.

### count_unique_dev_spines.py

This compares the spines used in two files and gives stats about those present in the second but not the first.
It can be run with:

```Shell
python3 count_unique_dev_spines.py known-parses.shp unknown-parses.shp
```

For example, when running with the training and development set it gives:

```Shell
python3 count_unique_dev_spines.py training.shp dev.shp
Dev sentences with all seen: (word, POS, spine) 503 29.6%
Dev sentences with all seen: (POS, spine) 1658 97.5%
Dev sentences with all seen: spine 1684 99.1%
Train spines (word, POS, spine) 95132
Train spines (POS, spine) 2723
Train spines spine 1305
Dev spines new (word, POS, spine) 2509
Dev spines new (POS, spine) 44
Dev spines new spine 18
Dev spines 40117
```

### graph_classifier.py

Reads parses in from standard input and prints out whether they are projective and if they contain an edge that goes from a word to itself (a self-arc).
Run as follows:

```Shell
python3 graph_classifier.py < parses.shp
```

