# Evaluation

These scripts provide evaluation metrics for (1) nulls and traces in the standard PTB representation, (2) all structure in our representation.

## Preparing output

First, when the parser fails it produces a parse where every word is assigned 'NO_TAG'.
That is not a valid parse, so run this script to replace those parses with a single NP.

```Shell
python3 fix-no-tag.py < data.system.out > data.system.shp
```

Then use the script in the format conversion folder to convert to the ptb format (see more details in the README.md with that file):

```Shell
python reprint_trees.py -i h -o o -e he < data.system.shp > data.system.ptb
```

## Null and Trace Evaluation - ptb-trace-eval.py

Basic usage is:

```Shell
python ptb-trace-eval.py data.gold.ptb data.system.ptb
```

Where `data.gold` is the gold standard parses and `data.system` is the output of a parser.

First, the script will print information about how all the options are set for the run.
Then, for each parse it will print the gold and system parses, the information about nulls in each, reprint mistakes (extra or missing), and the scores.
If either parse contains a null then the parses are printed over multiple lines for easier reading (if there are none, they are printed on a single line).
Finally, at the end, performance on each type of null and trace type is printed, as well as an overall score.

### Arguments

There must be two filenames provided, the gold and system data (as shown above).
There are also a range of options:

- `--help` Show help and exit
- `--gold_format` Input format for the gold file: ptb (single or multiple lines per parse), ontonotes (one file) (default: `ptb`)
- `--test_format` Input format for the test file: ptb (single or multiple lines per parse), ontonotes (one file) (default: `ptb`)
- `--null_only` Whether to only score the null itself (not coindexation)
- `--keep_function_labels` Keep the -TMP part in NP-TMP (for example)
- `--homogenise_top_label` Adjust the parse to have ROOT above any S, FRAG, etc, replacing TOP, S1, or other alternative top symbols
- `--remove_trivial_unaries` Remove unaries that go from a label to itself, e.g.  (NP (NP (NNP it)))
- `--equivalent_labels <labea_pairls>` Pairs of labels to treat as equivalent, separated by commas (default: `ADVP:PRT`)
- `--equivalent_words <word_pairs>` Pairs of words to treat as equivalent, separated by commas (default: ` `)
- `--labels_to_remove <labels>` Remove nodes with the given labels. Keep subtrees, but remove parents that now have a span of size 0 (default: ```#&''&,&.&``&:```)

The most import of these is `--null_only`, which will ignore the coindexation of traces when calculating scores.

## Split Head Representation Evaluation - shp-eval.py

Usage is:

```Shell
python shp-eval.py data.gold.shp data.system.shp
```

Where `data.gold.shp` is the gold standard parses and `data.system.shp` is the output of a parser, both in our representation.

The program will print results for:

- Labeled spines
- Labeled edges
- Labeled traces
- Unlabeled edges
- Unlabeled traces

In each case the results show:

1. The number that matched
2. The total in the gold parse
3. The total in the system parse
4. Precision
5. Recall
6. F-Score (equally weighted precision and recall)

A line will also be printed with how many parses were processed.
