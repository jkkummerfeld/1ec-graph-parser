# One-Endpoint Crossing Graph Parser

This repository contains software to do several things related to syntax - parsing, format conversion, and evaluation.
For a full description of our parsing algorithm, proofs of its properties, and results on the standard metrics, see:

   - [Parsing with Traces: An O(n^4) Algorithm and a Structural Representation](https://arxiv.org/abs/1707.04221)
   Jonathan K. Kummerfeld and Dan Klein,
   TACL 2017

   - [Algorithms for Identifying Syntactic Errors and Parsing with Graph Structured Output](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-138.html)
   Jonathan K. Kummerfeld
   PhD Dissertation, UC Berkeley


If you use this code in your own work, please cite the TACL paper:

```TeX
@Article{Kummerfeld-Klein:2017:TACL,
  author    = {Jonathan K. Kummerfeld and Dan Klein},
  title     = {Parsing with Traces: An $O(n^4)$ Algorithm and a Structural Representation},
  journal   = {Transactions of the Association for Computational Linguistics},
  volume    = {5},
  year      = {2017},
  pages     = {},
  url       = {},
  software  = {https://github.com/jkkummerfeld/1ec-graph-parser},
}
```

# What is this work about?

In the Penn Treebank, parses have (1) a core projective tree structure and (2) traces that represent control structures, wh-movement and more.
However, most parsers and the standard evaluation metric (evalb) ignore the traces and all null elements, focusing entirely on the tree structure.
These aspects of syntax are excluded not because of disagreements regarding theory, but rather because of the computational challenge of including them.

This work is about a new inference algorithm that efficiently finds the maximum scoring graph parse for a sentence when using a first-order model.
Our approach involves (1) the algorithm, which builds upon the non-projective tree parsing algorithm of [Pitler et al. (2013)](https://www.aclweb.org/anthology/Q/Q13/Q13-1002.pdf), and (2) a syntactic representation, similar to several prior approaches, particularly [Carreras et al. (2008)](https://www.aclweb.org/anthology/W/W08/W08-2102.pdf).
For more details, see the paper and dissertation above.

This repository contains:

- A parser that implements our algorithm
- An evaluation for traces that processes standard PTB data, using the metric proposed by [Johnson (2002)](https://www.aclweb.org/anthology/P/P02/P02-1018.pdf)
- An evaluation for parses that processes our representation
- Tools to convert back-and-forth between our representation and the standard PTB representation
- Tools to generate the inference rules for our algorithm from the templates we define in the paper

All of the scripts are in python and can be run without downloading or installing any additional resources (note, all should work with python 2.7, only some work with python 3).
The parser is in scala, but can be downloaded as a jar that runs with java.
For pruning we use a tagger written in C++ that depends on several libraries (we provide a makefile and information about what else needs to be installed).

For detailed information on each component, see the README.md file in each directory.

# Reproducing the results in the paper

You will need:

- `test.tok` The test sentences, one tokenised sentence per line (not provided here)
- `test.gold.ptb` The test parses, in Penn Treebank format (not provided here)
- `wsj23.pos.stanford` Automatically generated POS tags for the test data, space separated, one sentence per line (e.g. we used output of [the Stanford POS tagger](https://nlp.stanford.edu/software/tagger.shtml) with default settings. The file we used is [here](https://www.dropbox.com/s/1abj1gux8ssohvs/wsj23.pos.stanford?dl=0))
- The parser and models: download [here (494 Mb)](https://www.dropbox.com/s/ufvr9bbtpvikxod/Kummerfeld-Klein-2017.parser.with-models.jar?dl=0) as a single jar
- Various scripts from this repository
- Either (1) The output of the spine tagger (available [here](https://www.dropbox.com/s/345ow8rifmaiae1/wsj23.tagged.data?dl=0)), or (2) the models for the spine tagger (available [here](https://www.dropbox.com/s/m0jjylo1mantz7q/Kummerfeld-Klein-2017.tagger.models.tgz?dl=0)), in which case the spine tagger needs to be built (see parser/nn-tagger/makefile).

Assuming you download the contents of this repo and have all of those file and put them in the same directory as this README.md file, you will need to:

1. Add IDs to the sentences
2. [Only if running the tagger] Simplify the text
3. [Only if running the tagger] Run the spine tagger
4. Run the parser
5. Fix parses where we failed to produce output
6. Convert to standard PTB
7. Evaluate

Each of these steps is one command below:

```Shell
python3 parser/nn-tagger/add_sent_id.py 200001 < test.tok > test-with-ids.tok
python3 parser/nn-tagger/pre-process.py < test-with-ids.tok > test-with-ids.tok.simple
parser/nn-tagger/spine-tagger -word-dict dict.words -tag-dict dict.tags -model model.params -test test.tok -prefix wsj23.tagged
./run-parser.sh test.parser test-with-ids.tok wsj23.pos.stanford wsj23.tagged.data
python3 evaluation/fix-no-tag.py < test.parser.auto_all.localStageFinal > test.parser.shp
python format-conversion/reprint_trees.py -i h -o o -e he < test.parser.shp > test.parser.ptb
python evaluation/ptb-trace-eval.py test.gold.ptb test.system.ptb | tail -n 1
python evaluation/ptb-trace-eval.py test.gold.ptb test.system.ptb --null_only | tail -n 1
```

The final two commands give performance on (1) traces and other null items, (2) nulls only.
The results should be:

```Shell
TODO
```

## Questions

If you find bugs or have questions about any of this software please either contact me (jkk@cs.berkeley.edu) or create an issue.
Thank you!

## TODO

- Adjust the parser to avoid the need for the fix-no-tag.py script (and even better, see if these cases are indicative of a programming bug)
- Add models that are trained on the CCK-style head rules
- Add example output / figures
