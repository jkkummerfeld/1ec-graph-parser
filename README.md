# 1ec-graph-parser

TODO:
- Put the tagger dictionaries and model somewhere
- Put the parser jar (big and small) and models somewhere (CCK and normal models)
- Parser documentation
- Instructions for reproducing paper experiments
- Example output / figures

This repository contains software to do several things related to syntax - parsing, format conversion, and evaluation.
For a full description of the algorithm, proofs of properties, and results on the standard metrics, see:

   - Parsing with Traces: An O(n^4) Algorithm and a Structural Representation
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

To work on constituency parses, our algorithm requires a modification of the parse representation.
We provide 

The subdirectories contain documentation of each component:

- parser, the one-endpoint crossing graph parser
- parser/nn-tagger, the neural network based spine tagger we use for pruning
- evaluation, scripts to evaluate (1) parses in our representation, (2) PTB parses with traces (implementing Johnson (2002)'s metric)
- format-conversion, scripts to convert between different ways of representing Penn Treebank style annotations
- rule-generation, scripts to generate the inference rules for our algorithm from the templates we define
- properties, scripts to check properties of parses, and some stats about the data

All of the scripts are in python and can be run without downloading or installing any additional resources (note, all should work with python 2.7, only some work with python 3).
The parser is in scala, but can be downloaded as a jar that runs with java.
The nn-tagger is written in C++ and depends on several libraries (we provide a makefile and information about what else needs to be installed).

For detailed information on each component, see the relevant directory.

# Reproducing the results in the paper

You will need:

- `test.tok` The test sentences, one tokenised sentence per line
- `test.ptb` The test parses, in Penn Treebank format
- `test.pos.auto` Automatically generated POS tags for the test data, space separated, one sentence per line [TODO here]
- The parser and models [TODO here]
- Various scripts from this repository
- Either (1) The output of the spine tagger [TODO here], or (2) the models for the spine tagger [TODO here]

The spine tagger needs to be built (see parser/nn-tagger/makefile), but otherwise these should all run out of the box (and if you downloaded the output of the spine tagger you do not need to run it).
We need to:

1. Add IDs to the sentences
2. Simplify the text
3. Run the spine tagger
4. Run the parser
5. Convert to standard PTB
6. Evaluate

Each of these steps is one command below:

```Shell
python3 parser/nn-tagger/add_sent_id.py 200001 < test.tok > test-with-ids.tok
python3 parser/nn-tagger/pre-process.py < test-with-ids.tok > test-with-ids.tok.simple
parser/nn-tagger/spine-tagger -word-dict dict.words -tag-dict dict.tags -model model.params -test test.tok -prefix test.tagger.out
./run-parser.sh test.parser test-with-ids.tok test.pos test.tagger.out.data
convert
evaluate
```

## Questions

If you find bugs or have questions about any of this software please either contact me (jkk@cs.berkeley.edu) or create an issue.
Thank you!
