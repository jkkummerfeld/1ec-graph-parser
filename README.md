This repository contains software to do several things related to syntax - parsing, format conversion, and evaluation.
For a full definition of the parsing algorithm, proofs of its properties, and results on the standard metrics, see:

   - [Parsing with Traces: An O(n^4) Algorithm and a Structural Representation](http://aclweb.org/anthology/Q17-1031)
   Jonathan K. Kummerfeld and Dan Klein,
   TACL 2017

If you want to understand the algorithm, my thesis provides a better explanation (the TACL paper focuses on proving properties):

   - [Algorithms for Identifying Syntactic Errors and Parsing with Graph Structured Output](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-138.html)
   Jonathan K. Kummerfeld,
   PhD Dissertation, UC Berkeley

If you use this code in your own work, please cite the TACL paper:

```TeX
@article{kummerfeld-klein-2017-parsing,
    title = "Parsing with Traces: An ${O}(n^4)$ Algorithm and a Structural Representation",
    author = "Kummerfeld, Jonathan K.  and
      Klein, Dan",
    journal = "Transactions of the Association for Computational Linguistics",
    volume = "5",
    year = "2017",
    url = "https://www.aclweb.org/anthology/Q17-1031",
    doi = "10.1162/tacl_a_00072",
    pages = "441--454",
    software  = "https://github.com/jkkummerfeld/1ec-graph-parser",
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

- `wsj23.mrg` The test parses, in Penn Treebank format (not provided here).
- `wsj23.pos.stanford` Automatically generated POS tags for the test data, space separated, one sentence per line (e.g. we used output of [the Stanford POS tagger](https://nlp.stanford.edu/software/tagger.shtml) with default settings. The file we used is [here](https://www.dropbox.com/s/1abj1gux8ssohvs/wsj23.pos.stanford?dl=0)).
- The parser and models: download [here (494 Mb)](https://www.dropbox.com/s/ufvr9bbtpvikxod/Kummerfeld-Klein-2017.parser.with-models.jar?dl=0) as a single jar and unzip them in the current folder.
- Various scripts from this repository.
- Either (1) The output of the spine tagger (available [here](https://www.dropbox.com/s/345ow8rifmaiae1/wsj23.tagged.data?dl=0)), or (2) the models for the spine tagger (available [here](https://www.dropbox.com/s/m0jjylo1mantz7q/Kummerfeld-Klein-2017.tagger.models.tgz?dl=0)), in which case the spine tagger needs to be built (see parser/nn-tagger/makefile).

Assuming you download the contents of this repo and have all of those file and put them in the same directory as this README.md file, you will need to:

1. Get just the text from the gold parses
2. Add IDs to the sentences
3. [Only if running the tagger] Simplify the text
4. [Only if running the tagger] Run the spine tagger
5. Run the parser
6. Fix parses where we failed to produce output
7. Convert to standard PTB
8. Adjust the format of the gold data
9. Evaluate traces and nulls
10. Evaluse nulls only

Each of these steps is one command below:

```Shell
python2 format-conversion/reprint_trees.py -i p -o w < wsj23.mrg > test.tok
python3 parser/nn-tagger/add_sent_id.py 200001 < test.tok > test-with-ids.tok
python3 parser/nn-tagger/pre-process.py < test-with-ids.tok > test-with-ids.tok.simple
parser/nn-tagger/spine-tagger -word-dict dict.words -tag-dict dict.tags -model model.params -test test-with-ids.tok.simple -prefix wsj23.tagged
./run-parser.sh test.parser test-with-ids.tok wsj23.pos.stanford wsj23.tagged.data
python3 evaluation/fix-no-tag.py < test.parser.auto_all.localStageFinal > test.parser.shp
python2 format-conversion/reprint_trees.py -i h -o o -e he < test.parser.shp > test.parser.ptb
python2 format-conversion/reprint_trees.py -i p -o st -e fh < wsj23.mrg > test.gold.ptb
python2 evaluation/ptb-trace-eval.py test.gold.ptb test.parser.ptb | tail -n 1
python2 evaluation/ptb-trace-eval.py test.gold.ptb test.parser.ptb --null_only | tail -n 1
```

The final two commands give performance on (1) traces and other null items, (2) nulls only:

```
all 2621 3893 3524 74.3757094211 67.3259696892 70.6754752595
all 3153 3864 3522 89.5229982964 81.599378882 85.3777416734
```

These numbers are (count of matching, gold total, test total, precision, recall, f-score).

Note, the first line of results is slightly better than in the paper.
Prof. Yoshihide Kato identified a bug in my conversion script that meant correlates and remnants had the "=" and "-" indices reversed.
This bug has now been fixed.
The original values produced were:

```
all 2619 3893 3524 74.3189557321 67.2745954277 70.6215450991
```

## Questions

If you find bugs or have questions about any of this software please either contact me (jkk@cs.berkeley.edu) or create an issue.
Thank you!

## Misc Notes

The dynamic program definitions in the TACL paper and my thesis are slightly different:

- In response to reviewer feedback I changed the notation for parents.
- The problematic structures in my thesis got a name (locked chain) and a formal definition.
- The thesis discusses how to handle chains that are not locked, but I didn't include the necessary details in the dynamic program. The TACL version adds them, using the hat notation.

