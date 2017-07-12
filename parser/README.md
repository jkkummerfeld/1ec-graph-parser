This folder contains the first-order parser and a neural network tagger.

# Simple usage

The simplest way to run this code is to use the jar at (TODO) amd run:

```Shell
./run.sh example-output example-data.tok example-data.pos
```

If you wish to use the neural network spine pruner for faster and more accurate parsing, then
see the readme in its folder for directions, and run the parser like this:

```Shell
(run the spine tagger/pruner to generate example-spines.data)
./run.sh example-output example-data-with-ids.tok example-data.pos example-spines.data
```

# Options

The parser consists of a set of stages, which are run in order, with each stage used to prune options for the next stage.
Configuring a run involves setting options for the stages (which are saved in files, one file per stage) and then setting command line arguments.

## Stage options

Each stage file contains a series of options, one per line, specified with the name as the start of the line, followed by a space and the value (see run.sh for examples).
The options are:

- `arcPassConstraint` How to constrain what arc types we consider for a word pair, based on whether we saw this type with these tags in training (options are
`child, parent, both, either, neither`, default = `either`)
- `beamMaxLength` Maximum number of items on the beam
- `cubeDepth` Maximum number of items to generate in each cube for cube pruning
- `beamType` The data structure / algorithm used in the beam, though note, not all have been kept up to date with other development (options are `array, sorted, old, radix, compacity`, default = `array`)
- `doCrossing` Parse with crossing edges
- `doGraphs` Parse with graph structures (ie. multiple parents)
- `doNotPruneGoldInEval` Prevent pruning of the gold arcs and spines during evaluation (note, this means pruning by earlier passes, they could still fall off the beam)
- `doNotPruneGoldInTrain` Similarly for training
- `doNotPruneGoldArcsInEval` Similarly for arcs during evaluation
- `doNotPruneGoldArcsInTrain` Similarly for arcs during training
- `doNotPruneGoldSpinesInEval` Similarly for spines during evaluation
- `doNotPruneGoldSpinesInTrain` Similarly for spines during training
- `eval` Run evaluation with this stage (note, this means multiple stages can be evaluated in one run of the parser, though currently they are re-run rather than re-using earlier layers)
- `modelFilename` File containing the model
- `modelType` If a model is being trained, this is the type, currently only `discriminative` is implemented
- `name` The name of this pass, used to name output files and during logging
- `parserType` This determines what inference method is used. Options are `arc` for essentially dependency parsing, `local` for parsing with spines, `trace` for a trace scorer, `spine` for a spine scorer.
- `pruneWithExpScore` When true, the spine scorer will exponentiate the scores and normalise them, (default = `false`)
- `pruningRatioEval` The ratio at which to prune items based on their score from the preceding stage during evaluation
- `pruningRatioEvalInTrain` The same, but for evaluation runs during training
- `pruningRatioTrain` The same, but for training
- `requireTreeBasis` Enforce constraints so that there is a tree of structural edges in the final output
- `ruleCutoff` The minimum frequency a rule must have to be used
- `rules` File containing the inference rules to use (eg. the one we include has rules pruned to not include those not seen in training)
- `spineModel` Specify a file with a spine model to use to prune for this stage. To use a protocol buffer containing information from the nn-tagger write `external` followed by one or more protocol buffer data files
- `spineRatio` Cutoff ratio for pruning spines based on the spine model
- `spineRatioEval` Cutoff ratio for pruning spines during evaluation
- `subbeamType` See details below for why this exists, the options are `state` which will only consider structural properties and `spines` which will also consider the spines in the item
- `traceModel` Specify a file with a trace model to use to prune for this stage
- `traceRatio` Cutoff ratio for pruning traces based on the trace model
- `traceRatioEval`  Cutoff ratio for pruning traces during evaluation
- `verbosityEval` A number indicating what logging to do during evaluation
- `verbosityEvalInTrain` A number indicating what logging to do when evaluating during training
- `verbosityTrain` A number indicating what logging to do when training

For verbosity values, the number is a sum of values that each indicate a type of logging:

- 1, VParseExtraction, The final parse in terms of items
- 2, VGrammarExtraction, Currently not used
- 4, VDetailedStats, More information about cell and beam sizes
- 8, VMaking, Lots of detailed information during parsing when items are constructed to be inserted
- 16, VParseChart, The complete chart (all items in all cells)
- 32, VAdding, Lots of detailed information during parsing about what items are being added
- 64, VFeatures, Currently not used
- 128, VCellSizes, Number of items in each cell of the chart
- 256, VTrace, Subset of VAdding, only cases related to traces
- 512, VMarginals, Print the score for each item maxed over all structures
- 1024, VBeamStats, Information about beam usage
- 2048, VParseChartWithGold, Print information when an item is added but only because we are avoiding gold pruning

## Parser options
These are specified in a file with one line per option.

Input and General Configuration
- `evalAutoPOS` Filename for POS tags for evaluation data (specified as space separated POS tags, one sentence per line)
- `evalGoldPOS` Use gold POS tags during evaluation (overrides goldPOS)
- `evalIn` Filename for evaluation data
- `goldPOS` Use gold POS tags in both training and evaluation
- `indexPrefix` Filename prefix for the indices containing mappings for arguments, words, and non-terminals
- `keepTraces` Keep traces in data read in from file (default = `false`)
- `loadFactor` What fraction full hash tables should be before doubling in size
- `logToStdout` Print logging information to stdout, if false then it goes to a file `prefix.log` (default = `false`)
- `maxEvalSents` Maximum number of sentences to use in evaluation (default = `999999`)
- `maxLength` Maximum length of sentence (default = `999`)
- `maxTrainSents` Maximum number of sentences to use in training (default = `999999`)
- `minLength` Minimum length of sentences (default = `0`)
- `parallel` Run with parallelisation (default = `true`)
- `prefix` Prefix for files produced by this run (default = `experiment`)
- `printIndexes` Produce a text file with the indexes (default = `false`)
- `printInput` Print every line read from input data (default = `false`)
- `printModel` Print detailed model information in a text file (default = `false`)
- `printRuleCounts` Print how frequently each inference rule was used in training data (default = `false`)
- `stageConfigs` Comma separated filenames specifying the stages to use
- `threads` Number of threads to use during parsing
- `trainAutoPOS` Filename for POS tags for training data (specified as space separated POS tags, one sentence per line)
- `trainGoldPOS` Use gold POS tags during training (overrides goldPOS)
- `trainIn` Filename for training data
- `trainIterations` Number of passes through the data (default = `10`)
- `train` Do training (default = `false`)

Model
- `averagedLearning` For the perceptron, return the average weight vector across training (default = `false`)
- `batchSize` Number of parses to process before each update (default = `1`)
- `c` Regularization constant (default = `1e-6`)
- `delta` Double = 1e-6
- `devEvalFreq` Int = Int.MaxValue
- `distinguishSuffix` Boolean = true
- `earlyIterationStop` Finish an iteration before processing all sentences, this is useful for training a model that is aware of all edge types, but trains quickly (default = `Int.MaxValue`)
- `edgeHasSourceChild` This option and the next seven influence the representation of an edge. Each one adds a different piece of information to it. Source and target are the two ends of the edge, while child, parent, etc are the position of a non-terminal relative to the end point. It is possible for all types to exist because of traces (default = `false`)
- `edgeHasSourceGrandChild` (default = `false`)
- `edgeHasSourceGrandParent` (default = `false`)
- `edgeHasSourceParent` (default = `false`)
- `edgeHasTargetChild` (default = `false`)
- `edgeHasTargetGrandChild` (default = `false`)
- `edgeHasTargetGrandParent` (default = `false`)
- `edgeHasTargetParent` (default = `false`)
- `featureDev` A string of Fs and Ts used to switch on or off extra featurer types
- `featureFreqCutoff` Threshold for features to be considered rare (in which case they will be hashed rather than given a unique weight) (default = `0`)
- `includePOSInArg` Include the POS in the representation of the edge
- `includePOSInSpine` Include the POS in the representation of the spine
- `l2reg` Use L2 regularization instead of L1 (default = `false`)
- `learnerType` How to train, one of `counting, perceptron, primalSVM`
- `lossType` The loss function to use, either `zero` or `hamming`
- `lossWeightDiffS` These seven determine the relative weight of errors in the loss function. S and T refer to structural or trace edges. The types are Spines, or Extra, Missed, and Different edges. (default = `2.0`)
- `lossWeightDiffT` (default = `2.0`)
- `lossWeightExtraS` (default = `1.0`)
- `lossWeightExtraT` (default = `1.0`)
- `lossWeightMissedS` (default = `1.0`)
- `lossWeightMissedT` (default = `1.0`)
- `lossWeightSpine` (default = `1.0`)
- `lowercaseWordFeatures` Convert all words to lowercase (default = `false`)
- `mergeAllRareOrUnseen` Bucket rare and unseen words together for features (default = `true`)
- `mergeRareAndUnseenArcFeats` Bucket features together when the surface-arc pair has been seen rarely or never (default = `false`)
- `mergeRareAndUnseenSurfaceFeats` Bucket features together when the surface value has been rarely or never seen (default = `false`)
- `negativeFeaturePower` Make 2^(this value) times as man buckets for features as specific weights (default = `0`)
- `newLocalArcFeatures` Use the new arc features that are specific to the local stage (default = `false`)
- `newLocalBinaryFeatures` Use the new spine features that are specific to the local stage (default = `false`)
- `ruleStructuralFeatureOnly` Only use the complete rule representation, do not put weights on sub-parts of it (default = `true`)
- `shuffleTrainData` Before each training iteration, randomly sort the training data (default = `true`)
- `simpleSuffixMap` We have two ways to determine frequent word suffixes, this uses the simpler approach, see the code in model.scala for details (default = `false`)
- `suffixMapCutoff` How frequent a suffix has to be to avoid be used (default = `100`)
- `skipFinalOutside` Do not do the final outside pass, this saves time if we only want to max, but means we cannot calculate pruning thresholds (default = `false`)
- `spineFeatAllButNull` These four features determine what aspects of the spine get features assigned. This one is for a version of the spine without nulls (default = `false`)
- `spineFeatAllButTrace` Likewise, but without traces (default = `false`)
- `spineFeatComplete` Likewise, but for the complete spine (default = `true`)
- `spineFeatParts` Likewise, but for each individual element (default = `false`)
- `stepSize` The step size for learning (default = `0.001`)
- `ternaryTraces` Rather than scoring the entire trace, consider a version with some trace types merged (default = `false`)
- `useBetweenFeatures` Use features on the POS tags on words between the two ends of an edge (default = `true`)

# Design

### Beams

In each cell I have a beam, but that beam is actually composed of entirely
independent sub-beams, identifiable by a key (a Long). How should the key be
defined? Three options are:

1. Based on parents, item type, etc
2. Based on the spines
3. A hybrid

There is tension here between:

- Efficiency A, have as much on a single subbeam as possible, so states are pruned as soon as possible based on score.
- Efficiency B, have subbeams defined so that we can easily determine compatible pairs for binary composition (eg. put the spine for one side in, so we can filter those items based on the item they must combine with)
- Accuracy, maintain diversity in the overall beam by pushing things into more subbeams where they can live on until we know which is better.

Before having 1ec parsing I had a mixture of these. In the main beam I had a
version of (1), just whether it was created by the binary, init, or arc step. I
also had a second set of beams to be used only during the binary step, to get
efficiency B.

Assigning spines at the very bottom seems difficult, there just isn't much
information. We gain information as time goes on, because the spines constrain
the arcs, and the arcs have scores associated with them. I'm now switching to
having one beam per cell (rather than three, two of which were for binaries),
and defining subbeams based on the three spines.

### Scoring Spines

The spine scores are calculated at two points, during init and during Xterval
creation. They can be handled in several ways:

1 - Only include one side at the bottom. Not good, as different items get the
same scores and are pruned randomly by the beam.
2 - Include both sides at the bottom, then during combining, remove one of the
scores. Now we get more separation earlier on, but the combinations are not
monotonic, making cube pruning inexact.
3 - Include both sides, but with scores divided by two. Now we get some
separation, and cube pruning is exact.

Before 1ec I did (3), but that doesn't work for 1ec parsing as extra spines are introduced by Xtervals.
Instead I went with (2).
