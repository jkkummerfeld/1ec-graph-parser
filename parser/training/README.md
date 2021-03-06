# Training

The models are trained in several stages.
This saves time because the earlier stages can be used to prune during training for the later stages (though the parser is set up with an option to prevent pruning of the gold derivation during training).

In the sections below I have included the complete list of options I ran with to create the models used for the final experiments.
Some of these options are the same as the defaults, but I am including them here for completeness (and in case the defaults change in future).

NOTE - you will need to adjust the following filenames in the arguments below to suit your setup:

- Treebank files, train.txt and dev.txt
- POS tags, dev.pos.stanford

In addition, if you make changes to one of these you will need to change the others:

- Output prefixes, results.spine-tagger, results.spine, etc
- Model names (in the stage files), results.arc.model.gz, results.trace.model.gz, etc
- indexPrefix, these contain information about mappings from words and symbols to numbers. They are automatically generated by each stage and then used in the following stages

I tuned the various pruning cutoffs in the stage files (e.g. pruningRatioTrain) by looking at performance on the dev set.
The configuration provided here has verbosityTrain = 512, which means it will print information that can be used to figure out the right pruning cutoff.

Finally, when making design decisions I often erred on the side of using more RAM to get higher speed (caching, etc).
In each section below I mention how much RAM I used.

## Assumptions

If you want to train on different data you may break some assumptions in the code:

- There are at most 100 POS tag types
- Sentences have at most 253 words

The system may still run when these assumptions are violated (sorry...), but could behave strangely.
Number of arc types and spine types should be fine up to 2^32 (though memory will be issue before that point).
If there are more than 2^16 word types then some features may be miscalculated.

## Spine Classifier

Before implementing the tagger I had a simple classifier implemented, which is what I actually used to train the model.

- Used 14 Gb of RAM

```
-traintest
-arcFreqCutoff 0
-batchSize 10
-c 0.00001
-cachedInit False
-delta 1e-06
-devEvalFreq 39800
-earlyIterationStop 40000
-edgeHasSourceChild False
-edgeHasSourceGrandChild False
-edgeHasSourceGrandParent False
-edgeHasSourceParent False
-edgeHasTargetChild True
-edgeHasTargetGrandChild False
-edgeHasTargetGrandParent False
-edgeHasTargetParent False
-evalAutoPOS dev.pos.stanford
-evalGoldPOS False
-evalIn dev.txt
-featureDev FFFFFFFF
-featureFreqCutoff 0
-keepTraces True
-l2reg True
-learnerType primalSVM
-loadFactor 0.5
-logToStdout False
-lossWeightDiffS 2.0
-lossWeightDiffT 2.0
-lossWeightExtraS 1.0
-lossWeightExtraT 1.0
-lossWeightMissedS 1.0
-lossWeightMissedT 1.0
-lossWeightSpine 1.0
-maxEvalSents 999999
-maxLength 999
-maxTrainSents 40000
-mem 14
-mergeAllRareOrUnseen False
-minLength 0
-negativeFeaturePower -8
-newLocalArcFeatures False
-parallel True
-parseType psg
-prefix results.spine
-printIndexes True
-printModel True
-ruleStructuralFeatureOnly True
-shuffleTrainData True
-spineFeatAllButNull True
-spineFeatAllButTrace False
-spineFeatComplete True
-spineFeatParts False
-stageConfigs stage.spine-train.txt
-stepSize 0.01
-ternaryTraces False
-threads 0
-train true
-trainGoldPOS True
-trainIn train.txt
-trainIterations 8
-useBetweenFeatures True
```

## Trace Classifier

- Used 14 Gb of RAM

```
-traintest
-arcFreqCutoff 0
-batchSize 10
-c 0.00000001
-cachedInit False
-delta 1e-06
-devEvalFreq 39800
-earlyIterationStop 40000
-edgeHasSourceChild False
-edgeHasSourceGrandChild False
-edgeHasSourceGrandParent False
-edgeHasSourceParent False
-edgeHasTargetChild True
-edgeHasTargetGrandChild False
-edgeHasTargetGrandParent False
-edgeHasTargetParent False
-evalAutoPOS dev.pos.stanford
-evalGoldPOS False
-evalIn dev.txt
-featureDev FFFFFTF
-featureFreqCutoff 0
-keepTraces True
-l2reg False
-learnerType primalSVM
-loadFactor 0.5
-logToStdout False
-lossWeightDiffS 2.0
-lossWeightDiffT 2.0
-lossWeightExtraS 1.0
-lossWeightExtraT 1.0
-lossWeightMissedS 1.0
-lossWeightMissedT 1.0
-lossWeightSpine 1.0
-maxEvalSents 999999
-maxLength 999
-maxTrainSents 40000
-mem 14
-mergeAllRareOrUnseen False
-minLength 0
-negativeFeaturePower -10
-newLocalArcFeatures False
-parallel True
-parseType psg
-prefix results.trace
-printIndexes True
-printModel True
-ruleStructuralFeatureOnly True
-shuffleTrainData True
-spineFeatAllButNull False
-spineFeatAllButTrace False
-spineFeatComplete True
-spineFeatParts False
-stageConfigs stage.trace-train.txt
-stepSize 0.03
-ternaryTraces False
-threads 0
-train true
-trainGoldPOS True
-trainIn train.txt
-trainIterations 6
-useBetweenFeatures True
```

## Projective Arc Parser

- This is trained with 4 threads in the configuration below
- Used 29 Gb of RAM (and decreasing the number of threads will only slightly help this).

```
-traintest
-arcFreqCutoff 0
-batchSize 20
-c 0.00001
-cachedInit False
-delta 1e-06
-devEvalFreq 39830
-earlyIterationStop 40000
-edgeHasSourceChild False
-edgeHasSourceGrandChild False
-edgeHasSourceGrandParent False
-edgeHasSourceParent False
-edgeHasTargetChild True
-edgeHasTargetGrandChild False
-edgeHasTargetGrandParent False
-edgeHasTargetParent False
-evalAutoPOS dev.pos.stanford
-evalGoldPOS False
-evalIn dev.txt
-featureDev FFFFFFF
-featureFreqCutoff 0
-keepTraces False
-l2reg False
-learnerType primalSVM
-loadFactor 0.5
-logToStdout False
-lossWeightDiffS 2.0
-lossWeightExtraS 1.0
-lossWeightMissedS 1.0
-lossWeightSpine 1.0
-maxEvalSents 999999
-maxLength 999
-maxTrainSents 40000
-mem 29
-mergeAllRareOrUnseen False
-minLength 0
-negativeFeaturePower -14
-newLocalArcFeatures False
-parallel True
-parseType psg
-prefix results.arc-proj
-printIndexes True
-printModel True
-ruleStructuralFeatureOnly True
-shuffleTrainData True
-spineFeatAllButNull False
-spineFeatAllButTrace False
-spineFeatComplete True
-spineFeatParts False
-stageConfigs stage.arc-train.txt
-stepSize 0.005
-threads 4
-train true
-trainGoldPOS True
-trainIn train.txt
-trainIterations 4
-useBetweenFeatures True
```

## One-endpoint Crossing Arc Parser

- This is trained with 3 threads in the configuration below
- Used 29 Gb of RAM (and again, decreasing the number of threads will only slightly help this)

```
-traintest
-arcFreqCutoff 0
-batchSize 20
-c 0.000000001
-cachedInit False
-delta 1e-06
-devEvalFreq 39800
-earlyIterationStop 40000
-edgeHasSourceChild False
-edgeHasSourceGrandChild False
-edgeHasSourceGrandParent False
-edgeHasSourceParent False
-edgeHasTargetChild True
-edgeHasTargetGrandChild False
-edgeHasTargetGrandParent False
-edgeHasTargetParent False
-evalAutoPOS dev.pos.stanford
-evalGoldPOS False
-evalIn dev.txt
-featureDev FFFFFFTFF
-featureFreqCutoff 0
-indexPrefix results.arc-proj.index.
-keepTraces True
-l2reg False
-learnerType primalSVM
-loadFactor 0.5
-logToStdout False
-lossWeightDiffS 2.0
-lossWeightDiffT 2.0
-lossWeightExtraS 1.0
-lossWeightExtraT 1.0
-lossWeightMissedS 1.0
-lossWeightMissedT 1.0
-lossWeightSpine 1.0
-maxEvalSents 999999
-maxLength 999
-maxTrainSents 40000
-mem 29
-mergeAllRareOrUnseen False
-minLength 0
-negativeFeaturePower -8
-newLocalArcFeatures False
-parallel True
-parseType psg
-prefix results.arc1ec
-printIndexes True
-printModel True
-ruleStructuralFeatureOnly True
-shuffleTrainData True
-spineFeatAllButNull True
-spineFeatAllButTrace False
-spineFeatComplete True
-spineFeatParts False
-stageConfigs stage.arc-run.txt,stage.arc1ec-train.txt
-stepSize 0.03
-ternaryTraces False
-threads 3
-train true
-trainGoldPOS True
-trainIn train.txt
-trainIterations 1
-useBetweenFeatures True
```

## One-endpoint Crossing Full Parser

- Used 48 Gb of RAM

```
-traintest
-arcFreqCutoff 0
-batchSize 10
-c 0.000000001
-cachedInit False
-delta 1e-06
-devEvalFreq 19915
-earlyIterationStop 40000
-edgeHasSourceChild False
-edgeHasSourceGrandChild False
-edgeHasSourceGrandParent False
-edgeHasSourceParent False
-edgeHasTargetChild True
-edgeHasTargetGrandChild False
-edgeHasTargetGrandParent False
-edgeHasTargetParent False
-evalAutoPOS dev.pos.stanford
-evalGoldPOS False
-evalIn dev.txt
-featureDev FFFFFFTFF
-featureFreqCutoff 0
-indexPrefix results.arc1ec.index.
-keepTraces True
-l2reg False
-learnerType primalSVM
-loadFactor 0.5
-logToStdout False
-lossWeightDiffS 2.0
-lossWeightDiffT 2.0
-lossWeightExtraS 1.0
-lossWeightExtraT 1.0
-lossWeightMissedS 1.0
-lossWeightMissedT 1.0
-lossWeightSpine 1.0
-maxEvalSents 999999
-maxLength 999
-maxTrainSents 40000
-mem 48
-mergeAllRareOrUnseen False
-minLength 0
-negativeFeaturePower -2
-newLocalArcFeatures True
-parallel True
-parseType psg
-prefix results.full
-printIndexes True
-printModel True
-ruleStructuralFeatureOnly True
-shuffleTrainData True
-spineFeatAllButNull True
-spineFeatAllButTrace False
-spineFeatComplete True
-spineFeatParts False
-stageConfigs stage.arc-run.txt,stage.arc1ec-run.txt,stage.full-train.txt
-stepSize 0.01
-ternaryTraces False
-threads 6
-train true
-trainGoldPOS True
-trainIn train.txt
-trainIterations 3
-useBetweenFeatures True
```

## Spine Tagger

See the tagger directory.
