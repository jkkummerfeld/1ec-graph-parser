#!/bin/sh

prefix=$1

# Note, the IDs in the data must match the spine data
data=$2
data_pos=$3

arc_model=/model-arc-proj.gz
arc1ec_model=/model-arc-1ec.gz
trace_model=/model-trace.gz
full_model=/model-full.gz
inference_rules=/inference-rules.gz
index_prefix=/index-1ec.

# Special case - this can be either a model or a protocol buffer
spine_data=/model-spines.gz
spine_ratio="0.019"

if [ $# -ge 4 ] ; then
  spine_data="external $4"
  spine_ratio="0.747"
fi

echo "
beamMaxLength 10000
cubeDepth 100000
modelFilename ${arc_model}
name arcStage
parserType arc
pruningRatioEval 0.756
" > ${prefix}.arcStage

echo "
beamMaxLength 100
cubeDepth 500
doCrossing true
doGraphs true
modelFilename ${arc1ec_model}
name arcStage2
parserType arc
pruningRatioEval 0.734
rules ${inference_rules}
traceModel ${trace_model}
traceRankEval 0.067
traceRatioEval 0.011
" > ${prefix}.arcStage2

echo "
beamMaxLength 100
cubeDepth 10000
doCrossing True
doGraphs True
eval true
modelFilename ${full_model}
name localStageFinal
parserType local
rules ${inference_rules}
spineModel ${spine_data}
spineRatioEval ${spine_ratio}
subbeamType spines
traceModel ${trace_model}
traceRankEval 0.067
traceRatioEval 0.011
" > ${prefix}.localStageFinal

java -Xmx31g -jar Kummerfeld-Klein-2017.parser.with-models.jar edu.berkeley.nlp.graphparser.JKKMain \
  -runtest \
  -stageConfigs ${prefix}.arcStage,${prefix}.arcStage2,${prefix}.localStageFinal \
  -evalGoldPOS False \
  -evalAutoPOS ${data_pos} \
  -evalIn ${data} \
  -indexPrefix ${index_prefix} \
  -keepTraces True \
  -parallel False \
  -prefix ${prefix} \
  > ${prefix}.out 2> ${prefix}.err

