#!/bin/bash

export MKL_NUM_THREADS=4

train="training-data"
dev="development-data"
prefix="example-nn-model"

../dynet/build/spine_tagger/spine-tagger \
  -train $train \
  -dev $dev \
  --dynet-mem 6000 \
  -layers 2 \
  -input-dim 128 \
  -hidden-dim 256 \
  -tag-hidden-dim 64 \
  -prefix $prefix > $prefix.log 2> $prefix.err

