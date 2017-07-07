// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Set}

import edu.berkeley.nlp.graphparser.Log._

class ArcChart(
  stage: Stage,
  chartAbove: Chart,
  traceChart: Chart,
  spineChart: Chart,
  tokens: Vector[String],
  tags: Vector[String],
  sentenceID: Int,
  goldEdges: UnboxedArrayBuffer,
  goldSpines: ArrayBuffer[Int],
  goldStates: Set[(Int, Int, Int, Int, Int)],
  lossType: LossType.Value,
  doNotPruneGoldArcs: Boolean,
  doNotPruneGoldSpines: Boolean,
  pruningRatio: Double,
  pruningRank: Double,
  verbosity: Int,
  training: Boolean
) extends Chart(stage, chartAbove, traceChart, spineChart, tokens, tags, sentenceID, goldEdges, goldSpines, goldStates, lossType, doNotPruneGoldArcs, doNotPruneGoldSpines, pruningRatio, pruningRank, verbosity, training) {
  import Chart._
  import stage.model._
  import stage._

  override val thisClass = "ArcChart."+ stage.name

  override def minSplitGet(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = curSpan00 + 1
  override def maxSplitGet(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = curSpan01 - 1
  override def minSplitUpdate(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = { }
  override def maxSplitUpdate(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = { }

  override def checkCoarseState(
    state: Int, lSpine: Int, rSpine: Int, xSpine: Int, span: (Int, Int),
    child: Int, parent: Int, arg: Int, stateNoArg: Int, gold: Boolean,
    init: Boolean, arcSecondStage: Boolean = false, arcCreation: Int = 0,
    structural: Boolean = false, chain: Boolean = false
  ) = {
    coarseArcRatio = -1
    if (chartAbove == null || init) true
    else if (gold && doNotPruneGoldArcs && doNotPruneGoldSpines) {
      if (check(VAdding, verbosity)) logln(s"  is gold, not pruned")
      true
    } else if (
      (! chartAbove.stage.doCrossing) &&
      (
        Chart.crossingFromState(state) != CrossingState.Interval ||
        Chart.noStructuralEdgesFromState(state) ||
        ( child >= 0 &&
          ( (! Chart.noParentsFromState(stateNoArg)) || (! structural) )
        )
      )
    ) {
      true
    } else if (! chartAbove.model.argIndex.contains(arg)) {
      if (check(VAdding, verbosity))
        logln(s"  coarse arg $arg ($structural) is unknown")
      if (! structural) true
      else {
        if (gold) {
          logln(s"Lost gold: checkCoarseState0 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage")
        }
        false
      }
    } else if (child < 0) {
      val loc = chartAbove.spanToCell(span)
      val spine = chartAbove.model.NULL_SPINE
      val hash = chartAbove.stateHash(state, spine, spine, 0)
      val subbeamID = getSubbeamID(chartAbove.stage.subbeamType, span, state,
        spine, spine, 0, arcCreation)
      val pos = chartAbove.posForHash(loc, subbeamID, hash)
      if (pos < 0) {
        if (check(VAdding, verbosity)) logln(s"  coarse does not exist, $pos")
        if (gold) {
          logln(s"Lost gold: checkCoarseState1 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage $arcCreation $state")
        }
        false
      } else {
        val insideScore = chartAbove.insideForPos(loc, pos)
        val outsideScore = chartAbove.outsideForPos(loc, pos)
        val ans = chartAbove.checkPruningRatio( insideScore + outsideScore )
        if (!ans && gold) {
          logln(s"Lost gold: checkCoarseState2 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage $arcCreation ${insideScore + outsideScore} v ${chartAbove.curMinMarginal} / ${chartAbove.scoreRange}")
        }
        ans
      }
    } else {
      val spine = chartAbove.model.NULL_SPINE
      val hash = chartAbove.stateHash(state, spine, spine, 0)
      val subbeam = getSubbeamID(chartAbove.stage.subbeamType, span, state,
        spine, spine, 0, if (arcSecondStage) 2 else 1)
      val loc = chartAbove.spanToCell(span)
      val dist = span._2 - span._1
      val pos = chartAbove.posForHash(loc, subbeam, hash)
      if (pos < 0) {
        if (check(VAdding, verbosity))
          logln(s"  coarse does not exist, $pos, $state")
        if (gold) {
          logln(s"Lost gold: checkCoarseState3 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage $subbeam $hash $loc - $pos")
        }
        false
      } else {
        // Get outside from above it and inside from the predecessor below it
        val arcInside = chartAbove.insideForPos(loc, pos)
        val arcOutside = chartAbove.outsideForPos(loc, pos)

        // Get the arc scores
        val oldArg =
          if (structural) chartAbove.argSForPos(loc, pos)
          else if (chain) chartAbove.argCForPos(loc, pos)
          else chartAbove.argTForPos(loc, pos)
///        logln(s"Checking $oldArg $structural $chain for $span " +
///          stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
///          s" $child $parent $arg $arcSecondStage $subbeam $hash $loc - $pos")
        val oldArcScore = chartAbove.scoreArc(child, parent, NULL_SPINE,
          NULL_SPINE, oldArg, dist, false, LossType.ZERO, false).toFloat
        val arcScore = chartAbove.scoreArc(child, parent, NULL_SPINE,
          NULL_SPINE, arg, dist, false, LossType.ZERO, false).toFloat

        // Check against the ratio cutoff
        val total = arcScore - oldArcScore + arcInside + arcOutside
        val ratio = chartAbove.calcPruningRatio(total)
        val ans = chartAbove.checkPruningRatio(total)
        if (!ans && gold) {
          logln(s"Lost gold: checkCoarseState4 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage $total ($arcScore $arcInside $arcOutside -$oldArcScore) v ${chartAbove.curMinMarginal} / ${chartAbove.scoreRange}")
        }
        coarseArcRatio = ratio
        ans
///        Log.logln(s"Queried for arc and got $ratio (v $minRatio) fomr $total = $arcScore + $arcInside + $arcOutside compared to ${chartAbove.curMaxMarginal}")
      }
    }
  }
  override def checkCoarseSpine(
    pos: Int, spine: Int, gold: Boolean
  ) = true

  override val chartScoreCache = new LongDoubleMap(Config.loadFactor,
    sentenceLength * sentenceLength * model.argIndex.size * 2 / 100)
  override def scoreArc(
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) = {
    val cacheID =
      (arg.toLong << 17) +
      (childIndex.toLong << 9) +
      (parentIndex.toLong << 1) +
      (if (gold && doNotPruneGoldArcs) 1 else 0)

    val cur = chartScoreCache.getAndNote(cacheID)
    val ans =
      if (cur == chartScoreCache.NO_VALUE || countOnGet != null) {
        val score = getArcScore(tokenIDs, tokenFeatureIDs, tagIDs, childIndex,
          parentIndex, NULL_SPINE, NULL_SPINE, arg, length, gold, scoreCache,
          surfaceFeatureLists, countOnGet, useModelCache, edgeSpace)
        chartScoreCache.putNoted(score)
        score
      } else cur
///    if (check(VParseExtraction, verbosity))
///      Log.logln(s"scoreArc trying $cacheID for $childIndex $parentIndex $childSpine $parentSpine $arg $length $gold ${countOnGet == null} with $cur $ans $sentenceID $useModelCache")

    ans + super.scoreArc(childIndex, parentIndex, childSpine, parentSpine,
      arg, length, gold, lossType, goldHere, countOnGet)
  }

  val bestArcCache = Array.ofDim[((Int, Boolean, Double), (Int, Boolean, Double), (Int, Boolean, Double))](sentenceLength * sentenceLength * 4)
  override def findBestArc(
    span: (Int, Int), arc: (String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value]),
    nxSpine: Int, ext: Int, crossing: CrossingState.Value,
    secondStage: Boolean, outside: Boolean,
    arcScoresStructural: ArrayBuffer[Double],
    arcScoresOther: ArrayBuffer[Double], child: Int, parent: Int, cSpine: Int,
    pSpine: Int, parentIsRoot: Boolean, stateSisGold: Boolean,
    stateOisGold: Boolean, stateS: Int, stateO: Int
  ) = {
    if (outside && check(VMarginals, verbosity)) {
      super.findBestArc(span, arc, nxSpine, ext, crossing,
        secondStage, outside, arcScoresStructural, arcScoresOther, child,
        parent, cSpine, pSpine, parentIsRoot, stateSisGold, stateOisGold,
        stateS, stateO)
    } else {
      val cachePos =
        (child * sentenceLength + parent) * 4 +
        (if (stateSisGold && doNotPruneGoldArcs) 1 else 0) +
        (if (stateOisGold && doNotPruneGoldArcs) 2 else 0)
      if (bestArcCache(cachePos) == null) {
        val best =
          super.findBestArc(span, arc, nxSpine, ext, crossing, secondStage,
            outside, arcScoresStructural, arcScoresOther, child, parent,
            cSpine, pSpine, parentIsRoot, stateSisGold, stateOisGold, stateS,
            stateO)
        bestArcCache(cachePos) = best
        best
      } else bestArcCache(cachePos)
    }
  }

  override def scoreSpine(
    wIndex: Int, spine: Int, gold: Boolean, lossType: LossType.Value,
    countOnGet: (Int, Double) => Unit = null
  ) = 0.0

  override def insertDerivation(
    terminals: ArrayBuffer[(Int, Int)],
    arcs: ArrayBuffer[(Int, Int, Int)],
    insertingGold: Boolean = false
  ) = super.insertDerivation(
    terminals.map(v => (v._1, NULL_SPINE)),
    arcs,
    insertingGold
  )

  override def getSpineOptions(position: Int) = NULL_SPINE_SET

  override def addPreterminals = {
    spineScoresMap(0).put(NULL_SPINE, 0.0)
    spineScores(0).append((NULL_SPINE, 0.0f, true))
    for (position <- 1 until sentenceLength) {
      val span = (position - 1, position)
      spineScoresMap(position).put(NULL_SPINE, 0.0)
      spineScores(position).append((NULL_SPINE, 0.0f, true))
      makeInitFromState(span, NULL_SPINE, NULL_SPINE, 0.0f, false,
        true, false)
    }
  }

  override def addSpineTopOptions = {
    var start = 0
    while (start < sentenceLength) {
      var end = start + 1
      while (end < sentenceLength) {
        val span = (start, end)
        val dist = end - start
        setCurCell(span)
        while (next) {
          if (argS != model.NonArg) {
            // Inside score from item below, and outside score from this item
            val inScore = insideForPos(curCell0, curPtr02)
            val outScore = outsideScore
            val (l, r, x) = (curSpan00, curSpan01, externalPos)
            // Note, order matters here, as the lr and rl edges are always
            // added first (and so we may see them twice, once on their own,
            // and once with another edge)
            val (child, parent, childSpine, parentSpine) =
              if (lxArgState == ArgState.Direct) (l, x, lSpine, xSpine)
              else if (rxArgState == ArgState.Direct) (r, x, rSpine, xSpine)
              else if (xlArgState == ArgState.Direct) (x, l, xSpine, lSpine)
              else if (xrArgState == ArgState.Direct) (x, r, xSpine, rSpine)
              else if (lrArgState == ArgState.Direct) (l, r, lSpine, rSpine)
              else if (rlArgState == ArgState.Direct) (r, l, rSpine, lSpine)
              else throw new Exception("No valid arc found")
            val parentIsRoot = (end == sentenceLength - 1 && parent == end)
            labelGen.prepare(tagIDs(child), tagIDs(parent), childSpine,
              parentSpine, span, sentenceLength, parentIsRoot,
              arcPassConstraint)
            while (labelGen.hasNext) {
              val arg = labelGen.next
              if (model.isTreeEdge(arg)) {
                val score = inScore + outScore + scoreArc(child, parent,
                  childSpine, parentSpine, arg, dist, false, LossType.ZERO,
                  false)
                if (checkPruningRatio(score)) {
///                if (serialisableChartData != null)
///                  serialisableChartData.spineTopOptions(child).add(model.getChildSymbolForArg(arg))
                  spineTopOptions(child).add(model.getChildSymbolForArg(arg))
                }
              }
            }
          }
        }
        end += 1
      }
      start += 1
    }
  }
}

class ArcParser(
  stage: Stage,
  coarseParser: Option[Parser],
  traceParser: Option[Parser],
  spineParser: Option[Parser],
  pruningRatio: Double,
  pruningRank: Double,
  verbosity: Int,
  doNotPruneGoldArcs: Boolean,
  doNotPruneGoldSpines: Boolean,
  training: Boolean
) extends Parser(stage, coarseParser, traceParser, spineParser, pruningRatio, pruningRank, verbosity, doNotPruneGoldArcs, doNotPruneGoldSpines, training) {
  override val thisClass = "ArcParser."+ stage.name

  override def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldSpines: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    new ArcChart(stage, coarseChart, traceChart, spineChart, tokens, tags,
      sentenceID, goldEdges, goldSpines, goldStates, lossType,
      doNotPruneGoldArcs, doNotPruneGoldSpines, pruningRatio, pruningRank,
      verbosity, training)
  }

  override def calculateMatching(
    gterminals: ArrayBuffer[(Int, Int)], garcs: ArrayBuffer[(Int, Int, Int)],
    aterminals: ArrayBuffer[(Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)]
  ) : Double = aarcs.intersect(garcs).length

  override def calculateLoss(
    gterminals: ArrayBuffer[(Int, Int)], garcs: ArrayBuffer[(Int, Int, Int)],
    aterminals: ArrayBuffer[(Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    lossType match {
      case LossType.ZERO => 0.0
      case LossType.P => throw new Exception("Not implemented yet")
      case LossType.R => throw new Exception("Not implemented yet")
      case LossType.HAMMING => calculateArcHammingLoss(garcs, aarcs)
    }
  }

  override def calculateTotal(
    terminals: ArrayBuffer[(Int, Int)], arcs: ArrayBuffer[(Int, Int, Int)]
  ) : Int = arcs.length

///    if (Config.cacheWriteArc) {
///      Config.cachePrefixWrite.foreach{ prefix =>
///        fineChart.serialiseScores
///        fineChart.serialisableChartData.minMarginal = fineChart.curMinMarginal
///        fineChart.serialisableChartData.maxMarginal = fineChart.curMaxMarginal
///        CachedChartData.save(prefix, "Arc", fineChart.serialisableChartData)
///      }
///    }
}

