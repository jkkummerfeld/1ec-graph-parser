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

import edu.berkeley.nlp.graphparser.Hash._
import edu.berkeley.nlp.graphparser.Log._

class LocalChart(
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
  override val thisClass = "LocalChart."+ stage.name

  override val beamsL =
    if (stage.subbeamType != SubbeamType.SPINES) beams
    else Array.fill(numberOfCells)(stage.beamType(stage.beamMinLength,
      stage.beamMaxLength, stage.beamMinMultiple, stage.beamMaxMultiple,
      stage.beamMinFraction, stage.beamMaxFraction))
  override val beamsR =
    if (stage.subbeamType != SubbeamType.SPINES) beams
    else Array.fill(numberOfCells)(stage.beamType(stage.beamMinLength,
      stage.beamMaxLength, stage.beamMinMultiple, stage.beamMaxMultiple,
      stage.beamMinFraction, stage.beamMaxFraction))

  override def minSplitGet(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    require(false, "Need to change to suit new options")
    val right = (span2 << 2) + HasParent.id(parent)
    val ans = minSplit.get(right)
    if (ans < (span1 + 1) || ans > sentenceLength) span1 + 1
    else ans
  }
  override def maxSplitGet(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    require(false, "Need to change to suit new options")
    val left = (span1 << 2) + HasParent.id(parent)
    val ans = maxSplit.get(left)
    if (ans > (span2 - 1) || ans < 0) span2 - 1
    else ans
  }
  override def minSplitUpdate(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    require(false, "Need to change to suit new options")
    val right = (span2 << 2) + HasParent.id(parent)
    val prev = minSplit.get(right)
    if (prev > span1) minSplit.put(right, span1)
  }
  override def maxSplitUpdate(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    require(false, "Need to change to suit new options")
    val left = (span1 << 2) + HasParent.id(parent)
    val prev = maxSplit.get(left)
    if (prev < span2) maxSplit.put(left, span2)
  }

  override def checkCoarseState(
    state: Int, lSpine: Int, rSpine: Int, xSpine: Int, span: (Int, Int),
    child: Int, parent: Int, arg: Int, stateNoArg: Int, gold: Boolean,
    init: Boolean, arcSecondStage: Boolean = false, arcCreation: Int = 0,
    structural: Boolean = false, chain: Boolean = false
  ) = {
    if (check(VAdding, verbosity)) {
        logln(s" checkCoarseState $thisClass $sentenceID $span "+
          Chart.stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
          s" $child $parent $arg $stateNoArg $gold $init $arcSecondStage"+
          s" $arcCreation $structural $chain")
    }

    coarseArcRatio = -1.0
    if (chartAbove == null) true
    else if (gold && doNotPruneGoldArcs && doNotPruneGoldSpines) {
      if (check(VAdding, verbosity)) logln(s"  is gold, not pruned")
      true
    } else if (chartAbove.stage.parserType == ParserType.ARC && init) {
      true
    } else if (
      (! chartAbove.stage.doCrossing) &&
      (Chart.crossingFromState(state) != CrossingState.Interval ||
        (child != span._1 && child != span._2 && child >= 0) ||
        (parent != span._1 && parent != span._2 && parent >= 0))
    ) {
      true
    } else if (arg >= 0 && ! chartAbove.model.argIndex.contains(arg)) {
      if (check(VAdding, verbosity)) logln(s"  coarse arg is unknown")
      if (gold) {
        logln(s"Lost gold: checkCoarseState0 $thisClass $sentenceID $span "+
          Chart.stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
          s" $child $parent $arg $arcSecondStage")
      }
      false
    } else if (child < 0) {
      // TODO: Generalise to allow multiple local passes in a row
      require(chartAbove.stage.parserType == ParserType.ARC, "wrong spines")
      val loc = chartAbove.spanToCell(span)
      val spine = chartAbove.model.NULL_SPINE
      val extSpine =
        if (Chart.crossingFromState(state) == CrossingState.Interval) 0
        else 1
      val hash = chartAbove.stateHash(state, spine, spine, extSpine)
      val subbeamID = Chart.getSubbeamID(chartAbove.stage.subbeamType,
        span, state, spine, spine, extSpine, arcCreation)
      val pos = chartAbove.posForHash(loc, subbeamID, hash)
      if (pos < 0) {
        if (check(VAdding, verbosity)) logln(s"  coarse does not exist, $pos")
        if (gold) {
          logln(s"Lost gold: checkCoarseState1 $thisClass $sentenceID $span "+
            Chart.stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $extSpine $arcCreation $arcSecondStage $loc $hash $subbeamID $pos")
        }
        false
      } else {
        val insideScore = chartAbove.insideForPos(loc, pos)
        val outsideScore = chartAbove.outsideForPos(loc, pos)
        val ans = chartAbove.checkPruningRatio( insideScore + outsideScore )
        if (check(VAdding, verbosity)) {
          logln(s"  coarse gave $thisClass $sentenceID $span "+
            Chart.stateString(state, spine, spine, extSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcCreation $arcSecondStage $insideScore $outsideScore $ans $loc $pos ${chartAbove.curMinMarginal} ${chartAbove.scoreRange} ${chartAbove.minRatio}")
        }
        if (!ans && gold) {
          logln(s"Lost gold: checkCoarseState2 $thisClass $sentenceID $span "+
            Chart.stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $extSpine $arcCreation $arcSecondStage")
        }
        ans
      }
    } else {
      require(chartAbove.stage.parserType != ParserType.LOCAL, "wrong spines")
      val spine = chartAbove.model.NULL_SPINE
      val extSpine =
        if (Chart.crossingFromState(state) == CrossingState.Interval) 0
        else 1
      val hash = chartAbove.stateHash(state, spine, spine, extSpine)
      val subbeam = Chart.getSubbeamID(chartAbove.stage.subbeamType, span,
        state, spine, spine, extSpine, if (arcSecondStage) 2 else 1)
      val loc = chartAbove.spanToCell(span)
      val dist = span._2 - span._1
      val pos = chartAbove.posForHash(loc, subbeam, hash)
      if (pos < 0) {
        if (check(VAdding, verbosity))
          logln(s"  coarse does not exist, $pos, $state")
        if (gold) {
          logln(s"Lost gold: checkCoarseState3 $thisClass $sentenceID $span "+
            Chart.stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $extSpine $arcCreation $arcSecondStage $subbeam $hash $loc - $pos")
        }
        false
      } else {
        // Get outside from above it and inside from the predecessor below it
        val arcInside = chartAbove.insideForPos(loc, pos)
        val arcOutside = chartAbove.outsideForPos(loc, pos)

        // Get the score itself
        val oldArg =
          if (structural) chartAbove.argSForPos(loc, pos)
          else if (chain) chartAbove.argCForPos(loc, pos)
          else chartAbove.argTForPos(loc, pos)
        val oldArcScore =
          if (oldArg == 0) 0.0f
          else chartAbove.scoreArc(child, parent, chartAbove.model.NULL_SPINE,
            chartAbove.model.NULL_SPINE, oldArg, dist, false, LossType.ZERO,
            false).toFloat
        val arcScore = chartAbove.scoreArc(child, parent,
          chartAbove.model.NULL_SPINE, chartAbove.model.NULL_SPINE, arg, dist,
          false, LossType.ZERO, false).toFloat

        // Check against the ratio cutoff
        val total = arcScore - oldArcScore + arcInside + arcOutside
        val ratio = chartAbove.calcPruningRatio(total)
        val ans = chartAbove.checkPruningRatio(total)
        if (!ans && gold) {
          logln(s"Lost gold: checkCoarseState4 $thisClass $sentenceID $span "+
            Chart.stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $extSpine $arcCreation $arcSecondStage $total ($arcScore $arcInside $arcOutside -$oldArcScore) v ${chartAbove.curMinMarginal} / ${chartAbove.scoreRange}")
        }
        coarseArcRatio = ratio
        ans
      }
    }
  }

  def checkCoarseSpine(pos: Int, spine: Int, gold: Boolean) = {
    if (gold && doNotPruneGoldSpines) {
      if (check(VAdding, verbosity)) logln(s"  is gold, not pruned ($pos $spine)")
      true
    } else if (pos == sentenceLength - 1) {
      true
    } else {
      val chartAns = true
      // TODO: Re-introducing this will require changes in model and arc parser
///        if (chartAbove == null) true
///        else {
///          val spineTop = model.spines(spine)._1.filter(! _._3).last
///          chartAbove.hasSpineOption(pos, spineTop._1, spineTop._2)
///        }
///      if (!chartAns && gold)
///        logln(s"Lost gold: checkCoarseSpine0 $thisClass $pos $sentenceID $spine ")

      val spineAns =
        if (spineChart == null) true
        else {
          val spineString = stage.model.nonTerminalIndex.value(spine)
          val sModel = spineChart.stage.model
          val otherID = sModel.nonTerminalIndex.getOrElse(spineString, -1)
          if (otherID < 0) true
          else spineChart.checkCoarseSpine(pos, otherID, gold)
        }

      if (!spineAns && gold)
        logln(s"Lost gold: checkCoarseSpine1 $thisClass $pos $sentenceID $spine ")

      spineAns && chartAns
    }
  }

  val modelHasLocalArcFeatures =
    stage.model match {
      case d: DiscriminativeModel =>
        d.newLocalArcFeatures
      case _ => false
    }

  val localCacheSize =
    if (modelHasLocalArcFeatures) 2
    else (sentenceLength * sentenceLength * model.argIndex.size * 2 / 1000).max(100)
  override val chartScoreCache =
    new LongDoubleMap(Config.loadFactor, localCacheSize)
  override def scoreArc(
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) = {
    val posInt =
      (arg << 17) +
      (childIndex << 9) +
      (parentIndex << 1) +
      (if (gold && doNotPruneGoldArcs) 1 else 0)

    val cacheID = hashToLong(childSpine, parentSpine, posInt)

    val cur =
      if (modelHasLocalArcFeatures) 0.0
      else chartScoreCache.getAndNote(cacheID)
    val score =
      if (modelHasLocalArcFeatures ||
          cur == chartScoreCache.NO_VALUE ||
          countOnGet != null) {
        val score = stage.model.getArcScore(tokenIDs, tokenFeatureIDs, tagIDs,
          childIndex, parentIndex, childSpine, parentSpine, arg, length, gold,
          scoreCache, surfaceFeatureLists, countOnGet, useModelCache,
          edgeSpace)
        if (! modelHasLocalArcFeatures)
          chartScoreCache.putNoted(score)
        score
      } else cur


    score + super.scoreArc(childIndex, parentIndex, childSpine, parentSpine,
      arg, length, gold, lossType, goldHere, countOnGet)
  }

  override def addSpineTopOptions = {
    // TODO?
  }
}

class LocalParser(
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
  override val thisClass = "LocalParser."+ stage.name

  override def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldSpines: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    new LocalChart(stage, coarseChart, traceChart, spineChart, tokens, tags,
      sentenceID, goldEdges, goldSpines, goldStates, lossType,
      doNotPruneGoldArcs, doNotPruneGoldSpines, pruningRatio, pruningRank,
      verbosity, training)
  }

///    if (Config.cacheWriteLocal) {
///      Config.cachePrefixWrite.foreach{ prefix =>
///        fineChart.serialiseScores
///        fineChart.serialisableChartData.minMarginal = fineChart.curMinMarginal
///        fineChart.serialisableChartData.maxMarginal = fineChart.curMaxMarginal
///        CachedChartData.save(prefix, "Local", fineChart.serialisableChartData)
///      }
///    }
}

