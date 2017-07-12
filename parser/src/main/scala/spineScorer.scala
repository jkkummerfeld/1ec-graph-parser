// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Set}
import scala.math

import edu.berkeley.nlp.graphparser.Log._

/* This uses the *Parser hierarchy, but is only scoring spines.
 * Note, this is a simple classifier - not a linear CRF.
 */
class SpineChart(
  stage: Stage,
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
) extends Chart(stage, null, null, null, tokens, tags, sentenceID, goldEdges, goldSpines, goldStates, lossType, doNotPruneGoldArcs, doNotPruneGoldSpines, pruningRatio, pruningRank, verbosity, training) {
  override val thisClass = "SpineChart."+ stage.name

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
  ) = true
  override def addPreterminals = { }
  override def addSpineTopOptions = { }
  override def toDenseString(items: ArrayBuffer[(Int, Int)]) = ""
  override def itemsToStates(items: ArrayBuffer[(Int, Int)]) =
    new ArrayBuffer[(Int, Int, Int, Int, Int)]
  override def insideForPos(loc: Int, pos: Int) : Float = 0.0f
  override def outsideForPos(loc: Int, pos: Int) : Float = 0.0f
  override def argSForPos(loc: Int, pos: Int) : Int = -1
  override def argCForPos(loc: Int, pos: Int) : Int = -1
  override def argTForPos(loc: Int, pos: Int) : Int = -1
  override def scoreArc(
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) = 0.0

  override def toString() = ""

  override def checkCoarseSpine(pos: Int, spine: Int, gold: Boolean) = {
    val score = spineScoresMap(pos).getOrElse(spine, Double.MinValue)
    checkPruningRatio(score, pos)
  }
  override def calcPruningRatio(
    marginal: Double, child: Int = 0, parent: Int = 0
  ) = {
    if (stage.pruneWithExpScore) {
      val sum = spineScoreSums(child)
      math.exp(marginal) / sum
    } else {
      (marginal - spineScoreMins(child)) / spineScoreRanges(child)
    }
  }
  override def checkPruningRatio(
    marginal: Double, child: Int = 0, parent: Int = 0
  ) = {
    if (stage.pruneWithExpScore) {
      val sum = spineScoreSums(child)
      math.exp(marginal) > minRatio * sum
    } else {
      (marginal - spineScoreMins(child)) >
      (minRatio * spineScoreRanges(child))
    }
  }

  override def insertDerivation(
    terminals: ArrayBuffer[(Int, Int)],
    arcs: ArrayBuffer[(Int, Int, Int)],
    insertingGold: Boolean = false
  ) = {
    if (check(VParseExtraction, verbosity)) logln(arcs)
    if (check(VParseExtraction, verbosity)) logln(terminals)

    for ((position, spine) <- terminals) {
      val score = scoreSpine(position, spine, true, LossType.ZERO,
        featureCounter)
    }

    terminals
  }

  val spineScoreSums = Array.ofDim[Double](sentenceLength)
  val spineScoreMaxes = Array.ofDim[Double](sentenceLength)
  val spineScoreMins = Array.ofDim[Double](sentenceLength)
  val spineScoreRanges = Array.ofDim[Double](sentenceLength)
  val selectedSpines = Array.ofDim[Int](sentenceLength)
  var positiveArcSum = 0.0f
  override def doInsidePass = {
    for (position <- 0 until sentenceLength) {
      if (check(VAdding | VTrace, verbosity))
        logln(s"\naddPreterminals ${position-1} $position")

      // Score these, and record the scores
      var best = (Double.MinValue, -1)
      for (spine <- getSpineOptions(position)) {
        val gold = model.spineIsGold(position, spine, goldSpines)
        if (check(VAdding, verbosity))
          logln(s"   Considering $spine which is $gold")

        val score = scoreSpine(position, spine, gold, lossType)
        spineScoresMap(position).put(spine, score)
        spineScores(position).append((spine, score.toFloat, gold))
        spineScoreSums(position) += math.exp(score)
        if (spineScoreMaxes(position) < score)
          spineScoreMaxes(position) = score
        if (spineScoreMins(position) > score)
          spineScoreMins(position) = score
        if (score > best._1 || best._2 == -1) best = (score, spine)
      }
      require(best._2 != -1, s"Failure for $position")
      selectedSpines(position) = best._2
      positiveArcSum += best._1.toFloat

      spineScoreRanges(position) =
        spineScoreMaxes(position) - spineScoreMins(position)
    }

    insideDone = true
  }
  override def doOutsidePass = positiveArcSum
  override def hashesAndScores =
    ArrayBuffer[((Int, Int), Int, Int, Int, Double, Double, Boolean)]()
  override def hashesAndScores(items: ArrayBuffer[(Int, Int)]) = {
    val ans = ArrayBuffer[((Int, Int), Int, Int, Int, Double, Double, Boolean, Boolean, Boolean, Boolean)]()
    for ((pos, spine) <- items) {
      val score = spineScoresMap(pos).get(spine)
      ans.append(((pos, pos + 1), 0, 0, spine, score, 0.0f, false, true, false, false))
    }
    ans
  }

  override def extractParse = {
    val arcs = ArrayBuffer[(Int, Int, Int)]()
    val terminals = ArrayBuffer[(Int, Int)]()
    for (i <- 0 until sentenceLength)
      terminals.append((i, selectedSpines(i)))
    (arcs, terminals, positiveArcSum)
  }
  override def extractParseItems = {
    val items = ArrayBuffer[(Int, Int)]()
    for (i <- (0 until selectedSpines.length))
      items.append((i, 0))
    (items, positiveArcSum)
  }
}

class SpineScorer(
  stage: Stage,
  pruningRatio: Double,
  pruningRank: Double,
  verbosity: Int,
  doNotPruneGoldArcs: Boolean,
  doNotPruneGoldSpines: Boolean,
  training: Boolean
) extends Parser(stage, None, None, None, pruningRatio, pruningRank, verbosity, doNotPruneGoldArcs, doNotPruneGoldSpines, training) {
  override val thisClass = "SpineScorer."+ stage.name

  override def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldSpines: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    new SpineChart(stage, tokens, tags, sentenceID, goldEdges, goldSpines,
      goldStates, lossType, doNotPruneGoldArcs, doNotPruneGoldSpines,
      pruningRatio, pruningRank, verbosity, training)
  }

  override def calculateLoss(
    gterminals: ArrayBuffer[(Int, Int)], garcs: ArrayBuffer[(Int, Int, Int)],
    aterminals: ArrayBuffer[(Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)],
    lossType: LossType.Value
  ) = Config.lossWeightSpine * (aterminals.length - aterminals.intersect(gterminals).length)

  override def denseChartString = "No denseChartString for scorer"

  override def chartSizesString = "No chartSizesString for scorer"

  override def updatePruningStats(stats: PruningStats, goldParse: Parse) = {
    val chart = fineChart
    for ((spines, position) <- chart.spineScores.zipWithIndex) {
      for ((spine, score, gold) <- spines) {
        val ratio = chart.calcPruningRatio(score, position)
        stats.update(ratio, true, false, false, true)
      }
    }

    val (gterms, garcs) = stage.model.parseToArrays(goldParse, false, stage)
    val gitems = insertDerivation(gterms, garcs)
    for (item <- chart.hashesAndScores(gitems)) {
      val score = item._5
      val ratio = chart.calcPruningRatio(score, item._1._1)
      stats.update(ratio, true, true, false, true)
    }
  }
}

