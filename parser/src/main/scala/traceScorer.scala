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
import scala.math

import edu.berkeley.nlp.graphparser.Log._

/* This is uses the *Parser hierarchy, but is only scoring arcs. This is the used to prune traces.
 */
class TraceChart(
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
///  import Chart._
  import stage.model._
  import stage._

  override val thisClass = "TraceChart."+ stage.name

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
  override def checkCoarseSpine(
    pos: Int, spine: Int, gold: Boolean
  ) = true
  override def scoreSpine(
    wIndex: Int, spine: Int, gold: Boolean, lossType: LossType.Value,
    countOnGet: (Int, Double) => Unit = null
  ) = 0.0
  override def getSpineOptions(position: Int) = NULL_SPINE_SET
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

  override def toString() = ""

  val toRetain = new HashSet[(Int, Int, Int)]
  override def checkCoarseTrace(child: Int, parent: Int, trace: String) = {
    val ans =
      if (trace == "_") true
      else {
        val childTag = nonTerminalIndex.value(tagIDs(child))
        val parentTag = nonTerminalIndex.value(tagIDs(parent))
        val arg = getTraceEdgeID(trace, childTag, parentTag, true)
///        Log.logln(s"Checking $childTag $parentTag $arg for $child $parent $trace")
        toRetain.contains((child, parent, arg))
      }
///    Log.logln(s"checkCoarseTrace $child $parent $trace -> $ans")
    ans
  }

  override def scoreArc(
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) = {
    val score =
      if (arg < 0) 0.0
      else getArcScore(tokenIDs, tokenFeatureIDs, tagIDs, childIndex,
        parentIndex, NULL_SPINE, NULL_SPINE, arg, length, gold, scoreCache,
        surfaceFeatureLists, countOnGet, useModelCache, edgeSpace)

    val above = super.scoreArc(childIndex, parentIndex, childSpine,
      parentSpine, arg, length, gold, lossType, goldHere, countOnGet)

    score + above
  }

  val arcReference = ArrayBuffer[((Int, Int, Int), Float)]()
  var positiveArcSum = 0.0f
  override def insertDerivation(
    terminals: ArrayBuffer[(Int, Int)],
    arcs: ArrayBuffer[(Int, Int, Int)],
    insertingGold: Boolean = false
  ) = {
    // Since multiple arcs can occur over the same (child, parent) pair, we
    // consider all of them and record the lowest scoring one. This will not
    // affect our inference process, since we only choose one, but it will
    // affect the gold extraction.
    val itemMap = new HashMap[(Int, Int), (Int, Int)]
    if (check(VParseExtraction, verbosity)) logln(arcs)
    if (check(VParseExtraction, verbosity)) logln(terminals)

    // Find the lowest scores
    for ((child, parent, arg) <- arcs) {
      val dist = (parent - child).abs
      val score = scoreArc(child, parent, NULL_SPINE, NULL_SPINE, arg, dist,
        true, LossType.ZERO, true).toFloat
      val cur = itemMap.get((child, parent)).fold(Float.MaxValue){ pos =>
        arcReference(pos._1)._2
      }
      if (score < cur) {
        itemMap((child, parent)) = (arcReference.length, 0)
        arcReference.append(((child, parent, arg), score))
///        Log.logln(s"Inserted $child -> $parent : ($arg, $score), prev $cur")
      }
    }

    // Return only the values, saving to the featureCounter in the process
    val items = new ArrayBuffer[(Int, Int)]
    for ((key, value) <- itemMap) {
      if (featureCounter != null) {
        val (child, parent, arg) = arcReference(value._1)._1
        val dist = (parent - child).abs
        scoreArc(child, parent, NULL_SPINE, NULL_SPINE, arg, dist, true,
          LossType.ZERO, true, featureCounter).toFloat
      }
      items.append(value)
    }

    items
  }

  // Child token number, parent token number, arg
  val selectedArcs = new UnboxedArrayBuffer(128)
  var arcScoresForRanking = new UnboxedArrayBufferFloat(128)
  var sorted = false
  val arcScoreSums = new LongDoubleMap(Config.loadFactor, 1024)

  def checkForGold(
    parent: Int, child: Int, gEdges: UnboxedArrayBuffer, arg: Int
  ) = {
    checkArcIsGold(parent, child, gEdges, sentenceLength, arg, true, false) ||
    checkArcIsGold(parent, child, gEdges, sentenceLength, arg, false, true) ||
    checkArcIsGold(parent, child, gEdges, sentenceLength, arg, false, false)
  }

  def considerArcs(child: Int, parent: Int, inside: Boolean) = {
    val span = (child.min(parent), child.max(parent))
    val parentIsRoot = parent == sentenceLength - 1
    val childIsRoot = child == sentenceLength - 1
    labelGen.prepare(tagIDs(child), tagIDs(parent), NULL_SPINE, NULL_SPINE,
      span, sentenceLength, parentIsRoot, arcPassConstraint)
    val dist = (parent - child).abs

    val childTag = nonTerminalIndex.value(tagIDs(child))
    val parentTag = nonTerminalIndex.value(tagIDs(parent))
    val nonEdgeTrace = getTraceEdgeID(Model.SNonTrace, childTag, parentTag)
    val noGoldTrace = checkForGold(parent, child, goldEdges, nonEdgeTrace)
    val goldHere = ! noGoldTrace

    val nonLoss =
      if (lossType == LossType.HAMMING && goldHere) Config.lossWeightMissedT
      else 0.0
    val nonScore = scoreArc(child, parent, NULL_SPINE, NULL_SPINE,
      nonEdgeTrace, dist, true, LossType.ZERO, true).toFloat
    var bestTrace = (nonEdgeTrace, nonScore + nonLoss.toFloat)
    var scoreSum = math.exp(bestTrace._2)

    while (labelGen.hasNext) {
      val arg = labelGen.next
      val structural = model.isTreeEdge(arg)
      val chain = model.isChainTraceEdge(arg)
      if (!structural && !parentIsRoot && !childIsRoot &&
          arg != nonEdgeTrace) {
        val arcIsGold = checkForGold(parent, child, goldEdges, arg)
///        Log.logln(s"arcIsGold $arcIsGold $parent $child $sentenceLength $arg $structural $chain")
        val score = scoreArc(child, parent, NULL_SPINE, NULL_SPINE, arg, dist,
          arcIsGold, lossType, goldHere).toFloat

        if (inside) {
          if (bestTrace._2 < score) bestTrace = (arg, score)
          arcScoresForRanking.append(score)
          scoreSum += math.exp(score)

          if (check(VMarginals, verbosity)) {
            arcScoresForPruning.getOrElseUpdate(
              (child, parent), new ArrayBuffer[Double]
            ).append(score)
          }
        } else if (checkPruningRatio(score, child, parent)) {
          toRetain.add((child, parent, arg))
///          Log.logln(s"Retain ($parent, $child, $arg) pca")
        }
      }
    }
    if (inside) {
      arcScoreSums.put(child, parent, scoreSum)
      selectedArcs.append(arcReference.length)
      arcReference.append(((child, parent, bestTrace._1), bestTrace._2))
      positiveArcSum += bestTrace._2
    }
  }
  override def doInsidePass = {
    var child = 0
    while (child < sentenceLength - 1) {
      var parent = 0
      while (parent < sentenceLength - 1) {
        if (child != parent) considerArcs(child, parent, true)
        parent += 1
      }
      child += 1
    }
    insideDone = true

    // Do preprocessing to save time in querying for pruning.

    // Build up map of allowed traces
    arcScoresForRanking.sort(true)
    sorted = true
    child = 0
    while (child < sentenceLength - 1) {
      var parent = 0
      while (parent < sentenceLength - 1) {
        if (child != parent) considerArcs(child, parent, false)
        parent += 1
      }
      child += 1
    }

    // Build up map of edges that can be crossed by an allowed trace
    val traceable = new IntSet(Config.loadFactor, 1024)
    val crossable = new LongSet(Config.loadFactor, 1024)
    val done = new IntSet(Config.loadFactor, 1024)
    for ((child, parent, _) <- toRetain) {
      traceable.add(child, parent)
      val left = child.min(parent)
      val right = child.max(parent)
      if (! done.contains(left, right)) {
        done.add(left, right)
        // Consider all (in, out) pairs that would cross this edge.
        // ie in is within (left, right)
        //    out is within [0, left) + (right, max]
        var in = left + 1
        while (in < right) {
          var out = 0
          while (out < sentenceLength) {
            if (out < left || out > right) {
              // For each (in, out), (left, right) set, we are aiming to
              // consider a span + an edge, where the span does not contain
              // left or right.
              // First consider spans with the other end is in [left, right]
              var other = left
              while (other <= right) {
                crossable.add(
                  other.min(in),
                  other.max(in),
                  out.min(in),
                  out.max(in)
                )
                other += 1
              }
              // Now consider spans where the other end is outside (left, right)
              // (the overlap at edge points is to deal with the fact that the
              // span could go on either side when the 'other' point == left or
              // == right.
              if (out < left) other = 0
              else if (out > right) other = right
              while (other != left + 1 && other < sentenceLength) {
                crossable.add(
                  other.min(out),
                  other.max(out),
                  out.min(in),
                  out.max(in)
                )
                other += 1
              }
            }
            out += 1
          }
          in += 1
        }
      }
    }
    child = 0
    while (child < sentenceLength) {
      var parent = 0
      while (parent < sentenceLength) {
        if (child != parent) {
          var other = 0
          val left = child.min(parent)
          val right = child.max(parent)
          while (other < sentenceLength) {
            if (other < left) {
              if (! crossable.contains(other, left, left, right))
                crossingSet.add(other, left, left, right)
            } else if (other > right) {
              if (! crossable.contains(right, other, left, right))
                crossingSet.add(right, other, left, right)
            } else if (other != left && other != right) {
              if (! crossable.contains(left, other, left, right))
                crossingSet.add(left, other, left, right)
              if (! crossable.contains(other, right, left, right))
                crossingSet.add(other, right, left, right)
            }
            other += 1
          }
          if (! traceable.contains(child, parent))
            tracingSet.add(child, parent)
        }
        parent += 1
      }
      child += 1
    }

///    val unpruned = new HashSet[(Int, Int)]
///    for ((child, parent, _) <- toRetain) {
///      val span = (child.min(parent), child.max(parent))
///      unpruned.add(span)
///    }
///    Log.logln(s"Pruning info: spans ${unpruned.size} crossing ${crossable.size} & ${crossingSet.size} tracing ${traceable.size} & ${tracingSet.size} cells $numberOfCells")
  }
  override def doOutsidePass = positiveArcSum
  override def hashesAndScores =
    ArrayBuffer[((Int, Int), Int, Int, Int, Double, Double, Boolean)]()
  override def hashesAndScores(items: ArrayBuffer[(Int, Int)]) = {
    val ans = ArrayBuffer[((Int, Int), Int, Int, Int, Double, Double, Boolean, Boolean, Boolean, Boolean)]()
    for (pos <- items) {
      val (arc, score) = arcReference(pos._1)
      ans.append(((arc._1, arc._2), 0, 0, arc._3, score, 0.0f, false, false, false, false))
    }
    ans
  }

  override def extractParse = {
    val terminals = ArrayBuffer[(Int, Int)]()
    for (i <- 0 until sentenceLength) terminals.append((i, NULL_SPINE))
    val items = ArrayBuffer[(Int, Int, Int)]()
    for (i <- (0 until selectedArcs.length))
      items.append(arcReference(selectedArcs(i))._1)
    (items, terminals, positiveArcSum)
  }
  override def extractParseItems = {
    val items = ArrayBuffer[(Int, Int)]()
    for (i <- (0 until selectedArcs.length))
      items.append((selectedArcs(i), 0))
    (items, positiveArcSum)
  }

  override def calcPruningRatio(marginal: Double, child: Int, parent: Int) = {
    val expMarginal = math.exp(marginal)
    val total = arcScoreSums.getOrElse(child, parent, expMarginal)
    expMarginal / total
  }
  override def calcPruningRank(marginal: Double) = {
    require(sorted)
    var l = 0
    var r = arcScoresForRanking.length
    while (l < r) {
      val m = (l + r) / 2
      val cur = arcScoresForRanking(m)
      if (cur > marginal) {
        if (l == m) r = m
        l = m
      } else if (cur < marginal) {
        if (r == m) l = m
        r = m
      } else {
        l = m
        r = m
      }
    }
    l / arcScoresForRanking.length.toDouble
  }
  override def checkPruningRatio(marginal: Double, child: Int, parent: Int) = {
    val expMarginal = math.exp(marginal)
    val total = arcScoreSums.getOrElse(child, parent, expMarginal)

    (expMarginal > total * minRatio) &&
    calcPruningRank(marginal) < maxRank
  }
}

class TraceScorer(
  stage: Stage,
  pruningRatio: Double,
  pruningRank: Double,
  verbosity: Int,
  doNotPruneGoldArcs: Boolean,
  doNotPruneGoldSpines: Boolean,
  training: Boolean
) extends Parser(stage, None, None, None, pruningRatio, pruningRank, verbosity, doNotPruneGoldArcs, doNotPruneGoldSpines, training) {
  override val thisClass = "TraceScorer."+ stage.name

  override def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldSpines: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    new TraceChart(stage, tokens, tags, sentenceID, goldEdges, goldSpines,
      goldStates, lossType, doNotPruneGoldArcs, doNotPruneGoldSpines,
      pruningRatio, pruningRank, verbosity, training)
  }

  def getNonEdgeTrace(child: Int, parent: Int) = {
    val childTag = stage.model.nonTerminalIndex.value(fineChart.tagIDs(child))
    val parentTag = stage.model.nonTerminalIndex.value(fineChart.tagIDs(parent))
    stage.model.getTraceEdgeID(Model.SNonTrace, childTag, parentTag)
  }
  override def calculateLoss(
    gterminals: ArrayBuffer[(Int, Int)], garcs: ArrayBuffer[(Int, Int, Int)],
    aterminals: ArrayBuffer[(Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    var loss = 0.0
    val goldEdgePairs = garcs.filter{ case (child, parent, arg) =>
      arg != getNonEdgeTrace(child, parent)
    }.map( v => (v._1, v._2) )
    for (aarc <- aarcs) {
      if (! garcs.contains(aarc)) {
        val goldHere = goldEdgePairs.contains((aarc._1, aarc._2))
        val isTrace = (aarc._3 != getNonEdgeTrace(aarc._1, aarc._2))

        if (goldHere) {
          if (isTrace)
            loss += Config.lossWeightDiffT
          else
            loss += Config.lossWeightMissedT
        } else {
          if (isTrace)
            loss += Config.lossWeightExtraT
        }
      }
    }
    loss
  }

  override def denseChartString = "No denseChartString for scorer"

  override def chartSizesString = "No chartSizesString for scorer"

  override def updatePruningStats(stats: PruningStats, goldParse: Parse) = {
    val chart = fineChart
    val sentenceLength = chart.sentenceLength

    Log.logln(s"CrossingSet: ${chart.crossingSet.size} for $sentenceLength")
    Log.logln(s"TracingSet: ${chart.tracingSet.size} for $sentenceLength")

    // Update for arcs
    val unsortedScores = ArrayBuffer[(Double, Int, Int)]()
    var child = 0
    while (child < chart.sentenceLength - 1) {
      var parent = 0
      while (parent < chart.sentenceLength - 1) {
        if (child != parent &&
            chart.arcScoresForPruning.contains((child, parent))) {
          val cur = chart.arcScoresForPruning((child, parent))
          val sorted = cur.sortWith(_ < _)
          chart.arcScoresForPruning((child, parent)) = sorted
          for (score <- sorted) {
            unsortedScores.append((score, child, parent))
            val ratio = chart.calcPruningRatio(score, child, parent)
            stats.update(ratio, true, false, false, false)
///            Log.logln(s"Got $ratio for $score $child $parent")
          }
        }
        parent += 1
      }
      child += 1
    }
    // Update for cells
    child = 0
    while (child < chart.sentenceLength - 1) {
      var parent = 0
      while (parent < chart.sentenceLength - 1) {
        if (child != parent) {
          if (chart.arcScoresForPruning.contains((child, parent))) {
            val usableScores = chart.arcScoresForPruning((child, parent))
            val score = usableScores.max
            val ratio = chart.calcPruningRatio(score, child, parent)
            stats.update(ratio, false, false, false, false, true)
          } else {
            stats.update(0.0, false, false, false, false, true)
          }
        }
        parent += 1
      }
      child += 1
    }

///    // Info re: ranking
///    val sortedScores = unsortedScores.sortWith(_._1 > _._1)
///    val doneCells = HashSet[(Int, Int)]()
///    for (((score, child, parent), rank) <- sortedScores.zipWithIndex) {
///      val cell = (child, parent)
///      val ratio = chart.calcPruningRatio(score, child, parent)
///      Log.logln(s"rankingPruning combined $ratio $rank $sentenceLength")
///      if (! doneCells.contains(cell)) {
///        doneCells.add(cell)
///        Log.logln(s"rankingPruning cellrank $ratio $rank $sentenceLength")
///      }
///    }
///    child = 0
///    while (child < chart.sentenceLength - 1) {
///      var parent = 0
///      while (parent < chart.sentenceLength - 1) {
///        if (child != parent && ! doneCells.contains((child, parent)))
///          Log.logln(s"rankingPruning cellrank 0.0 999999 $sentenceLength")
///        parent += 1
///      }
///      child += 1
///    }

    val (gterms, garcs) = stage.model.parseToArrays(goldParse, false, stage)
    val gitems = insertDerivation(gterms, garcs)
    for (item <- chart.hashesAndScores(gitems)) {
      if (! stage.model.edges(item._4)._4) {
        val score = item._5
        val ratio = chart.calcPruningRatio(score, item._1._1, item._1._2)
        stats.update(ratio, true, true, false, false)
        val rank = chart.calcPruningRank(score)
        Log.logln(s"rankingPruning combined gold $ratio $rank $sentenceLength ${item._4}")
      }
    }
  }
}

