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

/** A kind of forest reranker, though it's really just a fine pass with fully
  * articulated parse structures (and could be trained with everything else).
  *
  * To consider:
  *  Tradeoff memory usage for complexity by only storing the backpointers and
  *  the state hash, then traverse the chart to rebuild for features.
  */
object FullStructureChart {
  // State
  // 0-11 (see baseParser)
  // 12  hash first half
  // 13  hash second half
  // 14  external, structural arg (24) parent pos (8)
  //  for each position, including edges:
  //     spine (32)
  //     structural arg (24) parent (8)
  // number of traces
  //  for each trace:
  //     arg (16) child (8) parent (8)

  final val SUB_START = 15

  @inline final def hashPart0(
    states: UnboxedArrayBuffer, statePos: Int
  ) = states(statePos + SUB_START - 3)
  @inline final def hashPart1(
    states: UnboxedArrayBuffer, statePos: Int
  ) = states(statePos + SUB_START - 2)
  @inline final def subArgParentForPosExternal(
    states: UnboxedArrayBuffer, statePos: Int
  ) = states(statePos + SUB_START - 1)
  @inline final def externalPosForPos(
    states: UnboxedArrayBuffer, statePos: Int
  ) = Chart.externalPosFromState(states(statePos))
  @inline final def externalParentForPos(
    states: UnboxedArrayBuffer, statePos: Int
  ) = subArgParentForPosExternal(states, statePos) & 0xff
  @inline final def externalArgForPos(
    states: UnboxedArrayBuffer, statePos: Int
  ) = subArgParentForPosExternal(states, statePos) >>> 8

  @inline final def subSpineForPos(
    pos: Int, span0: Int, states: UnboxedArrayBuffer, statePos: Int
  ) = {
    val delta = (pos - span0) * 2
    states(statePos + SUB_START + delta)
  }

  @inline final def subArgParentForPos(
    pos: Int, span0: Int, states: UnboxedArrayBuffer, statePos: Int
  ) = {
    val delta = (pos - span0) * 2 + 1
    states(statePos + SUB_START + delta)
  }
  @inline final def subParentForPos(
    pos: Int, span0: Int, states: UnboxedArrayBuffer, statePos: Int
  ) = subArgParentForPos(pos, span0, states, statePos) & 0xff
  @inline final def subArgForPos(
    pos: Int, span0: Int, states: UnboxedArrayBuffer, statePos: Int
  ) = subArgParentForPos(pos, span0, states, statePos) >>> 8

  @inline final def subNumTracesForPos(
    span0: Int, span1: Int, states: UnboxedArrayBuffer, statePos: Int
  ) = {
    val delta = (span1 - span0 + 1) * 2
    states(statePos + SUB_START + delta)
  }
  @inline final def subTraceForPos(
    num: Int, span0: Int, span1: Int, states: UnboxedArrayBuffer,
    statePos: Int
  ) = {
    val delta = (span1 - span0 + 1) * 2 + num + 1
    states(statePos + SUB_START + delta)
  }
  @inline final def subTraceArgForPos(
    num: Int, span0: Int, span1: Int, states: UnboxedArrayBuffer,
    statePos: Int
  ) = subTraceForPos(num, span0, span1, states, statePos) >>> 16
  @inline final def subTraceParentForPos(
    num: Int, span0: Int, span1: Int, states: UnboxedArrayBuffer,
    statePos: Int
  ) = subTraceForPos(num, span0, span1, states, statePos) & 0xff
  @inline final def subTraceChildForPos(
    num: Int, span0: Int, span1: Int, states: UnboxedArrayBuffer,
    statePos: Int
  ) = (subTraceForPos(num, span0, span1, states, statePos) >>> 8) & 0xff

  @inline final def makeSubArgParent(arg: Int, parent: Int) =
    (arg << 8) | parent
  @inline final def makeSubTrace(arg: Int, child: Int, parent: Int) =
    (arg << 16) | (child << 8) | parent
}

class FullStructureChart(
  stage: Stage,
  chartAbove: Chart,
  traceChart: Chart,
  spineChart: Chart,
  tokens: Vector[String],
  tags: Vector[String],
  sentenceID: Int,
  goldEdges: UnboxedArrayBuffer,
  goldUnaries: ArrayBuffer[Int],
  goldStates: Set[(Int, Int, Int, Int, Int)],
  lossType: LossType.Value,
  doNotPruneGoldArcs: Boolean,
  doNotPruneGoldSpines: Boolean,
  pruningRatio: Double,
  pruningRank: Double,
  verbosity: Int,
  training: Boolean
) extends Chart(stage, chartAbove, traceChart, spineChart, tokens, tags, sentenceID, goldEdges, goldUnaries, goldStates, lossType, doNotPruneGoldArcs, doNotPruneGoldSpines, pruningRatio, pruningRank, verbosity, training) {
  import Chart._
  import FullStructureChart._
  import stage.model._
  import stage._

  override val thisClass = "FullStructureChart."+ stage.name

  override val beamsL =
    if (stage.subbeamType != SubbeamType.SPINES) beams
    else Array.fill(numberOfCells)(beamType(beamMinLength, beamMaxLength,
      beamMinMultiple, beamMaxMultiple, beamMinFraction, beamMaxFraction))
  override val beamsR =
    if (stage.subbeamType != SubbeamType.SPINES) beams
    else Array.fill(numberOfCells)(beamType(beamMinLength, beamMaxLength,
      beamMinMultiple, beamMaxMultiple, beamMinFraction, beamMaxFraction))

  def curHash00 =
    hashPart0(curCellStates0, curStatePos0)
  def curHash01 =
    hashPart1(curCellStates0, curStatePos0)
  def subSpine(pos: Int) =
    subSpineForPos(pos, curSpan00, curCellStates0, curStatePos0)
  def subArgParent(pos: Int) =
    subArgParentForPos(pos, curSpan00, curCellStates0, curStatePos0)
  def subArgParentExternal =
    subArgParentForPosExternal(curCellStates0, curStatePos0)
  def subParent(pos: Int) =
    subParentForPos(pos, curSpan00, curCellStates0, curStatePos0)
  def subArg(pos: Int) =
    subArgForPos(pos, curSpan00, curCellStates0, curStatePos0)
  def subNumTraces =
    subNumTracesForPos(curSpan00, curSpan01, curCellStates0, curStatePos0)
  def subTrace(num: Int) =
    subTraceForPos(num, curSpan00, curSpan01, curCellStates0, curStatePos0)
  def subTraceArg(num: Int) =
    subTraceArgForPos(num, curSpan00, curSpan01, curCellStates0, curStatePos0)
  def subTraceParent(num: Int) =
    subTraceParentForPos(num, curSpan00, curSpan01, curCellStates0, curStatePos0)
  def subTraceChild(num: Int) =
    subTraceChildForPos(num, curSpan00, curSpan01, curCellStates0, curStatePos0)

  def curHash10 =
    hashPart0(curCellStates1, curStatePos1)
  def curHash11 =
    hashPart1(curCellStates1, curStatePos1)
  def subSpine1(pos: Int) =
    subSpineForPos(pos, curSpan10, curCellStates1, curStatePos1)
  def subArgParent1(pos: Int) =
    subArgParentForPos(pos, curSpan10, curCellStates1, curStatePos1)
  def subArgParentExternal1 =
    subArgParentForPosExternal(curCellStates1, curStatePos1)
  def subParent1(pos: Int) =
    subParentForPos(pos, curSpan10, curCellStates1, curStatePos1)
  def subArg1(pos: Int) =
    subArgForPos(pos, curSpan10, curCellStates1, curStatePos1)
  def subNumTraces1 =
    subNumTracesForPos(curSpan10, curSpan11, curCellStates1, curStatePos1)
  def subTrace1(num: Int) =
    subTraceForPos(num, curSpan10, curSpan11, curCellStates1, curStatePos1)
  def subTraceArg1(num: Int) =
    subTraceArgForPos(num, curSpan10, curSpan11, curCellStates1, curStatePos1)
  def subTraceParent1(num: Int) =
    subTraceParentForPos(num, curSpan10, curSpan11, curCellStates1, curStatePos1)
  def subTraceChild1(num: Int) =
    subTraceChildForPos(num, curSpan10, curSpan11, curCellStates1, curStatePos1)

  def curHash20 =
    hashPart0(curCellStates2, curStatePos2)
  def curHash21 =
    hashPart1(curCellStates2, curStatePos2)
  def subSpine2(pos: Int) =
    subSpineForPos(pos, curSpan20, curCellStates2, curStatePos2)
  def subArgParent2(pos: Int) =
    subArgParentForPos(pos, curSpan20, curCellStates2, curStatePos2)
  def subArgParentExternal2 =
    subArgParentForPosExternal(curCellStates2, curStatePos2)
  def subParent2(pos: Int) =
    subParentForPos(pos, curSpan20, curCellStates2, curStatePos2)
  def subArg2(pos: Int) =
    subArgForPos(pos, curSpan20, curCellStates2, curStatePos2)
  def subNumTraces2 =
    subNumTracesForPos(curSpan20, curSpan21, curCellStates2, curStatePos2)
  def subTrace2(num: Int) =
    subTraceForPos(num, curSpan20, curSpan21, curCellStates2, curStatePos2)
  def subTraceArg2(num: Int) =
    subTraceArgForPos(num, curSpan20, curSpan21, curCellStates2, curStatePos2)
  def subTraceParent2(num: Int) =
    subTraceParentForPos(num, curSpan20, curSpan21, curCellStates2, curStatePos2)
  def subTraceChild2(num: Int) =
    subTraceChildForPos(num, curSpan20, curSpan21, curCellStates2, curStatePos2)

  // Old and unused
  override def minSplitGet(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    val right = hashN(rightSpine, HasParent.id(parent), span2)
    val ans = minSplit.get(right)
    if (ans < (span1 + 1) || ans > sentenceLength) span1 + 1
    else ans
  }
  override def maxSplitGet(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    val left = hashN(leftSpine, HasParent.id(parent), span1)
    val ans = maxSplit.get(left)
    if (ans > (span2 - 1) || ans < 0) span2 - 1
    else ans
  }
  override def minSplitUpdate(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    val right = hashN(rightSpine, HasParent.id(parent), span2)
    val prev = minSplit.get(right)
    if (prev > span1) minSplit.put(right, span1)
  }
  override def maxSplitUpdate(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = {
    val left = hashN(leftSpine, HasParent.id(parent), span1)
    val prev = maxSplit.get(left)
    if (prev < span2) maxSplit.put(left, span2)
  }

  // override insert
  override def insert(
    loc: Int, splits: Int, gold_ante3: Int, ante: Int, ante2_arg: Int,
    score: Float, sum: Float, state: Int, leftSpine: Int, rightSpine: Int,
    externalSpine: Int, nStateHash: Long, binary: Boolean = false,
    ternary: Boolean = false, init: Boolean = false, child: Int = -1,
    parent: Int = -1, structural: Boolean = false
  ) : (Float, Int, Float) = {
    val curCellStates = denseStates(loc)
    val fullSpan = cellToSpan(loc)
///    val curStatePos = curCellStates.length
///    Log.logln(s"Inserting $fullSpan $leftSpine $rightSpine $externalSpine $binary $ternary $init : $curStatePos")
    val ans = super.insert(loc, splits, gold_ante3, ante, ante2_arg, score,
      sum, state, leftSpine, rightSpine, externalSpine, nStateHash, binary,
      ternary, init, child, parent, structural)

    val nExternalPos =
      if (externalSpine == 0) -1
      else externalPosFromState(state)
    val xFrom0 = (externalPos == nExternalPos)
    val xFrom1 = ((binary || ternary) && externalPos1 == nExternalPos)
    val xFrom2 = (ternary && externalPos2 == nExternalPos)

    // Hash that combines info on all current states
    curCellStates.append((nStateHash >>> 32).toInt)
    curCellStates.append((nStateHash & 0xffffffff).toInt)

    // Structural arg and parent for External point (0 if no external)
    if (externalSpine == 0) {
      // No external point
      curCellStates.append(0)
    } else if (structural && child == nExternalPos) {
      // Created now, x is the child
      curCellStates.append(makeSubArgParent(ante2_arg, parent))
    } else {
      // It is in one of the states being combined. One will contain it, the
      // others will be zero.
      val toInsert =
        ( if (xFrom0) curCellStates0(curStatePos0 + 14) else 0 ) +
        ( if (xFrom1) curCellStates1(curStatePos1 + 14) else 0 ) +
        ( if (xFrom2) curCellStates2(curStatePos2 + 14) else 0 )
      curCellStates.append(toInsert)
    }

    // Spines and structural edges:
    // - Add spines from left
    // - If that doesn't reach all the way to the end, add middle/right
    // - If that doesn't reach all the way to the end, add right
    // Helpful things that can be assumed
    // - Spines agree (ie, get a spine from any item and it's fine)
    // - A given arc can only occur in one item
    if (init) {
      val arg_parent = 0
      val numTraces = 0
      curCellStates.append(leftSpine)
      curCellStates.append(arg_parent)
      curCellStates.append(rightSpine)
      curCellStates.append(arg_parent)
      curCellStates.append(numTraces)
    } else {
      var pos = fullSpan._1
      while (pos <= fullSpan._2) {
        // Insert spine
        val spine =
          if (pos <= curSpan01) subSpine(pos)
          else if (pos <= curSpan11) subSpine1(pos)
          else if (pos <= curSpan21) subSpine2(pos)
          else throw new Exception(s"pos $pos is outside span $fullSpan")

        val arg_parent =
          if (structural && pos == child) {
            // being created here
            makeSubArgParent(ante2_arg, parent)
          } else if (pos == curSpan00) {
            // left, middle external, right external
            subArgParent(pos) +
            ( if ((binary || ternary) && externalPos1 == pos)
                subArgParentExternal1
              else 0) +
            ( if (ternary && externalPos2 == pos) subArgParentExternal2
              else 0 )
          } else if (pos < curSpan01) {
            // Left item
            subArgParent(pos)
          } else if (pos == curSpan01) {
            // left, middle, or right external
            subArgParent(pos) +
            (if (binary || ternary) subArgParent1(pos) else 0) +
            ( if (ternary && externalPos2 == pos) subArgParentExternal2
              else 0 )
          } else if ((binary|| ternary) && pos < curSpan11) {
            // middle / right
            subArgParent1(pos)
          } else if ((binary|| ternary) && pos == curSpan11) {
            // left external, middle / right, right
            ( if (externalPos == pos) subArgParentExternal else 0 ) +
            subArgParent1(pos) +
            (if (ternary) subArgParent2(pos) else 0)
          } else if (ternary && pos < curSpan21) {
            // right
            subArgParent2(pos)
          } else if (ternary && pos == curSpan21) {
            // left external, middle external, right
            ( if (externalPos == pos) subArgParentExternal else 0 ) +
            ( if (externalPos1 == pos) subArgParentExternal1 else 0 ) +
            subArgParent2(pos)
          } else throw new Exception(s"pos $pos is outside span $fullSpan")

        curCellStates.append(spine)
        curCellStates.append(arg_parent)

        pos += 1
      }

      // Trace edge fields
      val nTraces =
        subNumTraces +
        (if (binary || ternary) subNumTraces1 else 0) +
        (if (ternary) subNumTraces2 else 0) +
        (if (child >= 0 && (gold_ante3 & ARGT_MASK) != 0) 1 else 0) +
        (if (child >= 0 && (gold_ante3 & ARGC_MASK) != 0) 1 else 0)
      curCellStates.append(nTraces)

      // Insert new trace edges
      if (child >= 0) {
        if ((gold_ante3 & ARGT_MASK) != 0) {
          val trace = makeSubTrace((gold_ante3 & ARGT_MASK) >> 15, child, parent)
          curCellStates.append(trace)
        } else if ((gold_ante3 & ARGC_MASK) != 0) {
          val trace = makeSubTrace(gold_ante3 & ARGC_MASK, child, parent)
          curCellStates.append(trace)
        }
      }

      // Insert existing trace edges
      var tracePos = 0
      while (tracePos < subNumTraces) {
        curCellStates.append(subTrace(tracePos))
        tracePos += 1
      }
      if (binary || ternary) {
        var tracePos = 0
        while (tracePos < subNumTraces1) {
          curCellStates.append(subTrace1(tracePos))
          tracePos += 1
        }
      }
      if (ternary) {
        var tracePos = 0
        while (tracePos < subNumTraces2) {
          curCellStates.append(subTrace2(tracePos))
          tracePos += 1
        }
      }
    }

///    val newStatePos = curCellStates.length
///    Log.logln(s"Inserted $fullSpan $leftSpine $rightSpine $externalSpine $binary $ternary $init : $newStatePos - $curStatePos")

    ans
  }

  // To ensure items are always inserted, need to generate the state hash from
  // previous state hashes. Note, requires the assumption that whenever this
  // is called, the current state is set accordingly.
  override def stateHash(
    state: Int, leftSpine: Int, rightSpine: Int, externalSpine: Int,
    binary: Boolean = false, ternary: Boolean = false, init: Boolean = false
  ) = {
    if (ternary)
      hashToLong(state, leftSpine, rightSpine, externalSpine, curHash00,
        curHash01, curHash10, curHash11, curHash20, curHash21)
    else if (binary)
      hashToLong(state, leftSpine, rightSpine, externalSpine, curHash00,
        curHash01, curHash10, curHash11)
    else if (init)
      hashToLong(state, leftSpine, rightSpine, externalSpine)
    else
      hashToLong(state, leftSpine, rightSpine, externalSpine, curHash00,
        curHash01)
  }

  // Binary needs to do scoring for positions being closed off
  override def combiner(
    spanLeft: Int, spanRight: Int, split1: Int, split2: Int, leftPos: Int,
    middlePos: Int, rightPos: Int
  ) = {
    val score = super.combiner(spanLeft, spanRight, split1, split2, leftPos,
      middlePos, rightPos)
    val leftCell = spanToCell(spanLeft, split1)
    setCurCellAndPos(leftCell, leftPos)
    if (middlePos < 0) {
      val rightCell = spanToCell(split1, spanRight)
      setCurCellAndPos1(rightCell, rightPos)
    } else {
      val middleCell = spanToCell(split1, split2)
      val rightCell = spanToCell(split2, spanRight)
      setCurCellAndPos1(middleCell, middlePos)
      setCurCellAndPos2(rightCell, rightPos)
    }
    if (middlePos < 0) {
      val nscore = getFullBinaryScore(tokenIDs, tokenFeatureIDs, tagIDs,
        spanLeft, split1, -1, spanRight, split1, curCellStates0, curStatePos0,
        null, -1, curCellStates1, curStatePos1, featureCounter,
        lossType).toFloat
      score + nscore
    } else {
      val nscore0 = getFullBinaryScore(tokenIDs, tokenFeatureIDs, tagIDs,
        spanLeft, split1, split2, spanRight, split1, curCellStates0,
        curStatePos0, curCellStates1, curStatePos1, curCellStates2,
        curStatePos2, featureCounter, lossType).toFloat
      val nscore1 = getFullBinaryScore(tokenIDs, tokenFeatureIDs, tagIDs,
        spanLeft, split1, split2, spanRight, split2, curCellStates0,
        curStatePos0, curCellStates1, curStatePos1, curCellStates2,
        curStatePos2, featureCounter, lossType).toFloat
      score + nscore0 + nscore1
    }
  }

  override def curStateAsString =
    super.curStateAsString + s"-$curHash00-$curHash01"
  override def curStateAsString1 =
    super.curStateAsString1 + s"-$curHash10-$curHash11"
  override def curStateAsString2 =
    super.curStateAsString2 + s"-$curHash20-$curHash21"

  def fullCellsToString = {
    val map = HashMap[(Int, Int), ArrayBuffer[String]]()
    for {start <- 0 until sentenceLength
         end <- (start + 1) until sentenceLength} {
      val span = (start, end)
      setCurCell(span)
      while (next) {
        val clist = map.getOrElseUpdate(span, ArrayBuffer[String]())
        clist.append(curStateAsString +
          f" $insideScore%.2e $curCell0 $curStatePos0 $curPtr00 $curPtr01 $curPtr02")
        for (pos <- start to end)
          clist.append(s"${subParent(pos)} ${subArg(pos)} ${subSpine(pos)}")
        for (num <- 0 until subNumTraces)
          clist.append(s"${subTraceParent(num)} ${subTraceChild(num)} ${subTraceArg(num)}")
      }
    }

    val ans = ArrayBuffer[String]()
    map.foreach{ case (k, v) =>
      ans.append(f"Cell ${k._1}%2d ${k._2}%2d\n    "+ v.mkString("\n    "))
    }
    ans.sorted.mkString("\n") +"\n"
  }

  override def toString() = fullCellsToString

  override def checkCoarseState(
    state: Int, lSpine: Int, rSpine: Int, xSpine: Int, span: (Int, Int),
    child: Int, parent: Int, arg: Int, stateNoArg: Int, gold: Boolean,
    init: Boolean, arcSecondStage: Boolean = false, arcCreation: Int = 0,
    structural: Boolean = false, chain: Boolean = false
  ) = {
    coarseArcRatio = -1.0
    // Assume chart above is null (would be crazy to run otherwise)
    // Also assume that the chart above has the same structure (ie, also
    // projective or also 1ec).
    // Also assume we are never introducing arc types here that have not been
    // seen above (again, would be way too slow).
    if (gold && doNotPruneGoldArcs && doNotPruneGoldSpines) {
      if (check(VAdding, verbosity)) logln(s"  is gold, not pruned")
      true
    } else if (child < 0) {
      val loc = spanToCell(span)
      val hash = chartAbove.stateHash(state, lSpine, rSpine, xSpine)
      val subbeamID = getSubbeamID(chartAbove.stage.subbeamType,
        span, state, lSpine, rSpine, xSpine, arcCreation)
      val pos = chartAbove.posForHash(loc, subbeamID, hash)
      if (pos < 0) {
        if (check(VAdding, verbosity)) logln(s"  coarse does not exist, $pos")
        if (gold) {
          logln(s"Lost gold: checkCoarseState1 $thisClass $sentenceID $span "+
          stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
          s" $child $parent $arg $arcSecondStage $loc $hash $subbeamID $pos")
        }
        false
      } else {
        val insideScore = chartAbove.insideForPos(loc, pos)
        val outsideScore = chartAbove.outsideForPos(loc, pos)
        val total = insideScore + outsideScore
        val ans = chartAbove.checkPruningRatio(total)
        if (! ans && check(VAdding, verbosity)) {
          val ratio = chartAbove.calcPruningRatio(total)
          Log.logln(s"Coarse score too poor: $ratio < ${chartAbove.minRatio}")
        }
        if (!ans && gold) {
          logln(s"Lost gold: checkCoarseState2 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage")
        }
        ans
      }
    } else {
      val hash = chartAbove.stateHash(state, lSpine, rSpine, xSpine)
      val subbeam = getSubbeamID(chartAbove.stage.subbeamType, span, state,
        lSpine, rSpine, xSpine, if (arcSecondStage) 2 else 1)
      val loc = spanToCell(span)
      val dist = span._2 - span._1
      val pos = chartAbove.posForHash(loc, subbeam, hash)
      if (pos < 0) {
        if (check(VAdding, verbosity))
          logln(s"  coarse does not exist, $pos $state")
        if (gold) {
          logln(s"Lost gold: checkCoarseState3 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage")
        }
        false
      } else {
        // Get outside from above it and inside from the predecessor below it
        val arcInside = chartAbove.insideForPos(loc, pos)
        val arcOutside = chartAbove.outsideForPos(loc, pos)

        // Get the score itself
        val cSpine =
          if (child == span._1) lSpine
          else if (child == span._2) rSpine
          else xSpine
        val pSpine =
          if (parent == span._1) lSpine
          else if (parent == span._2) rSpine
          else xSpine
        val oldArg =
          if (structural) chartAbove.argSForPos(loc, pos)
          else if (chain) chartAbove.argCForPos(loc, pos)
          else chartAbove.argTForPos(loc, pos)
        val oldArcScore = chartAbove.scoreArc(child, parent, cSpine, pSpine,
          oldArg, dist, false, LossType.ZERO, false).toFloat
        val arcScore = chartAbove.scoreArc(child, parent, cSpine, pSpine, arg,
          dist, false, LossType.ZERO, false).toFloat

        // Check against the ratio cutoff
        val total = arcScore - oldArcScore + arcInside + arcOutside
        val ratio = chartAbove.calcPruningRatio(total)
        val ans = chartAbove.checkPruningRatio(total)
        if (! ans && check(VAdding, verbosity)) {
          Log.logln(s"Coarse score too poor: $ratio < ${chartAbove.minRatio}")
        }
        if (!ans && gold) {
          logln(s"Lost gold: checkCoarseState4 $thisClass $sentenceID $span "+
            stateString(state, lSpine, rSpine, xSpine, 0, 0, 0, 0) +
            s" $child $parent $arg $arcSecondStage")
        }
        coarseArcRatio = ratio
        ans
      }
    }
  }

  // This is not needed as the actual items will be checked
  def checkCoarseSpine(pos: Int, spine: Int, gold: Boolean) = true

  override def scoreArc(
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) = {
    val score = model.getArcScore(tokenIDs, tokenFeatureIDs, tagIDs,
      childIndex, parentIndex, childSpine, parentSpine, arg, length, gold,
      scoreCache, surfaceFeatureLists, countOnGet, useModelCache, edgeSpace)

    score + super.scoreArc(childIndex, parentIndex, childSpine, parentSpine,
      arg, length, gold, lossType, goldHere, countOnGet)
  }

  override def addSpineTopOptions = {
    // Not planning to use this as a pruning pass, so nothing to do here.
  }
}

class FullStructureParser(
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
  override val thisClass = "FullStructureParser."+ stage.name

  override def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldUnaries: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    new FullStructureChart(stage, coarseChart, traceChart, spineChart, tokens,
      tags, sentenceID, goldEdges, goldUnaries, goldStates, lossType,
      doNotPruneGoldArcs, doNotPruneGoldSpines, pruningRatio, pruningRank,
      verbosity, training)
  }

  override def parse(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    doOutside: Boolean = true, goldEdges: UnboxedArrayBuffer,
    goldUnaries: ArrayBuffer[Int], goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value = LossType.ZERO
  ) = {
    logln("\nParsing (full) "+ (tokens mkString " "))
    if (tags.length > 0) logln("With POS tags "+ (tags mkString " "))
    else logln("Without pre-defined POS tags")

    if (! prepared)
      prepare(tokens, tags, sentenceID, goldEdges, goldUnaries, goldStates,
        lossType)

    // Coarse
    coarseParser.foreach{
      _.parse(tokens, tags, sentenceID, true, goldEdges, goldUnaries, goldStates)
    }
    Log.logln(s"Coarse: ${coarseChart.curMinMarginal} ${coarseChart.scoreRange}")

    // Full
    val preParse = System.nanoTime()
    fineChart.doInsidePass
    val parse = fineChart.extractParse
    val postParse = System.nanoTime()

    postParseLog(postParse - preParse, parse._3, tokens, lossType)
    prepared = false

    parse
  }
}

