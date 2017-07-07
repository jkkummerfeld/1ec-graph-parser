// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Stack, Queue, Set}
import scala.math

import edu.berkeley.nlp.graphparser.Hash._
import edu.berkeley.nlp.graphparser.Log._

///import org.github.jamm.MemoryMeter

/** A chart parser that produces trees.
  *
  * Constraints:
  *  - Sentences cannot have more than 253 words (because of limits on the
  *  location of an external point, and split points for combinations)
  *
  * Possible optimisations:
  *  - Eliminiate 'hasFinal' by reversing the direction of the edge to the
  *    root. Then, 'hasFinal' becomes equivalent to 'Does this span extend to
  *    the end of the sentence? And does that end have a parent?'
  *  - Chart items could be made smaller by not recording a spine ID, but
  *    instead having the position from an index of used IDs in this parse.
  *    Switching to a 64 bit representation could also help, as would using a
  *    mapping for parent states (only ~134 valid ones exist).
  *  - Far less than 2^15 crossing+parent states (only 90 parent states, 5
  *    for I, 85 for NLRE, 20 for X = 365), so that part of the state
  *    could be reduced from 15 to 9 bits.
  *
  * AMR Notes
  * Note that this cannot produce full AMRs as it cannot produce graphs.
  * Implicit and explicit coreference are not captured. This should be used
  * mainly as a coarse pass to enable pruning for more sophisticated models.
  *
  * To avoid spurious ambiguity:
  *  - Tokens that map to the null concept must be combined to the right.
  *  - Unaries must be applied as high as possible (or low, depending on flag)
  *
  * Bugs
  *  - Able to construct structural edges that cross, e.g.
  *     # Sentence Gold also rose .
  *     # Tokens   Gold also rose .
  *     1 Gold NNP _ | 3 VP_0 NNP_0 _
  *     2 also RB ADVP | 3 S_0 ADVP_0 _
  *     3 rose VBD VP_S | 0 ROOT_0 S_0 _
  *     4 . . _ | 3 S_0 ._0 _
  *    Here 'Gold' attaches below 'also' when linking to 'rose'
  *    Solution - change spines to actually be unaries, and then when we go to
  *    traces, rely on the fact that the chain number tells us what we need to
  *    know?
  *
  * Improvements
  *  - Avoid random choice of head for unaries.
  *  AMR
  *  - Backoff process into propbank information for verbs.
  *  - Allow capitalised words that are unknown to become literals.
  *  - Allow the chains to have larger chunks, e.g. start state could be two
  *    concepts already linked together
  *  - allow insertion of initial states into the middle of the chart
  *    (assume the tokeniser has tended to over-split). Specifically, allow
  *    this for tokens that were not originally whitespace separated. This is
  *    necessary for cases like "People's",
  *    which forms:      "People's"
  *            not:      :poss "People"     or     :poss (p / person)
  *
  * Engineering
  *  - Remove magic numbers for indexing into the state.
  *
  * Behaviour unknown (probably crashes) for:
  *  - Chains with more than 15 states
  *  - Grammars with more than 127 edge types (should extend to use extra space)
  *  No longer issues?
  */

object HasParent extends Enumeration {
  type HasParent = Value
  val Neither, Right, Left = Value

  def id(value: HasParent.Value) = {
    if (value == Neither) 0
    else if (value == Right) 1
    else if (value == Left) 2
    else -1
  }
}

object ArgState extends Enumeration {
  type ArgState = Value
  val None, Direct, Indirect = Value

  def id(value: ArgState.Value) = {
    if (value == None) 0
    else if (value == Direct) 1
    else if (value == Indirect) 2
    else -1
  }

  def charToValue(char: Char) : Value = {
    if (char == 'F' || char == '_') None
    else if (! char.isLetter) {
      throw new IllegalArgumentException(s"Unknown ArgState - $char")
    } else if (char.toUpper == char) Direct
    else Indirect
  }
}

object CrossingState extends Enumeration {
  type CrossingState = Value
  val Interval, Xterval, Neither, Left, Right, Both = Value

  def id(value: CrossingState.Value) : Int = {
    if (value == Interval) 0
    else if (value == Xterval) 1
    else if (value == Neither) 2
    else if (value == Left) 3
    else if (value == Right) 4
    else if (value == Both) 5
    else -1
  }

  def charToValue(char: Char) : Value = {
    if (char == 'I') Interval
    else if (char == 'X') Xterval
    else if (char == 'N') Neither
    else if (char == 'L') Left
    else if (char == 'R') Right
    else if (char == 'E' || char == 'B') Both
    else throw new IllegalArgumentException(s"Unknown crossing state - $char")
  }
}

class PruningStats() {
  val arcsGoldStructural = UnboxedArrayBufferLong()
  val arcsGoldOther = UnboxedArrayBufferLong()
  val arcsAutoStructural = UnboxedArrayBufferLong()
  val arcsAutoOther = UnboxedArrayBufferLong()
  val binariesGold = UnboxedArrayBufferLong()
  val binariesAuto = UnboxedArrayBufferLong()
  val initGold = UnboxedArrayBufferLong()
  val initAuto = UnboxedArrayBufferLong()
  val cells = UnboxedArrayBufferLong()
  var potentialArcs = 0L
  var maxArgTypes = 0

  val ratioIncrement = 0.001
  val ratioMin = -0.01
  val ratioMax = 1.01

  {
    def addCount = {
      arcsGoldStructural.append(0L)
      arcsGoldOther.append(0L)
      arcsAutoStructural.append(0L)
      arcsAutoOther.append(0L)
      binariesGold.append(0L)
      binariesAuto.append(0L)
      initGold.append(0L)
      initAuto.append(0L)
      cells.append(0L)
    }
    var ratio = ratioMin
    addCount // holds total
    while (ratio <= ratioMax) {
      addCount
      ratio += ratioIncrement
    }
  }

  def updatePos(
    pos: Int, isArc: Boolean, isGold: Boolean, isInit: Boolean,
    hasStructuralArc: Boolean, hasTraceArc: Boolean, hasChainArc: Boolean,
    isCell: Boolean
  ) = {
    if (isCell) {
      cells(pos) += 1L
    } else if (isGold) {
      if (isArc) {
        if (hasStructuralArc) arcsGoldStructural(pos) += 1L
        if (hasTraceArc) arcsGoldOther(pos) += 1L
        if (hasChainArc) arcsGoldOther(pos) += 1L
      } else if (isInit) initGold(pos) += 1L
      else binariesGold(pos) += 1L
    } else {
      if (isArc) {
        if (hasStructuralArc) arcsAutoStructural(pos) += 1L
        if (hasTraceArc) arcsAutoOther(pos) += 1L
        if (hasChainArc) arcsAutoOther(pos) += 1L
      } else if (isInit) initAuto(pos) += 1L
      else binariesAuto(pos) += 1L
    }
  }

  def update(
    ratio: Double, isArc: Boolean, isGold: Boolean, isInit: Boolean,
    hasStructuralArc: Boolean = false, hasTraceArc: Boolean = false,
    hasChainArc: Boolean = false, isCell: Boolean = false
  ) = {
    updatePos(0, isArc, isGold, isInit, hasStructuralArc, hasTraceArc,
      hasChainArc, isCell)

    var pos = 1
    var cratio = ratioMin
    while (ratio > cratio && cratio <= ratioMax) {
      updatePos(pos, isArc, isGold, isInit, hasStructuralArc, hasTraceArc,
        hasChainArc, isCell)
      cratio += ratioIncrement
      pos += 1
    }
  }

  def updatePotential(length: Int, argTypes: Int) = {
    potentialArcs += length * length
    maxArgTypes = argTypes.max(maxArgTypes)
  }

  def addStatsToBuffer(
    first: Boolean, arcsG: Double, ratio: Double, nextGoal: Double, buffer: ArrayBuffer[String], pos: Int
  ) = {
    val binariesA = 100.0 * binariesAuto(pos) / binariesAuto(0)
    val initA = 100.0 * initAuto(pos) / initAuto(0)
    val cellsRatio = 100.0 * cells(pos) / cells(0)
    val arcsAS = 100.0 * arcsAutoStructural(pos) / arcsAutoStructural(0)
    val arcsAPS = 100.0 * arcsAutoStructural(pos) / (potentialArcs * maxArgTypes)
    val arcsAO = 100.0 * arcsAutoOther(pos) / arcsAutoOther(0)
    val arcsAPO = 100.0 * arcsAutoOther(pos) / (potentialArcs * maxArgTypes)

    val binariesG = 100.0 * binariesGold(pos) / binariesGold(0)
    val initG = 100.0 * initGold(pos) / initGold(0)
    val arcsGS = 100.0 * arcsGoldStructural(pos) / arcsGoldStructural(0)
    val arcsGO = 100.0 * arcsGoldOther(pos) / arcsGoldOther(0)

    buffer.append(f"Pruning at ~$nextGoal%.1f: $arcsG%.2f $arcsGS%.2f $arcsGO%.2f $binariesG%.2f $initG%.2f with $arcsAS%.3f $arcsAPS%.3f $arcsAO%.3f $arcsAPO%.3f $binariesA%.3f $initA%.3f $cellsRatio%.3f at $ratio%.3f")
    buffer.append(s"Pruning counts: $ratio"+
      s" as ${arcsAutoStructural(pos)} ${arcsAutoStructural(0)}"+
      s" ao ${arcsAutoOther(pos)} ${arcsAutoOther(0)}"+
      s" ap ${potentialArcs * maxArgTypes}"+
      s" i ${initAuto(pos)} ${initAuto(0)}"+
      s" b ${binariesAuto(pos)} ${binariesAuto(0)}"+
      s" c ${cells(pos)} ${cells(0)}"+
      s" gas ${arcsGoldStructural(pos)} ${arcsGoldStructural(0)}"+
      s" gao ${arcsGoldOther(pos)} ${arcsGoldOther(0)}"+
      s" gi ${initGold(pos)} ${initGold(0)}"+
      s" gc ${binariesGold(pos)} ${binariesGold(0)}"
    )
  }

  override def toString() = {
    val ans = ArrayBuffer[String]()
    var ratio = ratioMin
    var pos = 1
    var nextGoal = 99.999
    var firstDone = false
    while (ratio <= ratioMax && nextGoal > 89.9) {
      val arcsG = 100.0 *
        (arcsGoldStructural(pos) + arcsGoldOther(pos)) /
        (arcsGoldStructural(0) + arcsGoldOther(0))
      if (!firstDone) {
        firstDone = true
        addStatsToBuffer(true, arcsG, ratio, 101.0, ans, pos)
      }
      if (arcsG <= nextGoal) {
        addStatsToBuffer(false, arcsG, ratio, nextGoal, ans, pos)
        while (arcsG <= nextGoal) nextGoal -= 0.5
      }
      ratio += ratioIncrement
      pos += 1
    }

    ans.mkString("\n")
  }
}

object Chart {
  final val GOLD_MASK = 1 << 31
  final val ANTE3_MASK =  0x7fffffff
  final val ARGT_MASK =  0x3fff8000
  final val ARGC_MASK =  0x7fff
  final val EXTERNAL_NONE = 0xff
  final val EXTERNAL_LEFT = 0xfe
  final val EXTERNAL_RIGHT = 0xfd

  // All stored as ints, scores converted with:
  //  Float -> Int bits    java.lang.Float.floatToRawIntBits(  )
  //  Int bits -> Float    java.lang.Float.intBitsToFloat(  )
  // The states are stored in buckets based on the (i,j) span. The external
  // point is noted with them. This means they compete on the same beam as
  // other items covering that span with external points at other locations.
  //
  // TODO: At the moment the gold split1 and gold split2 only exist for
  // convenience when I look at output. They are not used anywhere. Consider
  // cutting? Then move isGold into that Int, simplifying things considerably.
  // State.
  // TODO: Don't include the three spines always, make that a local parser
  // addition. (unnecessary as the space used for states is actually small
  // anyway)
  // 0:
  //   (3)
  //   (3) Are there structural parents for L, R and X?
  //   (1) Is there a non-trace edge to X? (child or parent)
  //   (1) pencilworthy
  //   (8) external pos
  //   (3) IXNLRE
  //   (12) parents
  //   (1) hasFinal
  // 1  left state (32)
  // 2  right state (32)
  // 3  external state (32)
  // Backref
  // 4  split1 (8) split2 (8) gold split1 (8) gold split2 (8)
  // 5  isGold (1) [ ante3_pos (31) / empty (1) argT (15) argC (15) ]
  // 6  ante_pos (32)
  // 7  ante2_pos / arg (32)
  // 8  inside viterbi (32)
  // 9  outside viterbi (32)
  // 10 inside sum (32)
  // 11 outside sum (32)

  @inline final def genStateMiscInt(
    crossing: CrossingState.Value,
    externalPos: Int = EXTERNAL_NONE,
    pencilworthy: Boolean = true,
    hasFinal: Boolean = false,
    treeEdgesToX: Boolean = false,
    lHasStructuralParent: Boolean = false,
    rHasStructuralParent: Boolean = false,
    xHasStructuralParent: Boolean = false,
    argLR: ArgState.Value = ArgState.None,
    argLX: ArgState.Value = ArgState.None,
    argRL: ArgState.Value = ArgState.None,
    argRX: ArgState.Value = ArgState.None,
    argXL: ArgState.Value = ArgState.None,
    argXR: ArgState.Value = ArgState.None
  ) : Int = {
    var c : Int = 0

    c <<= 1
    if (lHasStructuralParent) c += 1
    c <<= 1
    if (rHasStructuralParent) c += 1
    c <<= 1
    if (xHasStructuralParent) c += 1
    c <<= 1
    if (treeEdgesToX) c += 1

    c <<= 1
    if (pencilworthy) c += 1

    c <<= 8
    c += externalPos

    c <<= 3
    c += CrossingState.id(crossing)

    c <<= 2
    c += ArgState.id(argLR)
    c <<= 2
    c += ArgState.id(argRL)
    c <<= 2
    c += ArgState.id(argLX)
    c <<= 2
    c += ArgState.id(argRX)
    c <<= 2
    c += ArgState.id(argXL)
    c <<= 2
    c += ArgState.id(argXR)

    c <<= 1
    if (hasFinal) c += 1

    c
  }

  @inline final def stateWithPositionalExt(state: Int, spanLeft: Int) = {
    val externalPos = externalPosFromState(state)
    val external =
      if (externalPos == EXTERNAL_NONE) EXTERNAL_NONE
      else if (externalPos == EXTERNAL_LEFT) EXTERNAL_LEFT
      else if (externalPos == EXTERNAL_RIGHT) EXTERNAL_RIGHT
      else if (externalPos < spanLeft) EXTERNAL_LEFT
      else EXTERNAL_RIGHT
    val ans = (state & 0xff00ffff) + (external << 16)
    ans
  }

  @inline final def stateIntToArcInt(state: Int, spanLeft: Int) = {
    val externalPos = externalPosFromState(state)
    val external =
      if (externalPos == EXTERNAL_NONE) EXTERNAL_NONE
      else if (externalPos == EXTERNAL_LEFT) EXTERNAL_LEFT
      else if (externalPos == EXTERNAL_RIGHT) EXTERNAL_RIGHT
      else if (externalPos < spanLeft) EXTERNAL_LEFT
      else EXTERNAL_RIGHT
    val ans = (state & 0x100fffe) + (external << 16)
///    println((ans, state, external, externalPos, spanLeft))
    ans
  }

  @inline final def posToNum(
    pos: Int, span0: (Int, Int, Int), span1: (Int, Int, Int),
    span2: (Int, Int, Int)
  ) = {
    // Decision to not use 0 ensures that ternary and binary cases will be
    // different in the function below.
    if (pos < 0 || pos == EXTERNAL_NONE) 1
    else if (pos < span0._1 || pos == EXTERNAL_LEFT) 2
    else if (pos > (span1._2.max(span2._2)) || pos == EXTERNAL_RIGHT) 3
    else if (pos == span0._1) 4
    else if (pos == span0._2) 5
    else if (pos == span1._2) 6
    else if (pos == span2._2) 7
    else 1
  }
  @inline final def stateIntToBinaryInt(state: Int, pos: Int) = {
    (pos << 16) +
    (if (pencilworthyFromState(state)) 1 << 15 else 0) +
    ((state >> 1) & 0x7fff)
  }
  @inline final def stateIntsToBinaryInt(
    state0: Int, span0: (Int, Int, Int),
    state1: Int, span1: (Int, Int, Int),
    state2: Int = -1, span2: (Int, Int, Int) = (-1, -1, -1)
  ) = {
    val pos2 = posToNum(span2._3, span0, span1, span2)
    val pos1 = posToNum(span1._3, span0, span1, span2)
    val pos0 = posToNum(span0._3, span0, span1, span2)
    val modState2 = if (state2 == -1) 0 else stateIntToBinaryInt(state2, pos2)
    val modState1 = stateIntToBinaryInt(state1, pos1)
    val modState0 = stateIntToBinaryInt(state0, pos0)

    var ans : Long = 0L
    ans += modState2
    ans <<= 19
    ans += modState1
    ans <<= 19
    ans += modState0

    // Handle the special case of:
    // ___I F_F___ | ___I ___I | F_F___ j_F___ | i+ __ | all | all
    // ___I F_F___ | ___I ___I | F_F___ J_F___ | i+ __ | all | all
    ans <<= 1
    if (span2._1 == -1 &&
      span0._2 - span0._1 == 1 &&
      crossingFromState(state0) == CrossingState.Interval &&
      crossingFromState(state1) == CrossingState.Interval &&
      noParentsFromState(state0) &&
      ArgState(lrFromState(state1)) != ArgState.None
    ) ans += 1

///    println((state0, span0, pos0, modState0, state1, span1, pos1, modState1, state2, span2, pos2, modState2, ans))
    ans
  }
  @inline final def stateIntsToBinaryIntTopDown(
    connections: Array[Boolean], leftHasParent: Boolean,
    goalCS: CrossingState.Value, spanG: (Int, Int, Int),
    child0CS: CrossingState.Value, span0: (Int, Int, Int),
    child1CS: CrossingState.Value, span1: (Int, Int, Int),
    child2CS: CrossingState.Value = null,
    span2: (Int, Int, Int) = (-1, -1, -1)
  ) : Long = {
    val pos0 = posToNum(span0._3, span0, span1, span2)
    val pos1 = posToNum(span1._3, span0, span1, span2)
    val pos2 = if (span2._1 < 0) 0 else posToNum(span2._3, span0, span1, span2)
    val cG = CrossingState.id(goalCS)
    val c0 = CrossingState.id(child0CS)
    val c1 = CrossingState.id(child1CS)
    val c2 = if (child2CS == null) 6 else CrossingState.id(child2CS)
///    println("top-down", connections.map(_.toString()(0)).mkString(""),
///      leftHasParent, goalCS, child0CS, child1CS, child2CS, pos0, pos1, pos2)
    var ans : Long = 0L
    if (leftHasParent) ans += 1
    for (connection <- connections) {
      ans <<= 1
      if (connection) ans += 1
    }
    ans <<= 3 ; ans += cG
    ans <<= 3 ; ans += c0
    ans <<= 3 ; ans += c1
    ans <<= 3 ; ans += c2
    ans <<= 3 ; ans += pos0
    ans <<= 3 ; ans += pos1
    ans <<= 3 ; ans += pos2
    ans
  }
  @inline final def stateIntsToBinaryIntTopDownFromFull(
    goal: Int, spanG: (Int, Int, Int),
    state0: Int, span0: (Int, Int, Int),
    state1: Int, span1: (Int, Int, Int),
    state2: Int = -1, span2: (Int, Int, Int) = (-1, -1, -1)
  ) : Long = {
    val cG = crossingFromState(goal)
    val c0 = crossingFromState(state0)
    val c1 = crossingFromState(state1)
    val c2 = if (state2 == -1) null else crossingFromState(state2)
    val connections = Array.fill(25)(false)
    val points = List(spanG._1, spanG._2, spanG._3, span0._2,
      if (state2 == -1) -1 else span1._2)
    val states = List((span0, state0), (span1, state1), (span2, state2))
    var pos = 0
    for {p <- points
         c <- points} {
      for ((span, state) <- states) {
        if (
          (span._1 == c && span._2 == p && lrFromState(state) == 1) ||
          (span._2 == c && span._1 == p && rlFromState(state) == 1) ||
          (span._1 == c && span._3 == p && lxFromState(state) == 1) ||
          (span._2 == c && span._3 == p && rxFromState(state) == 1) ||
          (span._3 == c && span._1 == p && xlFromState(state) == 1) ||
          (span._3 == c && span._2 == p && xrFromState(state) == 1)
        ) connections(pos) = true
      }
      pos += 1
    }
    val leftHasParent = (lrFromState(goal) == 2 || lxFromState(goal) == 2)

    stateIntsToBinaryIntTopDown(connections, leftHasParent, cG, spanG, c0,
      span0, c1, span1, c2, span2)
  }

  // Directly stored
  @inline final def treeEdgesToXFromState(state: Int) : Boolean =
    ((state >> 25) & 1) == 1
  @inline final def lHasStructuralParentFromState(state: Int) : Boolean =
    ((state >> 28) & 1) == 1
  @inline final def rHasStructuralParentFromState(state: Int) : Boolean =
    ((state >> 27) & 1) == 1
  @inline final def xHasStructuralParentFromState(state: Int) : Boolean =
    ((state >> 26) & 1) == 1
  @inline final def pencilworthyFromState(state: Int) : Boolean =
    ((state >> 24) & 1) == 1
  @inline final def externalPosFromState(state: Int) : Int =
    (state >>> 16) & 0xff
  @inline final def crossingFromState(state: Int) =
    CrossingState((state >>> 13) & 0x7)
  // To be read as "first" has a parent "second" (e.g. lrFromState means l is a
  // child of r)
  @inline final def lrFromState(state: Int) = (state >>> 11) & 0x3
  @inline final def rlFromState(state: Int) = (state >>> 9) & 0x3
  @inline final def lxFromState(state: Int) = (state >>> 7) & 0x3
  @inline final def rxFromState(state: Int) = (state >>> 5) & 0x3
  @inline final def xlFromState(state: Int) = (state >>> 3) & 0x3
  @inline final def xrFromState(state: Int) = (state >>> 1) & 0x3
  @inline final def noDirectEdgesFromState(state: Int) = {
    // This corresponds to an and on:
    // 01 01 01 01 01 01
    // This meands indirect (2 / 0b10) is allowed
    ((state >> 1) & 0x555) == 0
  }
  @inline final def noParentsFromState(state: Int) =
    ((state >> 1) & 0xfff) == 0
  @inline final def numDirectEdgesFromState(state: Int) = {
    (if (lrFromState(state) == 1) 1 else 0) +
    (if (rlFromState(state) == 1) 1 else 0) +
    (if (lxFromState(state) == 1) 1 else 0) +
    (if (rxFromState(state) == 1) 1 else 0) +
    (if (xlFromState(state) == 1) 1 else 0) +
    (if (xrFromState(state) == 1) 1 else 0)
  }
  @inline final def hasFinalFromState(state: Int) = (state & 0x1) != 0
  @inline final def noStructuralEdgesFromState(state: Int) = {
    ( ( !lHasStructuralParentFromState(state) ) &&
      ( lrFromState(state) != 0 || lxFromState(state) != 0) ) ||
    ( ( !rHasStructuralParentFromState(state) ) &&
      ( rlFromState(state) != 0 || rxFromState(state) != 0) ) ||
    ( ( !xHasStructuralParentFromState(state) ) &&
      ( xrFromState(state) != 0 || xlFromState(state) != 0) )
  }
  @inline final def parentsFromState(state: Int) = Vector(
    ArgState(lrFromState(state)), ArgState(lxFromState(state)),
    ArgState(rlFromState(state)), ArgState(rxFromState(state)),
    ArgState(xlFromState(state)), ArgState(xrFromState(state))
  )

  @inline final def stateString(
    state: Int, lSpine: Int, rSpine: Int, eSpine: Int, splits: Int,
    gold_ante3: Int, ante: Int, ante2_arg: Int
  ) = {
    val split = splits >>> 24
    val split2 = (splits >>> 16) & 0xff
    val isGold = (gold_ante3 & GOLD_MASK) != 0

    val t = crossingFromState(state).toString.apply(0) +
      ( if (! pencilworthyFromState(state)) "-" else "=" ) +
      ( if (! treeEdgesToXFromState(state)) "" else "p")
    val lState =
      (if (lHasStructuralParentFromState(state)) "s"
      else ".") +
      (if (ArgState(lrFromState(state)) == ArgState.None) "_"
      else if (ArgState(lrFromState(state)) == ArgState.Indirect) "r"
      else "R") +
      (if (ArgState(lxFromState(state)) == ArgState.None) "_"
      else if (ArgState(lxFromState(state)) == ArgState.Indirect) "x"
      else "X")
    val rState =
      (if (rHasStructuralParentFromState(state)) "s"
      else ".") +
      (if (ArgState(rlFromState(state)) == ArgState.None) "_"
      else if (ArgState(rlFromState(state)) == ArgState.Indirect) "l"
      else "L") +
      (if (ArgState(rxFromState(state)) == ArgState.None) "_"
      else if (ArgState(rxFromState(state)) == ArgState.Indirect) "x"
      else "X")
    val eState =
      (if (xHasStructuralParentFromState(state)) "s"
      else ".") +
      (if (ArgState(xlFromState(state)) == ArgState.None) "_"
      else if (ArgState(xlFromState(state)) == ArgState.Indirect) "l"
      else "L") +
      (if (ArgState(xrFromState(state)) == ArgState.None) "_"
      else if (ArgState(xrFromState(state)) == ArgState.Indirect) "r"
      else "R")
    val flags =
      (if (hasFinalFromState(state)) "f" else "_") +
      (if (isGold) "g" else "_")
    val ssplit =
      if (split > 0 && split2 > 0) s"$split+$split2"
      else if (split > 0) split.toString
      else "_"
    val ante2 = if (split > 0) ante2_arg.toString else "_"
    val arg =
      if (((state >> 1) & 0x555) == 0) "_"
      else ante2_arg.toString
    val arg2 =
      if (split == 0 && (gold_ante3 & 0x7fffffff) != 0) {
        (gold_ante3 & 0x7fff).toString +"."+
        ((gold_ante3 >> 15) & 0x7fff).toString
      } else "_._"
    val ante3 = if (split2 > 0) (gold_ante3 & 0x7fffffff).toString else "_"
    val ePos = externalPosFromState(state)
    s"$t.$lSpine-$lState.$rSpine-$rState.$eSpine-$eState-$ePos.$flags.[$arg.$arg2.$ssplit.$ante.$ante2.$ante3]"
  }

  @inline final def getSubbeamID(
    subbeamType: SubbeamType.Value, span: (Int, Int), state: Int, lSpine: Int,
    rSpine: Int, xSpine: Int, arcCreation: Int = 0
  ) : Long = {
    (arcCreation.toLong << 25) +
    (state.toLong & 0x1fffffe)
  }
}

abstract class Chart(
  val stage: Stage,
  val chartAbove: Chart,
  val traceChart: Chart,
  val spineChart: Chart,
  val tokens: Vector[String],
  val tags: Vector[String],
  val sentenceID: Int,
  val goldEdges: UnboxedArrayBuffer,
  val goldSpines: ArrayBuffer[Int],
  val goldStates: Set[(Int, Int, Int, Int, Int)],
  val lossType: LossType.Value,
  val doNotPruneGoldArcs: Boolean,
  val doNotPruneGoldSpines: Boolean,
  val minRatio: Double,
  val maxRank: Double,
  val verbosity: Int,
  val training: Boolean
) {
  import Chart._
  import stage.model._
  import stage._

  val model = stage.model
  val thisClass = "AbstractChart."+ stage.name

  val labelGen = new LabelGenerator(model)

  val chartScoreCache : LongDoubleMap = null

  val sentenceLength = tokens.length
  val numberOfCells = sentenceLength * (sentenceLength - 1) / 2
  val tokenIDs : Vector[Int] = model.idsForTokens(tokens)
  val tokenFeatureIDs : Vector[Int] = model.featuresForTokens(tokens)
  val tagIDs : Vector[Int] = tags.map(nonTerminalIndex(_))
  var curMinMarginal = Float.MaxValue
  var curMaxMarginal = Float.MinValue
  var scoreRange = 0.0
  var insideDone = false
  // First is a HashSet of (left, right, is chain?)
  // Second is a count of how many edges have an end at x and the other end is
  // either below y (if y < x) or above y (if y > x)
  // Third is a count of how many edges go between (a, b) and (c, d),
  // excluding edges.
  val (goldTraces, goldEdgeCounts, goldEdgeSpanCounts) =
    goldEdgesToTraces(goldEdges, sentenceLength, stage.parserType)

  val binaryExtensions = new BinaryRuleIterator
  val ternaryExtensions = new TernaryRuleIterator

  val serialisableChartData : CachedChartData =
    if (Config.cachePrefixWrite.isEmpty) null
    else new CachedChartData(tokenIDs, tagIDs)

  val (surfaceFeatureLists, scoreCache, edgeSpace) = model match {
    case dmodel: DiscriminativeModel =>
      dmodel.sentencePrepare(tokenIDs, tagIDs, training)
    case _ =>
      (Array[UnboxedArrayBuffer](null), Array[Double](0.0, 0.0), 1)
  }
  var featureCounter : (Int, Double) => Unit = null
  var useModelCache : Boolean = true

  // Tracking stats about pruning, function calls, and storage
  val statesLog = Array.fill[Long](10)(0)
  val binaryLog = Array.fill[Long](7)(0)
  val pruningLog = Array.fill[Long](6)(0)
  val arcLog = Array.fill[Long](6)(0)

  // Actual chart data storage
  val denseStates = Array.fill(numberOfCells)(UnboxedArrayBuffer())

  // Storage for tracking pruning patterns
  val arcScoresForLoggingStructural = UnboxedArrayBufferFloat()
  val arcScoresForLoggingMax = new LongDoubleMap(Config.loadFactor, 1024)
  val arcScoresForLoggingOther = UnboxedArrayBufferFloat()
///  val arcSumsForLogging = Array.fill(sentenceLength)(UnboxedArrayBufferFloat())
  val spineTopOptions = Array.fill(sentenceLength)(new LongSet(0.5, 1024))
  def hasSpineOption(pos: Int, symbol: Int, num: Int) = {
    val options = spineTopOptions(pos)
    options.contains(symbol, num)
  }

  // Map from state hashes to positions in the denseStates, managed as a beam
  val beams = Array.fill(numberOfCells)(beamType(beamMinLength,
    beamMaxLength, beamMinMultiple, beamMaxMultiple, beamMinFraction,
    beamMaxFraction))

  val beamsL = beams
  val beamsR = beams

  def minSplitGet(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) : Int
  def maxSplitGet(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) : Int
  def minSplitUpdate(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) : Unit
  def maxSplitUpdate(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) : Unit

  def checkCoarseState(
    state: Int, lSpine: Int, rSpine: Int, xSpine: Int, span: (Int, Int),
    child: Int, parent: Int, arg: Int, stateNoArg: Int, gold: Boolean,
    init: Boolean, arcSecondStage: Boolean = false, arcCreation: Int = 0,
    structural: Boolean = false, chain: Boolean = false
  ) : Boolean
  def checkCoarseSpine(pos: Int, spine: Int, gold: Boolean) : Boolean
  // TODO: Re-engineer to avoid this trace mapping via strings
  def checkCoarseTrace(child: Int, parent: Int, trace: String) = true
  // These are sets of not allowed cases:
  //   - Structural spans that cannot have an edge placed over them with the
  //   expectation that they will be crossed.
  //   - Trace edges that cannot be added.
  val crossingSet = new LongSet(Config.loadFactor, 128)
  val tracingSet = new IntSet(Config.loadFactor, 128)
  def checkCrossable(
    span: (Int, Int), edge: (Int, Int), checkGold: Boolean = false
  ) = {
    val ans =
      (
        // Not strictly necessary at the moment, as this gets checked separately
        // at the places where this is called.
        checkGold && doNotPruneGoldArcs && goldTraces.exists{ case (left, right, _) =>
          // Crosses this edge
          ( (left < edge._1 && edge._1 < right && right < edge._2) ||
            (edge._1 < left && left < edge._2 && edge._2 < right)) &&
          // Doesn't have an endpoint inside the span
          (left <= span._1 || left >= span._2) &&
          (right <= span._1 || right >= span._2)
        }
      ) || (
        ! crossingSet.contains(span._1, span._2, edge._1, edge._2)
      )
///    Log.logln(s"checkCrossable $span $edge $checkGold -> $ans")
    ans
  }
  def checkTraceable(child: Int, parent: Int, checkGold: Boolean = false) = {
    val ans =
      (
        // Not strictly necessary at the moment, as this gets checked separately
        // at the places where this is called.
        checkGold && doNotPruneGoldArcs && goldTraces.exists{ case (left, right, _) =>
          (child == left && parent == right) ||
          (child == right && parent == left)
        }
      ) || (
        ! tracingSet.contains(child, parent)
      )
///    Log.logln(s"checkTraceable $child $parent $checkGold -> $ans")
    ans
  }

  // Tracking split locations
  val maxSplit = new IntIntMap(0.5, numberOfCells, -1)
  val minSplit = new IntIntMap(0.5, numberOfCells, sentenceLength)

  def logSum(logA: Float, logB: Float) : Float = {
    val small : Float = logA.min(logB)
    val big : Float = logA.max(logB)
    // Short circuit to save time
    if (small - big < -50) big
    else big + math.log(1 + math.exp(small - big)).toFloat
  }

  def logSumExp(scores: ArrayBuffer[Double]) : Float = {
    if (scores.length == 0) 0.0f
    else {
      val big = scores.max
      var sum = 0.0
      var i = 0
      while (i < scores.length) {
        sum += math.exp(scores(i) - big)
        i += 1
      }
      (big + math.log(sum)).toFloat
    }
  }
  def logSumExp(
    scores0: ArrayBuffer[Double], scores1: ArrayBuffer[Double]
  ) : Float = {
    if (scores0.length == 0 && scores1.length == 0) 0.0f
    else {
      val big = (scores0.max).max(scores1.max)
      var sum = 0.0
      var i = 0
      while (i < scores0.length) {
        sum += math.exp(scores0(i) - big)
        i += 1
      }
      i = 0
      while (i < scores1.length) {
        sum += math.exp(scores1(i) - big)
        i += 1
      }
      (big + math.log(sum)).toFloat
    }
  }


  def stats = {
    var i = 0
    val ans = ArrayBuffer[String]()
    // 0 class
    // 0 Sentence length
    // -
    // 2 [statesLog] total state queries
    // 3 [statesLog] arc state queries
    // 4 [statesLog]
    // 5 [statesLog] init state queries
    // 6 [statesLog] binary state queries
    // 7 [statesLog] insertions
    // 8 [statesLog] updates
    // 9 [statesLog] arcs considered
    // 10 [statesLog] binary insertions
    // 11 [statesLog] binary updates
    // 12 [binaryLog] considered
    // 13 [binaryLog] not accepted based on rules
    // 14 [binaryLog] not accepted based on two non-traces crossing
    // 15 [binaryLog] not accepted based on multiple final arcs
    // 16 [binaryLog] not accepted based on lack of structural in middle
    // 17 [binaryLog] not accepted based on spines not matching
    // 18 [binaryLog] not accepted for multiple structural parents
    // 19 [pruningLog] arg skipped based on coarse
    // 20 [pruningLog] init skipped based on coarse
    // 21 [pruningLog] arg completely skipped based on coarse
    // 22 [pruningLog] spine skipped based on top pruning
    // 23 [pruningLog] arg skipped based on trace coarse
    // 24 [pruningLog] trace args considered
    // 25 [arcLog] items considered for adding to
    // 26 [arcLog] rule options considered
    // 27 [arcLog] external spines considered
    // 28 [arcLog] calls to addArcsForSpan
    // 29 [arcLog] args considered
    // 30 [arcLog]
    // 31 [spaceLog] size of max
    // 32 [spaceLog] size of min
    // 33 [spaceLog] capacity of max
    // 34 [spaceLog] capacity of min
    ans.append(
      s"s $thisClass $sentenceLength - "+
      ((for (i <- 0 until statesLog.length) yield statesLog(i).toString)
       mkString " ")+"  "+
      ((for (i <- 0 until binaryLog.length) yield binaryLog(i).toString)
       mkString " ")+"  "+
      ((for (i <- 0 until pruningLog.length) yield pruningLog(i).toString)
       mkString " ")+"  "+
      ((for (i <- 0 until arcLog.length) yield arcLog(i).toString)
       mkString " ")+"  "+
      s"${maxSplit.size} ${minSplit.size} ${maxSplit.data.length} ${minSplit.data.length}"
      )
    if (check(VDetailedStats, verbosity)) {
      while (i < numberOfCells) {
        if (denseStates(i).length > 0) {
          ans.append(s"$thisClass "+
            s"${cellToSpan(i)}\t" + s"${denseStates(i)}\t" +
            s"${beams(i).sizes}\t"
          )
        }
        i += 1
      }
    }
    if (check(VBeamStats, verbosity)) {
      for ((beam, index) <- beams.zipWithIndex) {
        val span = cellToSpan(index)
        if (beam.hasStats)
          ans.append(beam.stats(s"Beam Normal $thisClass $span stats: "))
      }
    }
    ans mkString "\n"
  }

  def stateHash(
    state: Int, leftSpine: Int, rightSpine: Int, externalSpine: Int,
    binary: Boolean = false, ternary: Boolean = false, init: Boolean = false
  ) = hashToLong(state, leftSpine, rightSpine, externalSpine)

  def spanToCell(from: Int, to: Int) : Int =
    (from * (2 * sentenceLength - from - 3) / 2 + to - 1)

  def spanToCell(span: (Int, Int)) : Int = spanToCell(span._1, span._2)

  def cellToSpan(cell: Int) = {
    var from = 0
    var increment = sentenceLength - 1
    var cur = 0
    while (cur <= cell) {
      from += 1
      cur += increment
      increment -= 1
    }
    from -= 1
    cur -= increment + 1
    (from, from + 1 + cell - cur)
  }

  var curCell0 = -1
  var curSpan00 = -1
  var curSpan01 = -1
  var curCellStates0 : UnboxedArrayBuffer = null
  var curStatePos0 = -1
  var curState00 = 0
  var curState01 = 0
  var curState02 = 0
  var curState03 = 0
  var curPtr00 = 0
  var curPtr01 = 0
  var curPtr02 = 0
  var curPtr03 = 0
  var curPtr04 = 0
  var curPtr05 = 0
  var curPtr06 = 0
  var curPtr07 = 0
  var curBeam0 : Beam = null

  var curCell1 = -1
  var curSpan10 = -1
  var curSpan11 = -1
  var curCellStates1 : UnboxedArrayBuffer = null
  var curStatePos1 = -1
  var curState10 = 0
  var curState11 = 0
  var curState12 = 0
  var curState13 = 0
  var curPtr10 = 0
  var curPtr11 = 0
  var curPtr12 = 0
  var curPtr13 = 0
  var curPtr14 = 0
  var curPtr15 = 0
  var curPtr16 = 0
  var curPtr17 = 0
  var curBeam1 : Beam = null

  var curCell2 = -1
  var curSpan20 = -1
  var curSpan21 = -1
  var curCellStates2 : UnboxedArrayBuffer = null
  var curStatePos2 = -1
  var curState20 = 0
  var curState21 = 0
  var curState22 = 0
  var curState23 = 0
  var curPtr20 = 0
  var curPtr21 = 0
  var curPtr22 = 0
  var curPtr23 = 0
  var curPtr24 = 0
  var curPtr25 = 0
  var curPtr26 = 0
  var curPtr27 = 0
  var curBeam2 : Beam = null

  def setCurCellVars(start: Int, end: Int) : Unit = {
    curSpan00 = start
    curSpan01 = end
    curCell0 = spanToCell(start, end)
    curCellStates0 = denseStates(curCell0)
  }
  def setCurCell(start: Int, end: Int, subbeams: Long*) : Unit = {
    setCurCellVars(start, end)
    curBeam0 = beams(curCell0)
    curBeam0.prepareToIterate(subbeams:_*)
  }
  def setCurCell(span: (Int, Int)) : Unit = setCurCell(span._1, span._2)
  def setCurCell(cell: Int) : Unit = {
    val span = cellToSpan(cell)
    setCurCell(span._1, span._2)
  }

  def setCurCellVars1(start: Int, end: Int) : Unit = {
    curSpan10 = start
    curSpan11 = end
    curCell1 = spanToCell(start, end)
    curCellStates1 = denseStates(curCell1)
  }
  def setCurCell1(start: Int, end: Int, subbeams: Long*) : Unit = {
    setCurCellVars1(start, end)
    curBeam1 = beams(curCell1)
    curBeam1.prepareToIterate(subbeams:_*)
  }
  def setCurCell1(span: (Int, Int)) : Unit = setCurCell1(span._1, span._2)
  def setCurCell1(cell: Int) : Unit = {
    val span = cellToSpan(cell)
    setCurCell1(span._1, span._2)
  }

  def setCurCellVars2(start: Int, end: Int) : Unit = {
    curSpan20 = start
    curSpan21 = end
    curCell2 = spanToCell(start, end)
    curCellStates2 = denseStates(curCell2)
  }
  def setCurCell2(start: Int, end: Int, subbeams: Long*) : Unit = {
    setCurCellVars2(start, end)
    curBeam2 = beams(curCell2)
    curBeam2.prepareToIterate(subbeams:_*)
  }
  def setCurCell2(span: (Int, Int)) : Unit = setCurCell2(span._1, span._2)
  def setCurCell2(cell: Int) : Unit = {
    val span = cellToSpan(cell)
    setCurCell2(span._1, span._2)
  }

  def convenienceSet0 = {
    curState00 = curCellStates0(curStatePos0)
    curState01 = curCellStates0(curStatePos0 + 1)
    curState02 = curCellStates0(curStatePos0 + 2)
    curState03 = curCellStates0(curStatePos0 + 3)
    curPtr00 = curCellStates0(curStatePos0 + 4)
    curPtr01 = curCellStates0(curStatePos0 + 5)
    curPtr02 = curCellStates0(curStatePos0 + 6)
    curPtr03 = curCellStates0(curStatePos0 + 7)
    curPtr04 = curCellStates0(curStatePos0 + 8)
    curPtr05 = curCellStates0(curStatePos0 + 9)
    curPtr06 = curCellStates0(curStatePos0 + 10)
    curPtr07 = curCellStates0(curStatePos0 + 11)
  }
  def convenienceSet1 = {
    curState10 = curCellStates1(curStatePos1)
    curState11 = curCellStates1(curStatePos1 + 1)
    curState12 = curCellStates1(curStatePos1 + 2)
    curState13 = curCellStates1(curStatePos1 + 3)
    curPtr10 = curCellStates1(curStatePos1 + 4)
    curPtr11 = curCellStates1(curStatePos1 + 5)
    curPtr12 = curCellStates1(curStatePos1 + 6)
    curPtr13 = curCellStates1(curStatePos1 + 7)
    curPtr14 = curCellStates1(curStatePos1 + 8)
    curPtr15 = curCellStates1(curStatePos1 + 9)
    curPtr16 = curCellStates1(curStatePos1 + 10)
    curPtr17 = curCellStates1(curStatePos1 + 11)
  }
  def convenienceSet2 = {
    curState20 = curCellStates2(curStatePos2)
    curState21 = curCellStates2(curStatePos2 + 1)
    curState22 = curCellStates2(curStatePos2 + 2)
    curState23 = curCellStates2(curStatePos2 + 3)
    curPtr20 = curCellStates2(curStatePos2 + 4)
    curPtr21 = curCellStates2(curStatePos2 + 5)
    curPtr22 = curCellStates2(curStatePos2 + 6)
    curPtr23 = curCellStates2(curStatePos2 + 7)
    curPtr24 = curCellStates2(curStatePos2 + 8)
    curPtr25 = curCellStates2(curStatePos2 + 9)
    curPtr26 = curCellStates2(curStatePos2 + 10)
    curPtr27 = curCellStates2(curStatePos2 + 11)
  }

  def setCurCellAndPos(cell: Int, statePos: Int) = {
    val span = cellToSpan(cell)
    curSpan00 = span._1
    curSpan01 = span._2
    curCell0 = cell
    curCellStates0 = denseStates(curCell0)
    curBeam0 = null
    curStatePos0 = statePos
    convenienceSet0
  }

  def setCurCellAndPos1(cell: Int, statePos: Int) = {
    val span = cellToSpan(cell)
    curSpan10 = span._1
    curSpan11 = span._2
    curCell1 = cell
    curCellStates1 = denseStates(curCell1)
    curBeam1 = null
    curStatePos1 = statePos
    convenienceSet1
  }

  def setCurCellAndPos2(cell: Int, statePos: Int) = {
    val span = cellToSpan(cell)
    curSpan20 = span._1
    curSpan21 = span._2
    curCell2 = cell
    curCellStates2 = denseStates(curCell2)
    curBeam2 = null
    curStatePos2 = statePos
    convenienceSet2
  }

  def next : Boolean = {
    if (curBeam0.hasNext) {
      curStatePos0 = curBeam0.next
      convenienceSet0
      true
    } else false
  }
  def next1 : Boolean = {
    if (curBeam1.hasNext) {
      curStatePos1 = curBeam1.next
      convenienceSet1
      true
    } else false
  }
  def next2 : Boolean = {
    if (curBeam2.hasNext) {
      curStatePos2 = curBeam2.next
      convenienceSet2
      true
    } else false
  }

  def posForHash(loc: Int, subBeam: Long, hash: Long) =
    beams(loc).get(subBeam, hash)

  val arcScoresForPruning = HashMap[(Int, Int), ArrayBuffer[Double]]()
  // Note: The pruning ratio is NOT actually used in pruning directly. To
  // avoid numerical issues (score ranges close to 0), the comparison is done
  // by rescaling the ratio.
  def calcPruningRatio(marginal: Double) =
    if (scoreRange < 1e-10) 1.0
    else (marginal - curMinMarginal) / scoreRange
  def checkPruningRatio(marginal: Double, child: Int = 0, parent: Int = 0) =
    marginal > ((minRatio * scoreRange) + curMinMarginal)
  def calcPruningRatio(marginal: Double, child: Int = 0, parent: Int = 0) = 1.0
  def calcPruningRank(marginal: Double) = 0.0

  // Not in use now, but intended for storing two floats in a long
  def upperFloat(num: Long) =
    java.lang.Float.intBitsToFloat((num >>> 32).toInt)
  def lowerFloat(num: Long) =
    java.lang.Float.intBitsToFloat((num & 0xffffffffL).toInt)

  // These are used when storing scores from the chart in a cache
  // First must be positive to pass checks
  def idForLocBeamHash(loc: Int, subBeam: Long, hash: Long) = {
    hashN(
      (hash >>> 32).toInt,
      (hash & 0xffffffffL).toInt,
      (subBeam >>> 32).toInt,
      (subBeam & 0xffffffffL).toInt
    ).abs
  }
  def idForLocPos(loc: Int, pos: Int) =
    (loc.toLong << 32) + (pos.toLong << 1) + 1L
  def idForArg(cIndex: Int, pIndex: Int, cSpine: Int, pSpine: Int, arg: Int) =
    hashToLong(cIndex, pIndex, cSpine, pSpine, arg) << 1

  @inline final def getFloatInPos(loc: Int, pos: Int) : Float = {
    val cell = denseStates(loc)
    java.lang.Float.intBitsToFloat(cell(pos))
  }
  @inline final def setFloatInPos(loc: Int, pos: Int, score: Float) = {
    val cell = denseStates(loc)
    cell(pos) = java.lang.Float.floatToRawIntBits(score)
  }
  def insideForPos(loc: Int, pos: Int) : Float =
    getFloatInPos(loc, pos + 8)
  def setInsideForPos(loc: Int, pos: Int, score: Float) =
    setFloatInPos(loc, pos + 8, score)
  def outsideForPos(loc: Int, pos: Int) : Float =
    getFloatInPos(loc, pos + 9)
  def setOutsideForPos(loc: Int, pos: Int, score: Float) =
    setFloatInPos(loc, pos + 9, score)
  def insideSumForPos(loc: Int, pos: Int) : Float =
    getFloatInPos(loc, pos + 10)
  def setInsideSumForPos(loc: Int, pos: Int, score: Float) =
    setFloatInPos(loc, pos + 10, score)
  def outsideSumForPos(loc: Int, pos: Int) : Float =
    getFloatInPos(loc, pos + 11)
  def setOutsideSumForPos(loc: Int, pos: Int, score: Float) =
    setFloatInPos(loc, pos + 11, score)
  def xPosForCellPos(loc: Int, pos: Int) : Int =
    externalPosFromState(denseStates(loc)(pos))
  def xSpineScoreForCellPos(loc: Int, pos: Int) : Float = {
    val x = xPosForCellPos(loc, pos)
    if (x == EXTERNAL_NONE) 0.0f
    else spineScoresMap(x).get(denseStates(loc)(pos + 3)).toFloat
  }
  def argSForPos(loc: Int, pos: Int) : Int =
    denseStates(loc)(pos + 7)
  def argCForPos(loc: Int, pos: Int) : Int =
    denseStates(loc)(pos + 5) & 0x7fff
  def argTForPos(loc: Int, pos: Int) : Int =
    (denseStates(loc)(pos + 5) >> 15) & 0x7fff

  def isPencilworthy : Boolean = pencilworthyFromState(curState00)
  def hasTreeEdgesToX : Boolean = treeEdgesToXFromState(curState00)
  def lHasStructuralParent : Boolean =
    lHasStructuralParentFromState(curState00)
  def rHasStructuralParent : Boolean =
    rHasStructuralParentFromState(curState00)
  def xHasStructuralParent : Boolean =
    xHasStructuralParentFromState(curState00)
  def lrArgState : ArgState.Value = ArgState(lrFromState(curState00))
  def lxArgState : ArgState.Value = ArgState(lxFromState(curState00))
  def rlArgState : ArgState.Value = ArgState(rlFromState(curState00))
  def rxArgState : ArgState.Value = ArgState(rxFromState(curState00))
  def xlArgState : ArgState.Value = ArgState(xlFromState(curState00))
  def xrArgState : ArgState.Value = ArgState(xrFromState(curState00))
  def crossingState : CrossingState.Value =  crossingFromState(curState00)
  def externalPos : Int = externalPosFromState(curState00)
  def hasFinal : Boolean = hasFinalFromState(curState00)
  def noDirectEdges : Boolean = noDirectEdgesFromState(curState00)
  def lSpine : Int = curState01
  def rSpine : Int = curState02
  def xSpine : Int = curState03
  def insideScore : Float = insideForPos(curCell0, curStatePos0)
  def outsideScore : Float = outsideForPos(curCell0, curStatePos0)
  def insideSum : Float = insideSumForPos(curCell0, curStatePos0)
  def outsideSum : Float = outsideSumForPos(curCell0, curStatePos0)
  def isGold : Boolean = (curPtr01 & GOLD_MASK) != 0
  def fromBinary : Boolean = (curPtr00 >>> 24) > 0
  def fromTernary : Boolean = ((curPtr00 >>> 16) & 0xff) > 0
  def goldFromBinary : Boolean = (curPtr00 & 0xff00) > 0
  def goldFromTernary : Boolean = (curPtr00 & 0xff) > 0
  def split : Int = if (fromBinary) (curPtr00 >>> 24) else -1
  def splitB : Int = if (fromTernary) ((curPtr00 >>> 16) & 0xff) else -1
  def argS : Int =
    if (fromBinary || fromTernary || curPtr03 == 0) NonArg
    else curPtr03
  def argT : Int =
    if (fromBinary || fromTernary || (curPtr01 & ARGT_MASK) == 0) NonArg
    else (curPtr01 & ARGT_MASK) >> 15
  def argC : Int =
    if (fromBinary || fromTernary || (curPtr01 & ARGC_MASK) == 0) NonArg
    else curPtr01 & ARGC_MASK
  def updateInside(score: Float, sum: Float) = {
    if (useSumMarginals) {
      val nSum = logSum(insideSum, sum)
      setInsideSumForPos(curCell0, curStatePos0, nSum)
    }
    if (insideScore < score)
      setInsideForPos(curCell0, curStatePos0, score)
  }
  def updateOutside(score: Float, sum: Float) = {
    if (check(VAdding | VTrace, verbosity))
      logln(s"  updateOutside to $score for "+
        curStateAsString +
        f" $insideScore%.4e $outsideScore%.4e $insideSum%.4e $outsideSum%.4e $curCell0"+
        s" $curStatePos0 $curPtr00 $curPtr01 $curPtr02 $curPtr03")
    if (useSumMarginals) {
      val nSum = logSum(outsideSum, sum)
      setOutsideSumForPos(curCell0, curStatePos0, nSum)
    }
    if (outsideScore < score)
      setOutsideForPos(curCell0, curStatePos0, score)
  }

  def hasTreeEdgesToX1 : Boolean = treeEdgesToXFromState(curState10)
  def lHasStructuralParent1 : Boolean =
    lHasStructuralParentFromState(curState10)
  def rHasStructuralParent1 : Boolean =
    rHasStructuralParentFromState(curState10)
  def xHasStructuralParent1 : Boolean =
    xHasStructuralParentFromState(curState10)
  def externalPos1 : Int = externalPosFromState(curState10)
  def hasFinal1 : Boolean = hasFinalFromState(curState10)
  def noDirectEdges1 : Boolean = noDirectEdgesFromState(curState10)
  def lrArgState1 : ArgState.Value = ArgState(lrFromState(curState10))
  def lxArgState1 : ArgState.Value = ArgState(lxFromState(curState10))
  def rlArgState1 : ArgState.Value = ArgState(rlFromState(curState10))
  def rxArgState1 : ArgState.Value = ArgState(rxFromState(curState10))
  def xlArgState1 : ArgState.Value = ArgState(xlFromState(curState10))
  def xrArgState1 : ArgState.Value = ArgState(xrFromState(curState10))
  def lSpine1 : Int = curState11
  def rSpine1 : Int = curState12
  def xSpine1 : Int = curState13
  def insideScore1 : Float = insideForPos(curCell1, curStatePos1)
  def outsideScore1 : Float = outsideForPos(curCell1, curStatePos1)
  def insideSum1 : Float = insideSumForPos(curCell1, curStatePos1)
  def outsideSum1 : Float = outsideSumForPos(curCell1, curStatePos1)
  def isGold1 : Boolean = (curPtr11 & GOLD_MASK) != 0
  def fromBinary1 : Boolean = (curPtr10 >>> 24) > 0
  def fromTernary1 : Boolean = ((curPtr10 >>> 16) & 0xff) > 0
  def goldFromBinary1 : Boolean = (curPtr10 & 0xffff) > 0
  def split1 : Int = if (fromBinary1) (curPtr10 >>> 24) else -1
  def splitB1 : Int = if (fromTernary1) ((curPtr10 >>> 16) & 0xff) else -1
  def arg1 : Int =
    if (fromBinary1 || fromTernary1 || curPtr13 == 0) NonArg
    else curPtr13
  def updateInside1(score: Float, sum: Float) = {
    if (useSumMarginals) {
      val nSum = logSum(insideSum1, sum)
      setInsideSumForPos(curCell1, curStatePos1, nSum)
    }
    if (insideScore1 < score)
      setInsideForPos(curCell1, curStatePos1, score)
  }
  def updateOutside1(score: Float, sum: Float) = {
    if (useSumMarginals) {
      val nSum = logSum(outsideSum1, sum)
      setOutsideSumForPos(curCell1, curStatePos1, nSum)
    }
    if (outsideScore1 < score)
      setOutsideForPos(curCell1, curStatePos1, score)
  }

  def hasTreeEdgesToX2 : Boolean = treeEdgesToXFromState(curState20)
  def lHasStructuralParent2 : Boolean =
    lHasStructuralParentFromState(curState20)
  def rHasStructuralParent2 : Boolean =
    rHasStructuralParentFromState(curState20)
  def xHasStructuralParent2 : Boolean =
    xHasStructuralParentFromState(curState20)
  def isGold2 : Boolean = (curPtr21 & GOLD_MASK) != 0
  def hasFinal2 : Boolean = hasFinalFromState(curState20)
  def externalPos2 : Int = externalPosFromState(curState20)
  def lSpine2 : Int = curState21
  def rSpine2 : Int = curState22
  def xSpine2 : Int = curState23
  def lrArgState2 : ArgState.Value = ArgState(lrFromState(curState20))
  def lxArgState2 : ArgState.Value = ArgState(lxFromState(curState20))
  def rlArgState2 : ArgState.Value = ArgState(rlFromState(curState20))
  def rxArgState2 : ArgState.Value = ArgState(rxFromState(curState20))
  def xlArgState2 : ArgState.Value = ArgState(xlFromState(curState20))
  def xrArgState2 : ArgState.Value = ArgState(xrFromState(curState20))
  def insideScore2 : Float = insideForPos(curCell2, curStatePos2)
  def outsideScore2 : Float = outsideForPos(curCell2, curStatePos2)
  def insideSum2 : Float = insideSumForPos(curCell2, curStatePos2)
  def outsideSum2 : Float = outsideSumForPos(curCell2, curStatePos2)
  def updateInside2(score: Float, sum: Float) = {
    if (useSumMarginals) {
      val nSum = logSum(insideSum2, sum)
      setInsideSumForPos(curCell2, curStatePos2, nSum)
    }
    if (insideScore2 < score)
      setInsideForPos(curCell2, curStatePos2, score)
  }
  def updateOutside2(score: Float, sum: Float) = {
    if (useSumMarginals) {
      val nSum = logSum(outsideSum2, sum)
      setOutsideSumForPos(curCell2, curStatePos2, nSum)
    }
    if (outsideScore2 < score)
      setOutsideForPos(curCell2, curStatePos2, score)
  }

  def curStateAsString =
    stateString(curState00, curState01, curState02, curState03, curPtr00,
      curPtr01, curPtr02, curPtr03)
  def curStateAsString1 =
    stateString(curState10, curState11, curState12, curState13, curPtr10,
      curPtr11, curPtr12, curPtr13)
  def curStateAsString2 =
    stateString(curState20, curState21, curState22, curState23, curPtr20,
      curPtr21, curPtr22, curPtr23)
  def curStateAsLongString =
      s"Direct from cell states:\n"+
      s"curState00 $curState00\n"+
      s"curState01 $curState01\n"+
      s"curState02 $curState02\n"+
      s"curState03 $curState03\n"+
      s"curPtr00 $curPtr00\n"+
      s"curPtr01 $curPtr01\n"+
      s"curPtr02 $curPtr02\n"+
      s"curPtr03 $curPtr03\n"+
      s"curPtr04 $curPtr04\n"+
      s"curPtr05 $curPtr05\n"+
      s"Other info:\n"+
      s"curCell0 $curCell0\n"+
      s"curSpan00 $curSpan00\n"+
      s"curSpan01 $curSpan01\n"+
      s"curStatePos0 $curStatePos0\n"
  def curStateAsLongString1 =
      s"Direct from cell states:\n"+
      s"curState10 $curState10\n"+
      s"curState11 $curState11\n"+
      s"curState12 $curState12\n"+
      s"curState13 $curState13\n"+
      s"curPtr10 $curPtr10\n"+
      s"curPtr11 $curPtr11\n"+
      s"curPtr12 $curPtr12\n"+
      s"curPtr13 $curPtr13\n"+
      s"curPtr14 $curPtr14\n"+
      s"curPtr15 $curPtr15\n"+
      s"Other info:\n"+
      s"curCell0 $curCell0\n"+
      s"curSpan10 $curSpan10\n"+
      s"curSpan11 $curSpan11\n"+
      s"curStatePos1 $curStatePos1\n"

  def update(
    curStatePos: Int, score: Float, splits: Int, gold_ante3: Int, ante: Int,
    ante2_arg: Int, loc: Int, outsidePass: Boolean, binary: Boolean
  ) : (Float, Int, Float) = {
    if (curStatePos >= 0) {
      statesLog(6) += 1
      if (! outsidePass) {
        val cScore = insideForPos(loc, curStatePos)
        val curCellStates = denseStates(loc)
        val makingGold = (gold_ante3 & GOLD_MASK) != 0
        if (check(VAdding | VTrace, verbosity))
          logln(s"\nupdate $curStatePos $score $splits $gold_ante3 $ante $ante2_arg $loc $outsidePass $binary and $cScore $makingGold")
        if (cScore < score) {
          curCellStates(curStatePos + 5) = gold_ante3
          curCellStates(curStatePos + 6) = ante
          curCellStates(curStatePos + 7) = ante2_arg
          setInsideForPos(loc, curStatePos, score)
          // If this is gold, overwrite entirely.  Otherwise for binary we
          // keep the gold split info and change the split.
          // For the rest we retain the current value.
          if (makingGold) {
            curCellStates(curStatePos + 4) = splits
          } else if (binary) {
            val nContent = (splits & 0xffff0000)
            val gSplit = curCellStates(curStatePos + 4) & 0xffff
            curCellStates(curStatePos + 4) = nContent | gSplit
          }
        } else if (makingGold) {
          // Needed to handle cases where we find the gold derivation of a
          // state after it has been created.
          if (binary) {
            val cContent = (curCellStates(curStatePos + 4) & 0xffff0000)
            val gSplit = splits & 0xffff
            curCellStates(curStatePos + 4) = cContent | gSplit
          }
        }
        (Float.MinValue, curStatePos, 0.0f)
      } else {
        val cOutside = outsideForPos(loc, curStatePos)
        val cOutsideSum = outsideSumForPos(loc, curStatePos)
        (cOutside, curStatePos, cOutsideSum)
      }
    } else (Float.MinValue, curStatePos, 0.0f)
  }

  def insert(
    loc: Int, splits: Int, gold_ante3: Int, ante: Int, ante2_arg: Int,
    score: Float, sum: Float, state: Int, leftSpine: Int, rightSpine: Int,
    externalSpine: Int, nStateHash: Long, binary: Boolean = false,
    ternary: Boolean = false, init: Boolean = false, child: Int = -1,
    parent: Int = -1, structural: Boolean = false
  ) : (Float, Int, Float) = {
///    logln(s"Inserting: $loc $ptrInt0 $ptrInt1 $ptrInt2 $ptrInt3 $score $stateInt0 $stateInt1 $stateInt2 $stateInt3")
    statesLog(5) += 1
    val curCellStates = denseStates(loc)
    val pos = curCellStates.length
    if (check(VAdding, verbosity))
      logln(s"   insert into $pos")
    curCellStates.append(state)
    curCellStates.append(leftSpine)
    curCellStates.append(rightSpine)
    curCellStates.append(externalSpine)
    curCellStates.append(splits)
    curCellStates.append(gold_ante3)
    curCellStates.append(ante)
    curCellStates.append(ante2_arg)
    curCellStates.append(java.lang.Float.floatToRawIntBits(score))
    curCellStates.append(java.lang.Float.floatToRawIntBits(Float.MinValue))
    curCellStates.append(java.lang.Float.floatToRawIntBits(sum))
    curCellStates.append(java.lang.Float.floatToRawIntBits(0.0f))
    (Float.MinValue, pos, 0.0f)
  }

  def makeBinaryFromStates(
    crossing: CrossingState.Value, argLR: ArgState.Value,
    argLX: ArgState.Value, argRL: ArgState.Value, argRX: ArgState.Value,
    argXL: ArgState.Value, argXR: ArgState.Value, score: Float,
    ternary: Boolean = false, pencilworthy: Boolean = true,
    outsidePass: Boolean = false, forcedAdd: Boolean = false
  ) : (Float, Int, Float) = {
    val span1 = curSpan00
    val split = curSpan01
    val split2 = if (ternary) curSpan11 else 0
    val span2 = if (ternary) curSpan21 else curSpan11
    // Note - ternary cases never leave an external point (hence not
    // considering externalPos2 here)
    val external =
      if (externalPos != EXTERNAL_NONE &&
        (externalPos < span1 || externalPos > span2)) externalPos
      else if (externalPos1 != EXTERNAL_NONE &&
        (externalPos1 < span1 || externalPos1 > span2)) externalPos1
      else EXTERNAL_NONE
    if (check(VMaking, verbosity)) {
      log(s"    $thisClass makeBinary ")
      log(s"$ternary $outsidePass $forcedAdd ")
      logln(s"($span1, $span2) $split $split2 $external from:")
      logln("      "+ curStateAsString + s" $insideScore at $curStatePos0")
      logln("      "+ curStateAsString1 + s" $insideScore1 at $curStatePos1")
      if (ternary)
        logln("      "+ curStateAsString2 + s" $insideScore2 at $curStatePos2")
    }
    statesLog(0) += 1
    statesLog(4) += 1

    // New state
    val cell = spanToCell((span1, span2))
    val hasFinalN = hasFinal || hasFinal1 || (ternary && hasFinal2)
    val treeEdgeToX = stage.requireTreeBasis &&
      ( (external != EXTERNAL_NONE) &&
        ( (external == externalPos && hasTreeEdgesToX) ||
          (external == externalPos1 && hasTreeEdgesToX1)))
    val lStructural = stage.requireTreeBasis &&
      ( lHasStructuralParent ||
        (externalPos1 == span1 && xHasStructuralParent1) ||
        (ternary && externalPos2 == span1 && xHasStructuralParent2))
    val rStructural = stage.requireTreeBasis &&
      ( (!ternary && rHasStructuralParent1) ||
        (ternary && rHasStructuralParent2) ||
        (externalPos == span2 && xHasStructuralParent) ||
        (ternary && externalPos1 == span2 && xHasStructuralParent1))
    val xStructural = stage.requireTreeBasis &&
      ( (external != EXTERNAL_NONE) &&
        ( (external == externalPos && xHasStructuralParent) ||
          (external == externalPos1 && xHasStructuralParent1)))
    val state = genStateMiscInt(crossing, external, pencilworthy, hasFinalN,
      treeEdgeToX, lStructural, rStructural, xStructural, argLR, argLX, argRL,
      argRX, argXL, argXR)
    val leftSpine = lSpine
    val rightSpine = if (ternary) rSpine2 else rSpine1
    val externalSpine =
      if (crossing == CrossingState.Interval) 0
      else if (external == externalPos) xSpine
      else if (external == externalPos1) xSpine1
      else if (external == externalPos2) xSpine2
      else 0

    val gold =
      isGold && isGold1 && (!ternary || isGold2) &&
      stateIsGold(cell, state, leftSpine, rightSpine, externalSpine,
        goldStates)

    val sum = insideSum + insideSum1 +
      (if (ternary) insideSum2 else 0.0f)
    val splits =
      (split << 24) +
      (if (ternary) split2 << 16 else 0) +
      (if (gold) split << 8 else 0) +
      (if (ternary && gold) split2 else 0)
    val gold_ante3 =
      (if (gold) GOLD_MASK else 0) +
      (if (ternary) curStatePos2 else 0)
    val ante = curStatePos0
    val ante2_arg = curStatePos1

    // Get location to insert (or existing location to update)
    val loc = spanToCell(span1, span2)
    val nStateHash = stateHash(state, leftSpine, rightSpine, externalSpine,
      !ternary, ternary)
    val curCellStates = denseStates(loc)
    val subbeamID = getSubbeamID(subbeamType, (span1, span2), state,
      leftSpine, rightSpine, externalSpine)
    if (check(VMaking, verbosity))
      logln(s"insertion combiantion: ($span1, $span2) $subbeamID ${stateString(state, leftSpine, rightSpine, externalSpine, splits, gold_ante3, ante, ante2_arg)}")

    val (didInsert, didUpdate, curStatePos) =
      beams(loc).update(subbeamID, nStateHash, score, curCellStates.length,
        forcedAdd)

    if (stage.parserType != ParserType.ARC && curStatePos >= 0 &&
        stage.subbeamType == SubbeamType.SPINES) {
      // Note, subbeamIDL is for things going in the left beam (which need
      // their right spine to be what is available, for comparison with the
      // left spine of the right beam).
      val subbeamIDL = subbeamID | (rightSpine.toLong << 32)
      val subbeamIDR = subbeamID | (leftSpine.toLong << 32)
      beamsL(loc).update(subbeamIDL, nStateHash, score, curStatePos, forcedAdd)
      beamsR(loc).update(subbeamIDR, nStateHash, score, curStatePos, forcedAdd)
    }

    if (check(VMaking, verbosity))
      logln(s"    $didInsert $didUpdate $curStatePos")
    if (check(VParseChartWithGold, verbosity) && forcedAdd && didInsert &&
        insideDone) {
      logln("Binary added by gold")
      log(s"    $thisClass makeBinary ")
      log(s"$ternary $outsidePass $gold $forcedAdd ")
      logln(s"($span1, $span2) $split $split2 $external from:")
      logln("      "+ curStateAsString + s" $insideScore")
      logln("      "+ curStateAsString1 + s" $insideScore1")
      if (ternary)
        logln("      "+ curStateAsString2 + s" $insideScore2")
    }

    if (didInsert) {
      statesLog(8) += 1
      insert(loc, splits, gold_ante3, ante, ante2_arg, score, sum,
        state, leftSpine, rightSpine, externalSpine, nStateHash, !ternary,
        ternary)
    } else if ((didUpdate || gold) && curStatePos >= 0) {
      statesLog(9) += 1
      update(curStatePos, score, splits, gold_ante3, ante, ante2_arg, loc,
        outsidePass, true)
    } else if (curStatePos >= 0) {
      val cOutside = outsideForPos(loc, curStatePos)
      val cOutsideSum = outsideSumForPos(loc, curStatePos)
      (cOutside, curStatePos, cOutsideSum)
    } else
      (Float.MinValue, curStatePos, 0.0f)
  }

  def makeArcFromState(
    argS: Int, argT: Int, argC: Int, child: Int, parent: Int,
    externalSpine: Int, external: Int, crossing: CrossingState.Value,
    argLR: ArgState.Value, argLX: ArgState.Value, argRL: ArgState.Value,
    argRX: ArgState.Value, argXL: ArgState.Value, argXR: ArgState.Value,
    scoreUpdate: Float, sumUpdate: Float, isSecond: Boolean, edgeToX: Boolean,
    pencilworthy: Boolean = true, isRoot: Boolean = false,
    outsidePass: Boolean = false, gold: Boolean = false,
    forcedAdd: Boolean = false
  ) : (Float, Int, Float) = {
    val span = (curSpan00, curSpan01)
    if (check(VMaking, verbosity)) {
      log(s"    $thisClass makeArc $span $scoreUpdate $argS $argT $argC ")
      log(s"$outsidePass $gold $forcedAdd from ")
      logln(curStateAsString + s" $insideScore at $curStatePos0")
    }
    statesLog(0) += 1
    statesLog(1) += 1

    // New state
    val structural = argS > 0
    val makesTreeEdgeToX = stage.requireTreeBasis &&
      (hasTreeEdgesToX || (structural && edgeToX))
    val lStructural = stage.requireTreeBasis &&
      (lHasStructuralParent || (structural && child == span._1))
    val rStructural = stage.requireTreeBasis &&
      (rHasStructuralParent || (structural && child == span._2))
    val xStructural = stage.requireTreeBasis &&
      (xHasStructuralParent ||
      (structural && child != span._1 && child != span._2))
    val state = genStateMiscInt(crossing, external,
      pencilworthy || isPencilworthy, isRoot || hasFinal, makesTreeEdgeToX,
      lStructural, rStructural, xStructural, argLR, argLX, argRL, argRX,
      argXL, argXR)
    val leftSpine = lSpine
    val rightSpine = rSpine
///    Log.logln(s"Making arc $arg for $child in $span with $scoreUpdate $insideScore")
    val score = scoreUpdate + insideScore
    val sum = sumUpdate + insideSum
    val splits = 0
    val gold_ante3 =
      (if (gold) GOLD_MASK else 0) +
      (if (argT > 0) argT << 15 else 0) +
      (if (argC > 0) argC else 0)
    val ante = curStatePos0
    val ante2_arg =
      if (argS > 0) argS
      else 0

    // Get pos
    val loc = spanToCell(span)
    var nStateHash = stateHash(state, leftSpine, rightSpine, externalSpine)
    val curCellStates = denseStates(loc)
    val arcCreation = if (isSecond) 2 else 1
    val subbeamID = getSubbeamID(subbeamType, span, state, leftSpine,
      rightSpine, externalSpine, arcCreation)
    if (check(VMaking, verbosity))
      logln(s"insertion arc: $span $subbeamID ${stateString(state, leftSpine, rightSpine, externalSpine, splits, gold_ante3, ante, ante2_arg)}")

    if (forcedAdd) {
      val curStatePos = beams(loc).get(subbeamID, nStateHash)
      if (curStatePos >= 0) {
        val prevArgS = curCellStates(curStatePos + 7)
        val prevArgT = (curCellStates(curStatePos + 5) >> 15) & 0x7fff
        val prevArgC = curCellStates(curStatePos + 5) & 0x7fff
        if (
          (prevArgS != argS.max(0)) ||
          (prevArgT != argT.max(0)) ||
          (prevArgC != argC.max(0))
        ) {
          // Change the hash to force this to be inserted, remaining different
          nStateHash = hashToLong(state, leftSpine, rightSpine, externalSpine,
            argS, argT, argC)
        }
      }
    }

    val (didInsert, didUpdate, curStatePos) =
      beams(loc).update(subbeamID, nStateHash, score, curCellStates.length,
        forcedAdd)

    if (stage.parserType != ParserType.ARC && curStatePos >= 0 &&
        stage.subbeamType == SubbeamType.SPINES) {
      // Note, subbeamIDL is for things going in the left beam (which need
      // their right spine to be what is available, for comparison with the
      // left spine of the right beam).
      val subbeamIDL = subbeamID | (rightSpine.toLong << 32)
      val subbeamIDR = subbeamID | (leftSpine.toLong << 32)
      beamsL(loc).update(subbeamIDL, nStateHash, score, curStatePos, forcedAdd)
      beamsR(loc).update(subbeamIDR, nStateHash, score, curStatePos, forcedAdd)
    }

    if (check(VMaking, verbosity))
      logln(s"    $didInsert $didUpdate $curStatePos")
    if (check(VParseChartWithGold, verbosity) && forcedAdd && didInsert &&
        insideDone) {
      logln("Arc added by gold")
      log(s"    $thisClass makeArc $span $scoreUpdate $argS $argT $argC ")
      log(s"$outsidePass $gold $forcedAdd from ")
      logln(curStateAsString + s" $insideScore")
    }

    if (didInsert)
      insert(loc, splits, gold_ante3, ante, ante2_arg, score, sum, state,
        leftSpine, rightSpine, externalSpine, nStateHash, false, false, false,
        child, parent, structural)
    else if ((didUpdate || gold) && curStatePos >= 0)
      update(curStatePos, score, splits, gold_ante3, ante, ante2_arg, loc,
        outsidePass, false)
    else if (curStatePos >= 0) {
      val cOutside = outsideForPos(loc, curStatePos)
      val cOutsideSum = outsideSumForPos(loc, curStatePos)
      (cOutside, curStatePos, cOutsideSum)
    } else
      (Float.MinValue, curStatePos, 0.0f)
  }

  def makeInitFromState(
    span: (Int, Int), leftSpine: Int, rightSpine: Int, score: Float,
    outsidePass: Boolean = false,
    gold: Boolean = false,
    forcedAdd: Boolean = false
  ) : (Float, Int, Float) = {
    if (check(VMaking, verbosity)) {
      log(s"    $thisClass makeInit $span $leftSpine $rightSpine")
      logln(s" $score $outsidePass $gold $forcedAdd")
    }
    statesLog(0) += 1
    statesLog(3) += 1

    // New state
    val state = genStateMiscInt(CrossingState.Interval)
    val externalSpine = 0
    val splits = 0
    val gold_ante3 = (if (gold) GOLD_MASK else 0)
    val ante = -1
    val ante2_arg = 0
    val sum = score

    // Get position
    val loc = spanToCell(span)
    val nStateHash = stateHash(state, leftSpine, rightSpine, externalSpine, false, false, true)
    val curCellStates = denseStates(loc)
    val subbeamID = getSubbeamID(subbeamType, span, state, leftSpine,
      rightSpine, externalSpine)

    if (check(VMaking, verbosity))
      logln(s"insertion init: $span $subbeamID ${stateString(state, leftSpine, rightSpine, externalSpine, splits, gold_ante3, ante, ante2_arg)}")

    val (didInsert, didUpdate, curStatePos) =
      beams(loc).update(subbeamID, nStateHash, score, curCellStates.length,
        forcedAdd)

    if (stage.parserType != ParserType.ARC && curStatePos >= 0 &&
        stage.subbeamType == SubbeamType.SPINES) {
      // Note, subbeamIDL is for things going in the left beam (which need
      // their right spine to be what is available, for comparison with the
      // left spine of the right beam).
      val subbeamIDL = subbeamID | (rightSpine.toLong << 32)
      val subbeamIDR = subbeamID | (leftSpine.toLong << 32)
      beamsL(loc).update(subbeamIDL, nStateHash, score, curStatePos, forcedAdd)
      beamsR(loc).update(subbeamIDR, nStateHash, score, curStatePos, forcedAdd)
    }

    if (check(VMaking, verbosity))
      logln(s"    $didInsert $didUpdate $curStatePos")
    if (check(VParseChartWithGold, verbosity) && forcedAdd && didInsert &&
        insideDone) {
      log("Init added by gold:")
      log(s" $thisClass makeInit $span $leftSpine $rightSpine")
      logln(s" $score $outsidePass $gold $forcedAdd")
    }

    if (didInsert)
      insert(loc, splits, gold_ante3, ante, ante2_arg, score, sum, state,
        leftSpine, rightSpine, externalSpine, nStateHash, false, false, true)
    else if ((didUpdate || gold) && curStatePos >= 0)
      update(curStatePos, score, splits, gold_ante3, ante, ante2_arg, loc,
        false, false)
    else if (curStatePos >= 0) {
      val cOutside = outsideForPos(loc, curStatePos)
      val cOutsideSum = outsideSumForPos(loc, curStatePos)
      (cOutside, curStatePos, cOutsideSum)
    } else
      (Float.MinValue, curStatePos, 0.0f)
  }

  def cellToString(span: (Int, Int)) = {
    val map = new HashMap[(Int, Int), ArrayBuffer[String]]
    setCurCell(span)
    while (next) map.getOrElseUpdate(span, ArrayBuffer[String]()).append(
      curStateAsString +
      f" $insideScore%.4e $outsideScore%.4e $insideSum%.4e $outsideSum%.4e $curCell0"+
      s" $curStatePos0 $curPtr00 $curPtr01 $curPtr02 $curPtr03")

    val ans = ArrayBuffer[String]()
    map.foreach{ case (k, v) =>
      if (v.length > 0)
        ans.append(f"Cell ${k._1}%2d ${k._2}%2d :  "+ v.mkString(" , "))
    }
    ans.sorted.mkString("\n") +"\n"
  }

  def setMinMarginal = {
    // TODO: This does not find the true minimum for two reasons:
    // 1 - Doesn't consider lower scoring arcs (only best is saved)
    // 2 - The rest of the parse is always the max
    // The first can be addressed by setting minMarginal during arc creation,
    // tracking the worst arc. The second can only be addressed by reparsing
    // with a min chart.
    var start = 0
    while (start < sentenceLength) {
      var end = start + 1
      while (end < sentenceLength) {
        val span = (start, end)
        setCurCell(span)
        while (next) {
          val score = outsideScore + insideScore
          if (score < curMinMarginal && score > -1e10) curMinMarginal = score
        }
        end += 1
      }
      start += 1
    }
  }

  def hashesAndScores = {
    val ans = ArrayBuffer[((Int, Int), Int, Int, Int, Double, Double, Boolean)]()
    var start = 0
    while (start < sentenceLength) {
      var end = start + 1
      while (end < sentenceLength) {
        val span = (start, end)
        setCurCell(span)
        while (next) {
          ans.append((span, curState00, curState01, curState02, outsideScore + insideScore, outsideSum + insideSum, fromBinary || fromTernary))
        }
        end += 1
      }
      start += 1
    }
    ans
  }

  def hashesAndScores(items: ArrayBuffer[(Int, Int)]) = {
    val ans = ArrayBuffer[((Int, Int), Int, Int, Int, Double, Double, Boolean, Boolean, Boolean, Boolean)]()
    for ((cell, pos) <- items) {
      setCurCellAndPos(cell, pos)
      ans.append((cellToSpan(cell), curState00, curState01, curState02, outsideScore + insideScore, outsideSum + insideSum, fromBinary || fromTernary, argS != NonArg, argT != NonArg, argC != NonArg))
    }
    ans
  }

  def cellsToString(filter: Option[HashSet[(Int, Int)]], posV: Boolean = false) = {
    val map = new HashMap[(Int, Int), ArrayBuffer[(Int, String)]]
    for {start <- 0 until sentenceLength
         end <- (start + 1) until sentenceLength} {
      val span = (start, end)
      setCurCell(span)
      while (next) {
        if ((!posV || outsideScore > 0 || (start == 0 && end == (sentenceLength - 1))) &&
            (filter.isEmpty || filter.exists(_.contains((curCell0, curStatePos0)))))
          map.getOrElseUpdate(span, ArrayBuffer[(Int, String)]()).append(
            (curStatePos0,
            curStateAsString +
            f" $insideScore%.4e $outsideScore%.4e $insideSum%.4e $outsideSum%.4e $curCell0 $curStatePos0 $curState00"))
      }
    }

    val ans = ArrayBuffer[String]()
    map.foreach{ case (k, v) =>
      if (v.length > 0)
        ans.append(f"Cell ${k._1}%2d ${k._2}%2d\n    "+
          v.sorted.map(_._2).mkString("\n    "))
    }
    ans.sorted.mkString("\n") +"\n"
  }

  override def toString() = {
    cellsToString(None)
  }

  def toDenseString(items: ArrayBuffer[(Int, Int)]) = {
    val itemStrings = items.map{ case (cell, pos) =>
      setCurCellAndPos(cell, pos)
      ((curSpan00, curSpan01), curStateAsString, pos)
    }
    val itemLists = ArrayBuffer(new ArrayBuffer[((Int, Int), String, Int)])
    val done = HashSet[((Int, Int), String, Int)]()
    def getNext(min: Int) : ((Int, Int), String, Int) = {
      var cur = ((-1, -1), "", -1)
      for (item <- itemStrings) {
        if (item._1._1 >= min && (! done.contains(item))) {
          if (cur._1._1 < 0 || cur._1._1 > item._1._1 ||
              (cur._1._1 == item._1._1 && cur._1._2 <= item._1._2)
          ) {
            if (cur._1 != item._1) cur = item
            else if (cur._3 < item._3) cur = item
          }
        }
      }
      cur
    }
    var min = 0
    while (done.size < itemStrings.length) {
      val next = {
        val tmp = getNext(min)
        if (tmp._1._1 >= 0)  tmp
        else {
          itemLists.append(new ArrayBuffer[((Int, Int), String, Int)])
          getNext(0)
        }
      }
      itemLists.last.append(next)
      done.add(next)
      min = next._1._2
    }

    // TODO: Sometimes this prints X/I out of order
    val numWidth = 4
    val ans = ArrayBuffer[String]()
    var numLine = ""
    for (i <- (0 until sentenceLength)) {
      numLine += i.toString
      numLine += " " * (numWidth * (i + 1) - numLine.length)
    }
    ans.append(numLine)
    for (itemList <- itemLists) {
      var cpos = 0
      var line = ""
      for ((span, text, pos) <- itemList) {
        if (span._1 > cpos && cpos > 0) line += "|"
        line += " " * (numWidth * span._1 - line.length)
        line += "|"
        val desc = text(0) + ""
        val space = (span._2 - span._1) * numWidth - 1 - desc.length
        if (space % 2 != 0) line += " "
        line += " " * (space / 2)
        line += desc
        line += " " * (space / 2)
        cpos = span._2
      }
      ans.append(line +"|")
    }
    ans.mkString("\n")
  }

  def extractParseItems = {
    val items = ArrayBuffer[(Int, Int)]()

    def extractParseItem(span: (Int, Int), pos: Int) : Unit = {
      val cell = spanToCell(span)
      setCurCellAndPos(cell, pos)
      items.append((cell, pos))
      val antePos = curPtr02
      if (antePos != -1) {
        // Store current values because the recursion will change the state
        val left = span._1
        val right = span._2
        val splitp1 = split
        if (splitp1 > 0) {
          val antePos1 = curPtr03
          val splitp2 = splitB
          if (splitp2 > 0) {
            val antePos2 = curPtr01 & ANTE3_MASK
            extractParseItem((left, splitp1), antePos)
            extractParseItem((splitp1, splitp2), antePos1)
            extractParseItem((splitp2, right), antePos2)
          } else {
            extractParseItem((left, splitp1), antePos)
            extractParseItem((splitp1, right), antePos1)
          }
        } else {
          extractParseItem((left, right), antePos)
        }
      }
    }

    val start = 0
    val end = sentenceLength - 1
    var max = Double.NegativeInfinity
    var statePos = -1
    setCurCell(start, end)
    while (next) {
      if (hasFinal && lrArgState != ArgState.None && insideScore > max &&
          (!stage.requireTreeBasis || lHasStructuralParent)
      ) {
        max = insideScore
        statePos = curStatePos0
      }
    }

    if (statePos != -1)
      extractParseItem((start, end), statePos)
    (items, max)
  }

  def insertDerivationBinary(
    span: (Int, Int, Int), crossing: CrossingState.Value,
    terminals: ArrayBuffer[(Int, Int)], arcs: ArrayBuffer[(Int, Int, Int)],
    items: ArrayBuffer[(Int, Int)], arcSet: HashSet[(Int, Int)],
    initItems: ArrayBuffer[Int], depth: Int, insertingGold: Boolean
  ) : (Int, Int) = {
///    if (insertingGold) Log.logln("Insert derivation binary")
    if (check(VParseExtraction, verbosity)) logln("  "*depth +s"{ Insert gold binary $span $crossing")

    // Find points corresponding to:
    //   Point with the shortest edge linking it to the external point
    //   Point with the longest edge linking it to the external point
    //   Point with the longest edge linking it to the left end
    //   Point with the longest edge linking it to the right end
    //   Point at (left + 1), only if left has no edges
    var splitL = span._1 + 1
    var splitR = -1
    var splitXnear = -1
    var splitXfar = -1
    for ((p1, p2, _) <- arcs) {
      val (edge, other) =
        if (
          (p1 == span._1 || p1 == span._2 || p1 == span._3) &&
          span._1 < p2 && p2 < span._2
        ) (p1, p2)
        else if (
          (p2 == span._1 || p2 == span._2 || p2 == span._3) &&
          span._1 < p1 && p1 < span._2
        ) (p2, p1)
        else (-1, -1)

      if (edge >= 0 && other >= 0) {
        if (edge == span._1 && (other > splitL || splitL < 0))
          splitL = other
        if (edge == span._2 && (other < splitR || splitR < 0))
          splitR = other
        if (edge == span._3 &&
            (splitXnear < 0 || (span._3 < span._1) == (other < splitXnear)))
          splitXnear = other
        if (edge == span._3 &&
            (splitXfar < 0 || ((span._3 < span._1) == (other > splitXfar))))
          splitXfar = other
      }
    }
    if (check(VParseExtraction, verbosity))
      logln("  "*depth +s"$splitL  $splitR  $splitXnear $splitXfar")

    def pencilPoint(edge: (Int, Int)) = {
      var inside = -1
      var outside = -1
      val options = arcs.map{ case (a0, a1, _) =>
        val in =
          if (edge._1 < a0 && a0 < edge._2) a0
          else if (edge._1 < a1 && a1 < edge._2) a1
          else -1
        val out =
          if (a0 < edge._1 || edge._2 < a0) a0
          else if (a1 < edge._1 || edge._2 < a1) a1
          else -1
        if (in != -1 && out != -1) {
          if (inside == -1) inside = in
          else if (inside != in) inside = -2
          if (outside == -1) outside = out
          else if (outside != out) outside = -2
        }
      }
      if (inside == -1 && outside == -1) -1
      else if (inside < 0) outside
      else if (outside < 0) inside
      else outside // convention defined in the DP
    }
    def itemTypes(
      span: (Int, Int, Int), subspan: (Int, Int),
      possibleExternal: Vector[Int]
    ) = {
      // Filter to arcs that are entirely within the larger span
      val arcsToConsider = arcs.filter{ case (a0, a1, _) =>
        (span._1 <= a0 && a0 <= span._2) ||
        (span._1 <= a1 && a1 <= span._2)
      }
      // Find arcs that have one end inside abd one end outside, the subspan.
      val fromInside = arcsToConsider.map{ case (a0, a1, _) =>
        val in0 = (subspan._1 < a0 && a0 < subspan._2)
        val in1 = (subspan._1 < a1 && a1 < subspan._2)
        val out0 = (a0 < subspan._1 || subspan._2 < a0)
        val out1 = (a1 < subspan._1 || subspan._2 < a1)
        if (in0 && out1) (a1, a0)
        else if (in1 && out0) (a0, a1)
        else (-1, -1)
      }.filter( _._1 >= 0 )

      // None that start from the inside and go external
      if (fromInside.length == 0) {
        val ans = ArrayBuffer(
          (CrossingState.Interval, (subspan._1, subspan._2, EXTERNAL_NONE)))
        val done = HashSet[Int]()
        val edgeArcs = arcsToConsider.filter{ case (a0, a1, _) =>
          (a0 != span._1 && a0 != span._2) ||
          (a1 != span._1 && a1 != span._2)
        }.foreach{ case (a0, a1, _) =>
          val equal0 = (a0 == subspan._1 || subspan._2 == a0)
          val equal1 = (a1 == subspan._1 || subspan._2 == a1)
          val out0 =
            (a0 < subspan._1 || subspan._2 < a0) &&
            ((span._1 <= a0 && a0 <= span._2) || a0 == span._3)
          val out1 =
            (a1 < subspan._1 || subspan._2 < a1) &&
            ((span._1 <= a1 && a1 <= span._2) || a1 == span._3)
          if (equal0 && out1 && !done.contains(a1) &&
              possibleExternal.contains(a1)) {
            done.add(a1)
            ans.append((CrossingState.Xterval, (subspan._1, subspan._2, a1)))
          } else if (equal1 && out0 && !done.contains(a0) &&
              possibleExternal.contains(a0)) {
            done.add(a0)
            ans.append((CrossingState.Xterval, (subspan._1, subspan._2, a0)))
          }
        }
        ans
      } else {
        // Work out the unique external points
        val externals = fromInside.map( _._1 ).distinct
        // There must be only one
        if (externals.length != 1) {
          // Nothing - can't have multiple external points
          null
        } else {
          val external = externals(0)
          var internalCrossing = false
          var leftCrossing = false
          var rightCrossing = false
          arcsToConsider.filter{ case (a0, a1, _) =>
            subspan._1 <= a0 && a0 <= subspan._2 &&
            subspan._1 <= a1 && a1 <= subspan._2 &&
            (a1 - a0).abs > 1
          }.foreach{ case (a0, a1, _) =>
            fromInside.foreach{ case (_, p) =>
              if ((a0 < p && p < a1) || (a1 < p && p < a0)) {
                val leftEnd = (a0 == subspan._1 || a1 == subspan._1)
                val rightEnd = (a0 == subspan._2 || a1 == subspan._2)
                if (leftEnd && rightEnd) { }
                else if (leftEnd) leftCrossing = true
                else if (rightEnd) rightCrossing = true
                else internalCrossing = true
              }
            }
          }
          if (internalCrossing) null
          else if (leftCrossing && rightCrossing)
            List((CrossingState.Both, (subspan._1, subspan._2, external)))
          else if (leftCrossing)
            List((CrossingState.Left, (subspan._1, subspan._2, external)))
          else if (rightCrossing)
            List((CrossingState.Right, (subspan._1, subspan._2, external)))
          else
            List((CrossingState.Neither, (subspan._1, subspan._2, external)))
        }
      }
    }

    val leftHasParent = arcs.exists{ case (child, parent, _) =>
      child == span._1 && span._1 <= parent && parent < span._2
    }
    val (leftDesc, middleDesc, rightDesc) =
      if (crossing == CrossingState.Interval) {
        val noEdgeToInside = arcs.filter{ case (a0, a1, _) =>
          (a0 == span._1 || a1 == span._1) &&
          ((span._1 < a1 && a1 < span._2) ||
           (span._1 < a0 && a0 < span._2))
        }.length == 0
        // No edge between left and internal, use split point (left + 1)
        if (noEdgeToInside) {
          val left = List(
            (CrossingState.Interval, (span._1, span._1 + 1, EXTERNAL_NONE))
          )
          val right = List(
            (CrossingState.Interval, (span._1 + 1, span._2, EXTERNAL_NONE))
          )
          (left, null, right)
        } else {
          val splits =
            if (leftHasParent) {
              val pencil = pencilPoint((splitR, span._2))
              if (pencil < 0 || pencil == span._1) (splitR, -1)
              else if (splitR < pencil) (splitR, pencil)
              else (pencil, splitR)
            } else {
              val pencil = pencilPoint((span._1, splitL))
              if (pencil < 0 || pencil == span._2) (splitL, -1)
              else if (splitL < pencil) (splitL, pencil)
              else (pencil, splitL)
            }
///          logln(s"Left indirect parent $leftHasParent, $span and $splits")
          if (splits._2 == -1) {
            val leftItems = itemTypes(span, (span._1, splits._1),
              Vector(span._2, span._3))
            val rightItems = itemTypes(span, (splits._1, span._2),
              Vector(span._1, span._3))
            (leftItems, null, rightItems)
          } else {
            val leftItems = itemTypes(span, (span._1, splits._1),
              Vector(splits._2, span._2, span._3))
            val middleItems = itemTypes(span, (splits._1, splits._2),
              Vector(span._1, span._2, span._3))
            val rightItems = itemTypes(span, (splits._2, span._2),
              Vector(splits._1, span._1, span._3))
            (leftItems, middleItems, rightItems)
          }
        }
      } else {
        val split =
          if (crossing == CrossingState.Both) {
            if (span._3 < span._1) splitR else splitL
          } else if (crossing == CrossingState.Neither) {
            splitXfar
          } else if (crossing == CrossingState.Left) {
            if (span._3 < span._1) splitXfar else splitXnear
          } else {
            if (span._3 < span._1) splitXnear else splitXfar
          }
        val leftItems = itemTypes(span, (span._1, split),
          Vector(span._2, span._3))
        val rightItems = itemTypes(span, (split, span._2),
          Vector(span._1, span._3))
        (leftItems, null, rightItems)
      }

    var validLeft : (CrossingState.Value, (Int, Int, Int), Vector[Boolean]) = null
    var validMiddle : (CrossingState.Value, (Int, Int, Int), Vector[Boolean]) = null
    var validRight : (CrossingState.Value, (Int, Int, Int), Vector[Boolean]) = null
    for {leftItem <- leftDesc
         rightItem <- rightDesc} {
      if (middleDesc == null) {
///        logln(s"$leftItem $rightItem")
        // Find direct edges between the points present:
        val edges = Array.fill(25)(false) // i j x k l
        var pos = 0
        val points = List(span._1, span._2, span._3, leftItem._2._2, -1)
        for (p <- points) {
          for (c <- points) {
///            logln(s"$parent $child ${arcSet.contains(parent, child)}")
            if (arcSet.contains((c, p)) &&
               (((p != span._1) && (p != span._2) && (p != span._3)) ||
                ((c != span._1) && (c != span._2) && (c != span._3)))
            ) edges(pos) = true
            pos += 1
          }
        }
        val id = stateIntsToBinaryIntTopDown(edges, leftHasParent, crossing,
          span, leftItem._1, leftItem._2, rightItem._1, rightItem._2)
///        logln(s"$id $crossing $span")
        rules.combineTopDown.get(id).foreach{ rule =>
///          logln(s"$id $rule")
          val stateL = rule._2(0)._1
          val stateR = rule._2(1)._1
          val leftDirect = parentsFromState(stateL).map(_ == ArgState.Direct)
          val rightDirect = parentsFromState(stateR).map(_ == ArgState.Direct)
          validLeft = (crossingFromState(stateL), leftItem._2, leftDirect)
          validRight = (crossingFromState(stateR), rightItem._2, rightDirect)
        }
      } else {
        for (middleItem <- middleDesc) {
///          logln(s"$leftItem $middleItem $rightItem")
          // Find direct edges between the points present:
          val edges = Array.fill(25)(false) // i j x k l
          val points = List(span._1, span._2, span._3, leftItem._2._2,
            middleItem._2._2)
          var pos = 0
          for (p <- points) {
            for (c <- points) {
              if (arcSet.contains((c, p)) &&
                 (((p != span._1) && (p != span._2) && (p != span._3)) ||
                  ((c != span._1) && (c != span._2) && (c != span._3))
                )
              ) edges(pos) = true
              pos += 1
            }
          }
          val id = stateIntsToBinaryIntTopDown(edges, leftHasParent,
            crossing, span, leftItem._1, leftItem._2, middleItem._1,
            middleItem._2, rightItem._1, rightItem._2)
            // For each rule consistent with this configuration
          rules.combineTopDown.get(id).foreach{ rule =>
///            logln(s"$id $rule")
            val stateL = rule._2(0)._1
            val stateM = rule._2(1)._1
            val stateR = rule._2(2)._1
            val leftDirect = parentsFromState(stateL).map(_ == ArgState.Direct)
            val middleDirect =
              parentsFromState(stateM).map(_ == ArgState.Direct)
            val rightDirect =
              parentsFromState(stateR).map(_ == ArgState.Direct)
            validLeft = (crossingFromState(stateL), leftItem._2, leftDirect)
            validMiddle = (crossingFromState(stateM), middleItem._2,
              middleDirect)
            validRight = (crossingFromState(stateR), rightItem._2, rightDirect)
          }
        }
      }
    }

    if (check(VParseExtraction, verbosity)) {
      logln("  "*depth +s"Left: $validLeft")
      logln("  "*depth +s"Middle: $validMiddle")
      logln("  "*depth +s"Right: $validRight")
    }

    // Recurse
    val left =
      if (validLeft._3.exists( e => e ))
        insertDerivationArc(validLeft._2, validLeft._1, validLeft._3,
          terminals, arcs, items, arcSet, initItems, depth + 1, insertingGold)
      else if ((validLeft._2._2 - validLeft._2._1) == 1)
        insertDerivationInit(validLeft._2, initItems, depth + 1, insertingGold)
      else
        insertDerivationBinary(validLeft._2, validLeft._1, terminals, arcs,
          items, arcSet, initItems, depth + 1, insertingGold)
    val middle =
      if (validMiddle == null) null
      else if (validMiddle._3.exists( e => e ))
        insertDerivationArc(validMiddle._2, validMiddle._1, validMiddle._3,
          terminals, arcs, items, arcSet, initItems, depth + 1, insertingGold)
      else if ((validMiddle._2._2 - validMiddle._2._1) == 1)
        insertDerivationInit(validMiddle._2, initItems, depth + 1, insertingGold)
      else
        insertDerivationBinary(validMiddle._2, validMiddle._1, terminals, arcs,
           items, arcSet,initItems, depth + 1, insertingGold)
    val right =
      if (validRight._3.exists( e => e ))
        insertDerivationArc(validRight._2, validRight._1, validRight._3,
          terminals, arcs, items, arcSet, initItems, depth + 1, insertingGold)
      else if ((validRight._2._2 - validRight._2._1) == 1)
        insertDerivationInit(validRight._2, initItems, depth + 1, insertingGold)
      else
        insertDerivationBinary(validRight._2, validRight._1, terminals, arcs,
          items, arcSet, initItems, depth + 1, insertingGold)

    // Make this binary
    setCurCellAndPos(left._1, left._2)
    if (middle != null) {
      setCurCellAndPos1(middle._1, middle._2)
      setCurCellAndPos2(right._1, right._2)
    } else {
      setCurCellAndPos1(right._1, right._2)
    }

    if (check(VParseExtraction, verbosity)) {
      logln("  "*depth +s"Binary got $left $middle $right")
      logln("  "*depth + curStateAsString)
      logln("  "*depth + curStateAsString1)
      if (middle != null) logln("  "*depth + curStateAsString2)
    }

    val binaryID =
      if (middle == null) stateIntsToBinaryInt(
        curState00, (curSpan00, curSpan01, externalPos),
        curState10, (curSpan10, curSpan11, externalPos1))
      else stateIntsToBinaryInt(
        curState00, (curSpan00, curSpan01, externalPos),
        curState10, (curSpan10, curSpan11, externalPos1),
        curState20, (curSpan20, curSpan21, externalPos2))

    val info = rules.getAndCountRuleID(binaryID)
///    Log.logln(s"r$binaryID $sentenceID $sentenceLength")
///    logln("  "*depth +s"Binary $binaryID ${stateString(info._1, 0, 0, 0, 0, 0, 0, 0)}")

    val score =
      if (middle == null)
        combiner(curSpan00, curSpan11, curSpan01, -1, curStatePos0,
          -1, curStatePos1).toFloat
      else
        combiner(curSpan00, curSpan21, curSpan01, curSpan11, curStatePos0,
          curStatePos1, curStatePos2).toFloat
    val (_, pos, _) = makeBinaryFromStates(crossing,
      ArgState(lrFromState(info._1)), ArgState(lxFromState(info._1)),
      ArgState(rlFromState(info._1)), ArgState(rxFromState(info._1)),
      ArgState(xlFromState(info._1)), ArgState(xrFromState(info._1)),
      score, middle != null, pencilworthyFromState(info._1), false, true
    )

    val cell = spanToCell((span._1, span._2))
    items.append((cell, pos))
    setCurCellAndPos(cell, pos)
///    logln(s"Inserted   ${(cell, pos)} $span ${curStateAsString} ${insideScore}")
    if (check(VParseExtraction, verbosity))
      logln("  "*depth +s"} Binary returning ${(cell, pos)} "+ curStateAsString)
    (cell, pos)
  }

  def insertDerivationArc(
    span: (Int, Int, Int), crossing: CrossingState.Value,
    directEdges: Vector[Boolean], terminals: ArrayBuffer[(Int, Int)],
    arcs: ArrayBuffer[(Int, Int, Int)], items: ArrayBuffer[(Int, Int)],
    arcSet: HashSet[(Int, Int)], initItems: ArrayBuffer[Int], depth: Int,
    insertingGold: Boolean
  ) : (Int, Int) = {
///    if (insertingGold) Log.logln("Insert derivation arc")
    if (check(VParseExtraction, verbosity))
      logln("  "*depth +s"{ Insert arc $span $crossing ${directEdges.toString}")
    // Recurse
    val nDirect = directEdges.map( if (_) 1 else 0).sum
    val secondStage = nDirect > 1
    val below =
      if (secondStage) {
        val (nspan, ncrossing) =
          if (crossing != CrossingState.Xterval) (span, crossing)
          else ((span._1, span._2, EXTERNAL_NONE), CrossingState.Interval)
        // We always remove the non-IJ edge first, and there is no case when
        // both IX and JX exist.
        val nEdges = Vector(directEdges(0), false, directEdges(2), false,
          false, false)
        insertDerivationArc(nspan, ncrossing, nEdges, terminals, arcs,
          items, arcSet, initItems, depth + 1, insertingGold)
      } else if ((span._2 - span._1) == 1)
        insertDerivationInit((span._1, span._2, EXTERNAL_NONE), initItems,
          depth + 1, insertingGold)
      else if (crossing == CrossingState.Xterval)
        insertDerivationBinary(span, CrossingState.Interval, terminals, arcs,
          items, arcSet, initItems, depth + 1, insertingGold)
      else
        insertDerivationBinary(span, crossing, terminals, arcs, items, arcSet,
          initItems, depth + 1, insertingGold)

    // Determine which arc is being created
    setCurCellAndPos(below._1, below._2)
    if (check(VParseExtraction, verbosity))
      logln("  "*depth +s"Arc got $below ${curStateAsString} $curState00")
    var narcs : (ArrayBuffer[(Int, Int, Int)], Boolean, IndexedSeq[ArgState.Value], (String, String, Boolean, Boolean, Boolean, IndexedSeq[edu.berkeley.nlp.graphparser.ArgState.Value])) = null
    val arcState = stateIntToArcInt(curState00, span._1)

    val arcOptions = rules.getAndCountArcID(arcState, secondStage)
    for (option <- arcOptions) {
///      logln("  "*depth +s"Option: $option v $secondStage $doGraphs $doCrossing")
      if (
        ((! option._4) || doGraphs) &&
        ((! option._5) || doCrossing)
      ) {
        val child = rules.positionForArcSymbol(option._2, span._1, span._2,
          span._3)
        val parent = rules.positionForArcSymbol(option._1, span._1, span._2,
          span._3)
///        logln("  "*depth +s"$child $parent from $span $directEdges")
        val possibleArcs = arcs.filter{v =>
          v._1 == child && v._2 == parent &&
          ((child == span._1 && parent == span._2 && directEdges(0)) ||
           (child == span._1 && parent == span._3 && directEdges(1)) ||
           (child == span._2 && parent == span._1 && directEdges(2)) ||
           (child == span._2 && parent == span._3 && directEdges(3)) ||
           (child == span._3 && parent == span._1 && directEdges(4)) ||
           (child == span._3 && parent == span._2 && directEdges(5)))
        }
        if (possibleArcs.length > 0) {
///          logln("  "*depth +s"Arcs: ${possibleArcs.map(_.toString).mkString(" ")}")
          // The i-j edge should always go earlier if there is a choice
          if (narcs == null) narcs = (possibleArcs, option._3, option._6, option)
          else if (
            (parent == span._1 || parent == span._2) &&
            (child == span._1 || child == span._2)
          ) narcs = (possibleArcs, option._3, option._6, option)
        }
      }
    }

    // Make
    val (subarcs, pencilworthy, newParents, arc) = narcs
    val child = subarcs(0)._1
    val parent = subarcs(0)._2
    val ext =
      if (parent < span._1 || parent > span._2) parent
      else if (child < span._1 || child > span._2) child
      else externalPos
    val nxSpine =
      if (ext == EXTERNAL_NONE) 0
      else terminals(ext)._2
    val pSpine =
      if (parent == span._1) lSpine
      else if (parent == span._2) rSpine
      else nxSpine
    val cSpine =
      if (child == span._1) lSpine
      else if (child == span._2) rSpine
      else nxSpine
    val dist = (parent - child).abs
    var argS = -2
    var argT = -2
    var argC = -2
    var score = 0.0
    var arcIsGold = true
    for ((_, _, arg) <- subarcs) {
      if (isTreeEdge(arg)) argS = arg
      else if (isChainTraceEdge(arg)) argC = arg
      else argT = arg

      val partIsGold = checkArcIsGold(parent, child, goldEdges,
        sentenceLength, arg, argS == arg, argC == arg)
      arcIsGold = arcIsGold && partIsGold

      val curScore = scoreArc(child, parent, cSpine, pSpine, arg, dist,
        partIsGold, LossType.ZERO, false, featureCounter)
///      if (check(VParseExtraction, verbosity))
///        logln("  "*depth +s"Adding arc $arg with score $curScore for $child -> $parent $cSpine $pSpine $dist")
      score += curScore
    }
    val sum = insideSum + score.toFloat
    val gold = isGold && arcIsGold
    val argLR = newParents(0)
    val argLX = newParents(1)
    val argRL = newParents(2)
    val argRX = newParents(3)
    val argXL = newParents(4)
    val argXR = newParents(5)

    val externalSpine =
      if (crossing == CrossingState.Interval) 0
      else if (crossing != CrossingState.Xterval) xSpine
      else if (child != span._3 && parent != span._3) 0
      else if (child == span._1 || child == span._2) terminals(parent)._2
      else terminals(child)._2
    val isRoot = parent == (sentenceLength - 1)
    val edgeToX = (child == span._3 || parent == span._3)
    val (_, pos, _) = makeArcFromState(argS, argT, argC, child, parent,
      externalSpine, span._3, crossing, argLR, argLX, argRL, argRX, argXL,
      argXR, score.toFloat, sum, secondStage, edgeToX, pencilworthy, isRoot,
      false, gold || insertingGold, true)

    // Check if a new state was created. If so, copy across outside scores
    // from the existing state that has a different arc.
    val structural = argS >= 0
    val makesTreeEdgeToX =
      hasTreeEdgesToX || (structural && edgeToX)
    val lStructural =
      lHasStructuralParent || (structural && child == span._1)
    val rStructural =
      rHasStructuralParent || (structural && child == span._2)
    val xStructural =
      xHasStructuralParent ||
      (structural && child != span._1 && child != span._2)
    val state0 = genStateMiscInt(crossing, span._3, pencilworthy ||
      isPencilworthy, isRoot, makesTreeEdgeToX, lStructural, rStructural,
      xStructural, argLR, argLX, argRL, argRX, argXL, argXR)
    val nStateHash = stateHash(state0, curState01, curState02, externalSpine)
    val coreSpan = (span._1, span._2)
    val loc = spanToCell(coreSpan)
    val arcCreation = if (secondStage) 2 else 1
    val subbeamID = getSubbeamID(subbeamType, coreSpan, state0, curState01,
      curState02, externalSpine, arcCreation)
    val oPos = beams(loc).get(subbeamID, nStateHash)
    if (oPos != pos) {
      val cOutside = outsideForPos(loc, oPos)
      val cOutsideSum = outsideSumForPos(loc, oPos)
      setCurCellAndPos(loc, pos)
      updateOutside(cOutside, cOutsideSum)
    }

    items.append((loc, pos))
    setCurCellAndPos(loc, pos)
///    logln(s"Inserted   ${(loc, pos)} $span ${curStateAsString} ${insideScore}")
    if (check(VParseExtraction, verbosity)) {
      logln("  "*depth +s"} Arc return ($loc, $pos) "+ curStateAsString)
      logln(s"Positions $pos / $oPos from ${denseStates(loc).length}")
      setCurCell(loc)
      while (next) {
        logln(s"$curStatePos0 ${curStateAsString}" +
            f" $insideScore%.4e $outsideScore%.4e $insideSum%.4e $outsideSum%.4e $curCell0 $curStatePos0 $curState00")
      }

      setCurCellAndPos(loc, pos)
    }
    (loc, pos)
  }

  def insertDerivationInit(
    span: (Int, Int, Int), initItems: ArrayBuffer[Int], depth: Int, insertingGold: Boolean
  ) = {
///    if (insertingGold) Log.logln("Insert derivation init")
    val ans = (spanToCell((span._1, span._2)), initItems(span._1))
    if (check(VParseExtraction, verbosity)) logln("  "*depth +s"Init returning $ans")
    ans
  }

  def insertDerivation(
    terminals: ArrayBuffer[(Int, Int)],
    arcs: ArrayBuffer[(Int, Int, Int)],
    insertingGold: Boolean = false
  ) = {
///    if (insertingGold) Log.logln("Insert derivation")
    val items = ArrayBuffer[(Int, Int)]()
    val initItems = ArrayBuffer[Int]()
    if (check(VParseExtraction, verbosity)) logln(arcs)
    if (check(VParseExtraction, verbosity)) logln(terminals)

    // We need to make sure we only query each spine's score once, to avoid
    // double counting. Also, score all with a lossType of ZERO
    var pSpine =
      if (stage.parserType == ParserType.ARC) NULL_SPINE
      else terminals(0)._2
    var pscore =
      if (stage.parserType == ParserType.ARC) 0.0f
      else scoreSpine(0, pSpine, true, LossType.ZERO, featureCounter).toFloat
    var pIsGold =
      if (insertingGold || stage.parserType == ParserType.ARC) true
      else spineIsGold(0, pSpine, goldSpines)
    for (position <- 1 until sentenceLength) {
      val nSpine =
        if (stage.parserType == ParserType.ARC) NULL_SPINE
        else terminals(position)._2
      val nscore =
        if (stage.parserType == ParserType.ARC) 0.0f
        else scoreSpine(position, nSpine, true, LossType.ZERO,
          featureCounter).toFloat
      val nIsGold =
        if (insertingGold || stage.parserType == ParserType.ARC) true
        else spineIsGold(position, nSpine, goldSpines)
      val score =
        if (position == 1 && position == sentenceLength - 1) pscore + nscore
        else if (position == 1) pscore + (nscore / 2.0f)
        else if (position == sentenceLength - 1) (pscore / 2.0f) + nscore
        else (pscore / 2.0f) + (nscore / 2.0f)
///      if (check(VParseExtraction, verbosity))
///        logln(s"Adding spines $pSpine $nSpine at $position with score $score based on $pscore $nscore")
      val cell = spanToCell(position - 1, position)
      val gold = pIsGold && nIsGold
      val (_, pos, _) = makeInitFromState((position - 1, position), pSpine,
        nSpine, score, false, gold || insertingGold, true)
      items.append((cell, pos))
      initItems.append(pos)
      pscore = nscore
      pIsGold = nIsGold
      pSpine = nSpine
    }

    val arcSet = HashSet[(Int, Int)]()
    for (arc <- arcs) arcSet.add((arc._1, arc._2))
    val finalArc = arcSet.contains( (0, sentenceLength - 1) )
    if (finalArc)
      insertDerivationArc((0, sentenceLength - 1, EXTERNAL_NONE),
        CrossingState.Interval, Vector(true, false, false, false, false,
          false), terminals, arcs, items, arcSet, initItems, 0, insertingGold)
    else
      insertDerivationBinary((0, sentenceLength - 1, EXTERNAL_NONE),
        CrossingState.Interval, terminals, arcs, items, arcSet, initItems, 0, insertingGold)
    items
  }

  def itemsToStates(items: ArrayBuffer[(Int, Int)]) = {
    items.map{ case (cell, pos) =>
      setCurCellAndPos(cell, pos)
      (cell, curState00, curState01, curState02, curState03)
    }
  }

  def extractParse = {
    // Child token number, parent token number, arg
    val arcs = ArrayBuffer[(Int, Int, Int)]()
    // Token number, unary number
    val terminals = ArrayBuffer[(Int, Int)]()
    if (check(VParseExtraction, verbosity))
      logln(s"Extracting parse for: ${tokens.mkString(" ")}")

    val (items, max) = extractParseItems
    val states = HashSet[(Int, Int)]()
    for (item <- items) {
      setCurCellAndPos(item._1, item._2)
      states.add((curCell0, curStatePos0))
///      logln(s"Extracted:  $curSpan00 $curSpan01 ${curStateAsString} $insideScore")

      if ((curSpan01 - curSpan00 == 1) && noDirectEdges) {
        // Add the terminal
        val terminal = (curSpan00, lSpine)
        if (check(VParseExtraction, verbosity)) logln(s"Terminal: $terminal")
        terminals.append(terminal)
        if (curSpan01 == sentenceLength - 1) {
          val terminal = (curSpan01, rSpine)
          if (check(VParseExtraction, verbosity)) logln(s"Terminal: $terminal")
          terminals.append(terminal)
        }
      } else if (fromBinary || fromTernary) {
        // Nothing to do
      } else {
        val args = Vector(argS, argT, argC).filter(_ != NonArg)
        val antePos = curPtr02
        setCurCellAndPos1(curCell0, antePos)
        val (l, r, x) = (curSpan00, curSpan01, externalPos)
        args.map{ arg =>
          val arc =
            if (lrArgState1 != lrArgState && lrArgState == ArgState.Direct)
              (l, r, arg)
            else if (lxArgState1 != lxArgState && lxArgState == ArgState.Direct)
              (l, x, arg)
            else if (rlArgState1 != rlArgState && rlArgState == ArgState.Direct)
              (r, l, arg)
            else if (rxArgState1 != rxArgState && rxArgState == ArgState.Direct)
              (r, x, arg)
            else if (xlArgState1 != xlArgState && xlArgState == ArgState.Direct)
              (x, l, arg)
            else if (xrArgState1 != xrArgState && xrArgState == ArgState.Direct)
              (x, r, arg)
            else throw new Exception("Can't work out which arc was created")
          if (check(VParseExtraction, verbosity)) logln(s"Arc: $arc")
          arcs.append(arc)
        }
      }
    }

    if (check(VParseChart, verbosity)) {
      logln("States for parse:\n"+ cellsToString(Some(states)))
      logln(s"States: $states\nArcs: $arcs\nterminals: $terminals\nmax: $max")
    }
    if (check(VParseExtraction, verbosity)) logln(toDenseString(items))

    (arcs, terminals, max)
  }

  def lossForArc(end0: Int, end1: Int) = {
    (
      if (
        checkArcIsGold(end0, end1, goldEdges, sentenceLength, 0,
          true, false, false) ||
        checkArcIsGold(end1, end0, goldEdges, sentenceLength, 0,
          true, false, false)
      ) Config.lossWeightMissedS
      else 0.0
    ) + (
      if (
        checkArcIsGold(end0, end1, goldEdges, sentenceLength, 0,
          false, false, false) ||
        checkArcIsGold(end1, end0, goldEdges, sentenceLength, 0,
          false, false, false)
      ) Config.lossWeightMissedT
      else 0.0
    ) + (
      if (
        checkArcIsGold(end0, end1, goldEdges, sentenceLength, 0,
          false, true, false) ||
        checkArcIsGold(end1, end0, goldEdges, sentenceLength, 0,
          false, true, false)
      ) Config.lossWeightMissedT
      else 0.0
    )
  }
  def combiner(
    spanLeft: Int, spanRight: Int, split1: Int, split2: Int, leftPos: Int,
    middlePos: Int, rightPos: Int
  ) : Double = {
    if (middlePos < 0) {
      val leftCell = spanToCell(spanLeft, split1)
      val rightCell = spanToCell(split1, spanRight)
      val leftX = xPosForCellPos(leftCell, leftPos)
      val rightX = xPosForCellPos(rightCell, rightPos)

      val externalScore =
        if (leftX == rightX && leftX != EXTERNAL_NONE)
          xSpineScoreForCellPos(leftCell, leftPos)
        else if (leftX == spanRight)
          xSpineScoreForCellPos(leftCell, leftPos)
        else if (rightX == spanLeft)
          xSpineScoreForCellPos(rightCell, rightPos)
        else
          0.0f

      val baseScore =
        insideForPos(leftCell, leftPos) +
        insideForPos(rightCell, rightPos) -
        externalScore

      setCurCellAndPos(leftCell, leftPos)
      setCurCellAndPos1(rightCell, rightPos)
      var additionalScore = getBinaryScorePSG(tokenIDs, tokenFeatureIDs,
        tagIDs, spanLeft, split1, -1, spanRight, curState00, -1, curState10,
        lSpine, rSpine, -1, rSpine1, xSpine, -1, xSpine1, sentenceID.toInt,
        scoreCache, featureCounter, useModelCache)

      // TODO: Rather than all of this complicated and time consuming
      // calculating, just assign half of the loss to each end of the missed
      // arc. Be careful of arcs with an endpoint at 0 (other end can't happen
      // by construction).
      //
      // Handle hamming loss for missing edges ending at split1.  If there
      // is a gold edge between them, but there is no direct edge in these
      // items, then add lossWeight to the score.
      if (lossType == LossType.HAMMING) {
        //     (split1, spanLeft)
        if (lrArgState != ArgState.Direct &&
            rlArgState != ArgState.Direct && (
              rightX != spanLeft || (
                lxArgState1 != ArgState.Direct &&
                xlArgState1 != ArgState.Direct
        ))) additionalScore += lossForArc(spanLeft, split1)

        //     (split1, spanRight)
        if (lrArgState1 != ArgState.Direct &&
            rlArgState1 != ArgState.Direct && (
              leftX != spanRight || (
                rxArgState != ArgState.Direct &&
                xrArgState != ArgState.Direct
        ))) additionalScore += lossForArc(spanRight, split1)

        //     (split1, outside to the left)
        val curL = goldEdgeCounts.get2IntsOrElse(split1, spanLeft, (0, 0))
        additionalScore +=
          Config.lossWeightMissedS * curL._1 +
          Config.lossWeightMissedT * curL._2
        //     (split1, outside to the right)
        val curR = goldEdgeCounts.get2IntsOrElse(split1, spanRight, (0, 0))
        additionalScore +=
          Config.lossWeightMissedS * curR._1 +
          Config.lossWeightMissedT * curR._2
        // Remove cases that are double counted by the application of the
        // previous two lines in items that contributed to these two items (ie
        // edges that go from within the left span to within the right span)
        val curB = goldEdgeSpanCounts.get2IntsOrElse(
          spanLeft, split1, split1, spanRight, (0, 0))
        additionalScore -=
          Config.lossWeightMissedS * curB._1 +
          Config.lossWeightMissedT * curB._2

        // Remove cases that are incorrectly counted because of external
        // points
        //     (split1, leftX) - careful for double ups here
        if (leftX != spanLeft && leftX != spanRight &&
            leftX != EXTERNAL_NONE && (
              xrArgState == ArgState.Direct ||
              rxArgState == ArgState.Direct
        )) additionalScore -= lossForArc(leftX, split1)
        //     (split1, rightX)
        if (rightX != spanLeft && rightX != spanRight &&
            rightX != EXTERNAL_NONE && (
              xlArgState1 == ArgState.Direct ||
              lxArgState1 == ArgState.Direct
        )) additionalScore -= lossForArc(rightX, split1)

        // If this is the final rule (ie, full span of sentence, no
        // ability to add an arc from 0 to end)
        if (spanLeft == 0 && spanRight == sentenceLength - 1 &&
            (hasFinal || hasFinal1)
        ) additionalScore += lossForArc(spanLeft, spanRight)
      }

      baseScore + additionalScore
    } else {
      val leftCell = spanToCell(spanLeft, split1)
      val middleCell = spanToCell(split1, split2)
      val rightCell = spanToCell(split2, spanRight)

      val leftX = xPosForCellPos(leftCell, leftPos)
      val middleX = xPosForCellPos(middleCell, middlePos)
      val rightX = xPosForCellPos(rightCell, rightPos)

      val additionalScore =
        if (stage.doCrossing || stage.doGraphs) {
          setCurCellAndPos(leftCell, leftPos)
          setCurCellAndPos1(middleCell, middlePos)
          setCurCellAndPos2(rightCell, rightPos)
          var score = getBinaryScorePSG(tokenIDs, tokenFeatureIDs, tagIDs,
            spanLeft, split1, split2, spanRight, curState00, curState10,
            curState20, lSpine, rSpine, rSpine1, rSpine2, xSpine, xSpine1,
            xSpine2, sentenceID.toInt, scoreCache, featureCounter,
            useModelCache)

          // Handle hamming loss for missing edges ending at split1 or split2.
          // If there is an edge between them, but there is no direct edge in
          // these items, then add to the score.
          if (lossType == LossType.HAMMING) {
            //     (split1, spanLeft)
            if (lrArgState != ArgState.Direct &&
                rlArgState != ArgState.Direct && (
                  middleX != spanLeft || (
                    lxArgState1 != ArgState.Direct &&
                    xlArgState1 != ArgState.Direct
            ))) score += lossForArc(spanLeft, split1)

            //     (split2, spanRight)
            if (lrArgState2 != ArgState.Direct &&
                rlArgState2 != ArgState.Direct && (
                  middleX != spanRight || (
                    rxArgState1 != ArgState.Direct &&
                    xrArgState1 != ArgState.Direct
            ))) score += lossForArc(spanRight, split2)

            //     (split1, split2)
            if (lrArgState1 != ArgState.Direct &&
                rlArgState1 != ArgState.Direct && (
                  leftX != split2 || (
                    rxArgState != ArgState.Direct &&
                    xrArgState != ArgState.Direct
                )) && (
                  rightX != split1 || (
                    lxArgState2 != ArgState.Direct &&
                    xlArgState2 != ArgState.Direct
                ))
            ) score += lossForArc(split1, split2)

            //     (split1, spanRight)
            if ((leftX != spanRight || (
                  rxArgState != ArgState.Direct &&
                  xrArgState != ArgState.Direct
                )) && (
                middleX != spanRight || (
                  lxArgState1 != ArgState.Direct &&
                  xlArgState1 != ArgState.Direct
                )) && (
                rightX != split1 || (
                  rxArgState2 != ArgState.Direct &&
                  xrArgState2 != ArgState.Direct
                ))
            ) score += lossForArc(split1, spanRight)

            //     (split2, spanLeft)
            if ((leftX != split2 || (
                  lxArgState != ArgState.Direct &&
                  xlArgState != ArgState.Direct
                )) && (
                middleX != spanLeft || (
                  rxArgState1 != ArgState.Direct &&
                  xrArgState1 != ArgState.Direct
                )) && (
                rightX != spanLeft || (
                  lxArgState2 != ArgState.Direct &&
                  xlArgState2 != ArgState.Direct
                ))
            ) score += lossForArc(split2, spanLeft)

            //     (split1, outside to the left)
            val cur1L = goldEdgeCounts.get2IntsOrElse(split1, spanLeft, (0, 0))
            score +=
              Config.lossWeightMissedS * cur1L._1 +
              Config.lossWeightMissedT * cur1L._2
            //     (split2, outside to the left)
            val cur2L = goldEdgeCounts.get2IntsOrElse(split2, spanLeft, (0, 0))
            score +=
              Config.lossWeightMissedS * cur2L._1 +
              Config.lossWeightMissedT * cur2L._2
            //     (split1, outside to the right)
            val cur1R = goldEdgeCounts.get2IntsOrElse(split1, spanRight, (0, 0))
            score +=
              Config.lossWeightMissedS * cur1R._1 +
              Config.lossWeightMissedT * cur1R._2
            //     (split2, outside to the right)
            val cur2R = goldEdgeCounts.get2IntsOrElse(split2, spanRight, (0, 0))
            score +=
              Config.lossWeightMissedS * cur2R._1 +
              Config.lossWeightMissedT * cur2R._2
            // Remove cases that are double counted by the previous four lines
            val cur3 = goldEdgeSpanCounts.get2IntsOrElse(
              spanLeft, split1, split1, split2, (0, 0))
            score -=
              (Config.lossWeightMissedS * cur3._1 +
              Config.lossWeightMissedT * cur3._2)
            val cur4 = goldEdgeSpanCounts.get2IntsOrElse(
              spanLeft, split1, split2, spanRight, (0, 0))
            score -=
              (Config.lossWeightMissedS * cur4._1 +
              Config.lossWeightMissedT * cur4._2)
            val cur5 = goldEdgeSpanCounts.get2IntsOrElse(
              split1, split2, split2, spanRight, (0, 0))
            score -=
              (Config.lossWeightMissedS * cur5._1 +
              Config.lossWeightMissedT * cur5._2)

            // If this is the final rule (ie, full span of sentence, no
            // ability to add an arc from 0 to end)
            if (spanLeft == 0 && spanRight == sentenceLength - 1 &&
                (hasFinal || hasFinal1 || hasFinal2)
            ) score += lossForArc(spanLeft, spanRight)
          }

          score
        } else 0.0

      val baseScore =
        insideForPos(leftCell, leftPos) -
        xSpineScoreForCellPos(leftCell, leftPos) +
        insideForPos(middleCell, middlePos) -
        xSpineScoreForCellPos(middleCell, middlePos) +
        insideForPos(rightCell, rightPos) -
        xSpineScoreForCellPos(rightCell, rightPos)

      baseScore + additionalScore
    }
  }

  def prepareBinaryPruning(
    pruners: ArrayBuffer[MetaPruningCube], beamLeft: Beam, span: (Int, Int), split: Int
  ) = {
    pruners.append(new MetaPruningCube(cubeDepth))

///    val prunerMap = new IntIntMap(0.5, 1024)
    val cellRightBinary = spanToCell(split, span._2)
    val beamRightBinary = beamsR(cellRightBinary)
    // Iterate through all IDs on the left beam
    val idIterLeft = beamLeft.idIterator
    while (idIterLeft.hasNext) {
      val idLeft = idIterLeft.next
      val leftPseudoState = (idLeft & 0xffffffff).toInt
      val left_I_FF_long =
        noParentsFromState(leftPseudoState) &&
        crossingFromState(leftPseudoState) == CrossingState.Interval &&
        (split - span._1) > 1
      val leftX = Chart.externalPosFromState(idLeft.toInt)

      // Get all valid binary rule extensions
      rules.getAllowedExtensionsBinary(idLeft, span, split,
        sentenceLength - 1, binaryExtensions)
      // Loop through valid right IDs
      while (binaryExtensions.hasNext) {
        binaryExtensions.next
        val idRight : Long = binaryExtensions.idRight +
          ( if (stage.parserType == ParserType.ARC ||
                stage.subbeamType != SubbeamType.SPINES) 0
            else idLeft & 0xffffffff00000000L )
        val rightPseudoState = (idRight & 0xffffffff).toInt
        if (left_I_FF_long &&
            crossingFromState(rightPseudoState) == CrossingState.Interval) {
          // skip, this is a case of:
          //   I-FF[i, i+n]   I
          // Which is not allowed if n > 1
        } else if (beamRightBinary.hasID(idRight)) {
///          val prunerNum = prunerMap.getOrElseUpdate(binaryExtensions.idGoal, pruners.length)
///          val pruner =
///            if (prunerNum < pruners.length) pruners(prunerNum)
///            else {
///              val ans = new MetaPruningCube(cubeDepth)
///              pruners.append(ans)
///              ans
///            }
          val pruner = pruners(0)
          pruner.addBeamPair(beamLeft, null, beamRightBinary, idLeft, -1,
            idRight, span, split, -1, this)
        }
      }

      if (doCrossing && (span._2 - split) > 1) {
        if (leftX == EXTERNAL_NONE) {
          // If this lefID has no external, loop through second split points
          val split1 = split
          rules.getAllowedExtensionsTernary(idLeft, span, split1,
            ternaryExtensions)
          // Loop through valid middle and right IDs
          while (ternaryExtensions.hasNext) {
            ternaryExtensions.next
            val idMiddle = ternaryExtensions.idMiddle +
              ( if (stage.parserType == ParserType.ARC ||
                    stage.subbeamType != SubbeamType.SPINES) 0
                else idLeft & 0xffffffff00000000L )
            val idRight = ternaryExtensions.idRight
            var split2 = split + 1
            while (split2 < span._2) {
              val cellMiddle = spanToCell(split1, split2)
              val beamMiddle = beamsR(cellMiddle)
              if (beamMiddle.hasID(idMiddle)) {
                val cellRight = spanToCell(split2, span._2)
                val beamRight = beams(cellRight)
                if (beamRight.hasID(idRight)) {
///                  val prunerNum = prunerMap.getOrElseUpdate(binaryExtensions.idGoal, pruners.length)
///                  val pruner =
///                    if (prunerNum < pruners.length) pruners(prunerNum)
///                    else {
///                      val ans = new MetaPruningCube(cubeDepth)
///                      pruners.append(ans)
///                      ans
///                    }
                  val pruner = pruners(0)
                  pruner.addBeamPair(beamLeft, beamMiddle, beamRight, idLeft,
                    idMiddle, idRight, span, split1, split2, this)
                }
              }
              split2 += 1
            }
          }
        } else {
          // Use external as second split
          val split1 = split
          val split2 = leftX
          val cellMiddle = spanToCell(split1, split2)
          val beamMiddle = beamsR(cellMiddle)
          val cellRight = spanToCell(split2, span._2)
          val beamRight = beams(cellRight)
          rules.getAllowedExtensionsTernary(idLeft, span, split1,
            ternaryExtensions)
          // Loop through valid middle and right IDs
          while (ternaryExtensions.hasNext) {
            ternaryExtensions.next
            val idMiddle = ternaryExtensions.idMiddle +
              ( if (stage.parserType == ParserType.ARC ||
                    stage.subbeamType != SubbeamType.SPINES) 0
                else idLeft & 0xffffffff00000000L )
            val idRight = ternaryExtensions.idRight
            if (beamMiddle.hasID(idMiddle) && beamRight.hasID(idRight)) {
///              val prunerNum = prunerMap.getOrElseUpdate(binaryExtensions.idGoal, pruners.length)
///              val pruner =
///                if (prunerNum < pruners.length) pruners(prunerNum)
///                else {
///                  val ans = new MetaPruningCube(cubeDepth)
///                  pruners.append(ans)
///                  ans
///                }
              val pruner = pruners(0)
              pruner.addBeamPair(beamLeft, beamMiddle, beamRight, idLeft,
                idMiddle, idRight, span, split1, split2, this)
            }
          }
        }
      }
    }
  }

  def addBinaries(span: (Int, Int), outside: Boolean = false) = {
    val (start, end) = span
    if (check(VAdding | VTrace, verbosity))
      logln(s"\naddBinaries $start $end $outside $thisClass")

    val pruners = new ArrayBuffer[MetaPruningCube]()

    // Initialise
    // TODO: Make use of minSplit and maxSplit
    // TODO: Add version that prunes based on coarse state
    {
      var split = span._1 + 1
      while (split < span._2) {
        val leftCell = spanToCell(start, split)
        val leftBeam = beamsL(leftCell)
        prepareBinaryPruning(pruners, leftBeam, span, split)
        split += 1
      }
    }

    // Process
    val iter = pruners.iterator
    while (iter.hasNext) {
      val pruner = iter.next
      pruner.prepare
      var considered = 0
      while (pruner.hasNext && considered < cubeDepth) {
        considered += 1
        binaryLog(0) += 1
        val (leftPos, middlePos, rightPos, score, split1, split2) = pruner.next
        val ternary = split2 > 0
        if (ternary) {
          val leftCell = spanToCell(start, split1)
          val middleCell = spanToCell(split1, split2)
          val rightCell = spanToCell(split2, end)
          setCurCellAndPos(leftCell, leftPos)
          setCurCellAndPos1(middleCell, middlePos)
          setCurCellAndPos2(rightCell, rightPos)
        } else {
          val leftCell = spanToCell(start, split1)
          val rightCell = spanToCell(split1, end)
          setCurCellAndPos(leftCell, leftPos)
          setCurCellAndPos1(rightCell, rightPos)
        }

        val multiFinal = (hasFinal && hasFinal1) ||
          (ternary && hasFinal && hasFinal2) ||
          (ternary && hasFinal1 && hasFinal2)
        val mid1Structural = (!stage.requireTreeBasis) ||
          rHasStructuralParent || lHasStructuralParent1 ||
          (ternary && xHasStructuralParent2 && externalPos2 == split1)
        val mid2Structural = (!stage.requireTreeBasis) || (!ternary) ||
          (rHasStructuralParent1 || lHasStructuralParent2 ||
            (xHasStructuralParent && externalPos == split2))

        val structuralCount0 =
          (if (lHasStructuralParent) 1 else 0) +
          (if (externalPos1 == curSpan00 && xHasStructuralParent1) 1 else 0) +
          (if (ternary && externalPos2 == curSpan00 && xHasStructuralParent2) 1
           else 0)
        val structuralCount1 =
          (if (rHasStructuralParent) 1 else 0) +
          (if (lHasStructuralParent1) 1 else 0) +
          (if (ternary && externalPos2 == curSpan01 && xHasStructuralParent2) 1
           else 0)
        val structuralCount2 =
          (if (externalPos == curSpan11 && xHasStructuralParent) 1 else 0) +
          (if (rHasStructuralParent1) 1 else 0) +
          (if (ternary && lHasStructuralParent2) 1 else 0)
        val structuralCount3 =
          (if (ternary && externalPos == curSpan21 && xHasStructuralParent) 1
           else 0) +
          (if (ternary && externalPos1 == curSpan21 && xHasStructuralParent1) 1
           else 0) +
          (if (ternary && rHasStructuralParent2) 1 else 0)

        val ruleID =
          if (ternary) stateIntsToBinaryInt(
            curState00, (curSpan00, curSpan01, externalPos),
            curState10, (curSpan10, curSpan11, externalPos1),
            curState20, (curSpan20, curSpan21, externalPos2))
          else stateIntsToBinaryInt(
            curState00, (curSpan00, curSpan01, externalPos),
            curState10, (curSpan10, curSpan11, externalPos1))

        val info = rules.combineRules.getOrElse(ruleID, null)
///        Log.logln(s"qr$ruleID $sentenceID $sentenceLength")
///        stage.synchronized{
///          val ruleCounter = stage.ruleCounters(sentenceLength)
///          val curCount = ruleCounter.getOrElse(ruleID, 0L)
///          ruleCounter.put(ruleID, curCount + 1L)
///        }
  ///      logln("   "+ s"$ruleID $info with ($start, $end) $split1 $split2")
  ///      logln("   "+ curStateAsString + s" $insideScore at $curStatePos0")
  ///      logln("   "+ curStateAsString1 + s" $insideScore1 at $curStatePos1")
  ///      if (ternary)
  ///        logln("   "+ curStateAsString2 + s" $insideScore2 at $curStatePos2")

        // Check non-trace crossing constraint. Specifically, if more than one
        // item has a non tree edge to its X, then this combination is not
        // allowed. The only exception is for rules generating a Both instance.
        val multiCrossing = stage.requireTreeBasis &&
          ( (hasTreeEdgesToX && hasTreeEdgesToX1) ||
            (ternary && hasTreeEdgesToX1 && hasTreeEdgesToX2) ||
            (ternary && hasTreeEdgesToX2 && hasTreeEdgesToX))

        if (info == null) {
  ///        if (ternary) {
  ///          if (
  ///              (span._1 < externalPos && externalPos < split1) ||
  ///              (span._1 < externalPos1 && externalPos1 < split1) ||
  ///              (span._1 < externalPos2 && externalPos2 < split1) ||
  ///              (split1 < externalPos && externalPos < split2) ||
  ///              (split1 < externalPos1 && externalPos1 < split2) ||
  ///              (split1 < externalPos2 && externalPos2 < split2) ||
  ///              (split2 < externalPos && externalPos < span._2) ||
  ///              (split2 < externalPos1 && externalPos1 < span._2) ||
  ///              (split2 < externalPos2 && externalPos2 < span._2)
  ///            ) binaryLog(3) += 1
  ///        } else {
  ///          if (
  ///              (span._1 < externalPos && externalPos < split1) ||
  ///              (span._1 < externalPos1 && externalPos1 < split1) ||
  ///              (split1 < externalPos && externalPos < span._2) ||
  ///              (split1 < externalPos1 && externalPos1 < span._2)
  ///            ) binaryLog(3) += 1
  ///        }
          if (ternary)
            logln(s"Ternary Block: ${curStateAsString} ${curStateAsString1} ${curStateAsString2} at $span $split1 $split2 from ${pruner.tmpIDs}")
          else
            logln(s"Binary Block: ${curStateAsString} ${curStateAsString1} at $span $split1 from ${pruner.tmpIDs}")
          binaryLog(1) += 1
        } else if (info._6 && multiCrossing) binaryLog(2) += 1
        else if (multiFinal) binaryLog(3) += 1
        else if (! (mid1Structural && mid2Structural) ) binaryLog(4) += 1
        else if (
          structuralCount0 > 1 ||
          structuralCount1 > 1 ||
          structuralCount2 > 1 ||
          structuralCount3 > 1
        ) {
          // Do not create multiple structural parents
          binaryLog(6) += 1
        } else if (stage.parserType != ParserType.ARC && (
          rSpine != lSpine1 ||
          (ternary && rSpine1 != lSpine2) ||
          (externalPos == curSpan11 && xSpine != rSpine1) ||
          (ternary && externalPos == curSpan21 && xSpine != rSpine2) ||
          (externalPos1 == curSpan00 && xSpine1 != lSpine) ||
          (ternary && externalPos1 == curSpan21 && xSpine1 != rSpine2) ||
          (ternary && externalPos2 == curSpan00 && xSpine2 != lSpine) ||
          (ternary && externalPos2 == curSpan01 && xSpine2 != rSpine)
        )) {
          // Spines do not match
          binaryLog(5) += 1
        } else {
          val argLR = ArgState(lrFromState(info._1))
          val argLX = ArgState(lxFromState(info._1))
          val argRL = ArgState(rlFromState(info._1))
          val argRX = ArgState(rxFromState(info._1))
          val argXL = ArgState(xlFromState(info._1))
          val argXR = ArgState(xrFromState(info._1))
          val pencil = pencilworthyFromState(info._1)
          val crossing = crossingFromState(info._1)
          val outsideScore = makeBinaryFromStates(crossing, argLR, argLX,
            argRL, argRX, argXL, argXR, score.toFloat, ternary, pencil, outside)
          if (outside) {
            if (ternary) {
              // TODO: Work out how sums need to be changed to consider external
              // spine scores.
              val spineScore =
                ( if (externalPos == EXTERNAL_NONE) 0.0f
                  else spineScoresMap(externalPos).get(xSpine).toFloat) +
                ( if (externalPos1 == EXTERNAL_NONE) 0.0f
                  else spineScoresMap(externalPos1).get(xSpine1).toFloat) +
                ( if (externalPos2 == EXTERNAL_NONE) 0.0f
                  else spineScoresMap(externalPos2).get(xSpine2).toFloat)
              val score0 = outsideScore._1 + insideScore1 + insideScore2 -
                spineScore
              val sum0 = outsideScore._3 + insideSum1 + insideSum2
              updateOutside(score0, sum0)
              val score1 = outsideScore._1 + insideScore + insideScore2 -
                spineScore
              val sum1 = outsideScore._3 + insideSum + insideSum2
              updateOutside1(score1, sum1)
              val score2 = outsideScore._1 + insideScore + insideScore1 -
                spineScore
              val sum2 = outsideScore._3 + insideSum + insideSum1
              updateOutside2(score2, sum2)
            } else {
              val spineScoreL =
                if (externalPos == EXTERNAL_NONE) {
                  if (externalPos1 == curSpan00)
                    spineScoresMap(externalPos1).get(xSpine1).toFloat
                  else 0.0f
                } else if (externalPos == externalPos1)
                  spineScoresMap(externalPos).get(xSpine).toFloat
                else if (externalPos == curSpan11)
                  spineScoresMap(externalPos).get(xSpine).toFloat
                else if (externalPos1 == curSpan00)
                  spineScoresMap(externalPos1).get(xSpine1).toFloat
                else 0.0f
              val spineScoreR =
                if (externalPos1 == EXTERNAL_NONE) {
                  if (externalPos == curSpan11)
                    spineScoresMap(externalPos).get(xSpine).toFloat
                  else 0.0f
                } else if (externalPos == externalPos1)
                  spineScoresMap(externalPos1).get(xSpine1).toFloat
                else if (externalPos1 == curSpan00)
                  spineScoresMap(externalPos1).get(xSpine1).toFloat
                else if (externalPos == curSpan11)
                  spineScoresMap(externalPos).get(xSpine).toFloat
                else 0.0f
              val score0 = outsideScore._1 + insideScore1 - spineScoreL
              val sum0 = outsideScore._3 + insideSum1
              updateOutside(score0, sum0)
              val score1 = outsideScore._1 + insideScore - spineScoreR
              val sum1 = outsideScore._3 + insideSum
              updateOutside1(score1, sum1)
            }
          }
        }
      }
    }
  }

  def scoreArc(
    childIndex: Int, parentIndex: Int, childChain: Int, parentChain: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) : Double = {
    val loss =
      if (lossType == LossType.ZERO) 0.0
      else if (lossType == LossType.HAMMING) {
        if (gold) 0.0
        else if (goldHere) {
          if (isTreeEdge(arg)) Config.lossWeightDiffS
          else Config.lossWeightDiffT
        } else {
          if (isTreeEdge(arg)) Config.lossWeightExtraS
          else Config.lossWeightExtraT
        }
      } else throw new IllegalArgumentException("Loss type")

    val weightedScore =
      if (model.useCoarseArcScore && chartAbove != null &&
        model.isTreeEdge(arg) && arg < chartAbove.model.edges.length
      ) {
        val coarseScore = chartAbove.scoreArc(childIndex, parentIndex,
          childChain, parentChain, arg, length, false, LossType.ZERO, false)
        getArcCoarseScore(coarseScore, childIndex, parentIndex, arg,
          stage.parent.fold(ParserType.ARC)(_.parserType), countOnGet)
      } else if (model.useCoarseTraceScore && traceChart != null &&
        ! model.isTreeEdge(arg)
      ) {
        val childTag = nonTerminalIndex.value(tagIDs(childIndex))
        val parentTag = nonTerminalIndex.value(tagIDs(parentIndex))
        val trace = model.getEdgeTrace(edges(arg))
        val targ = traceChart.stage.model.getTraceEdgeID(trace, childTag,
          parentTag, true)
        val coarseScore = traceChart.scoreArc(childIndex, parentIndex,
          childChain, parentChain, targ, length, false, LossType.ZERO, false)
        getArcCoarseScore(coarseScore, childIndex, parentIndex, targ,
          ParserType.TSCORER, countOnGet)
      } else 0.0

    loss + weightedScore
  }

  def scoreSpine(
    wIndex: Int, spine: Int, gold: Boolean, lossType: LossType.Value,
    countOnGet: (Int, Double) => Unit = null
  ) : Double = {
    val score = getInitScore(tokenIDs, tokenFeatureIDs, tagIDs, sentenceID,
      wIndex, spine, gold, countOnGet)

    val loss =
      if (lossType == LossType.ZERO) 0.0
      else if (lossType == LossType.HAMMING) {
        if (gold) 0.0
        else Config.lossWeightSpine
      } else throw new IllegalArgumentException("Loss type")

    val coarse =
      if (model.useCoarseSpineScore && spineChart != null) {
        val coarseScore = spineChart.scoreSpine(wIndex, spine, false,
          LossType.ZERO)

        val weightedScore = getSpineCoarseScore(coarseScore, wIndex,
          sentenceLength, spine, ParserType.SSCORER, countOnGet)

        weightedScore
      } else 0.0

    score + loss + coarse
  }

  // TODO: There are subtle details in this loop that mean caching scores
  // is difficult to set up. Sure, we can store both the structural and
  // trace edges, but we prune inside the loop based on the item state,
  // including crossing type, external position, etc.
  //
  // For arc this doesn't prune much anyway, so when arc parsing, don't prune
  // inside here, and then we can cache the best option.
  def projectiveStateForArc(
    span: (Int, Int), arc: (String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value]),
    nxSpine: Int, ext: Int, parentIsRoot: Boolean
  ) = {
    val argLR =
      if (
        (arc._2 == "x" && ext < span._1) ||
        (arc._2 == "i" && (arc._1 != "x" || ext > span._2)) ||
        (arc._2 == "j" && arc._1 == "x" && ext > span._2)
      ) ArgState.Direct
      else ArgState.None
    val argRL =
      if (
        (arc._2 == "x" && ext > span._2) ||
        (arc._2 == "j" && (arc._1 != "x" || ext < span._1)) ||
        (arc._2 == "i" && arc._1 == "x" && ext < span._1)
      ) ArgState.Direct
      else ArgState.None
    val spanL =
      if (arc._1 != "x" && arc._2 != "x") span._1
      else if (ext < span._1) ext
      else if (arc._1 == "j" || arc._2 == "j") span._2
      else span._1
    val spanR =
      if (arc._1 != "x" && arc._2 != "x") span._2
      else if (ext > span._1) ext
      else if (arc._1 == "i" || arc._2 == "i") span._1
      else span._2
    val leftSpine =
      if (spanL == span._1) lSpine
      else if (spanL == span._2) rSpine
      else nxSpine
    val rightSpine =
      if (spanR == span._1) lSpine
      else if (spanR == span._2) rSpine
      else nxSpine

    val lStructural = stage.requireTreeBasis && argLR == ArgState.Direct
    val rStructural = stage.requireTreeBasis && argRL == ArgState.Direct
    val stateS = genStateMiscInt(CrossingState.Interval, EXTERNAL_NONE,
      true, parentIsRoot, false, lStructural, rStructural, false, argLR,
      ArgState.None, argRL, ArgState.None, ArgState.None, ArgState.None)
    (stateS, (spanL, spanR), leftSpine, rightSpine)
  }
  var coarseArcRatio = 0.0
  def findBestArc(
    span: (Int, Int), arc: (String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value]),
    nxSpine: Int, ext: Int, crossing: CrossingState.Value,
    secondStage: Boolean, outside: Boolean,
    arcScoresStructural: ArrayBuffer[Double],
    arcScoresOther: ArrayBuffer[Double], child: Int, parent: Int, cSpine: Int,
    pSpine: Int, parentIsRoot: Boolean, stateSisGold: Boolean,
    stateOisGold: Boolean, stateS: Int, stateO: Int
  ) = {
///    logln(s"      findBest $span $arc $nxSpine $ext $crossing $secondStage $outside $arcScoresStructural $arcScoresOther $child $parent $cSpine $pSpine $parentIsRoot $stateSisGold $stateOisGold $stateS $stateO ${curStateAsString}")

    var bestStructural = (-1, false, Double.MinValue)
    var bestTrace = (-1, false, Double.MinValue)
    var bestChain = (-1, false, Double.MinValue)

    labelGen.prepare(tagIDs(child), tagIDs(parent), cSpine, pSpine,
      span, sentenceLength, parentIsRoot, arcPassConstraint)

    val dist = (parent - child).abs
    val edgeToX = arc._1 == "x" || arc._2 == "x"

    val goldHereS = checkArcIsGold(parent, child, goldEdges, sentenceLength,
      0, true, false, false)
    val goldHereT = checkArcIsGold(parent, child, goldEdges, sentenceLength,
      0, false, false, false)
    val goldHereC = checkArcIsGold(parent, child, goldEdges, sentenceLength,
      0, false, true, false)

    while (labelGen.hasNext) {
      val arg = labelGen.next
      statesLog(7) += 1
      arcLog(4) += 1
      val structural = isTreeEdge(arg)
      val chain = !structural && isChainTraceEdge(arg)
      if (check(VAdding, verbosity))
        logln(s"      arg $arg $structural $chain ${labelGen.mustBeTrace}")

      // State possibilities:
      // Creating only an S - use stateS
      // Creating only an O - use stateO
      // Creating an S and an O - use stateS
      // We can't tell the 2nd and 3rd apart for the trace
      val stateIsGold =
        if (structural) stateSisGold
        else stateOisGold || stateSisGold
      val arcIsGold = checkArcIsGold(parent, child, goldEdges,
        sentenceLength, arg, structural, chain)

      val coarseUnpruned =
        if (structural && chartAbove != null &&
            chartAbove.stage.doCrossing != stage.doCrossing) {
          // We are querying a non-1ec coarse chart from within a 1ec chart
          val (stateS, tspan, leftSpine, rightSpine) = projectiveStateForArc(
            span, arc, nxSpine, ext, parentIsRoot)
          val stateSbelow = genStateMiscInt(CrossingState.Interval)
          checkCoarseState(stateS, leftSpine, rightSpine, 0, tspan, child,
            parent, arg, stateSbelow, stateIsGold && arcIsGold, false,
            false, 1, true, false)
        } else {
          checkCoarseState(stateS, lSpine, rSpine, nxSpine, span, child,
            parent, arg, curState00, stateSisGold && arcIsGold, false,
            secondStage, -1, structural, chain) ||
          ( !structural &&
            checkCoarseState(stateO, lSpine, rSpine, nxSpine, span,
              child, parent, arg, curState00, stateOisGold && arcIsGold,
              false, secondStage, -1, structural, chain))
        }

      val traceUnpruned =
        if (traceChart == null || structural) true
        else if (doNotPruneGoldArcs && stateIsGold && arcIsGold) true
        else {
          pruningLog(5) += 1
          val trace = model.getEdgeTrace(edges(arg))
          traceChart.checkCoarseTrace(child, parent, trace)
        }
      if (check(VAdding, verbosity))
        logln(s"      unpruned $coarseUnpruned $traceUnpruned $stateIsGold $arcIsGold ${stateString(stateS, lSpine, rSpine, nxSpine, 0, 0, 0, 0)}")

      if (coarseUnpruned && traceUnpruned) {
        // Note - score with the loss-augmented adjustment only being based
        // on the arc itself (not the state it's being built on top of).
        var score : Double = 0.0
        val goldHere =
          if (structural) goldHereS
          else if (chain) goldHereC
          else goldHereT
        score += scoreArc(child, parent, cSpine, pSpine, arg, dist,
          arcIsGold, lossType, goldHere)

        // Now require full gold (state and arc), so we (1) correctly record
        // the gold-ness, and (2) so we only avoid pruning things that are
        // fully gold.
        if (structural) {
          if (bestStructural._3 < score)
            bestStructural = (arg, stateIsGold && arcIsGold, score)
        } else if (chain) {
          if (bestChain._3 < score)
            bestChain = (arg, stateIsGold && arcIsGold, score)
        } else {
          if (bestTrace._3 < score)
            bestTrace = (arg, stateIsGold && arcIsGold, score)
        }

        if (check(VAdding, verbosity))
          logln(s"      $score for arg $arg p${arc._1},c${arc._2}  $child $parent cp")

        if (outside && check(VMarginals, verbosity)) {
          if (structural) {
            if (! (hasFinal && parentIsRoot) )
              arcScoresStructural.append(score)
          } else arcScoresOther.append(score)
        }
///        if (outside && serialisableChartData != null) {
///          val id = idForArg(child, parent, lSpine, rSpine, arg)
///          val longScore =
///            java.lang.Float.floatToRawIntBits(
///              score.toFloat).toLong
///          serialisableChartData.scores.put(id, longScore)
///        }
      } else {
        if (coarseUnpruned) pruningLog(4) += 1
        else pruningLog(0) += 1
        if (check(VAdding, verbosity))
          logln(s"      coarse blocked $arg p${arc._1},c${arc._2}")
      }
    }
    if (check(VAdding, verbosity))
      logln(s"      best p${arc._1},c${arc._2} $bestStructural $bestTrace $bestChain")

    (bestStructural, bestTrace, bestChain)
  }
  def addArcsForSpan(
    span: (Int, Int), arc: (String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value]),
    nxSpine: Int, ext: Int, crossing: CrossingState.Value, spineScore: Float,
    secondStage: Boolean, considerStructural: Boolean, outside: Boolean,
    usableS: Boolean, usableOwithS: Boolean, usableO: Boolean
  ) = {
    if (check(VAdding, verbosity))
      logln(s"      addArcs $span $arc $nxSpine $ext $crossing $spineScore $secondStage $considerStructural $outside $usableS $usableO")
    // Allowed options:
    //  - S, Structural
    //  - T, Trace
    //  - C, Chain trace
    //  - S T
    //  - T C
    //  - S T C
    // Track max for each of the three, then do insertion (note, in terms of
    // state, S = S T = S T C and T = C = T C, so we insert just twice,
    // choosing the max)
    arcLog(3) += 1
    val (parent, pSpine) =
      if (arc._1 == "i") (span._1, lSpine)
      else if (arc._1 == "j") (span._2, rSpine)
      else (ext, nxSpine)
    val (child, cSpine) =
      if (arc._2 == "i") (span._1, lSpine)
      else if (arc._2 == "j") (span._2, rSpine)
      else (ext, nxSpine)
    val parentIsRoot = parent == sentenceLength - 1
    val arcScoresStructural = new ArrayBuffer[Double]
    val arcScoresOther = new ArrayBuffer[Double]

    val nFinal = hasFinal || parentIsRoot
    val nPencil = arc._3 || isPencilworthy

    // Construct the possible next states
    val edgeToX = arc._1 == "x" || arc._2 == "x"
    val makesTreeEdgeToX = stage.requireTreeBasis &&
      (hasTreeEdgesToX || edgeToX)
    val lStructural = stage.requireTreeBasis &&
      (lHasStructuralParent || child == span._1)
    val rStructural = stage.requireTreeBasis &&
      (rHasStructuralParent || child == span._2)
    val xStructural = stage.requireTreeBasis &&
      (xHasStructuralParent || (child != span._1 && child != span._2))
    val stateS = genStateMiscInt(crossing, ext, nPencil, nFinal,
      makesTreeEdgeToX, lStructural, rStructural, xStructural, arc._6(0),
      arc._6(1), arc._6(2), arc._6(3), arc._6(4), arc._6(5))
    val stateSisGold = stateIsGold(curCell0, stateS, lSpine, rSpine, nxSpine,
      goldStates)

    val stateO = genStateMiscInt(crossing, ext, nPencil, nFinal,
      hasTreeEdgesToX, lHasStructuralParent, rHasStructuralParent,
      xHasStructuralParent, arc._6(0), arc._6(1), arc._6(2), arc._6(3),
      arc._6(4), arc._6(5))
    val stateOisGold = stateIsGold(curCell0, stateO, lSpine, rSpine, nxSpine,
      goldStates)

    val best = findBestArc(span, arc, nxSpine, ext, crossing,
      secondStage, outside, arcScoresStructural, arcScoresOther, child,
      parent, cSpine, pSpine, parentIsRoot, stateSisGold, stateOisGold,
      stateS, stateO)
    if (check(VAdding, verbosity)) logln(s"      got $best")

    // Check:
    //  - It is allowed according to pruning
    //  - We will not be creating an invalid link to root
    //  - There is an arg (best is -1 otherwise)
    //  - We will not be creating a crossing
    val makeS =
      considerStructural &&
      usableS &&
      (! (parentIsRoot && hasFinal)) &&
      (best._1._1 >= 0) &&
      (! (stage.requireTreeBasis && hasTreeEdgesToX && (! edgeToX)))
    if (check(VAdding, verbosity) && stage.requireTreeBasis &&
        hasTreeEdgesToX && (! edgeToX)) {
      // Not allowed becaus:
      //  - There is already an edge from X to the centre of this item that
      //    is not a trace edge.
      //  - This edge is not a trace edge
      //  - This edge goes between the ends of the interval
      // Therefore we would produce a crossing pair of tree edges.
        logln(s"      treeEdgeCross blocked sttuctural")
    }

    val makeTCwithS = usableOwithS && (! parentIsRoot) && (best._2._1 >= 0 || best._3._1 >= 0)
    val makeTC = usableO && (! parentIsRoot) && (best._2._1 >= 0 || best._3._1 >= 0)

    // Note - we effectively treat 'no trace' as having a score of 0.0. This
    // means a trace must have a positive score (and one above losWeight
    // potentially) to be included. One view of this is that we can always
    // remove a trace, so if that makes our score go up, clearly we would do
    // it. On the other hand, maybe there should be an explicit 'no trace'
    // option?
    val reverseGoldScore =
      if (lossType != LossType.HAMMING) 0.0f
      else {
        var total = 0.0
        if (checkArcIsGold(child, parent, goldEdges, sentenceLength, 0, true,
              false, false)) total += Config.lossWeightMissedS
        if (checkArcIsGold(child, parent, goldEdges, sentenceLength, 0, false,
              false, false)) total += Config.lossWeightMissedT
        if (checkArcIsGold(child, parent, goldEdges, sentenceLength, 0, false,
              true, false)) total += Config.lossWeightMissedT
        total.toFloat
      }
    val comparisonS =
      if (lossType == LossType.HAMMING &&
          checkArcIsGold(parent, child, goldEdges, sentenceLength, 0, true,
            false, false)) Config.lossWeightMissedS.toFloat
      else 0.0f
    val comparisonT =
      if (lossType == LossType.HAMMING &&
          checkArcIsGold(parent, child, goldEdges, sentenceLength, 0, false,
            false, false)) Config.lossWeightMissedT.toFloat
      else 0.0f
    val comparisonC =
      if (lossType == LossType.HAMMING &&
          checkArcIsGold(parent, child, goldEdges, sentenceLength, 0, false,
            true, false)) Config.lossWeightMissedT.toFloat
      else 0.0f
    if (stage.requireTreeBasis) {
      if (makeS) {
        val argS = best._1._1
        var gold = best._1._2
        var score = best._1._3.toFloat + reverseGoldScore + spineScore

        val argT =
          if (makeTCwithS && best._2._3 > comparisonT) {
            score += best._2._3.toFloat
            gold = gold && best._2._2
            best._2._1
          } else {
            score += comparisonT
            -1
          }
        val argC =
          if (makeTCwithS && best._3._3 > comparisonC) {
            score += best._3._3.toFloat
            gold = gold && best._3._2
            best._3._1
          } else {
            score += comparisonC
            -1
          }

        val outsideAbove =
          makeArcFromState(argS, argT, argC, child, parent, nxSpine, ext,
            crossing, arc._6(0), arc._6(1), arc._6(2), arc._6(3), arc._6(4),
            arc._6(5), score, 0.0f, secondStage,
            (arc._1 == "x" || arc._2 == "x"), arc._3, parentIsRoot, outside,
            gold && isGold)
///        if (check(VParseExtraction, verbosity))
///          logln(s"Adding arc for $span $curCell0 $curStatePos0: $argS $argT $argC $best => $outsideAbove ($child $parent)")
        if (outside) {
          updateOutside(outsideAbove._1 + score, outsideAbove._3)
          if (check(VMarginals, verbosity)) {
            val allButArc = outsideAbove._1 + insideScore
            for (oscore <- arcScoresStructural)
              arcScoresForLoggingStructural.append(allButArc + oscore.toFloat)

            val cur = arcScoresForLoggingMax.getOrElseUpdate(curCell0, score)
            if (score > cur)
              arcScoresForLoggingMax.put(curCell0, score)
          }
        }
      }

      if (makeTC) {
        var argT = -1
        var argC = -1
        var score = comparisonS + reverseGoldScore + spineScore
        var gold = true
        if (best._2._3 > comparisonT && best._3._3 > comparisonC) {
          // Use both
          argT = best._2._1
          argC = best._3._1
          score += best._2._3.toFloat + best._3._3.toFloat
          gold = best._2._2 && best._3._2
        } else if ((best._2._3 + comparisonC) > (best._3._3 + comparisonT)) {
          // T only
          argT = best._2._1
          score += best._2._3.toFloat + comparisonC
          gold = best._2._2
          // TODO: This does not consider whether there should have been a T
          // here. At the moment this is okay as we don't use gold for
          // anything but debug info, but it could be important for fancy
          // pruning techniques.
        } else {
          // C only
          argC = best._3._1
          score += best._3._3.toFloat + comparisonT
          gold = best._3._2
          // TODO: Likewise here
        }

        val outsideAbove =
          makeArcFromState(-1, argT, argC, child, parent, nxSpine, ext,
            crossing, arc._6(0), arc._6(1), arc._6(2), arc._6(3), arc._6(4),
            arc._6(5), score, 0.0f, secondStage,
            (arc._1 == "x" || arc._2 == "x"), arc._3, false, outside,
            gold && isGold)

///        if (check(VParseExtraction, verbosity))
///          logln(s"Adding arc for $span $curCell0 $curStatePos0: -1 $argT $argC $best => $outsideAbove")
        if (outside) {
          updateOutside(outsideAbove._1 + score, outsideAbove._3)
          if (check(VMarginals, verbosity)) {
            val allButArc = outsideAbove._1 + insideScore
            for (oscore <- arcScoresOther)
              arcScoresForLoggingOther.append(allButArc + oscore.toFloat)
          }
        }
      }
    } else if (makeS || makeTC) {
      var argS = -1
      var argT = -1
      var argC = -1
      var gold = true
      var score = reverseGoldScore + spineScore

      val scoreSTC = best._1._3 + best._2._3 + best._3._3
      val scoreST = best._1._3 + best._2._3 + comparisonC
      val scoreSC = best._1._3 + comparisonT + best._3._3
      val scoreTC = comparisonS + best._2._3 + best._3._3
      val scoreS = best._1._3 + comparisonT + comparisonC
      val scoreT = comparisonS + best._2._3 + comparisonC
      val scoreC = comparisonS + comparisonT + best._3._3
      val highest =
        if (! makeTC) scoreS
        else if (! makeS) Array(scoreTC, scoreT, scoreC).max
        else Array(scoreSTC, scoreST, scoreSC, scoreS, scoreTC, scoreT,
          scoreC).max

      if (scoreSTC == highest && makeS && makeTC) {
        argS = best._1._1
        argT = best._2._1
        argC = best._3._1
        gold = best._1._2 && best._2._2 && best._3._2
        score += scoreSTC.toFloat
      } else if (scoreST == highest && makeS && makeTC) {
        argS = best._1._1
        argT = best._2._1
        gold = best._1._2 && best._2._2
        score += scoreST.toFloat
      } else if (scoreSC == highest && makeS && makeTC) {
        argS = best._1._1
        argC = best._3._1
        gold = best._1._2 && best._3._2
        score += scoreSC.toFloat
      } else if (scoreTC == highest && makeTC) {
        argT = best._2._1
        argC = best._3._1
        gold = best._2._2 && best._3._2
        score += scoreTC.toFloat
      } else if (scoreS == highest && makeS) {
        argS = best._1._1
        gold = best._1._2
        score += scoreS.toFloat
      } else if (scoreT == highest && makeTC) {
        argT = best._2._1
        gold = best._2._2
        score += scoreT.toFloat
      } else if (scoreC == highest && makeTC) {
        argC = best._3._1
        gold = best._3._2
        score += scoreC.toFloat
      }

      val outsideAbove = makeArcFromState(argS, argT, argC, child, parent,
        nxSpine, ext, crossing, arc._6(0), arc._6(1), arc._6(2), arc._6(3),
        arc._6(4), arc._6(5), score, 0.0f, secondStage,
        (arc._1 == "x" || arc._2 == "x"), arc._3, parentIsRoot, outside,
        gold && isGold)

      if (outside) {
        updateOutside(outsideAbove._1 + score, outsideAbove._3)
        if (check(VMarginals, verbosity)) {
          val allButArc = outsideAbove._1 + insideScore
          for (oscore <- arcScoresStructural)
            arcScoresForLoggingStructural.append(allButArc + oscore.toFloat)
          for (oscore <- arcScoresOther)
            arcScoresForLoggingOther.append(allButArc + oscore.toFloat)

          val cur = arcScoresForLoggingMax.getOrElseUpdate(curCell0, score)
          if (score > cur)
            arcScoresForLoggingMax.put(curCell0, score)
        }
      }
    }
  }
  def arcCheckCoarseParents(
    span: (Int, Int),
    arc: (String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value]),
    nxSpine: Int, ext: Int, crossing: CrossingState.Value,
    parentIsRoot: Boolean, secondStage: Boolean
  ) = {
    // Construct the possible next states
    val nFinal = hasFinal || parentIsRoot
    val nPencil = arc._3 || isPencilworthy

    val makesTreeEdgeToX = stage.requireTreeBasis &&
      (hasTreeEdgesToX || arc._1 == "x" || arc._2 == "x")
    val lStructural = stage.requireTreeBasis &&
      (lHasStructuralParent || arc._2 == "i")
    val rStructural = stage.requireTreeBasis &&
      (rHasStructuralParent || arc._2 == "j")
    val xStructural = stage.requireTreeBasis &&
      (xHasStructuralParent || arc._2 == "x")
    val stateS = genStateMiscInt(crossing, ext, nPencil, nFinal,
      makesTreeEdgeToX, lStructural, rStructural, xStructural, arc._6(0),
      arc._6(1), arc._6(2), arc._6(3), arc._6(4), arc._6(5))
    val stateSisGold = stateIsGold(curCell0, stateS, lSpine, rSpine, nxSpine,
      goldStates)
    if (check(VAdding, verbosity))
      logln(s"      Check structural: ${stateString(stateS, lSpine, rSpine, nxSpine, 0, 0, 0, 0)} with $stateSisGold")

    val stateO = genStateMiscInt(crossing, ext, nPencil, nFinal,
      hasTreeEdgesToX, lHasStructuralParent, rHasStructuralParent,
      xHasStructuralParent, arc._6(0), arc._6(1), arc._6(2), arc._6(3),
      arc._6(4), arc._6(5))
    val stateOisGold = stateIsGold(curCell0, stateO, lSpine, rSpine, nxSpine,
      goldStates)
    if (check(VAdding, verbosity))
      logln(s"      Check other: ${stateString(stateO, lSpine, rSpine, nxSpine, 0, 0, 0, 0)} with $stateOisGold")

    // Check the trace scorer
    val (usableSs, usableOs) =
      if (traceChart == null) (true, true)
      else {
        val child =
          if (arc._2 == "i") span._1
          else if (arc._2 == "j") span._2
          else ext
        val parent =
          if (arc._1 == "i") span._1
          else if (arc._1 == "j") span._2
          else ext
        val usable = traceChart.checkTraceable(child, parent)
        if (arc._1 == "x" || arc._2 == "x") {
          val edgeSpan = (child.min(parent), child.max(parent))
          val crossable = traceChart.checkCrossable(span, edgeSpan)
          (crossable, usable)
        } else (true, usable)
      }

    val (usableSp, usableOp) =
      if (chartAbove.stage.doCrossing != stage.doCrossing) {
        // We are querying a non 1ec coarse chart, from within a 1ec chart
        val (stateS, tspan, leftSpine, rightSpine) = projectiveStateForArc(
          span, arc, nxSpine, ext, parentIsRoot)
        val usable = checkCoarseState(stateS, leftSpine, rightSpine, 0,
          tspan, -1, -1, -1, -1, stateSisGold, false, false, 1)

        (usable, true)
      } else {
        val arcCreation = if (secondStage) 2 else 1
        val usableS = checkCoarseState(stateS, lSpine, rSpine, nxSpine, span,
          -1, -1, -1, -1, stateSisGold, false, false, arcCreation)
        val usableO = checkCoarseState(stateO, lSpine, rSpine, nxSpine, span,
          -1, -1, -1, -1, stateOisGold, false, false, arcCreation)

        (usableS, usableO)
      }

    val usableS =
      (usableSp && usableSs) ||
      (doNotPruneGoldArcs && stateSisGold)
    val usableOwithS =
      (usableSp && usableSs && usableOs) ||
      (doNotPruneGoldArcs && stateSisGold)
    val usableO =
      (usableOp && usableOs) ||
      (doNotPruneGoldArcs && stateOisGold)

    (usableS, usableOwithS, usableO)
  }

  /*
   * Takes existing states and adds arcs, creating Xtervals if needed. Note,
   * the max over arcs occurs separately for each item. Currently this simply
   * hits the cache (probably not too expensive). In future, this structure
   * could mean we add features that look at both the arc and the spines.
   */
  def addArcs(
    span: (Int, Int), secondStage: Boolean = false, outside: Boolean = false
  ) = {
    if (check(VAdding | VTrace, verbosity))
      logln(s"\naddArcs $span $secondStage $outside $thisClass")
    setCurCell(span)
    while (next) {
      arcLog(0) += 1
      val arcState = stateIntToArcInt(curState00, span._1)
      val arcOptions =
        if (secondStage) rules.arcRulesSecondAllowed.getOrElse(arcState, null)
        else rules.arcRulesFirstAllowed.getOrElse(arcState, null)
      if (check(VAdding, verbosity)) logln(s"  Consider: ${curStateAsString}")
      // AMR edit needed: Never link nulls to the left (works as the final
      // token is the root)
      if (arcOptions != null) {
        var option = 0
        while (option < arcOptions.length) {
          arcLog(1) += 1
          val arc = arcOptions(option)
          if (check(VAdding, verbosity)) logln(s"    Option: $arc")
          option += 1
          val child =
            if (arc._2 == "i") span._1
            else if (arc._2 == "j") span._2
            else externalPos
          val doStructural =
            if (arc._2 == "i" && lHasStructuralParent) false
            else if (arc._2 == "j" && rHasStructuralParent) false
            else if (arc._2 == "x" && xHasStructuralParent) false
            else true
          if (
            ((! arc._4) || doGraphs) &&
            ((! arc._5) || doCrossing) &&
            child != sentenceLength - 1
          ) {
            // If we are converting an Interval into an Xterval
            if (externalPos == EXTERNAL_NONE &&
               (arc._1 == "x" || arc._2 == "x")) {
              var ext = 0
              while (ext < sentenceLength) {
                if (
                  // The 2nd and 3rd constraints relate to the fact that an X
                  // cannot have an external point adjacent to the edge its arc
                  // starts from.
                  (ext < span._1 || ext > span._2) &&
                  (ext != span._1 - 1 || arc._1 == "j" || arc._2 == "j") &&
                  (ext != span._2 + 1 || arc._1 == "i" || arc._2 == "i") &&
                  (child != EXTERNAL_NONE || ext < sentenceLength - 1)
                ) {
                  val parentIsRoot =
                    (arc._1 == "j" && span._2 == sentenceLength - 1) ||
                    (ext == sentenceLength - 1)

                  // check coarse here if parent chart has null spines
                  val nxGold =
                    if (ext < goldSpines.length) goldSpines(ext)
                    else NULL_SPINE
                  val (usableS, usableOwithS, usableO) =
                    if (chartAbove == null ||
                        chartAbove.stage.parserType != ParserType.ARC)
                      (true, true, true)
                    else
                      arcCheckCoarseParents(span, arc, nxGold, ext,
                        CrossingState.Xterval, parentIsRoot, secondStage)
                  if (check(VAdding, verbosity))
                    logln(s"      Considering $span $ext $secondStage $outside $arc had $usableS $usableO")

                  if (usableS || usableO) {
                    var spinePos = 0
                    while (spinePos < spineScores(ext).length) {
                      arcLog(2) += 1
                      val spineOption = spineScores(ext)(spinePos)

                      // check coarse here if parent chart has spines
                      val (usableS2, usableOwithS2, usableO2) =
                        if (chartAbove == null ||
                            chartAbove.stage.parserType == ParserType.ARC)
                          (true, true, true)
                        else
                          arcCheckCoarseParents(span, arc, spineOption._1, ext,
                            CrossingState.Xterval, parentIsRoot, secondStage)

                      if (check(VAdding, verbosity)) logln(s"      And $usableS2 $usableO2")
                      if (usableS2 || usableO2) {
                        addArcsForSpan(span, arc, spineOption._1, ext,
                          CrossingState.Xterval, spineOption._2, secondStage,
                          doStructural, outside, usableS2 && usableS,
                          usableOwithS2 && usableOwithS, usableO2 && usableO)
                      }
                      spinePos += 1
                    }
                  } else pruningLog(2) += 1
                }
                ext += 1
              }
            } else {
              val parentIsRoot =
                (arc._1 == "j" && span._2 == sentenceLength - 1) ||
                (arc._1 == "x" && externalPos == sentenceLength - 1)
              val (usableS, usableOwithS, usableO) =
                if (chartAbove == null)
                  (true, true, true)
                else
                  arcCheckCoarseParents(span, arc, xSpine, externalPos,
                    crossingState, parentIsRoot, secondStage)

              if (check(VAdding, verbosity))
                logln(s"      Considering $span $secondStage $outside $arc had $usableS $usableO")
              if (usableS || usableO) {
                addArcsForSpan(span, arc, xSpine, externalPos, crossingState,
                  0.0f, secondStage, doStructural, outside, usableS,
                  usableOwithS, usableO)
              } else pruningLog(2) += 1
            }
          }
        }
      }
    }
  }

  def getSpineOptions(position: Int) = {
    val ctoken = tokenIDs(position)
    val ctag = if (tagIDs.length <= 1) -1 else tagIDs(position)

    if (position == sentenceLength - 1) getSpineOptionsByToken(RootWord)
    else if (ctag >= 0) getSpineOptionsByTag(ctag)
    else getSpineOptionsByToken(ctoken)
  }

  val spineScoresMap = Array.fill[LongDoubleMap](sentenceLength)(
    new LongDoubleMap(Config.loadFactor, 128))
  val spineScores =
    Array.fill[ArrayBuffer[(Int, Float, Boolean)]](sentenceLength)(
      new ArrayBuffer[(Int, Float, Boolean)])
  def addPreterminals = {
    for (position <- 0 until sentenceLength) {
      if (check(VAdding | VTrace, verbosity))
        logln(s"\naddPreterminals ${position-1} $position")

      // Score these, and record the scores
      for (spine <- getSpineOptions(position)) {
        val gold = spineIsGold(position, spine, goldSpines)
///        if (check(VAdding, verbosity))
///          logln(s"   Considering $spine which is $gold")
        if (checkCoarseSpine(position, spine, gold)) {
          val score = scoreSpine(position, spine, gold, lossType)
          spineScoresMap(position).put(spine, score)
          spineScores(position).append((spine, score.toFloat, gold))
        } else {
          pruningLog(3) += 1
          if (check(VAdding, verbosity))
            logln(s"   Coarse blocked $spine, gold $gold")
        }
      }

      // Create items
      if (position > 0) {
        val span = (position - 1, position)
        for ((lspine, lscore, leftIsGold) <- spineScores(position - 1)) {
          for ((rspine, rscore, rightIsGold) <- spineScores(position)) {
            val gold = leftIsGold && rightIsGold
            val score =
              if (position == 1 && position == sentenceLength - 1)
                lscore.toFloat + rscore.toFloat
              else if (position == 1)
                lscore.toFloat + (rscore.toFloat / 2.0f)
              else if (position == sentenceLength - 1)
                (lscore.toFloat / 2.0f) + rscore.toFloat
              else
                (lscore.toFloat / 2.0f) + (rscore.toFloat / 2.0f)
///            if (check(VParseExtraction, verbosity))
///              logln(s"Adding spines $lspine $rspine at $position with score $score based on $lscore $rscore")

            val state = genStateMiscInt(CrossingState.Interval)
            val usable = checkCoarseState(state, lspine, rspine, 0, span, -1,
              -1, -1, -1, gold, true)

            if (usable)
              makeInitFromState(span, lspine, rspine, score, false, gold)
            else
              pruningLog(1) += 1
          }
        }
      }
    }
  }

  def doInsidePass = {
    addPreterminals
    var length = 1
    while (length < sentenceLength) {
      var start = 0
      while (start + length < sentenceLength) {
        val span = (start, start + length)

        // All binaries and ternaries
        if (length > 1) addBinaries(span)

        // INLRE arcs
        addArcs(span, false, false)

        // NLR arcs 2 and X unaries / arcs
        if (doCrossing) addArcs(span, true, false)

        start += 1
      }
      length += 1
    }
    insideDone = true
  }

  def doOutsidePass = {
    var complete = false
    setCurCell(0, sentenceLength - 1)
    while (next) {
      if (hasFinal && lrArgState != ArgState.None &&
          (!stage.requireTreeBasis || lHasStructuralParent)
      ) {
        complete = true

        // Set the max marginal. Note, this will definitely find it since the
        // inside pass produces the parse with the max score.  The same cannot
        // be said for the min marginal.
        if (curMaxMarginal < insideScore) curMaxMarginal = insideScore

        // Prepare the top of the parse for the outside pass
        updateOutside(0.0f, 1.0f)
      } else updateOutside(Float.MinValue, 0.0f)
    }

    // Internal states
    if (complete) {
      var length = sentenceLength - 1
      while (length > 0) {
        var start = 0
        while (start + length < sentenceLength) {
          val span = (start, start + length)
          if (doCrossing) addArcs(span, true, true)
          addArcs(span, false, true)
          if (length > 1) addBinaries(span, true)
          start += 1
        }
        length -= 1
      }
    }

    curMaxMarginal
  }

  def serialiseScores = {
    require(false, "Not updated for 1ec")
    var length = 1
    while (length < sentenceLength) {
      var start = 0
      while (start + length < sentenceLength) {
///        val loc = spanToCell(start, start + length)
///        val binaries = serialisableChartData.binaryStates(loc)
        setCurCell(start, start + length)
        while (next) {
          val hash = stateHash(curState00, curState01, curState02, curState03)
          val subBeam = 0
          val pos = idForLocBeamHash(curCell0, subBeam, hash)
          val id = idForLocPos(curCell0, pos)
          val combined =
            (java.lang.Float.floatToRawIntBits(insideScore).toLong << 32) +
            java.lang.Float.floatToRawIntBits(outsideScore).toLong
          serialisableChartData.scores.put(id, combined)

///          if (subBeam != SubBeamArc) {
///            binaries.append(curState00)
///            binaries.append(curState01)
///            binaries.append(curState02)
///            binaries.append(curState03)
///            binaries.append(curPtr04)
///            binaries.append(curPtr05)
///          }
        }
        start += 1
      }
      length += 1
    }
  }

  def addSpineTopOptions : Unit
}

abstract class Parser(
  val stage: Stage,
  val coarseParser: Option[Parser],
  val traceParser: Option[Parser],
  val spineParser: Option[Parser],
  val pruningRatio: Double,
  val pruningRank: Double,
  val verbosity: Int,
  val doNotPruneGoldArcs: Boolean,
  val doNotPruneGoldSpines: Boolean,
  val training: Boolean
) {
  val thisClass = "AbstractParser."+ stage.name

  var coarseChart : Chart = null
  var fineChart : Chart = null
  var traceChart : Chart = null
  var spineChart : Chart = null
  var prepared = false

  def insertDerivation(
    terminals: ArrayBuffer[(Int, Int)],
    arcs: ArrayBuffer[(Int, Int, Int)],
    insertingGold: Boolean = false
  ) = fineChart.insertDerivation(terminals, arcs, insertingGold)

  def extractParseItems = fineChart.extractParseItems

  def denseChartString = {
    val ans = ArrayBuffer[String]()
    for (start <- 0 until fineChart.tokens.length) {
      ans.append(f"$start%2d ")
      ans.append("---" * start + "   ")
      for (end <- start + 1 until fineChart.tokens.length) {
        val span = (start, end)
        fineChart.setCurCell(span)
        if (fineChart.next) ans.append(" . ")
        else ans.append("   ")
      }
      ans.append("\n")
    }
    ans.mkString("")
  }

  def chartSizesString = {
    val ans = ArrayBuffer[String]()
    for (start <- 0 to fineChart.tokens.length) {
      for (end <- start + 1 to fineChart.tokens.length) {
        val loc = fineChart.spanToCell(start, end)
        ans.append(s"$start $end : ")
        for ((key, num) <- fineChart.beams(loc).sizes)
          ans.append(s" $key->$num")
        ans.append("\n")
      }
    }
    ans.mkString("")
  }

  /** Parsing for both training and evaluation.
    * The prepare / parse split is so that a derivation can be inserted
    * without doing parsing (run prepare, then insert).
    */
  def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldUnaries: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) : Chart
  def prepare(
    rtokens: Vector[String], rtags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldUnaries: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) : Unit = {
///    if (goldStates != null) {
///      for (state <- goldStates) Log.logln(s"Gold state $state $thisClass")
///    }
    coarseParser.foreach{ parser =>
      parser.prepare(rtokens, rtags, sentenceID, goldEdges, goldUnaries,
        goldStates, lossType)
      coarseChart = parser.fineChart
    }
    traceParser.foreach{ parser =>
      parser.prepare(rtokens, rtags, sentenceID, goldEdges, goldUnaries,
        goldStates, lossType)
      traceChart = parser.fineChart
    }
    spineParser.foreach{ parser =>
      parser.prepare(rtokens, rtags, sentenceID, goldEdges, goldUnaries,
        goldStates, lossType)
      spineChart = parser.fineChart
    }
    val tokens = rtokens ++ Vector(Model.SRootWord)
    val tags = rtags ++ Vector(Model.SRootTag)
    fineChart = makeFineChart(tokens, tags, sentenceID, goldEdges,
      goldUnaries, goldStates, lossType)
    prepared = true
  }
  def parse(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    doOutside: Boolean = true, goldEdges: UnboxedArrayBuffer = null,
    goldUnaries: ArrayBuffer[Int] = null,
    goldStates: Set[(Int, Int, Int, Int, Int)] = null,
    lossType: LossType.Value = LossType.ZERO
  ) : (ArrayBuffer[(Int, Int, Int)], ArrayBuffer[(Int, Int)], Double) = {
    logln(s"Parsing ($thisClass) "+ (tokens mkString " "))
    if (tags.length > 0) logln("With POS tags "+ (tags mkString " "))
    else logln("Without pre-defined POS tags")
//  - In base parser, have the arc creation step query that model to see if trace edges are allowed. Note, it will need to map the edges to the relevant type (ignoring all but the trace type, and simplifying that to remove chains)

    if (! prepared)
      prepare(tokens, tags, sentenceID, goldEdges, goldUnaries, goldStates,
        lossType)

    // Coarse
    coarseParser.foreach{ cp =>
      cp.parse(tokens, tags, sentenceID, true, goldEdges, goldUnaries,
        goldStates)
      if (stage.parserType != ParserType.ARC &&
          cp.stage.parserType == ParserType.ARC)
        cp.fineChart.addSpineTopOptions
    }

    traceParser.foreach{ tp =>
      tp.parse(tokens, tags, sentenceID, true, goldEdges, goldUnaries,
        goldStates)
    }

    spineParser.foreach{ sp =>
      sp.parse(tokens, tags, sentenceID, true, goldEdges, goldUnaries,
        goldStates)
    }

    // Fine
    val preParse = System.nanoTime()
    fineChart.doInsidePass
    if (doOutside) {
      fineChart.doOutsidePass
      fineChart.setMinMarginal
      fineChart.scoreRange =
        fineChart.curMaxMarginal - fineChart.curMinMarginal
    }
    val parse = fineChart.extractParse
    val postParse = System.nanoTime()

    postParseLog(postParse - preParse, parse._3, tokens, lossType)
    prepared = false

    parse
  }

  def postParseLog(
    time: Long, score: Double, tokens: Vector[String],
    lossType: LossType.Value
  ) = {
    // Calculate average beam size and number of beams
///    var subbeamCount = 0
///    var itemCount = 0
///    var beamCount = 0
///    for (beam <- fineChart.beams) {
///      beamCount += 1
///      subbeamCount += beam.numberOfIDs
///      itemCount += beam.size
///    }
///    logln(s"Beam stats: $beamCount $subbeamCount $itemCount")

///    var cur = 0.0
///    var sorted = 0.0
///    for (beam <- fineChart.beams) {
///      val size = beam.size
///      val buckets = beam.nBuckets
///      cur += size * buckets
///      sorted += size * math.log(size)
///    }

    // Memory stats

    val mem_info =
      if (Log.check(Log.VMemoryStats, verbosity)) {
        val pairs = new ArrayBuffer[(String, Long)]
///        val meter = new MemoryMeter()

///        def measureCheckNull(o: Object) = {
///          if (o == null) 0L
///          else meter.measureDeep(o)
///        }

///        fineChart.stage.model match {
///          case d: DiscriminativeModel =>
///            pairs.append(("local-model-surfaceFeatToID", measureCheckNull(d.weights.surfaceFeatToID)))
///            pairs.append(("local-model-structureMapping", measureCheckNull(d.weights.structureMapping)))
///            pairs.append(("local-model-values", measureCheckNull(d.weights.values)))
///          case e => ""
///        }
///        if (coarseChart != null) {
///          coarseChart.stage.model match {
///            case d: DiscriminativeModel =>
///              pairs.append(("arc1ec-model-surfaceFeatToID", measureCheckNull(d.weights.surfaceFeatToID)))
///              pairs.append(("arc1ec-model-structureMapping", measureCheckNull(d.weights.structureMapping)))
///              pairs.append(("arc1ec-model-values", measureCheckNull(d.weights.values)))
///            case e => ""
///          }
///        }
///        if (coarseChart != null || coarseChart.chartAbove != null) {
///          coarseChart.chartAbove.stage.model match {
///            case d: DiscriminativeModel =>
///              pairs.append(("arcproj-model-surfaceFeatToID", measureCheckNull(d.weights.surfaceFeatToID)))
///              pairs.append(("arcproj-model-structureMapping", measureCheckNull(d.weights.structureMapping)))
///              pairs.append(("arcproj-model-values", measureCheckNull(d.weights.values)))
///            case e =>
///          }
///        }
///        if (spineChart != null) {
///          spineChart.stage.model match {
///            case d: DiscriminativeModel =>
///              pairs.append(("spine-model-surfaceFeatToID", measureCheckNull(d.weights.surfaceFeatToID)))
///              pairs.append(("spine-model-structureMapping", measureCheckNull(d.weights.structureMapping)))
///              pairs.append(("spine-model-values", measureCheckNull(d.weights.values)))
///            case e =>
///          }
///        }
///        if (traceChart != null) {
///          traceChart.stage.model match {
///            case d: DiscriminativeModel =>
///              pairs.append(("traceLocal-model-surfaceFeatToID", measureCheckNull(d.weights.surfaceFeatToID)))
///              pairs.append(("traceLocal-model-structureMapping", measureCheckNull(d.weights.structureMapping)))
///              pairs.append(("traceLocal-model-values", measureCheckNull(d.weights.values)))
///            case e =>
///          }
///        }
///        if (coarseChart != null || coarseChart.traceChart != null) {
///          coarseChart.traceChart.stage.model match {
///            case d: DiscriminativeModel =>
///              pairs.append(("traceArc-model-surfaceFeatToID", measureCheckNull(d.weights.surfaceFeatToID)))
///              pairs.append(("traceArc-model-structureMapping", measureCheckNull(d.weights.structureMapping)))
///              pairs.append(("traceArc-model-values", measureCheckNull(d.weights.values)))
///            case e =>
///          }
///        }

///        pairs.append(("local", measureCheckNull(fineChart)))
///        pairs.append(("local-chartScoreCache", measureCheckNull(fineChart.chartScoreCache)))
///        pairs.append(("local-scoreCache", measureCheckNull(fineChart.scoreCache)))
///        pairs.append(("local-denseStates", measureCheckNull(fineChart.denseStates)))
///        pairs.append(("local-beams", measureCheckNull(fineChart.beamsL) + measureCheckNull(fineChart.beamsR)))
///        var beamInfomaps : Long = 0L
///        var beamInfomaxes : Long = 0L
///        var beamInfomaxPos : Long = 0L
///        var beamInfoids : Long = 0L
///        var beamInfoidSet : Long = 0L
///        var beamInfofirsts : Long = 0L
///        for (beam <- fineChart.beamsL) {
///          beam match {
///            case a: ArrayBeam =>
///              beamInfomaps += measureCheckNull(a.maps)
///              beamInfomaxes += measureCheckNull(a.maxes)
///              beamInfomaxPos += measureCheckNull(a.maxPos)
///              beamInfoids += measureCheckNull(a.ids)
///              beamInfoidSet += measureCheckNull(a.idSet)
///              beamInfofirsts += measureCheckNull(a.firsts)
///            case _ =>
///          }
///        }
///        for (beam <- fineChart.beamsR) {
///          beam match {
///            case a: ArrayBeam =>
///              beamInfomaps += measureCheckNull(a.maps)
///              beamInfomaxes += measureCheckNull(a.maxes)
///              beamInfomaxPos += measureCheckNull(a.maxPos)
///              beamInfoids += measureCheckNull(a.ids)
///              beamInfoidSet += measureCheckNull(a.idSet)
///              beamInfofirsts += measureCheckNull(a.firsts)
///            case _ =>
///          }
///        }
///        pairs.append(("local-beams-maps", beamInfomaps))
///        pairs.append(("local-beams-maxes", beamInfomaxes))
///        pairs.append(("local-beams-maxPos", beamInfomaxPos))
///        pairs.append(("local-beams-ids", beamInfoids))
///        pairs.append(("local-beams-idSet", beamInfoidSet))
///        pairs.append(("local-beams-firsts", beamInfofirsts))
///
///        if (traceChart != null) {
///          traceChart match {
///            case c: TraceChart =>
///              pairs.append(("traceLocal-scoreCache", measureCheckNull(c.scoreCache)))
///              pairs.append(("traceLocal-beams", measureCheckNull(c.beams)))
///            case _ =>
///          }
///        }
///        if (coarseChart != null || coarseChart.traceChart != null) {
///          coarseChart.traceChart match {
///            case c: TraceChart =>
///              pairs.append(("traceArc-scoreCache", measureCheckNull(c.scoreCache)))
///              pairs.append(("traceArc-beams", measureCheckNull(c.beams)))
///            case _ =>
///          }
///        }
///        if (spineChart != null) {
///          pairs.append(("spine-scoreCache", measureCheckNull(spineChart.scoreCache)))
///          pairs.append(("spine-beams", measureCheckNull(spineChart.beams)))
///        }
///        if (coarseChart != null) {
///          coarseChart match {
///            case a: ArcChart =>
///              pairs.append(("arc1ec-scoreCache", measureCheckNull(a.scoreCache)))
///              pairs.append(("arc1ec-beams", measureCheckNull(a.beams)))
///            case _ =>
///          }
///        }
///        if (coarseChart != null && coarseChart.chartAbove != null) {
///          coarseChart.chartAbove match {
///            case a: ArcChart =>
///              pairs.append(("arcproj-scoreCache", measureCheckNull(a.scoreCache)))
///              pairs.append(("arcproj-beams", measureCheckNull(a.beams)))
///            case _ =>
///          }
///        }

        pairs.map{ case (name, mem) => s"memory-usage  $mem   $name" }.mkString("\n")
      } else ""

    val info =
      s"Post $thisClass parse for sentence: "+ tokens.mkString(" ") +"\n"+
      s"$thisClass post parse : $score ${tokens.length} $time $lossType ${fineChart.stats}\n"+
      mem_info+
      (
        if (fineChart.chartScoreCache == null) s"$thisClass cache used 0 of 1"
        else s"$thisClass cache used ${fineChart.chartScoreCache.size} of ${fineChart.chartScoreCache.data.length}\n"
      ) +
      (if (check(VParseExtraction + VParseChart, verbosity)) fineChart.toString() +"\n" else "") +
      (if (check(VParseExtraction, verbosity)) denseChartString.toString() +"\n" else "") +
      (if (check(VCellSizes, verbosity)) chartSizesString.toString() +"\n" else "")
///      s"$thisClass buckets $cur $sorted\n"+

    logln(info)
  }

  def updatePruningStats(stats: PruningStats, goldParse: Parse) = {
    val chart = fineChart

    // Count pruning
    chart.arcScoresForLoggingStructural.prepareToIterate
    while (chart.arcScoresForLoggingStructural.hasNext) {
      val score = chart.arcScoresForLoggingStructural.next
      val ratio = chart.calcPruningRatio(score)
      stats.update(ratio, true, false, false, true)
    }
    chart.arcScoresForLoggingOther.prepareToIterate
    while (chart.arcScoresForLoggingOther.hasNext) {
      val score = chart.arcScoresForLoggingOther.next
      val ratio = chart.calcPruningRatio(score)
      stats.update(ratio, true, false, false, false, true)
    }
    val maxScoreInCell = new UnboxedArrayBufferDouble(chart.numberOfCells)
    maxScoreInCell.extend(chart.numberOfCells, Double.MinValue)
    for (item <- chart.hashesAndScores) {
      val cell = chart.spanToCell(item._1)
      val ratio = chart.calcPruningRatio(item._5)
      val cur = maxScoreInCell(cell)
      if (cur < ratio) maxScoreInCell(cell) = ratio
      if ((! item._7) && Chart.noDirectEdgesFromState(item._2))
        stats.update(ratio, false, false, true)
      stats.update(ratio, false, false, false)
    }
    for (i <- 0 until chart.numberOfCells)
      stats.update(maxScoreInCell(i), false, false, false, false, false,
        false, true)

    var maxScore = Double.MinValue
    var minScore = Double.MaxValue
    chart.arcScoresForLoggingMax.foreachPair{ case (_, score) =>
      if (score > maxScore) maxScore = score
      if (score < minScore) minScore = score
    }

    val (gterms, garcs) = stage.model.parseToArrays(goldParse, false, stage)
    val gitems = insertDerivation(gterms, garcs, true)
///    Log.logln(fineChart.toString())
    for (item <- chart.hashesAndScores(gitems)) {
      val ratio = chart.calcPruningRatio(item._5)
      Log.logln(s"Gold $ratio from ${item._5} ${item._1} ${item._2} and ${chart.curMaxMarginal} ${chart.curMinMarginal}, ${chart.scoreRange} ${!Chart.noDirectEdgesFromState(item._2)} ${item._8} ${item._9} ${item._10}")
      stats.update(ratio, false, true, false)
      if (!Chart.noDirectEdgesFromState(item._2))
        stats.update(ratio, true, true, false, item._8, item._9, item._10)
      else if (! item._7)
        stats.update(ratio, false, true, true)

      if (! Chart.noDirectEdgesFromState(item._2)) {
        val cell = chart.spanToCell(item._1)
        val score = chart.arcScoresForLoggingMax.getOrElse(cell, minScore)
        val sRatio = (score - minScore) / (maxScore - minScore)
///        logln(s"pruningInfo gold $sRatio ${item._1._2 - item._1._1}")
        chart.arcScoresForLoggingMax.put(cell, minScore)
      }
    }
///    for ((cell, pos) <- gitems) {
///      chart.setCurCellAndPos(cell, pos)
///      Log.logln(s"Gold item ${chart.cellToSpan(cell)} $cell $pos ${chart.curStateAsString} ${chart.outsideScore} ${chart.insideScore}")
///    }

    chart.arcScoresForLoggingMax.foreachPair{ case (cell, score) =>
      val span = chart.cellToSpan(cell.toInt)
      val sRatio = (score - minScore) / (maxScore - minScore)
///      logln(s"pruningInfo auto $sRatio ${span._2 - span._1}")
    }

    stats.updatePotential(goldParse.tokens.length, stage.model.argIndex.size)
  }

  def calculateArcHammingLoss(
    garcs: ArrayBuffer[(Int, Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)]
  ) = {
    var loss = 0.0
    val garcsNoLabel = garcs.map{ v =>
      val isStructural = stage.model.isTreeEdge(v._3)
      val isChain = stage.model.isChainTraceEdge(v._3)
      (v._1, v._2, isStructural, isChain)
    }
    for (aarc <- aarcs) {
      if (! garcs.contains(aarc)) {
        val isStructural = stage.model.isTreeEdge(aarc._3)
        val isChain = stage.model.isChainTraceEdge(aarc._3)
        if (isStructural) {
          if (! garcsNoLabel.contains((aarc._1, aarc._2, isStructural, isChain)))
            loss += Config.lossWeightExtraS
          else
            loss += Config.lossWeightDiffS
        } else {
          if (! garcsNoLabel.contains((aarc._1, aarc._2, isStructural, isChain)))
            loss += Config.lossWeightExtraT
          else
            loss += Config.lossWeightDiffT
        }
      }
    }
    for (garc <- garcs) {
      var exactMatch = false
      var edgeMatch = false
      val isStructural = stage.model.isTreeEdge(garc._3)
      val isChain = stage.model.isChainTraceEdge(garc._3)
      for (aarc <- aarcs) {
        if (isStructural == stage.model.isTreeEdge(aarc._3) &&
            isChain == stage.model.isChainTraceEdge(aarc._3)) {
          if (garc == aarc) {
            exactMatch = true
          } else if (garc._1 == aarc._1 && garc._2 == aarc._2) {
            edgeMatch = true
          }
        }
      }
      if (exactMatch || edgeMatch) { }
      else if (isStructural) loss += Config.lossWeightMissedS
      else loss += Config.lossWeightMissedT
    }
    loss
  }

  def calculateMatching(
    gterminals: ArrayBuffer[(Int, Int)], garcs: ArrayBuffer[(Int, Int, Int)],
    aterminals: ArrayBuffer[(Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)]
  ) : Double = {
    aterminals.intersect(gterminals).length + aarcs.intersect(garcs).length
  }

  def calculateLoss(
    gterminals: ArrayBuffer[(Int, Int)], garcs: ArrayBuffer[(Int, Int, Int)],
    aterminals: ArrayBuffer[(Int, Int)], aarcs: ArrayBuffer[(Int, Int, Int)],
    lossType: LossType.Value
  ) : Double = {
    lossType match {
      case LossType.ZERO => 0.0
      case LossType.P => throw new Exception("Not implemented yet")
      case LossType.R => throw new Exception("Not implemented yet")
      case LossType.HAMMING =>
        val spineLoss = Config.lossWeightSpine * (
          aterminals.length - aterminals.intersect(gterminals).length)
        val arcLoss = calculateArcHammingLoss(garcs, aarcs)
        spineLoss + arcLoss
    }
  }

  def calculateTotal(
    terminals: ArrayBuffer[(Int, Int)], arcs: ArrayBuffer[(Int, Int, Int)]
  ) : Int = terminals.length + arcs.length
}

