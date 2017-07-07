// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
  * To improve (AMR mainly)
  *  - Backoff process into propbank information for verbs.
  *  - Also backoff into uncapitalised words.
  *  - Allow capitalised words that are unknown to become literals.
  */
@SerialVersionUID(1L)
class GenerativeModel(
  formalism: Formalism.Value,
  wordIndex: IntIndexer,
  argIndex: ArgumentIndexer,
  nonTerminalIndex: NonTerminalIndexer,
  filterByTag: Boolean
) extends Model(formalism, wordIndex, argIndex, nonTerminalIndex, filterByTag) {
  // Counts for dependency model
  val c_Wd_Td_L_Wh_Th_D = HashMap[(Int, Int, Int, Int, Int, Int), Double]().withDefaultValue(0)
  val c_Wh_Th_D = HashMap[(Int, Int, Int), Double]().withDefaultValue(0)
  val c_Wd_Td_L_Th_D = HashMap[(Int, Int, Int, Int, Int), Double]().withDefaultValue(0)
  val c_Th_D = HashMap[(Int, Int), Double]().withDefaultValue(0)
  val c_Wd_Td = HashMap[(Int, Int), Double]().withDefaultValue(0)
  val c_Td = HashMap[Int, Double]().withDefaultValue(0)
  var c_Tdsum = 0.0
  val c_Td_L_Wh_Th_D = HashMap[(Int, Int, Int, Int, Int), Double]().withDefaultValue(0)
  val c_Td_L_Th_D = HashMap[(Int, Int, Int, Int), Double]().withDefaultValue(0)

  // word -> (sequence ID -> score)
  val unaryScores = HashMap[Int, HashMap[Int, Double]]()

  def dependencyIncrement(
    wd: Int, td: Int, l: Int, wh: Int, th: Int, d: Int
  ) : Unit = {
    c_Td(td) += 1
    c_Tdsum += 1
    c_Td_L_Th_D((td, l, th, d)) += 1
    c_Td_L_Wh_Th_D((td, l, wh, th, d)) += 1
    c_Th_D((th, d)) += 1
    c_Wd_Td((wd, td)) += 1
    c_Wd_Td_L_Th_D((wd, td, l, th, d)) += 1
    c_Wd_Td_L_Wh_Th_D((wd, td, l, wh, th, d)) += 1
    c_Wh_Th_D((wh, th, d)) += 1
  }

  def dependencyIncrement(
    wd: String, td: String, l: String, wh: String, th: String, d: Int
  ) : Unit = {
    dependencyIncrement(wordIndex(wd), nonTerminalIndex(td), argIndex(l), wordIndex(wh), nonTerminalIndex(th), d)
  }

  def incrementInit(w: String, id: Int) = {
    val wID = wordIndex(w)
    val map = unaryScores.getOrElseUpdate(wID, HashMap[Int, Double]())
    if (map.contains(id)) map(id) += 1
    else map(id) = 1
  }

  def normaliseInit = {
    for ((w, map) <- unaryScores) {
      val sum = map.foldLeft(0.0)(_ + _._2)
      for ((k, v) <- map) map(k) = v / sum
    }
  }

  override def getInitScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int,
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) =
    unaryScores.getOrElse(words(wIndex), HashMap[Int, Double]()).getOrElse(spine, 0.0)

  override def getInitScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int,
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) = getInitScoreDep(words, prefixes, tags, sentID, wIndex, spine, countOnGet)

  def getArcScoreWP(wd: Int, wh: Int, td: Int, th: Int, l: Int, d: Int) = {
    val alpha = 0.2
    val beta = 0.1
    val gamma = 0.1
    val cTd = c_Td(td)
    val cTd_L_Th_D = c_Td_L_Th_D((td, l, th, d))
    val cTd_L_Wh_Th_D = c_Td_L_Wh_Th_D((td, l, wh, th, d))
    val cTh_D = c_Th_D((th, d))
    val cWd_Td = c_Wd_Td((wd, td))
    val cWd_Td_L_Th_D = c_Wd_Td_L_Th_D((wd, td, l, th, d))
    val cWd_Td_L_Wh_Th_D = c_Wd_Td_L_Wh_Th_D((wd, td, l, wh, th, d))
    val cWh_Th_D = c_Wh_Th_D((wh, th, d))
///    println(s"$wd $wh $td $th $l $d")
///    println(s"$cTd $cTd_L_Th_D $cTd_L_Wh_Th_D $cTh_D $cWd_Td $cWd_Td_L_Th_D $cWd_Td_L_Wh_Th_D $cWh_Th_D")

    val ans =
      if (cTd == 0 || cTh_D == 0) 0
      else {
        val backoff1 = cWd_Td_L_Th_D / cTh_D
        val backoff2 = cTd_L_Th_D / cTh_D
        val first =
          (cWd_Td_L_Wh_Th_D + beta * backoff1) / (cWh_Th_D + beta)
        val second =
          (cTd_L_Wh_Th_D + gamma * backoff2) / (cWh_Th_D + gamma)
        val wGt =
          if (cWd_Td < 1e-7) 1.0
          else cWd_Td / cTd

        alpha * first + (1 - alpha) * wGt * second
      }
    ans
  }

  override def getArcScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    parentIndex: Int, childIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, direction: Int, length: Int,
    scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit,
    useCache: Boolean
  ) = {
    val wd = words(childIndex)
    val wh = words(parentIndex)
    val td = tags(childIndex)
    val th = tags(parentIndex)
    getArcScoreWP(wd, wh, td, th, arg, direction)
  }

  override def getArcScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    parentIndex: Int, childIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, direction: Int, length: Int, scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit, useCache: Boolean, edgeSpace: Int
  ) = {
    val wd = words(childIndex)
    val wh = words(parentIndex)
    val td = tags(childIndex)
    val th = tags(parentIndex)
    getArcScoreWP(wd, wh, td, th, arg, direction)
  }

  override def getArcCoarseScore(
    score: Double, childIndex: Int, parentIndex: Int, arg: Int,
    source: ParserType.Value, countOnGet: (Int, Double) => Unit
  ) = 0.0
  override def getSpineCoarseScore(
    score: Double, index: Int, sentenceLength: Int, spine: Int,
    source: ParserType.Value, countOnGet: (Int, Double) => Unit
  )  = 0.0

  override def getBinaryScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, stateLeft: Int, stateMiddle: Int,
    stateRight: Int, spineLeft: Int, spineSplit1: Int, spineSplit2: Int,
    spineRight: Int, spineXL: Int, spineXM: Int, spineXR: Int, sentenceID: Int,
    scoreCache: Array[Double], countOnGet: (Int, Double) => Unit,
    useCache: Boolean
  ) = 0.0

  override def getFullBinaryScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit
  ) = 0.0

  override def getFullBinaryScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit
  ) = 0.0

  override def toString() = {
    val ans = ArrayBuffer(super.toString())

///    ans.append("\nModel Unary Scores:")
///    for ((word, map) <- unaryScores) {
///      val sword = wordIndex.value(word)
///      for ((uID, score) <- map) {
///        val (aseq, wseq) = unaries(uID)
///        val text = unaryChainToString(aseq, wseq)
///        ans.append(s"UScore $sword-$word --> $text $score")
///      }
///    }

    def dToString(d: Int) =
      if (d == 0) "Lparent"
      else "Rparent"

    ans.append("\nModel (Wd Td L Wh Th D)")
    for (((wd, td, l, wh, th, d), c) <- c_Wd_Td_L_Wh_Th_D)
      ans.append(s"Wd Td L Wh Th D   $wd $td $l $wh $th $d ${c}    ${wordIndex.value(wd)} ${nonTerminalIndex.value(td)} ${argIndex.value(l)} ${wordIndex.value(wh)} ${nonTerminalIndex.value(th)} ${dToString(d)}")

    ans.append("\nModel (Wd Td L Th D)")
    for (((wd, td, l, th, d), c) <- c_Wd_Td_L_Th_D)
      ans.append(s"Wd Td L Th D      $wd $td $l $th $d ${c}    ${wordIndex.value(wd)} ${nonTerminalIndex.value(td)} ${argIndex.value(l)} ${nonTerminalIndex.value(th)} ${dToString(d)}")

    ans.append("\nModel (Td L Wh Th D)")
    for (((td, l, wh, th, d), c) <- c_Td_L_Wh_Th_D)
      ans.append(s"Td L Wh Th D      $td $l $wh $th $d ${c}    ${nonTerminalIndex.value(td)} ${argIndex.value(l)} ${wordIndex.value(wh)} ${nonTerminalIndex.value(th)} ${dToString(d)}")

    ans.append("\nModel (Td L Th D)")
    for (((td, l, th, d), c) <- c_Td_L_Th_D)
      ans.append(s"Td L Th D         $td $l $th $d ${c}    ${nonTerminalIndex.value(td)} ${argIndex.value(l)} ${nonTerminalIndex.value(th)} ${dToString(d)}")

    ans.append("\nModel (Wh Th D)")
    for (((w, t, d), c) <- c_Wh_Th_D)
      ans.append(s"Wh Th D           $w $t $d ${c}    ${wordIndex.value(w)} ${nonTerminalIndex.value(t)} ${dToString(d)}")

    ans.append("\nModel (Th D)")
    for (((t, d), c) <- c_Th_D)
      ans.append(s"Th D              $t $d ${c}    ${nonTerminalIndex.value(t)} ${dToString(d)}")

    ans.append("\nModel (Wd Td)")
    for (((w, t), c) <- c_Wd_Td)
      ans.append(s"Wd Td             $w $t ${c}    ${wordIndex.value(w)} ${nonTerminalIndex.value(t)}")

    ans.append("\nModel (Td)")
    for ((t, c) <- c_Td)
      ans.append(s"Td                $t ${c}    ${nonTerminalIndex.value(t)}")

///    ans.append("\nModel Grammar Constants")
///    var count = unaryScores.size
///    var gSum = 0.0
///    var tSum = 0.0
///    for ((w, map) <- unaryScores) {
///      for ((uID, score) <- map) {
///        gSum += 1
///        tSum += unaries(uID)._2.size
///      }
///    }
///    ans.append(s"Paths  per word (g):   ${gSum / count} = $gSum / $count")
///    ans.append(s"States per word (t):   ${tSum / count} = $tSum / $count")
///    ans.append(s"Arc types       (m):   ${argIndex.size}")
///    ans.append(s"Word types         :   ${wordIndex.size}")

    ans mkString "\n"
  }

  // ======================================================
  // AMR

  // ======================================================
  // Common
}

