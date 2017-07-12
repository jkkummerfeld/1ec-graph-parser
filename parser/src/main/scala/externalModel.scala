// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import java.io.BufferedInputStream
import java.io.FileInputStream

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

@SerialVersionUID(1L)
class ExternalModel(
  formalism: Formalism.Value,
  wordIndex: IntIndexer,
  argIndex: ArgumentIndexer,
  nonTerminalIndex: NonTerminalIndexer,
  filterByTag: Boolean,
  val datafiles: String
) extends Model(formalism, wordIndex, argIndex, nonTerminalIndex, filterByTag) {
  // Convenient storage of all the scores in the datafile
  // Keys for loadedScores are (sentence ID, word Index, Spine ID)
  val loadedScores = new LongDoubleMap(Config.loadFactor, 1 << 21)
  val backupScores = new HashMap[(Int, Int), Double]
  val allSpines = HashSet(RootSpineID)

  Log.logln("Loading external spine scores")

  // When using these, ranking and ratio based pruning decisions should be at
  // a per-word level (i.e. the top 1% of tags for each word). For the ratio,
  // scores should be left as is (no exponential).

  val (maxScore, minScore) = {
    var max = Double.MinValue
    var min = Double.MaxValue
    for (datafile <- datafiles.split(" ").tail) {
      Log.logln(s"Reading from $datafile")
      // Read the file
      val src = new BufferedInputStream(new FileInputStream(datafile))
      val data = ExternalSpines.Scores.parseFrom(src)

      // Read the spine-ID map for this file
      val idToSpine = new HashMap[Int, psg.Spine]
      for (spine <- data.spines) {
        val raw = spine.text
        val cleaned =
          if (! raw.contains(";")) psg.Spine.fromText(raw)
          else {
            val parts = raw.split(";")
            val base = psg.Spine.fromText(parts(0))
            val edge_strings = parts.tail.filter( _.length > 0)
            val edges = edge_strings.map{ content =>
              //"WHNP_0:T:NP_0:T:*T*"
              val pieces = content.split(":")
              // Find src node
              val targetNode = {
                val subparts = pieces(0).split("_")
                val symbol = subparts.head.split("-").head
                val functions = subparts.head.split("-").tail.toVector
                val count = subparts.last.toInt
                val isNull = pieces(1) == "T"
                base.getMatchingNode(symbol, functions, count, isNull)
              }
              val srcNode = {
                val subparts = pieces(2).split("_")
                val symbol = subparts.head.split("-").head
                val functions = subparts.head.split("-").tail.toVector
                val count = subparts.last.toInt
                val isNull = pieces(3) == "T"
                base.getMatchingNode(symbol, functions, count, isNull)
              }
              val trace = pieces(4)
              new psg.Edge(0, srcNode, 0, targetNode, trace)
            }.toVector
            new psg.Spine(base.nodes, edges)
          }
        idToSpine(spine.id) = cleaned
      }

      // Read the scores
      for (sentence <- data.sentences) {
        for ((scoreSet, wordID) <- sentence.scores.zipWithIndex) {
          backupScores((sentence.id, wordID)) = scoreSet.minScore
          for (scoreInfo <- scoreSet.scores) {
            val readSpine = idToSpine(scoreInfo.id)
            val spine = addSpinePSG(Model.SNoTag, readSpine, true)
            allSpines.add(spine)
            if (scoreInfo.score > max) max = scoreInfo.score
            if (scoreInfo.score < min) min = scoreInfo.score
            loadedScores.put(sentence.id, wordID, spine, scoreInfo.score)
          }
        }
      }
    }
    (max, min)
  }

  Log.logln("Loaded external spine scores")

  override def getInitScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int,
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) = {
    if (wIndex == words.size - 1) {
      if (spine == RootSpineID) 2.0
      else 1.0
    } else {
      val backup = backupScores((sentID, wIndex))
      loadedScores.getOrElse(sentID, wIndex, spine, backup)
    }
  }

  override def getSpineOptionsByToken(token: Int) = allSpines
  override def getSpineOptionsByTag(tag: Int) = allSpines

  override def toString() = s"Spine scores from $datafiles"


  // Unused methods:

  override def getInitScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int,
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) = 0.0
  override def getArcScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    parentIndex: Int, childIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, direction: Int, length: Int,
    scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit,
    useCache: Boolean
  ) = 0.0
  override def getArcScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    parentIndex: Int, childIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, direction: Int, length: Int, scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit, useCache: Boolean, edgeSpace: Int
  ) = 0.0
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
}

