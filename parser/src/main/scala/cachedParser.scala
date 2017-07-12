// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Stack, Queue, Set}
import scala.math

import edu.berkeley.nlp.graphparser.Log._

// TODO: Define a CoarseParser trait and use it to effectively restrict the space of callable methods here

// TODO: Switch to index-agnostic representation (using strings)
@SerialVersionUID(1L)
class CachedChartData(
  val tokenIDs: Vector[Int],
  val tagIDs: Vector[Int]
) extends Serializable {
  val sentenceLength = tokenIDs.length
  val entries = sentenceLength * (sentenceLength - 1) / 2
  var minMarginal = Float.MaxValue
  var maxMarginal = Float.MinValue
  val scores = new LongLongMap(0.5, 1 << 16)
  val binaryStates = Array.fill(entries)(UnboxedArrayBuffer())
  val spineTopOptions = Array.fill(sentenceLength)(new LongSet(0.5, 1 << 10))

  final val notPresent : Long =
    (java.lang.Float.floatToRawIntBits(Float.MinValue).toLong << 32) +
    java.lang.Float.floatToRawIntBits(Float.MinValue).toLong

  def getScore(id: Long) = scores.getOrElse(id, notPresent)

  def compact = {
    // Remove unfilled buffer elements
    for (buffer <- binaryStates) buffer.resize(buffer.length - 1)
  }

  def size = {
    val binarySize = binaryStates.map(_.storage.length).sum
    val spineSize = spineTopOptions.map(_.data.length).sum
    s"scores: ${scores.data.length}   binary: $binarySize   spines: $spineSize"
  }
}

object CachedChartData {
  def sentenceFilename(
    prefix: String, chartType: String, tokenIDs: Vector[Int],
    tagIDs: Vector[Int]
  ) = {
    // Note, we must include tags because we can have the same sentence
    // with different sets of tags (this occurs in the training data).
    val sentenceID = Hash.hashToLong((tokenIDs ++ tagIDs): _*)
///    Log.logln(s"sentenceFilename $prefix $chartType ${tokenIDs} ${tagIDs} $sentenceID")
    prefix + chartType +"."+ sentenceID.toString +".gz"
  }

  def save(prefix: String, chartType: String, data: CachedChartData) = {
    val filename = sentenceFilename(prefix, chartType, data.tokenIDs,
      data.tagIDs)
///    data.compact
///    Log.logln(s"Saving $chartType length ${data.tagIDs.length} size ${data.size}")
    ObjectWriter.save(data, filename)
  }

  def read(
    prefix: String, chartType: String, tokenIDs: Vector[Int],
    tagIDs: Vector[Int]
  ) : CachedChartData = {
    // Read in the parse from file
///    Log.logln(s"Reading:\n$prefix $chartType $tokenIDs $tagIDs")
    val filename = sentenceFilename(prefix, chartType, tokenIDs, tagIDs)
    ObjectReader.load(filename) match {
      case data: CachedChartData => data
      case a => require(false, s"Invalid data in file $filename") ; null
    }
  }
}

class CachedChart(
  stage: Stage,
  tokens: Vector[String],
  tags: Vector[String],
  sentenceID: Int,
  doNotPruneGoldArcs: Boolean,
  doNotPruneGoldSpines: Boolean,
  minRatio: Double,
  maxRank: Double,
  val chartType: String,
  val filePrefix: String
) extends Chart(stage, null, null, null, tokens, tags, sentenceID, null, null, null, LossType.P, doNotPruneGoldArcs, doNotPruneGoldSpines, minRatio, maxRank, 0, false) {
  override val thisClass = "CachedChart"

///  override val minRatio =
///    if (chartType == "Arc") minRatios(0)
///    else if (chartType == "Local") minRatios(1)
///    else 0.0

  override val serialisableChartData = CachedChartData.read(filePrefix,
    chartType, tokenIDs, tagIDs)
  curMinMarginal = serialisableChartData.minMarginal
  curMaxMarginal = serialisableChartData.maxMarginal
  scoreRange = curMaxMarginal - curMinMarginal

  override val spineTopOptions = null
  override def hasSpineOption(pos: Int, symbol: Int, num: Int) = {
    val options = serialisableChartData.spineTopOptions(pos)
    options.contains(symbol, num)
  }

  // Not used here
  override val beams = null
  override val labelGen = null
  override val scoreCache = null
  featureCounter = null
  useModelCache = false
  override val statesLog = null
  override val binaryLog = null
  override val pruningLog = null
  override val arcScoresForLoggingStructural = null
  override val arcScoresForLoggingOther = null
///  override val arcSumsForLogging = null
  override val denseStates = null
  override val maxSplit = null
  override val minSplit = null
  override def stats = ""

  override def minSplitGet(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = span1 + 1
  override def maxSplitGet(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = span2 - 1
  override def minSplitUpdate(
    rightSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = { }
  override def maxSplitUpdate(
    leftSpine: Int, parent: HasParent.Value, span1: Int, span2: Int
  ) = { }

  override def toString() = ""

  override def setCurCellVars(start: Int, end: Int) = { }
  override def setCurCellVars1(start: Int, end: Int) = { }
  override def setCurCell1(start: Int, end: Int, subbeams: Long*) = { }
  override def setCurCell1(cell: Int) : Unit = { }
  override def convenienceSet0 = { }
  override def convenienceSet1 = { }
  override def setCurCellAndPos(cell: Int, statePos: Int) = { }
  override def setCurCellAndPos1(cell: Int, statePos: Int) = { }
  override def next1 : Boolean = false

  override def setCurCell(start: Int, end: Int, subbeams: Long*) : Unit = {
    curSpan00 = start
    curSpan01 = end
    curCell0 = spanToCell(start, end)
    curCellStates0 = serialisableChartData.binaryStates(curCell0)
    curStatePos0 = -6
  }
  override def next : Boolean = {
    curStatePos0 += 6
    if (curStatePos0 < curCellStates0.length) {
      curState00 = curCellStates0(curStatePos0)
      curState01 = curCellStates0(curStatePos0 + 1)
      curState02 = curCellStates0(curStatePos0 + 2)
      curState03 = curCellStates0(curStatePos0 + 3)
      curPtr04 = curCellStates0(curStatePos0 + 4)
      curPtr05 = curCellStates0(curStatePos0 + 5)
      true
    } else false
  }

  override def posForHash(loc: Int, subBeam: Long, hash: Long) =
    idForLocBeamHash(loc, subBeam, hash)

  override def insideForPos(loc: Int, pos: Int) : Float = {
    val id = idForLocPos(loc, pos)
    val num = serialisableChartData.getScore(id)
    upperFloat(num)
  }
  override def outsideForPos(loc: Int, pos: Int) : Float = {
    val id = idForLocPos(loc, pos)
    val num = serialisableChartData.getScore(id)
    lowerFloat(num)
  }
  override def insideScore : Float = java.lang.Float.intBitsToFloat(curPtr04)
  override def outsideScore : Float = java.lang.Float.intBitsToFloat(curPtr05)

  override def checkCoarseState(
    state: Int, lSpine: Int, rSpine: Int, xSpine: Int, span: (Int, Int),
    child: Int, parent: Int, arg: Int, stateNoArg: Int, gold: Boolean,
    init: Boolean, arcSecondStage: Boolean = false, arcCreation: Int = 0,
    structural: Boolean = false, chain: Boolean = false
  ) = true
  override def checkCoarseSpine(
    pos: Int, spine: Int, gold: Boolean
  ) = true

  override def scoreArc(
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, lossType: LossType.Value,
    goldHere: Boolean, countOnGet: (Int, Double) => Unit = null
  ) = {
    val id = idForArg(childIndex, parentIndex, childSpine, parentSpine, arg)
    val num = serialisableChartData.getScore(id)
    lowerFloat(num)
  }

  override def doInsidePass = { }

  override def doOutsidePass = curMaxMarginal

  override def addSpineTopOptions = {
    // TODO
  }
}

class CachedParser(
  stage: Stage,
  filePrefix: String,
  minRatio: Double,
  maxRank: Double,
  val chartType: String
) extends Parser(stage, None, None, None, minRatio, maxRank, 0, false, false, false) {
  override val thisClass = "CachedParser"

  override def makeFineChart(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    goldEdges: UnboxedArrayBuffer, goldUnaries: ArrayBuffer[Int],
    goldStates: Set[(Int, Int, Int, Int, Int)],
    lossType: LossType.Value
  ) = {
    new CachedChart(stage, tokens, tags, sentenceID, doNotPruneGoldArcs,
      doNotPruneGoldSpines, minRatio, maxRank, chartType, filePrefix)
  }
  override def parse(
    tokens: Vector[String], tags: Vector[String], sentenceID: Int,
    doOutside: Boolean = true, goldEdges: UnboxedArrayBuffer = null,
    goldUnaries: ArrayBuffer[Int] = null,
    goldStates: Set[(Int, Int, Int, Int, Int)] = null,
    lossType: LossType.Value = LossType.ZERO
  ) = {
    logln("Parsing (cached) "+ (tokens mkString " "))
    if (tags.length > 0) logln("With POS tags "+ (tags mkString " "))
    else logln("Without pre-defined POS tags")

    if (! prepared)
      prepare(tokens, tags, sentenceID, goldEdges, goldUnaries, goldStates,
        lossType)

    prepared = false

    (null, null, fineChart.curMinMarginal)
  }
}

