// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.ArrayBuffer
import scala.math

@SerialVersionUID(2L)
class FeatureIndexer
extends Serializable {
  // There are several classes of weights, where frequent/rare/never are based
  // on frequency of gold arcs only:
  //
  //  1 surface frequent, arc frequent
  //  2 surface frequent, arc rare
  //  3 surface frequent, arc unseen
  //  4 surface rare, arc frequent
  //  5 surface rare, arc rare
  //  6 surface rare, arc unseen
  //  7 surface unseen
  //
  // 1 and 4 get their own weight
  // 2, 3, 5, 6, and 7 are hashed into a set of weights, and so there may be
  // collisions. However, they hash into different ranges. This means, for
  // example, that a type 5 feature will not collide with a type 7 one. This
  // behaviour can be varied with flags.
  //
  // To get a weight:
  //   Input is the surface feature and the arg ID
  //   Convert the argID into the position defined by the edgeMap (maybe change this to just reorder the argument index?)
  //   For each surface feature number, store an unboxed array of length to fit the highest arg seen with this surface feature
  //   At each position, store the relevant weight ID

  val surfaceFeatToID = new LongIntMap(Config.loadFactor, 2 << 10, -1)
  val structureMapping = ArrayBuffer[UnboxedArrayBuffer]()
  val mergeAllRareOrUnseen = Config.mergeAllRareOrUnseen
  val mergeRareAndUnseenSurfaceFeats = Config.mergeRareAndUnseenSurfaceFeats
  val mergeRareAndUnseenArcFeats = Config.mergeRareAndUnseenArcFeats
  val negativeFeaturePower = Config.negativeFeaturePower
  var values = UnboxedArrayBufferDouble()
  var positiveFeaturesDone = false
  var maskType2 = -1
  var maskType3 = -1
  var maskType5 = -1
  var maskType6 = -1
  var maskType7 = -1
  var startType2 = -1
  var startType3 = -1
  var startType5 = -1
  var startType6 = -1
  var startType7 = -1

  // Function to update a weight when querying (e.g. for AdaGrad sparse
  // updates).
  @transient var updateOnGet : Int => Unit = null

  def size = values.length

  def sizes = (values.length, structureMapping.size,
    surfaceFeatToID.size, surfaceFeatToID.data.length,
    surfaceFeatToID.threshold)
  def sizeDesc = "Weights, Structures, = Surface Feats, Surface Feat [Memory, Space, Next]"
  def nonZeroCounts = {
    val counts = Array.fill[Long](8)(0)
    values.prepareToIterate
    while (values.hasNext) {
      val next = values.next
      val pos = values.cpos
      if (next.abs > 1e-8) {
        if (pos < startType2) counts(0) += 1
        else if (pos < startType3) counts(2) += 1
        else if (pos < startType5) counts(3) += 1
        else if (pos < startType6) counts(5) += 1
        else if (pos < startType7) counts(6) += 1
        else counts(7) += 1
      }
    }
    (
      counts(0), startType2,
      counts(2), startType3 - startType2,
      counts(3), startType5 - startType3,
      counts(5), startType6 - startType5,
      counts(6), startType7 - startType6,
      counts(7), values.length - startType7
    )
  }

  def set(id: Int, weight: Double) = values(id) = weight
  def update(id: Int, delta: Double) = values(id) += delta

  def squareSum = {
    var ans = 0.0
    values.prepareToIterate
    while (values.hasNext) {
      val num = values.next
      ans += num * num
    }
    ans
  }

  def sum = {
    var ans = 0.0
    values.prepareToIterate
    while (values.hasNext) ans += values.next.abs
    ans
  }

  /** ID is a positive integer assigned in order of observation.
    */
  @inline def getOrAddID(sid: Int, structure: Int) : Int = {
    if (sid < 0 || structureMapping(sid) == null) {
      // Type 7
      startType7 + (Hash.hashN(sid, structure) & maskType7)
    } else {
      val feats = structureMapping(sid)
      var ans =
        if (structure < feats.length) feats(structure)
        else if (mergeAllRareOrUnseen) -7
        else if (feats.length > 0 && positiveFeaturesDone)
          feats.last // Set to be -6 or -3 when pruning
        else -6

      if (ans < 0) {
        // Two possibilities:
        //   - We are not done adding positive features
        //   - We need to hash to get this value
        if (! positiveFeaturesDone) {
          if (structure >= feats.length)
            feats.extend(structure, -6)
          ans = values.length
          values.append(0.0)
          feats(structure) = ans
        } else {
          if (ans == -6) {
            // type 6
            ans = startType6 + (Hash.hashN(sid, structure) & maskType6)
          } else if (ans == -5) {
            // type 5
            ans = startType5 + (Hash.hashN(sid, structure) & maskType5)
          } else if (ans == -3) {
            // type 3
            ans = startType3 + (Hash.hashN(sid, structure) & maskType3)
          } else if (ans == -2) {
            // type 2
            ans = startType2 + (Hash.hashN(sid, structure) & maskType2)
          } else {
            // Type 7
            ans = startType7 + (Hash.hashN(sid, structure) & maskType7)
          }
        }
      }
      ans
    }
  }

  def getByID(
    surfaceFeatID: Int, structure: Int, countOnGet: (Int, Double) => Unit,
    value: Double = 1.0, addingBias: Boolean = false
  ) : Double = {
    val id = getOrAddID(surfaceFeatID, structure)

    // Initalise the bias
    if (addingBias) values(id) = value

    // Track queries to weights
    if (countOnGet != null) countOnGet(id, value)

    // Run the update function, used by AdaGrad for just in time updates
    if (updateOnGet != null) updateOnGet(id)

    if (addingBias) values(id)
    else value * values(id)
  }

  def getSumByID(
    surfaceFeatIDs: UnboxedArrayBuffer, part: Int,
    countOnGet: (Int, Double) => Unit
  ) : Double = {
    var score = 0.0
    var feat = 0
    while (feat < surfaceFeatIDs.length) {
      score += getByID(surfaceFeatIDs(feat), part, countOnGet)
      feat += 1
    }
    score
  }

  var totalFeatsInLists = 0
  def getByFeat(
    surfaceFeat: Long, structure: Int = -1,
    countOnGet: (Int, Double) => Unit = null,
    addFeatureOnGet: UnboxedArrayBuffer = null, value: Double = 1.0,
    isBias: Boolean = false
  ) : Double = {
    // Gets the ID for this surface feature
    var surfaceFeatID = surfaceFeatToID.getAndNote(surfaceFeat)
    var addingBias = false
    if (surfaceFeatID < 0) {
      // This feature is not in the map, so either:
      //   - It is of type 7
      //   - We are still collecting positive features and it is 1-6
      if (positiveFeaturesDone) {
        surfaceFeatID = - (Hash.hashLongToInt(surfaceFeat).abs)
      } else {
        surfaceFeatID = surfaceFeatToID.size
        surfaceFeatToID.putNoted(surfaceFeatID)
        val nArray = UnboxedArrayBuffer(1)
        nArray(0) = -6
        structureMapping.append(nArray)
        if (isBias) addingBias = true
      }
    }

    // Add the feature to the list being created
    // Checking structure < 0 as well means we can concurrently do queries that
    // use this method for scoring. TODO: This seems redundant.
    if (structure < 0 && addFeatureOnGet != null) {
      addFeatureOnGet.append(surfaceFeatID)
      totalFeatsInLists += 1
    }

    // Return the weight if a valid structure was given
    if (structure < 0) 0.0
    else getByID(surfaceFeatID, structure, countOnGet, value, addingBias)
  }

  def pruneAndPrepare(
    surfaceCutoff: Double, arcCutoff: Double, featCounts: IntIntMap
  ) = {
    if (! positiveFeaturesDone) {
      positiveFeaturesDone = true
      val prev = values.length
      values = UnboxedArrayBufferDouble()

      def fn(h: Long, sid: Int) = {
        val feats = structureMapping(sid)

        // Determine overall count for the surface feature
        var surfaceFreq = 0.0
        var pos = 0
        var maxFreqArc = -1
        while (pos < feats.length) {
          if (feats(pos) >= 0) {
            val freq = featCounts.getOrElse(feats(pos), 0)
            surfaceFreq += freq
            if (freq > arcCutoff) maxFreqArc = pos
          }
          pos += 1
        }

        if (mergeRareAndUnseenSurfaceFeats &&
            surfaceFreq < surfaceCutoff && maxFreqArc < 0) {
          // TODO: Think about the case where maxFreqArc < 0, can we always
          // do this?
          // merge 4-7, eliminate a chunk of surface feats entirely
          structureMapping(sid) = null
          surfaceFeatToID.put(h, -1)
        } else {
          if (mergeRareAndUnseenArcFeats) {
            // merge 2-3, 5-6  shrink feats to extend to the last frequent
            // arc
            feats.resize(maxFreqArc.max(0))
          }

          // Sort out what happens to each surface+arc feature based on
          // the frequencies of each.
          // Set default for this surface feat
          if (surfaceFreq >= surfaceCutoff) feats.append(-3)
          else if (mergeRareAndUnseenSurfaceFeats) feats.append(-7)
          else feats.append(-6)
          pos = 0
          while (pos < feats.length) {
            if (feats(pos) >= 0) {
              val arcFreq = featCounts.getOrElse(feats(pos), 0)
              if (arcFreq < arcCutoff) {
                if (surfaceFreq < surfaceCutoff) {
                  feats(pos) =
                    if (mergeAllRareOrUnseen) -7
                    else if (mergeRareAndUnseenArcFeats) -6
                    else -5
                } else {
                  feats(pos) =
                    if (mergeAllRareOrUnseen) -7
                    else if (mergeRareAndUnseenArcFeats) -3
                    else -2
                }
              } else {
                // types 1 and 4
                feats(pos) = values.length
                values.append(0.0)
              }
            } else {
              feats(pos) =
                if (mergeAllRareOrUnseen) -7
                else if (surfaceFreq < surfaceCutoff) -6 // type 6
                else -3 // type 3
            }
            pos += 1
          }
        }
      }
      surfaceFeatToID.foreachPair(fn)

      // Set masks and starts
      val shift = ((math.log(values.length) / math.log(2)).floor.toInt + 1 + negativeFeaturePower).max(1)
      val mask = (1 << shift) - 1
      maskType2 = mask
      maskType3 = mask
      maskType5 = mask
      maskType6 = mask
      if (mergeAllRareOrUnseen) {
        maskType2 = 0
        maskType3 = 0
        maskType5 = 0
        maskType6 = 0
      }
      if (mergeRareAndUnseenArcFeats) {
        maskType2 = 0
        maskType5 = 0
      }
      if (mergeRareAndUnseenSurfaceFeats) {
        maskType5 = 0
        maskType6 = 0
      }
      maskType7 = mask

      startType2 = values.length
      startType3 = startType2 + maskType2 + 1
      startType5 = startType3 + maskType3 + 1
      startType6 = startType5 + maskType5 + 1
      startType7 = startType6 + maskType6 + 1

      val maxLength = startType7 + mask + 1
      values.extend(maxLength, 0.0)

      Log.logln(s"Keeping $startType2 of $prev features")
      Log.logln(s"Allowing:")
      Log.logln(s"Type 2 - $startType2 to ${startType2 + maskType2}, $maskType2")
      Log.logln(s"Type 3 - $startType3 to ${startType3 + maskType3}, $maskType3")
      Log.logln(s"Type 5 - $startType5 to ${startType5 + maskType5}, $maskType5")
      Log.logln(s"Type 6 - $startType6 to ${startType6 + maskType6}, $maskType6")
      Log.logln(s"Type 7 - $startType7 to ${startType7 + maskType7}, $maskType7")
    }
  }

  override def toString() = {
    val ans = ArrayBuffer[String]()
    ans.append(s"\nModel ${sizeDesc} ${sizes}")
    ans.append(s"Model non-zero counts ${nonZeroCounts}")
    ans.append(s"surfaceFeatureList contents ${totalFeatsInLists}")
    ans.mkString("\n")
  }
}

@SerialVersionUID(1L)
class DiscriminativeModel(
  formalism: Formalism.Value,
  wordIndex: IntIndexer,
  argIndex: ArgumentIndexer,
  nonTerminalIndex: NonTerminalIndexer,
  filterByTag: Boolean
) extends Model(formalism, wordIndex, argIndex, nonTerminalIndex, filterByTag) {
  val newLocalArcFeatures = Config.newLocalArcFeatures
  val newLocalBinaryFeatures = Config.newLocalBinaryFeatures
  val useBetweenFeatures = Config.useBetweenFeatures

  val weights : FeatureIndexer = new FeatureIndexer
  val multiStructureMap : LongIntMap =
    new LongIntMap(Config.loadFactor, 1 << 15, -2)
  def getMultiStructure(num0: Int, num1: Int) = {
    val key = (num0.toLong << 32) + num1
    if (! weights.positiveFeaturesDone) {
      multiStructureMap.synchronized {
        multiStructureMap.getOrElseUpdate(key, multiStructureMap.size)
      }
    } else {
      multiStructureMap.getOrElse(key,
        (Hash.hashLongToInt(key) & 0x0fff) + multiStructureMap.size
      )
    }
  }
  def getMultiStructure(num0: Int, num1: Int, num2: Int) = {
    val key = (num0.toLong << 42) + (num1.toLong << 21) + num2
    if (! weights.positiveFeaturesDone) {
      multiStructureMap.synchronized {
        multiStructureMap.getOrElseUpdate(key, multiStructureMap.size)
      }
    } else {
      multiStructureMap.getOrElse(key,
        (Hash.hashLongToInt(key) & 0x0fff) + multiStructureMap.size
      )
    }
  }

  val MAX_TAG = 0xff
  val MAX_WORD = 0xffff
  val MAX_ID = 0x3f
  val MAX_NUM = 0x3f

  // Feature definition to Long - inspired by CCK
  // Symbol    Space in bits
  //  POS tag     8
  //  word       16
  //  num         6
  //  id          6
  // NOTE: ID must be last, otherwise we can get collisions
  // TODO: Switch to Value Classes (no cost at runtime, but means there is a check here)
  @inline final def tttttComb(id: Int, tag0: Int, tag1: Int, tag2: Int, tag3: Int, tag4: Int) = {
    var ans : Long = 0
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 8 ; ans += tag2
    ans <<= 8 ; ans += tag3
    ans <<= 8 ; ans += tag4
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def tttttnComb(id: Int, tag0: Int, tag1: Int, tag2: Int, tag3: Int, tag4: Int, num: Int) = {
    var ans : Long = 0
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 8 ; ans += tag2
    ans <<= 8 ; ans += tag3
    ans <<= 8 ; ans += tag4
    ans <<= 8 ; ans += num
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def wtComb(id: Int, word: Int, tag: Int) = {
    var ans : Long = 0
    ans <<= 16 ; ans += word
    ans <<= 8 ; ans += tag
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def niwComb(id: Int, id1: Int, i: Int, word: Int) = {
    var ans : Long = 0
    ans <<= 32 ; ans += i
    ans <<= 16 ; ans += word
    ans <<= 8 ; ans += id1
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def nittComb(id: Int, id1: Int, i: Int, tag0: Int, tag1: Int) = {
    var ans : Long = 0
    ans <<= 32 ; ans += i
    ans <<= 16 ; ans += tag0
    ans <<= 16 ; ans += tag1
    ans <<= 8 ; ans += id1
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def nwtnComb(id: Int, id1: Int, word: Int, tag: Int, num: Int) = {
    var ans : Long = 0
    ans <<= 16 ; ans += word
    ans <<= 8 ; ans += tag
    ans <<= 8 ; ans += num
    ans <<= 8 ; ans += id1
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def wtnComb(id: Int, word: Int, tag: Int, num: Int) = {
    var ans : Long = 0
    ans <<= 16 ; ans += word
    ans <<= 8 ; ans += tag
    ans <<= 8 ; ans += num
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def wwwnComb(
    id: Int, word0: Int, word1: Int, word2: Int, num: Int
  ) = {
    var ans : Long = 0
    ans <<= 16 ; ans += word0
    ans <<= 16 ; ans += word1
    ans <<= 16 ; ans += word2
    ans <= 8 ; ans += num
    ans <<= 6 ; ans += id
    ans
  }

  @inline final def dhwtComb(
    id: Int, dir: Int, isHead: Int, word: Int, tag: Int
  ) = {
    var ans : Long = 0
    ans <<= 1 ; ans += isHead
    ans <<= 1 ; ans += dir
    ans <<= 16 ; ans += word
    ans <<= 8 ; ans += tag
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dhwwwComb(
    id: Int, dir: Int, isHead: Int, word0: Int, word1: Int, word2: Int
  ) = {
    var ans : Long = 0
    ans <<= 1 ; ans += isHead
    ans <<= 1 ; ans += dir
    ans <<= 16 ; ans += word0
    ans <<= 16 ; ans += word1
    ans <<= 16 ; ans += word2
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dhtttComb(
    id: Int, dir: Int, isHead: Int, tag0: Int, tag1: Int, tag2: Int
  ) = {
    var ans : Long = 0
    ans <<= 1 ; ans += isHead
    ans <<= 1 ; ans += dir
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 8 ; ans += tag2
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dhtttttComb(
    id: Int, dir: Int, isHead: Int, tag0: Int, tag1: Int, tag2: Int,
    tag3: Int, tag4: Int
  ) = {
    var ans : Long = 0
    ans <<= 1 ; ans += isHead
    ans <<= 1 ; ans += dir
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 8 ; ans += tag2
    ans <<= 8 ; ans += tag3
    ans <<= 8 ; ans += tag4
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dttwwComb(id: Int, dir: Int, tag0: Int, tag1: Int, word0: Int, word1: Int) = {
    var ans : Long = 0
    ans <<= 1 ; ans += dir
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 16 ; ans += word0
    ans <<= 16 ; ans += word1
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dttttComb(id: Int, dir: Int, tag0: Int, tag1: Int, tag2: Int, tag3: Int) = {
    var ans : Long = 0
    ans <<= 1 ; ans += dir
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 8 ; ans += tag2
    ans <<= 8 ; ans += tag3
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dnttComb(id: Int, dir: Int, num: Int, tag0: Int, tag1: Int) = {
    var ans : Long = 0
    ans <<= 1 ; ans += dir
    ans <<= 6 ; ans += num
    ans <<= 8 ; ans += tag0
    ans <<= 8 ; ans += tag1
    ans <<= 6 ; ans += id
    ans
  }
  @inline final def dnwwComb(
    id: Int, dir: Int, num: Int, word0: Int, word1: Int
  ) = {
    var ans : Long = 0
    ans <<= 1 ; ans += dir
    ans <<= 6 ; ans += num
    ans <<= 16 ; ans += word0
    ans <<= 16 ; ans += word1
    ans <<= 6 ; ans += id
    ans
  }

  // ======================================================
  // Init

  def initGet(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) = {
    // Note - the IDs used for spines overlap with IDs used for arcs. This is
    // okay because the surface features are distinguished.
    // TODO: Consider separating the negative features too.
    val structuralFeatures = getPartsForPSG(spine, true)
    if (featureDev.length < 4 || featureDev(3) == 'F') {
      val bw = if (wIndex < 1) MAX_WORD - 2 else wordFeatures(wIndex - 1)
      val bbw = if (wIndex < 2) MAX_WORD - 2 else wordFeatures(wIndex - 2)
      val aw =
        if (wIndex >= wordFeatures.length - 1) MAX_WORD - 2
        else wordFeatures(wIndex + 1)
      val aaw =
        if (wIndex >= wordFeatures.length - 2) MAX_WORD - 2
        else wordFeatures(wIndex + 2)
      val w =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex >= wordFeatures.length) MAX_WORD - 2
        else wordFeatures(wIndex)
      val t =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex >= tags.length) MAX_WORD - 2
        else tags(wIndex)

      val at =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex >= tags.length - 1) MAX_WORD - 2
        else tags(wIndex + 1)
      val aat =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex >= tags.length - 2) MAX_WORD - 2
        else tags(wIndex + 2)

      val bt =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex <= 0) MAX_WORD - 2
        else tags(wIndex - 1)
      val bbt =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex <= 1) MAX_WORD - 2
        else tags(wIndex - 2)

      var symPos = 0
      var ans = 0.0
      while (symPos < structuralFeatures.length) {
        val sym = structuralFeatures(symPos)
        if (sym >= 0) {
          ans += weights.getByFeat(tttttComb(0, bbt, bt, t, at, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, bt, t, at, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, 0, t, at, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, bt, t, 0, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, bt, t, at, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, 0, t, at, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, bt, t, 0, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, bt, t, at, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, 0, t, 0, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, 0, t, at, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, bt, t, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, bbt, 0, t, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, bt, t, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, 0, t, at, 0), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, 0, t, 0, aat), sym, countOnGet)
          ans += weights.getByFeat(tttttComb(0, 0, 0, t, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wtComb(1, w, t), sym, countOnGet)
          ans += weights.getByFeat(wtComb(1, w, 0), sym, countOnGet)
          ans += weights.getByFeat(wtComb(1, 0, t), sym, countOnGet)
          ans += weights.getByFeat(wtComb(22, bw, bt), sym, countOnGet)
          ans += weights.getByFeat(wtComb(22, bw, 0), sym, countOnGet)
          ans += weights.getByFeat(wtComb(22, 0, bt), sym, countOnGet)
          ans += weights.getByFeat(wtComb(23, bbw, bbt), sym, countOnGet)
          ans += weights.getByFeat(wtComb(23, bbw, 0), sym, countOnGet)
          ans += weights.getByFeat(wtComb(23, 0, bbt), sym, countOnGet)
          ans += weights.getByFeat(wtComb(24, aw, at), sym, countOnGet)
          ans += weights.getByFeat(wtComb(24, aw, 0), sym, countOnGet)
          ans += weights.getByFeat(wtComb(24, 0, at), sym, countOnGet)
          ans += weights.getByFeat(wtComb(25, aaw, aat), sym, countOnGet)
          ans += weights.getByFeat(wtComb(25, aaw, 0), sym, countOnGet)
          ans += weights.getByFeat(wtComb(25, 0, aat), sym, countOnGet)
        }
        symPos += 1
      }
      ans
    } else {
      val w =
        if (wIndex == -1) MAX_WORD - 1
        else if (wIndex >= wordFeatures.length) MAX_WORD - 2
        else wordFeatures(wIndex)
      val bw =
        if (wIndex < 1) MAX_WORD - 2
        else wordFeatures(wIndex - 1)
      val bbw =
        if (wIndex < 2) MAX_WORD - 2
        else wordFeatures(wIndex - 2)
      val aw =
        if (wIndex >= wordFeatures.length - 1) MAX_WORD - 2
        else wordFeatures(wIndex + 1)
      val aaw =
        if (wIndex >= wordFeatures.length - 2) MAX_WORD - 2
        else wordFeatures(wIndex + 2)

      var symPos = 0
      var ans = 0.0
      while (symPos < structuralFeatures.length) {
        val sym = structuralFeatures(symPos)
        if (sym >= 0) {
          ans += weights.getByFeat(wwwnComb(0, w, 0, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(1, bw, 0, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(1, bw, w, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(1, bw, 0, aw, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(2, bbw, 0, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(2, bbw, w, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(2, bbw, 0, bw, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(3, aw, 0, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(3, aw, w, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(3, aw, 0, aaw, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(4, aaw, 0, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(4, aaw, w, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(4, aaw, 0, bbw, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(5, bbw, aw, 0, 0), sym, countOnGet)
          ans += weights.getByFeat(wwwnComb(5, aaw, 0, bw, 0), sym, countOnGet)
        }
        symPos += 1
      }
      ans
    }
  }
  def initGetForTrace(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    wIndex: Int, sym: Int, isHead: Int, dir: Int,
    countOnGet: (Int, Double) => Unit
  ) = {
    val bw = if (wIndex < 1) MAX_WORD - 2 else wordFeatures(wIndex - 1)
    val bbw = if (wIndex < 2) MAX_WORD - 2 else wordFeatures(wIndex - 2)
    val aw =
      if (wIndex >= wordFeatures.length - 1) MAX_WORD - 2
      else wordFeatures(wIndex + 1)
    val aaw =
      if (wIndex >= wordFeatures.length - 2) MAX_WORD - 2
      else wordFeatures(wIndex + 2)
    val w =
      if (wIndex == -1) MAX_WORD - 1
      else if (wIndex >= wordFeatures.length) MAX_WORD - 2
      else wordFeatures(wIndex)
    val t =
      if (wIndex == -1) MAX_WORD - 1
      else if (wIndex >= tags.length) MAX_WORD - 2
      else tags(wIndex)
    val at =
      if (wIndex == -1) MAX_WORD - 1
      else if (wIndex >= tags.length - 1) MAX_WORD - 2
      else tags(wIndex + 1)
    val aat =
      if (wIndex == -1) MAX_WORD - 1
      else if (wIndex >= tags.length - 2) MAX_WORD - 2
      else tags(wIndex + 2)
    val bt =
      if (wIndex == -1) MAX_WORD - 1
      else if (wIndex <= 0) MAX_WORD - 2
      else tags(wIndex - 1)
    val bbt =
      if (wIndex == -1) MAX_WORD - 1
      else if (wIndex <= 1) MAX_WORD - 2
      else tags(wIndex - 2)

    weights.getByFeat(dhwtComb(0, dir, isHead, w, t), sym, countOnGet) +
    weights.getByFeat(dhwtComb(0, dir, isHead, w, 0), sym, countOnGet) +
    weights.getByFeat(dhwtComb(0, dir, isHead, 0, t), sym, countOnGet) +
    weights.getByFeat(dhwtComb(1, dir, isHead, bw, bt), sym, countOnGet) +
    weights.getByFeat(dhwtComb(1, dir, isHead, bw, 0), sym, countOnGet) +
    weights.getByFeat(dhwtComb(1, dir, isHead, 0, bt), sym, countOnGet) +
    weights.getByFeat(dhwtComb(2, dir, isHead, bbw, bbt), sym, countOnGet) +
    weights.getByFeat(dhwtComb(2, dir, isHead, bbw, 0), sym, countOnGet) +
    weights.getByFeat(dhwtComb(2, dir, isHead, 0, bbt), sym, countOnGet) +
    weights.getByFeat(dhwtComb(3, dir, isHead, aw, at), sym, countOnGet) +
    weights.getByFeat(dhwtComb(3, dir, isHead, aw, 0), sym, countOnGet) +
    weights.getByFeat(dhwtComb(3, dir, isHead, 0, at), sym, countOnGet) +
    weights.getByFeat(dhwtComb(4, dir, isHead, aaw, aat), sym, countOnGet) +
    weights.getByFeat(dhwtComb(4, dir, isHead, aaw, 0), sym, countOnGet) +
    weights.getByFeat(dhwtComb(4, dir, isHead, 0, aat), sym, countOnGet) +
    weights.getByFeat(dhtttComb(5, dir, isHead, 0, t, at), sym, countOnGet) +
    weights.getByFeat(dhtttComb(5, dir, isHead, bt, t, 0), sym, countOnGet) +
    weights.getByFeat(dhtttComb(5, dir, isHead, t, at, aat), sym, countOnGet) +
    weights.getByFeat(dhtttComb(6, dir, isHead, bbt, bt, 0), sym, countOnGet) +
    weights.getByFeat(dhtttComb(6, dir, isHead, 0, at, aat), sym, countOnGet) +
    weights.getByFeat(dhtttComb(7, dir, isHead, bt, t, at), sym, countOnGet) +
    weights.getByFeat(dhtttComb(8, dir, isHead, bbt, bt, t), sym, countOnGet)
  }

  override def getInitScoreDep(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    sentID: Int, wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) = initGet(words, wordFeatures, tags, wIndex, spine, countOnGet)

  override def getInitScorePSG(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    sentID: Int, wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ) = initGet(words, wordFeatures, tags, wIndex, spine, countOnGet)


  // ======================================================
  // Arc

  private def setUnigramFeatures(
    dir: Int, l: Int, w: Int, t: Int, p: Int, isHead: Int, bw: Int, bbw: Int,
    aw: Int, aaw: Int, bt: Int, bbt: Int, at: Int, aat: Int,
    storage: UnboxedArrayBuffer
  ) = {
    // NOTE - Currently assume l, p are unused
    weights.getByFeat(dhwtComb(2, dir, isHead, w, t), -1, null, storage)
    weights.getByFeat(dhwtComb(2, dir, isHead, w, 0), -1, null, storage)
    weights.getByFeat(dhwtComb(2, dir, isHead, 0, t), -1, null, storage)
    weights.getByFeat(dhwtComb(3, dir, isHead, bw, bt), -1, null, storage)
    weights.getByFeat(dhwtComb(3, dir, isHead, bw, 0), -1, null, storage)
    weights.getByFeat(dhwtComb(3, dir, isHead, 0, bt), -1, null, storage)
    weights.getByFeat(dhwtComb(4, dir, isHead, bbw, bbt), -1, null, storage)
    weights.getByFeat(dhwtComb(4, dir, isHead, bbw, 0), -1, null, storage)
    weights.getByFeat(dhwtComb(4, dir, isHead, 0, bbt), -1, null, storage)
    weights.getByFeat(dhwtComb(5, dir, isHead, aw, at), -1, null, storage)
    weights.getByFeat(dhwtComb(5, dir, isHead, aw, 0), -1, null, storage)
    weights.getByFeat(dhwtComb(5, dir, isHead, 0, at), -1, null, storage)
    weights.getByFeat(dhwtComb(6, dir, isHead, aaw, aat), -1, null, storage)
    weights.getByFeat(dhwtComb(6, dir, isHead, aaw, 0), -1, null, storage)
    weights.getByFeat(dhwtComb(6, dir, isHead, 0, aat), -1, null, storage)
    weights.getByFeat(dhtttComb(7, dir, isHead, 0, t, at), -1, null, storage)
    weights.getByFeat(dhtttComb(7, dir, isHead, bt, t, 0), -1, null, storage)
    weights.getByFeat(dhtttComb(7, dir, isHead, t, at, aat), -1, null, storage)
    weights.getByFeat(dhtttComb(8, dir, isHead, bt, t, at), -1, null, storage)
    weights.getByFeat(dhtttComb(9, dir, isHead, bbt, bt, t), -1, null, storage)
  }

  private def setMultigramFeatures(
    tags: Vector[Int], child: Int, parent: Int, dir: Int, l: Int, wd: Int,
    wh: Int, td: Int, th: Int, atd: Int, ath: Int, btd: Int, bth: Int, pd: Int,
    ph: Int, length: Int, storage: UnboxedArrayBuffer
  ) = {
    weights.getByFeat(dttwwComb(10, dir, th, td, wh, wd), -1, null, storage)
    weights.getByFeat(dttwwComb(10, dir,  0, td, wh, wd), -1, null, storage)
    weights.getByFeat(dttwwComb(10, dir, th,  0, wh, wd), -1, null, storage)
    weights.getByFeat(dttwwComb(10, dir,  0,  0, wh, wd), -1, null, storage)
    weights.getByFeat(dttwwComb(10, dir, th, td,  0, wd), -1, null, storage)
    weights.getByFeat(dttwwComb(10, dir, th, td, wh,  0), -1, null, storage)
    weights.getByFeat(dttwwComb(10, dir, th, td,  0,  0), -1, null, storage)

    if (length > 1) {
      weights.getByFeat(dttttComb(11, dir, th, td, ath, atd), -1, null, storage)
      weights.getByFeat(dttttComb(12, dir, th, td, ath, btd), -1, null, storage)
      weights.getByFeat(dttttComb(13, dir, th, td, bth, atd), -1, null, storage)
      weights.getByFeat(dttttComb(14, dir, th, td, bth, btd), -1, null, storage)
      weights.getByFeat(dttttComb(11, dir, th, td, ath,   0), -1, null, storage)
      weights.getByFeat(dttttComb(11, dir, th, td,   0, atd), -1, null, storage)
      weights.getByFeat(dttttComb(12, dir, th, td,   0, btd), -1, null, storage)
      weights.getByFeat(dttttComb(13, dir, th, td, bth,   0), -1, null, storage)
    } else {
      if (child < parent) {
        // Consider:  __  child  parent  __
        weights.getByFeat(dttttComb(15, dir, th, td, ath, btd), -1, null, storage)
        weights.getByFeat(dttttComb(15, dir, th, td, ath,   0), -1, null, storage)
        weights.getByFeat(dttttComb(15, dir, th, td,   0, btd), -1, null, storage)
      } else {
        // Consider:  __  parent  child  __
        weights.getByFeat(dttttComb(15, dir, th, td, atd, bth), -1, null, storage)
        weights.getByFeat(dttttComb(15, dir, th, td, atd,   0), -1, null, storage)
        weights.getByFeat(dttttComb(15, dir, th, td,   0, bth), -1, null, storage)
      }
    }

    if (tags.length > 0 && useBetweenFeatures) {
      val left = child.min(parent)
      val right = child.max(parent)
      var index = left + 1
      val done = UnboxedArrayBuffer(100)
      var coord =
        (if (nonTerminalIndex.isCoord(th)) 1 else 0) +
        (if (nonTerminalIndex.isCoord(td)) 1 else 0)
      var punct =
        (if (nonTerminalIndex.isPunct(th)) 1 else 0) +
        (if (nonTerminalIndex.isPunct(td)) 1 else 0)
      var verb =
        (if (nonTerminalIndex.isVerb(th)) 1 else 0) +
        (if (nonTerminalIndex.isVerb(td)) 1 else 0)
      while (index < right) {
        val tb = tags(index)
        if (nonTerminalIndex.isPunct(tb)) punct += 1
        if (nonTerminalIndex.isVerb(tb)) verb += 1
        if (nonTerminalIndex.isCoord(tb)) coord += 1
        if (tb >= done.length || done(tb) == 0) {
          weights.getByFeat(dttttComb(16, dir, th, td, tb, 0), -1, null, storage)
          done.extend(tb, 0)
          done(tb) = 1
        }
        index += 1
      }
      weights.getByFeat(dnttComb(17, dir, coord, th, td), -1, null, storage)
      weights.getByFeat(dnttComb(17, dir, coord, 0, 0), -1, null, storage)
      weights.getByFeat(dnttComb(18, dir, verb, th, td), -1, null, storage)
      weights.getByFeat(dnttComb(18, dir, verb, 0, 0), -1, null, storage)
      weights.getByFeat(dnttComb(19, dir, punct, th, td), -1, null, storage)
      weights.getByFeat(dnttComb(19, dir, punct, 0, 0), -1, null, storage)
    }

    val clippedLength = if (length <= 40) length else 41
    weights.getByFeat(dnttComb(20, dir, clippedLength,  0,  0), -1, null, storage)
    weights.getByFeat(dnttComb(20, dir, clippedLength, th,  0), -1, null, storage)
    weights.getByFeat(dnttComb(20, dir, clippedLength,  0, td), -1, null, storage)
    weights.getByFeat(dnttComb(20, dir, clippedLength, th, td), -1, null, storage)
    if (length > 2) {
      weights.getByFeat(dnttComb(21, dir, 2,  0,  0), -1, null, storage)
      weights.getByFeat(dnttComb(21, dir, 2, th,  0), -1, null, storage)
      weights.getByFeat(dnttComb(21, dir, 2,  0, td), -1, null, storage)
      weights.getByFeat(dnttComb(21, dir, 2, th, td), -1, null, storage)
      if (length > 5) {
        weights.getByFeat(dnttComb(21, dir, 5,  0,  0), -1, null, storage)
        weights.getByFeat(dnttComb(21, dir, 5, th,  0), -1, null, storage)
        weights.getByFeat(dnttComb(21, dir, 5,  0, td), -1, null, storage)
        weights.getByFeat(dnttComb(21, dir, 5, th, td), -1, null, storage)
        if (length > 10) {
          weights.getByFeat(dnttComb(21, dir, 10,  0,  0), -1, null, storage)
          weights.getByFeat(dnttComb(21, dir, 10, th,  0), -1, null, storage)
          weights.getByFeat(dnttComb(21, dir, 10,  0, td), -1, null, storage)
          weights.getByFeat(dnttComb(21, dir, 10, th, td), -1, null, storage)
          if (length > 20) {
            weights.getByFeat(dnttComb(21, dir, 20,  0,  0), -1, null, storage)
            weights.getByFeat(dnttComb(21, dir, 20, th,  0), -1, null, storage)
            weights.getByFeat(dnttComb(21, dir, 20,  0, td), -1, null, storage)
            weights.getByFeat(dnttComb(21, dir, 20, th, td), -1, null, storage)
            if (length > 30) {
              weights.getByFeat(dnttComb(21, dir, 30,  0,  0), -1, null, storage)
              weights.getByFeat(dnttComb(21, dir, 30, th,  0), -1, null, storage)
              weights.getByFeat(dnttComb(21, dir, 30,  0, td), -1, null, storage)
              weights.getByFeat(dnttComb(21, dir, 30, th, td), -1, null, storage)
              if (length > 40) {
                weights.getByFeat(dnttComb(21, dir, 40,  0,  0), -1, null, storage)
                weights.getByFeat(dnttComb(21, dir, 40, th,  0), -1, null, storage)
                weights.getByFeat(dnttComb(21, dir, 40,  0, td), -1, null, storage)
                weights.getByFeat(dnttComb(21, dir, 40, th, td), -1, null, storage)
              }
            }
          }
        }
      }
    }
  }
  private def setUnigramWordFeatures(
    dir: Int, h: Int, w: Int, bw: Int, bbw: Int, aw: Int, aaw: Int,
    storage: UnboxedArrayBuffer
  ) = {
    weights.getByFeat(dhwwwComb(10, dir, h,   w,   0,   0), -1, null, storage)
    weights.getByFeat(dhwwwComb(11, dir, h,  bw,   0,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(11, dir, h,  bw,   w,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(11, dir, h,  bw,   0,  aw), -1, null, storage)
///    weights.getByFeat(dhwwwComb(12, dir, h, bbw,   0,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(12, dir, h, bbw,   w,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(12, dir, h, bbw,   0,  bw), -1, null, storage)
    weights.getByFeat(dhwwwComb(13, dir, h,  aw,   0,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(13, dir, h,  aw,   w,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(13, dir, h,  aw,   0, aaw), -1, null, storage)
///    weights.getByFeat(dhwwwComb(14, dir, h, aaw,   0,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(14, dir, h, aaw,   w,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(14, dir, h, aaw,   0, bbw), -1, null, storage)
///    weights.getByFeat(dhwwwComb(15, dir, h, bbw,  aw,   0), -1, null, storage)
///    weights.getByFeat(dhwwwComb(16, dir, h, aaw,   0,  bw), -1, null, storage)
  }
  private def setMultigramWordFeatures(
    wordFeatures: Vector[Int], child: Int, parent: Int, dir: Int, length: Int,
    wd: Int, wh: Int, bwh: Int, bbwh: Int, awh: Int, aawh: Int, bwd: Int,
    bbwd: Int, awd: Int, aawd: Int, storage: UnboxedArrayBuffer
  ) = {
    weights.getByFeat(dnwwComb(20, dir, 0, wh, wd), -1, null, storage)

    val l =
      if (length <= 5) length
      else if (length <= 10) 6
      else if (length <= 20) 7
      else 8
    weights.getByFeat(dnwwComb(21, dir, l, 0, 0), -1, null, storage)

///    val left = child.min(parent)
///    val right = child.max(parent)
///    var pos = left + 1
///    while (pos < right) {
///      val mw = wordFeatures(pos)
///      val dl = pos - left
///      val dr = right - pos
///      weights.getByFeat(dnwwComb(22, dir, 0, mw, 0), -1, null, storage)
///      weights.getByFeat(dnwwComb(23, dir, dl, mw, 0), -1, null, storage)
///      weights.getByFeat(dnwwComb(24, dir, dr, mw, 0), -1, null, storage)
///      pos += 1
///    }
  }

  private def getArcSpineScore(
    wordFeatures: Vector[Int], tags: Vector[Int], child: Int, parent: Int,
    childSpine: Int, parentSpine: Int, arg: Int, dir: Int, len: Int,
    countOnGet: (Int, Double) => Unit
  ) = {
    val tc = tags(child)
    val tp = tags(parent)
    val wc = wordFeatures(child)
    val wp = wordFeatures(parent)
    val atc = if (child >= tags.length - 1) MAX_TAG - 2 else tags(child + 1)
    val atp = if (parent >= tags.length - 1) MAX_TAG - 2 else tags(parent + 1)
    val btc = if (child < 1) MAX_TAG - 2 else tags(child - 1)
    val btp = if (parent < 1) MAX_TAG - 2 else tags(parent - 1)
    val awc =
      if (child >= wordFeatures.length - 1) MAX_WORD - 2
      else wordFeatures(child + 1)
    val awp =
      if (parent >= wordFeatures.length - 1) MAX_WORD - 2
      else wordFeatures(parent + 1)
    val bwc = if (child < 1) MAX_WORD - 2 else wordFeatures(child - 1)
    val bwp = if (parent < 1) MAX_WORD - 2 else wordFeatures(parent - 1)
    val pos =
      if (child < parent) {
        if (len <= 5) len
        else if (len <= 10) 6
        else if (len <= 20) 7
        else 8
      } else {
        if (len <= 5) 9 + len
        else if (len <= 10) 10
        else if (len <= 20) 11
        else 12
      }

    val cS = childSpine
    val pS = parentSpine
    val cS_a = getMultiStructure(childSpine, arg)
    val pS_a = getMultiStructure(parentSpine, arg)
    val pS_cS = getMultiStructure(parentSpine, childSpine)
    val pS_cS_a = getMultiStructure(parentSpine, childSpine, arg)

    var ans = 0.0

    ans += weights.getByFeat(niwComb(29, 0, 0, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 0, 0, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 0, 0, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 0, 0, dir), pS_cS_a, countOnGet)

    ans += weights.getByFeat(niwComb(29, 10, btp, dir), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, dir), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, dir), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, dir), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, dir), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, dir), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, dir), pS_cS_a, countOnGet)

    ans += weights.getByFeat(niwComb(29, 11, bwc, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, dir), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, dir), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, dir), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, dir), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, dir), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, dir), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, dir), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, dir), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, dir), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, dir), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, dir), pS_cS_a, countOnGet)

    ans += weights.getByFeat(niwComb(29, 0, 0, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 0, 0, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 0, 0, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 0, 0, pos), pS_cS_a, countOnGet)

    ans += weights.getByFeat(niwComb(29, 10, btp, pos), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 10, btp, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, pos), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 12, bwp, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, pos), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 2, tp, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, pos), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 4, wp, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, pos), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 6, atp, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, pos), cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 8, awp, pos), pS_cS_a, countOnGet)

    ans += weights.getByFeat(niwComb(29, 11, bwc, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, pos), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 11, bwc, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, pos), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 1, tc, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, pos), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 3, wc, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, pos), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 5, atc, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, pos), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 7, awc, pos), pS_cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, pos), cS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, pos), pS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, pos), pS_a, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, pos), pS_cS, countOnGet)
    ans += weights.getByFeat(niwComb(29, 9, btc, pos), pS_cS_a, countOnGet)

    ans
  }


  override def getArcScoreDep(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int], child: Int,
    parent: Int, childSpine: Int, parentSpine: Int, label: Int, dir: Int,
    length: Int, scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit, useCache: Boolean
  ) = {
    // TODO: Fix to use the new scheme where surface features are always cached
///    unigramFeatures(0, dir, l, wh, th, ph, bwh, bbwh, awh, aawh, bth,
///      bbth, ath, aath, wd, td, pd, bwd, bbwd, awd, aawd, btd, bbtd, atd, aatd)
///    multigramFeatures(tags, child, parent, 0, dir, l, wd, wh, td, th,
///      atd, ath, btd, bth, pd, ph, length)
    0.0
  }

  /* Generating a score cache for this sentence, and updating the surface
   * feature cache to have space for the sentence's features.
   * Note, if we consider caching a sentence's feature lists across runs, we
   * must include tags because we can have the same sentence with different
   * sets of tags (this occurs in the training data).
   */
  def sentencePrepare(
    tokens: Vector[Int], tags: Vector[Int], training: Boolean
  ) = {
    val curSentLength = tokens.length

    // An array, each entry is an array of features.
    val cacheLength =
      (4 * curSentLength) + // For unilexical features
      (curSentLength * (1 + curSentLength)) // For bilexical features
    val surfaceFeatureLists = Array.ofDim[UnboxedArrayBuffer](cacheLength)

    // Make the score cache for this run over this sentence
    // The cache is:
    //  <p >p ...  (length * 2)
    //  <c >c ...  (length * 2)
    //  cp cp ...  (length * length)
    // The argSymbolMap can grow during training (apparently? or has that
    // changed?), so we need to make space for additional entries.
    val edgeSpace = (argSymbolMap.size * (if (training) 1.5 else 1.0)).toInt
    val cacheSize =
      ( (4 * curSentLength) + // For unilexical features
        (curSentLength * curSentLength)) * // For bilexical features
      edgeSpace
    val scoreCache = Array.ofDim[Double](cacheSize)

    (surfaceFeatureLists, scoreCache, edgeSpace)
  }

  @inline def cachedFeatures(
    cachePos: Int, feats: UnboxedArrayBuffer, part: Int,
    curSentWeightCache: Array[Double], countOnGet: (Int, Double) => Unit,
    useCache: Boolean, edgeSpace: Int
  ) = {
    if (part >= edgeSpace) {
      Log.logToErr(s"Getting score for $part when edges space is $edgeSpace")
      Double.MinValue
    } else {
      val pos = cachePos * edgeSpace + part
      val cScore = curSentWeightCache(pos)
      if (useCache && cScore != 0.0) {
        cScore
      } else {
        val calcScore = weights.getSumByID(feats, part, countOnGet)
        // To avoid recomputing, if it is 0, make it something small.  It
        // won't actually change the final score, since it will be lost in
        // rounding.
        val score =
          if (calcScore == 0.0) Double.MinPositiveValue
          else calcScore
        if (useCache) curSentWeightCache(pos) = score
        score
      }
    }
  }

  override def getArcScorePSG(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    child: Int, parent: Int, childSpine: Int, parentSpine: Int, arg: Int,
    dir: Int, len: Int, scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit, useCache: Boolean, edgeSpace: Int
  ) = {
    val structuralFeatures = getPartsForPSG(arg)
    if (edges(arg)._4) {
      // *NON* as the trace
      // This is needed as we don't want so many features in the trace scorer
      val tc = tags(child)
      val tp = tags(parent)
      val lenBin = 3 + (child - parent).abs / 5
      val sym = structuralFeatures.last
      val wc = wordFeatures(child)
      val wp = wordFeatures(parent)

      var ans = 0.0

      if (featureDev(0) == 'T') {
        ans += initGetForTrace(words, wordFeatures, tags, child, sym, 0, dir, countOnGet)
        ans += initGetForTrace(words, wordFeatures, tags, parent, sym, 1, dir, countOnGet)
      } else {
        ans += weights.getByFeat(wwwnComb(31, 0, wc, 0, dir), sym, countOnGet)
        ans += weights.getByFeat(wwwnComb(31, 0, 0, wp, dir), sym, countOnGet)
      }

      if (featureDev(2) == 'T') {
        ans += weights.getByFeat(wwwnComb(31, wc, wp, dir, lenBin), sym, countOnGet)
        ans += weights.getByFeat(wwwnComb(31, wc, wp, dir, 0), sym, countOnGet)
        ans += weights.getByFeat(wwwnComb(31, wc, wp, 0, lenBin), sym, countOnGet)
        ans += weights.getByFeat(wwwnComb(31, wc, wp, 0, 0), sym, countOnGet)
      }

      ans += weights.getByFeat(dnttComb(30, dir, lenBin, tc, tp), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, lenBin, tc,  0), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, lenBin,  0, tp), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, lenBin,  0,  0), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, 0, tc, tp), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, 0, tc,  0), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, 0,  0, tp), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, dir, 0,  0,  0), sym, countOnGet)
      ans += weights.getByFeat(dnttComb(30, 0, 1, tc, tp), sym, countOnGet)

      if (featureDev(1) == 'T') {
        ans += weights.getByFeat(dnttComb(30, 0, 2,  0,  0), sym, countOnGet,
          null, 1.0, true)
      }
      ans
    } else {
      val curSentLength = wordFeatures.length

      val pos = (child * curSentLength + parent) * 3
      val cachePosUni1 = parent * 2 + dir
      val cachePosUni2 = 2 * curSentLength + child * 2 + dir
      val cachePosMulti = 4 * curSentLength + child * curSentLength + parent
      var featsUni1 = surfaceFeatureLists(cachePosUni1)
      var featsUni2 = surfaceFeatureLists(cachePosUni2)
      var featsMulti = surfaceFeatureLists(cachePosMulti)
      var ans = 0.0

      if (featsUni1 == null || featsUni2 == null || featsMulti == null) {
        // Feature Components code start
        val wd = wordFeatures(child)
        val wh = wordFeatures(parent)
        val td = tags(child)
        val th = tags(parent)
        val atd = if (child >= tags.length - 1) MAX_TAG - 2 else tags(child + 1)
        val ath = if (parent >= tags.length - 1) MAX_TAG - 2 else tags(parent + 1)
        val aatd = if (child >= tags.length - 2) MAX_TAG - 2 else tags(child + 2)
        val aath = if (parent >= tags.length - 2) MAX_TAG - 2 else tags(parent + 2)
        val btd = if (child < 1) MAX_TAG - 2 else tags(child - 1)
        val bth = if (parent < 1) MAX_TAG - 2 else tags(parent - 1)
        val bbtd = if (child < 2) MAX_TAG - 2 else tags(child - 2)
        val bbth = if (parent < 2) MAX_TAG - 2 else tags(parent - 2)
        val awd =
          if (child >= wordFeatures.length - 1) MAX_WORD - 2
          else wordFeatures(child + 1)
        val awh =
          if (parent >= wordFeatures.length - 1) MAX_WORD - 2
          else wordFeatures(parent + 1)
        val aawd =
          if (child >= wordFeatures.length - 2) MAX_WORD - 2
          else wordFeatures(child + 2)
        val aawh =
          if (parent >= wordFeatures.length - 2) MAX_WORD - 2
          else wordFeatures(parent + 2)
        val bwd = if (child < 1) MAX_WORD - 2 else wordFeatures(child - 1)
        val bwh = if (parent < 1) MAX_WORD - 2 else wordFeatures(parent - 1)
        val bbwd = if (child < 2) MAX_WORD - 2 else wordFeatures(child - 2)
        val bbwh = if (parent < 2) MAX_WORD - 2 else wordFeatures(parent - 2)
        val l =
          if (len <= 5) len
          else if (len <= 10) 6
          else if (len <= 20) 7
          else 8
        val pd = wordFeatures(child)
        val ph = wordFeatures(parent)
        // Feature Components code end

        if (featsUni1 == null) {
          featsUni1 = UnboxedArrayBuffer(20)
          if (featureDev.length < 4 || featureDev(3) == 'F') {
            setUnigramFeatures(dir, l, wh, th, ph, 0, bwh, bbwh, awh, aawh,
              bth, bbth, ath, aath, featsUni1)
          } else {
            setUnigramWordFeatures(dir, 0, wh, bwh, bbwh, awh, aawh,
              featsUni1)
          }
          surfaceFeatureLists(cachePosUni1) = featsUni1
        }

        if (featsUni2 == null) {
          featsUni2 = UnboxedArrayBuffer(20)
          if (featureDev.length < 4 || featureDev(3) == 'F') {
            setUnigramFeatures(dir, l, wd, td, pd, 1, bwd, bbwd, awd, aawd,
              btd, bbtd, atd, aatd, featsUni2)
          } else {
            setUnigramWordFeatures(dir, 1, wd, bwd, bbwd, awd, aawd,
              featsUni2)
          }
          surfaceFeatureLists(cachePosUni2) = featsUni2
        }

        if (featsMulti == null) {
          featsMulti = UnboxedArrayBuffer(64)
          if (featureDev.length < 4 || featureDev(3) == 'F') {
            setMultigramFeatures(tags, child, parent, dir, l, wd, wh, td, th,
              atd, ath, btd, bth, pd, ph, len, featsMulti)
          } else {
            setMultigramWordFeatures(wordFeatures, child, parent, dir, len,
              wd, wh, bwh, bbwh, awh, aawh, bwd, bbwd, awd, aawd, featsMulti)
          }
          surfaceFeatureLists(cachePosMulti) = featsMulti
        }
      }

      var symbolPos = 0
      if (ruleStructuralFeatureOnly)
        symbolPos = structuralFeatures.length - 1
      if (featureDev.length > 4 && featureDev(4) == 'T') symbolPos -= 1
      while (symbolPos < structuralFeatures.length) {
        val symbol = structuralFeatures(symbolPos)
        if (symbol >= 0) {
          ans += cachedFeatures(cachePosUni1, featsUni1, symbol, scoreCache,
            countOnGet, useCache, edgeSpace)
          ans += cachedFeatures(cachePosUni2, featsUni2, symbol, scoreCache,
            countOnGet, useCache, edgeSpace)
          ans += cachedFeatures(cachePosMulti, featsMulti, symbol, scoreCache,
            countOnGet, useCache, edgeSpace)
        }

        symbolPos += 1
      }

      // When parsing with spines, include joint features
      if (childSpine != NULL_SPINE &&
          parentSpine != NULL_SPINE &&
          newLocalArcFeatures) {
        ans += getArcSpineScore(wordFeatures, tags, child, parent, childSpine,
          parentSpine, arg, dir, len, countOnGet)
      }

      ans
    }
  }

  override def getArcCoarseScore(
    score: Double, childIndex: Int, parentIndex: Int, arg: Int,
    source: ParserType.Value, countOnGet: (Int, Double) => Unit
  ) = {
    var dir = Model.arcToDirection(childIndex, parentIndex)
    var length = (parentIndex - childIndex).abs

    val id0 = dnttComb(33, 0, 0, ParserType.id(source), 0)
    val id1 = dnttComb(33, dir, 0, ParserType.id(source), 1)
    val id2 = dnttComb(33, dir, length, ParserType.id(source), 1)
    val ans =
      weights.getByFeat(id0, 0, countOnGet, null, score) +
      weights.getByFeat(id1, 0, countOnGet, null, score) +
      weights.getByFeat(id2, 0, countOnGet, null, score)
    ans
  }
  override def getSpineCoarseScore(
    score: Double, index: Int, sentenceLength: Int, spine: Int,
    source: ParserType.Value, countOnGet: (Int, Double) => Unit
  ) = {
    val id0 = wwwnComb(34, 0, ParserType.id(source), 0, 0)
    val id1 = wwwnComb(34, index, ParserType.id(source), 1, 0)
    val id2 = wwwnComb(34, sentenceLength - index, ParserType.id(source), 2, 0)
    val ans =
      weights.getByFeat(id0, 0, countOnGet, null, score) +
      weights.getByFeat(id1, 0, countOnGet, null, score) +
      weights.getByFeat(id2, 0, countOnGet, null, score)
    ans
  }

  override def getBinaryScorePSG(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    left: Int, split1: Int, split2: Int, right: Int, stateLeft: Int,
    stateMiddle: Int, stateRight: Int, spineLeft: Int, spineSplit1: Int,
    spineSplit2: Int, spineRight: Int, spineXL: Int, spineXM: Int,
    spineXR: Int, sentenceID: Int, scoreCache: Array[Double],
    countOnGet: (Int, Double) => Unit, useCache: Boolean
  ) : Double = {
    // TODO: Adapt to new features
    if (! newLocalBinaryFeatures) 0.0
    else {
      val fID =
        if (split2 <= 0) 30
        else 31
      var ans = 0.0

      val dist1 = split1 - left
      val dist2 =
        if (split2 <= 0) right - split1
        else split2 - split1
      val dist3 =
        if (split2 <= 0) -1
        else right - split2

      val w0 = wordFeatures(left)
      val w1 = wordFeatures(split1)
      val w2 = if (split2 <= 0) -1 else wordFeatures(split2)
      val w3 = wordFeatures(right)
///      val t0 = tags(left)
      val t1 = tags(split1)
      val t2 = if (split2 <= 0) -1 else tags(split2)
///      val t3 = tags(right)

      val stateComb = Hash.hashN(
        Chart.stateWithPositionalExt(stateLeft, left),
        Chart.stateWithPositionalExt(stateRight, split2.max(split1)),
        Chart.stateWithPositionalExt(stateMiddle, split1)
      )

      // Size of the two halves, plus aspects of the middle
      ans += weights.getByFeat(nwtnComb(fID, 0, t1, dist1, dist2), 0, countOnGet)
      ans += weights.getByFeat(nwtnComb(fID, 0, t1, dist1, dist2), spineSplit1, countOnGet)
      ans += weights.getByFeat(nwtnComb(fID, 1, w1, dist1, dist2), 0, countOnGet)
      ans += weights.getByFeat(nwtnComb(fID, 1, w1, dist1, dist2), spineSplit1, countOnGet)
      ans += weights.getByFeat(nittComb(fID, 2, stateComb, dist1, dist2), 0, countOnGet)
      ans += weights.getByFeat(nittComb(fID, 2, stateComb, dist1, dist2), spineSplit1, countOnGet)
      if (split2 > 0) {
        ans += weights.getByFeat(nwtnComb(fID, 3, t2, dist2, dist3), 0, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 3, t2, dist2, dist3), spineSplit2, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 4, w2, dist2, dist3), 0, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 4, w2, dist2, dist3), spineSplit2, countOnGet)
        ans += weights.getByFeat(nittComb(fID, 5, stateComb, dist2, dist3), 0, countOnGet)
        ans += weights.getByFeat(nittComb(fID, 5, stateComb, dist2, dist3), spineSplit2, countOnGet)
        // With full width
        ans += weights.getByFeat(nwtnComb(fID, 6, t1, dist1, dist2 + dist3), 0, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 6, t1, dist1, dist2 + dist3), spineSplit1, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 7, w1, dist1, dist2 + dist3), 0, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 7, w1, dist1, dist2 + dist3), spineSplit1, countOnGet)
        ans += weights.getByFeat(nittComb(fID, 8, stateComb, dist1, dist2 + dist3), 0, countOnGet)
        ans += weights.getByFeat(nittComb(fID, 8, stateComb, dist1, dist2 + dist3), spineSplit1, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 9, t2, dist1 + dist2, dist3), 0, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 9, t2, dist1 + dist2, dist3), spineSplit2, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 10, w2, dist1 + dist2, dist3), 0, countOnGet)
        ans += weights.getByFeat(nwtnComb(fID, 10, w2, dist1 + dist2, dist3), spineSplit2, countOnGet)
        ans += weights.getByFeat(nittComb(fID, 11, stateComb, dist1 + dist2, dist3), 0, countOnGet)
        ans += weights.getByFeat(nittComb(fID, 11, stateComb, dist1 + dist2, dist3), spineSplit2, countOnGet)
      }

      // Representation of the rule being applied
      ans += weights.getByFeat(niwComb(fID, 12, stateComb, 0), 0, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 13, stateComb, w0), 0, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 13, stateComb, w0), spineLeft, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 13, stateComb, 0), spineLeft, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 14, stateComb, w1), 0, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 14, stateComb, w1), spineSplit1, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 14, stateComb, 0), spineSplit1, countOnGet)
      if (split2 > 0) {
        ans += weights.getByFeat(niwComb(fID, 15, stateComb, w2), 0, countOnGet)
        ans += weights.getByFeat(niwComb(fID, 15, stateComb, w2), spineSplit2, countOnGet)
        ans += weights.getByFeat(niwComb(fID, 15, stateComb, 0), spineSplit2, countOnGet)
      }
      ans += weights.getByFeat(niwComb(fID, 16, stateComb, w3), 0, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 16, stateComb, w3), spineRight, countOnGet)
      ans += weights.getByFeat(niwComb(fID, 16, stateComb, 0), spineRight, countOnGet)

      // TODO:
      // For each point that does not have a parent in this item, find the
      // position furthest away from it that it is the parent of. Then add
      // features based on that:
      //   Structure / arg - top of the spine
      //   Surface:
      //     - POS and word one step further away
      //     - Distnace
      //     - POS and word of the spine

      ans
    }
  }

  override def getFullBinaryScoreDep(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    left: Int, split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit
  ) = 0.0

  def subSpine(
    stateArray: UnboxedArrayBuffer, statePos: Int, query: Int, span0: Int
  ) = FullStructureChart.subSpineForPos(query, span0, stateArray, statePos)
  def subParent(
    stateArray: UnboxedArrayBuffer, statePos: Int, query: Int, span0: Int
  ) = FullStructureChart.subParentForPos(query, span0, stateArray, statePos)
  def subArg(
    stateArray: UnboxedArrayBuffer, statePos: Int, query: Int, span0: Int
  ) = FullStructureChart.subArgForPos(query, span0, stateArray, statePos)
  def externalPos(
    stateArray: UnboxedArrayBuffer, statePos: Int
  ) = FullStructureChart.externalPosForPos(stateArray, statePos)
  def externalArg(
    stateArray: UnboxedArrayBuffer, statePos: Int
  ) = FullStructureChart.externalArgForPos(stateArray, statePos)
  def externalParent(
    stateArray: UnboxedArrayBuffer, statePos: Int
  ) = FullStructureChart.externalParentForPos(stateArray, statePos)

  def higherOrderGet(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    indexA: Int, indexB: Int, indexC: Int, ftype: Int, arg: Int,
    countOnGet: (Int, Double) => Unit
  ) = {
    val wA = wordFeatures(indexA)
    val wB = wordFeatures(indexB)
    val wC = if (indexC >= 0) wordFeatures(indexC) else MAX_WORD - 2
    val tA = tags(indexA)
    val tB = tags(indexB)
    val tC = if (indexC >= 0) tags(indexC) else MAX_TAG - 2

    weights.getByFeat(tttttnComb(26, 0, 0, tA, tB, tC, ftype), arg, countOnGet) +
    weights.getByFeat(tttttnComb(26, 0, 0,  0, tB, tC, ftype), arg, countOnGet) +
    weights.getByFeat(tttttnComb(26, 0, 0, tA,  0, tC, ftype), arg, countOnGet) +
    weights.getByFeat(tttttnComb(26, 0, 0, tA, tB,  0, ftype), arg, countOnGet) +
    weights.getByFeat(wwwnComb(27, wA, wB, wC, ftype), arg, countOnGet) +
    weights.getByFeat(wwwnComb(27,  0, wB, wC, ftype), arg, countOnGet) +
    weights.getByFeat(wwwnComb(27, wA,  0, wC, ftype), arg, countOnGet) +
    weights.getByFeat(wwwnComb(27, wA, wB,  0, ftype), arg, countOnGet) +
    weights.getByFeat(wtnComb(28, wA, tB, ftype*6), arg, countOnGet) +
    weights.getByFeat(wtnComb(28, wA, tC, ftype*6 + 1), arg, countOnGet) +
    weights.getByFeat(wtnComb(28, wB, tA, ftype*6 + 2), arg, countOnGet) +
    weights.getByFeat(wtnComb(28, wB, tC, ftype*6 + 3), arg, countOnGet) +
    weights.getByFeat(wtnComb(28, wC, tA, ftype*6 + 4), arg, countOnGet) +
    weights.getByFeat(wtnComb(28, wC, tB, ftype*6 + 5), arg, countOnGet)
  }

  override def getFullBinaryScorePSG(
    words: Vector[Int], wordFeatures: Vector[Int], tags: Vector[Int],
    left: Int, split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit
  ) = {
    var score = 0.0
    val ternary = split2 > 0

    val lStructStart = left
    val mStructStart = if (split2 < 0) -1 else split1
    val rStructStart = if (split2 < 0) split1 else split2

    // TODO: Handle edges involving external points

    // CCK sibling arcs, each pair of adjacent siblings plus the parent (where
    // the parent is the focus point). The arg is the label of the outer
    // sibling.
    var cur = left
    var prev = -1
    var prevArg = -1
    var closestLeft = -1 // Determined here, for use in grandparent features
    var closestRight = -1 // Determined here, for use in grandparent features

    // If right-x has lStructStart as a child of focus, do that
    if (externalPos(rStructArray, rStructPos) == left &&
        externalParent(rStructArray, rStructPos) == focus) {
      prev = left
      closestLeft = left
      prevArg = externalArg(rStructArray, rStructPos)
    }
    // If middle-x has lStructStart as a child of focus, do that
    if (ternary && externalPos(mStructArray, mStructPos) == left &&
        externalParent(mStructArray, mStructPos) == focus) {
      prev = left
      closestLeft = left
      prevArg = externalArg(mStructArray, mStructPos)
    }

    // Do left (only do rightmost point if focus != split1)
    val leftEnd = split1.min(focus - 1)
    while (cur < leftEnd) {
      val parent = subParent(lStructArray, lStructPos, cur, lStructStart)
      if (parent == focus) {
        if (prev >= 0) {
          // We now have (prev, cur, focus), where focus is the parent of both
          val arg = getPartsForPSG(prevArg).last
          score += higherOrderGet(words, wordFeatures, tags, focus, prev, cur,
            0, arg, countOnGet)
        }
        prev = cur
        closestLeft = cur
        prevArg = subArg(lStructArray, lStructPos, cur, lStructStart)
      }
      cur += 1
    }

    // Do external of right if focus != split1, this is ternary, and right
    // external is at split1 and is a child of focus
    if (ternary && externalPos(rStructArray, rStructPos) == split1 &&
        focus != split1 && externalParent(rStructArray, rStructPos) == focus) {
      cur = split1
      if (prev >= 0) {
        val arg = getPartsForPSG(prevArg).last
        score += higherOrderGet(words, wordFeatures, tags, focus, prev, cur,
          0, arg, countOnGet)
      }
      prev = split1
      closestLeft = split1
      prevArg = externalArg(rStructArray, rStructPos)
    }

    // If ternary, do middle (skip edges depending on focus point)
    if (ternary) {
      val passedFocus =
        if (focus == split1) 1
        else 0
      cur =
        if (passedFocus == 1) split1 + 1
        else split1
      val midEnd =
        if (passedFocus == 1) split2
        else split2 - 1
      if (passedFocus == 1) prev = -1
      while (cur < midEnd) {
        val parent = subParent(mStructArray, mStructPos, cur, mStructStart)
        if (parent == focus) {
          val tArg = subArg(lStructArray, lStructPos, cur, lStructStart)
          if (passedFocus == 1) prevArg = tArg
          if (prev >= 0) {
            // We now have (prev, cur) and focus on one side or the other
            val arg = getPartsForPSG(prevArg).last
            score += higherOrderGet(words, wordFeatures, tags, focus, prev,
              cur, passedFocus, arg, countOnGet)
          }
          prev = cur
          if (passedFocus == 0) {
            prevArg = tArg
            closestLeft = cur
          } else if (closestRight < 0) closestRight = cur
        }
        cur += 1
      }
    }

    // Do external of left if
    //  - focus != split2
    //  - this is ternary
    //  - left external is at split2 and is a child of focus
    if (ternary && externalPos(lStructArray, lStructPos) == split2 &&
        focus != split2 && externalParent(lStructArray, lStructPos) == focus) {
      cur = split2
      if (prev >= 0) {
        val tArg = externalArg(rStructArray, rStructPos)
        val arg = getPartsForPSG(tArg).last
        score += higherOrderGet(words, wordFeatures, tags, focus, prev, cur,
          1, arg, countOnGet)
      }
      prev = split2
      if (closestRight < 0) closestRight = split2
    }

    // Do right (only do leftmost point if focus != split2)
    cur =
      if (ternary && focus == split2) split2 + 1
      else if (ternary) split2
      else split1 + 1
    if (! ternary) prev = -1
    while (cur <= right) {
      val parent = subParent(rStructArray, rStructPos, cur, rStructStart)
      if (parent == focus) {
        if (prev >= 0) {
          // We now have (focus, prev, cur), where focus is the parent of both
          val tArg = subArg(rStructArray, rStructPos, cur, rStructStart)
          val arg = getPartsForPSG(tArg).last
          score += higherOrderGet(words, wordFeatures, tags, focus, prev, cur,
            1, arg, countOnGet)
        }
        if (closestRight < 0) closestRight = cur
        prev = cur
      }
      cur += 1
    }

    // If left-x has right end of span as a child of focus, do that
    // If middle-x has right end of span as a child of focus, do that
    if (externalPos(lStructArray, lStructPos) == right &&
        externalParent(lStructArray, lStructPos) == focus) {
      if (prev >= 0) {
        val tArg = externalArg(lStructArray, lStructPos)
        val arg = getPartsForPSG(tArg).last
        score += higherOrderGet(words, wordFeatures, tags, focus, prev, cur,
          1, arg, countOnGet)
      }
      if (closestRight < 0) closestRight = right
    }
    // If middle-x has lStructStart as a child of focus, do that
    if (ternary && externalPos(mStructArray, mStructPos) == right &&
        externalParent(mStructArray, mStructPos) == focus) {
      if (prev >= 0) {
        val tArg = externalArg(mStructArray, mStructPos)
        val arg = getPartsForPSG(tArg).last
        score += higherOrderGet(words, wordFeatures, tags, focus, prev, cur,
          1, arg, countOnGet)
      }
      if (closestRight < 0) closestRight = right
    }


    // Have (h,m) be the arc from the focus to its parent
    val m = focus
    val (rawArg, h) =
      if (focus == split1) {
        if (subArg(lStructArray, lStructPos, m, lStructStart) != 0) {
          // in left
          ( subArg(lStructArray, lStructPos, m, lStructStart),
            subParent(lStructArray, lStructPos, m, lStructStart) )
        } else if (ternary &&
            subArg(mStructArray, mStructPos, m, mStructStart) != 0) {
          // in middle (if ternary)
          ( subArg(mStructArray, mStructPos, m, mStructStart),
            subParent(mStructArray, mStructPos, m, mStructStart) )
        } else if (!ternary &&
            subArg(rStructArray, rStructPos, m, rStructStart) != 0) {
          // in right (if not ternary)
          ( subArg(rStructArray, rStructPos, m, rStructStart),
            subParent(rStructArray, rStructPos, m, rStructStart) )
        } else if (ternary &&
            externalPos(rStructArray, rStructPos) == split1 &&
            externalArg(rStructArray, rStructPos) != 0) {
          // right external (if ternary)
          ( externalArg(rStructArray, rStructPos),
            externalParent(rStructArray, rStructPos) )
        } else throw new Exception("Considering focus word with no parent")
      } else {
        if (subArg(mStructArray, mStructPos, m, mStructStart) != 0) {
          // in middle
          ( subArg(mStructArray, mStructPos, m, mStructStart),
            subParent(mStructArray, mStructPos, m, mStructStart) )
        } else if (subArg(rStructArray, rStructPos, m, rStructStart) != 0) {
          // in right
          ( subArg(rStructArray, rStructPos, m, rStructStart),
            subParent(rStructArray, rStructPos, m, rStructStart) )
        } else if (ternary &&
            externalPos(lStructArray, lStructPos) == split2 &&
            externalArg(lStructArray, lStructPos) != 0) {
          // left external
          ( externalArg(lStructArray, lStructPos),
            externalParent(lStructArray, lStructPos) )
        } else throw new Exception("Considering focus word with no parent")
      }
    val arg = getPartsForPSG(rawArg).last
    // CCK grandparent 1, an arc (h,m) and the arc (g,m) with g closest to m,
    // to its left
    score += higherOrderGet(words, wordFeatures, tags, h, m, closestLeft, 2,
      arg, countOnGet)
    // CCK grandparent 2, same but on the right
    score += higherOrderGet(words, wordFeatures, tags, h, m, closestRight, 3,
      arg, countOnGet)

    // Additionally, if left == 0, and there is an edge left -> focus, then we
    // need:
    //  - that edge, plus the arc modifying 0 that is closest to 0
    //  - all pairs of children of 0
    // These are needed because 0 is never the middle of a binary step. The end
    // of the sentence has the same issue, but is always the root, so doesn't
    // produce features (only one child, so no sibling features, and no parent,
    // so no grandparent features).
    if (left == 0 &&
        ( subParent(lStructArray, lStructPos, 0, lStructStart) == focus ||
          ( externalPos(rStructArray, rStructPos) == 0 &&
            externalParent(rStructArray, rStructPos) == focus) ||
          ( ternary &&
            externalPos(mStructArray, mStructPos) == 0 &&
            externalParent(mStructArray, mStructPos) == focus))
    ) {
      // sibling features
      var closestRight = -1
      var cur = 1
      var prev = -1
      while (cur < split1.min(focus - 1)) {
        val parent = subParent(lStructArray, lStructPos, cur, lStructStart)
        if (parent == 0) {
          if (prev >= 0) {
            val tArg = subArg(lStructArray, lStructPos, cur, lStructStart)
            val arg = getPartsForPSG(tArg).last
            // We now have two siblings (prev, cur) with a parent (focus)
            score += higherOrderGet(words, wordFeatures, tags, 0, cur, prev, 1,
              arg, countOnGet)
          }
          if (closestRight < 0) closestRight = cur
          prev = cur
        }
        cur += 1
      }

      if (ternary && focus == split2) {
        cur = split1
        while (cur < split2) {
          val parent = subParent(rStructArray, rStructPos, cur, rStructStart)
          if (parent == 0) {
            if (prev >= 0) {
              val tArg = subArg(rStructArray, rStructPos, cur, rStructStart)
              val arg = getPartsForPSG(tArg).last
              // We now have two siblings (prev, cur) with a parent (focus)
              score += higherOrderGet(words, wordFeatures, tags, 0, cur, prev,
                1, arg, countOnGet)
            }
            if (closestRight < 0) closestRight = cur
            prev = cur
          }
          cur += 1
        }
      }

      // grandparent features
      val rawArg =
        if (subParent(lStructArray, lStructPos, 0, lStructStart) == focus)
          subArg(lStructArray, lStructPos, 0, lStructStart)
        else externalArg(rStructArray, rStructPos)
      val arg = getPartsForPSG(rawArg).last
      score += higherOrderGet(words, wordFeatures, tags, focus, 0, -1, 2, arg,
        countOnGet)
      score += higherOrderGet(words, wordFeatures, tags, focus, 0,
        closestRight, 3, arg, countOnGet)
    }


    // Bracket span features - tricky, as this bracket could extend beyond this
    // span (e.g. if the far right is a child of this, in which case that may
    // in turn have children that expand the bracket further that way). Also
    // unclear where and how these are defined in the CCK code. Finally, they
    // are made very hard by discontinuous structures.

    // Trace features:
    //  - Each trace arc paired with the structural arc for this edge

    score
  }

  // ======================================================
  // Common

  override def toString() = {
    val ans = ArrayBuffer(super.toString())
    ans.append(weights.toString())

    ans mkString "\n"
  }
}

