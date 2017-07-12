// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.util.Random

/** Data structures for storing a set of items that are to be kept sorted based
  * on a score. The number of items stored can either be set as a fixed value,
  * or all that are within some ratio of the best item.
  *
  * This interface is implemented in several ways, varying in insertion and
  * space efficiency.
  *
  * An id is used to allow multiple sub-beams, to ensure variation in the beam.
  *
  * NOTE: Assumes that subbeams will not be inserted into after being sorted.
  */
abstract class Beam(
  val minLength: Int,
  val maxLength: Int,
  val minMultiple: Double,
  val maxMultiple: Double,
  val minFraction: Double,
  val maxFraction: Double
) {
  // TODO: Consider, would returning the -ve of the position be more useful
  // than NOT_PRESENT for evicted items?
  val NOT_PRESENT = -1
  val INIT_MAP_SIZE = 1 << 5

  def idIterator : Iterator[Long]
  def hasID(id: Long) : Boolean
  // TODO: Implement for more than just ArrayBeam
  def numberOfIDs : Int = 0

  // insert or update (and confirm whether an insert occurred)
  // Note, forcedAdd is not forcedKeep, it will only ensure that the value is
  // returned as inserted (if not already present)
  def update(
    id: Long, key: Long, priority: Double, value: Int, forcedAdd: Boolean
  ) : (Boolean, Boolean, Int)

  def get(id: Long, key: Long) : Int
  def getData(id: Long) : ArrayBuffer[(Double, Long, Int)]
  def getMaxPos(id: Long) : Int = throw new Exception("Not implemented")

  def size : Int
  def size(id: Long) : Int
  def sizes : Iterable[(Long, Int)]
  def nBuckets = 0
  def stats(prefix: String) : String
  def hasStats : Boolean = false

  // iterate
  def prepareToIterate(ids: Long*) : Unit
  def hasNext : Boolean
  def next : Int // position

  def reset(hard: Boolean = false) : Unit

  // Calculate the cutoffs for a given max value
  def multipleCutoffs(maxVal: Double) = {
    if (maxVal.abs < 1e-3) {
      (
        ( if (minMultiple <= 1.0) Double.MaxValue
          else -minMultiple),
        ( if (maxMultiple < Double.MaxValue) -maxMultiple
          else Double.MinValue)
      )
    } else if (maxVal > 0) {
      (
        ( if (minMultiple <= 1.0) Double.MaxValue
          else maxVal / minMultiple),
        if (maxMultiple < Double.MaxValue) maxVal / maxMultiple
        else Double.MinValue
      )
    } else {
      (
        ( if (minMultiple <= 1.0) Double.MaxValue
          else maxVal * minMultiple),
        if (maxMultiple < Double.MaxValue) maxVal * maxMultiple
        else Double.MinValue
      )
    }
  }
  // Calculate whether this value meets the cutoff implied by the max value
  def checkMinMultiple(maxVal: Double, otherVal: Double) = {
    if (minMultiple <= 1.0) true
    else if (maxVal > 1e-2) otherVal * minMultiple >= maxVal
    else if (maxVal < -1e-2) otherVal >= maxVal * minMultiple
    else otherVal >= -minMultiple
  }
  def checkMaxMultiple(maxVal: Double, otherVal: Double) = {
    if (maxMultiple == Double.MaxValue) true
    else if (maxVal > 1e-2) otherVal * maxMultiple >= maxVal
    else if (maxVal < -1e-2) otherVal >= maxVal * maxMultiple
    else otherVal >= -maxMultiple
  }
}

/** Avoiding spreading data out in memory.
  * Simplest implementation - stores and updates all states, then sorts when
  * iteration begins.
  */
class OldBeam(
  minLength: Int,
  maxLength: Int,
  minMultiple: Double,
  maxMultiple: Double,
  minFraction: Double,
  maxFraction: Double
) extends Beam(
  minLength, maxLength, minMultiple, maxMultiple, minFraction, maxFraction
) {
  // First hashtable maps from beam ID to a second hashtable.
  // Second hashtable maps from key to position in the beam, this position will
  // be negative if a value is evicted.
  var maps = new HashMap[Long, LongIntMap]

  var dataArrays = new HashMap[Long, ArrayBuffer[(Double, Long, Int)]]

  def idIterator = maps.keys.iterator
  def hasID(id: Long) : Boolean = maps.contains(id)

  def addSubBeam(id: Long) = {
    maps(id) = new LongIntMap(0.5, INIT_MAP_SIZE, NOT_PRESENT)
    dataArrays(id) = ArrayBuffer[(Double, Long, Int)]()
  }

  override def update(
    id: Long, key: Long, priority: Double, insertValue: Int, gold: Boolean
  ) = {
    // TODO: At the moment 'gold' (meaning, definitely keep) is ignored.
    if (! maps.contains(id)) addSubBeam(id)
    val data = dataArrays(id)
    val map = maps(id)

    var didInsert = false
    var didUpdate = false
    var nValue = insertValue
    val cpos = map.get(key)
    if (cpos >= 0) {
      nValue = data(cpos)._3
      if (data(cpos)._1 < priority) {
        data(cpos) = (priority, key, nValue)
        didUpdate = true
      }
    } else {
      map.put(key, data.length)
      data.append((priority, key, nValue))
      didInsert = true
    }

    (didInsert, didUpdate, nValue)
  }

  override def get(id: Long, key: Long) =
    if (! maps.contains(id)) NOT_PRESENT
    else {
      val cpos = maps(id).get(key)
      if (cpos < 0) cpos
      else dataArrays(id)(cpos)._3
    }

  override def getData(id: Long) = {
    val ans = new ArrayBuffer[(Double, Long, Int)]
    prepareToIterate(id)
    while (hasNext) {
      next
      ans.append(carray(cpos))
    }
    ans
  }

  override def size = dataArrays.foldLeft(0)(_ + _._2.length)
  override def size(id: Long) = dataArrays.get(id).fold(0)(_.length)
  override def sizes = dataArrays.map{ case (k, v) => (k, v.length) }

  // iterate
  var cpos : Int = Int.MaxValue
  var cid : Long = Long.MaxValue
  var carray : ArrayBuffer[(Double, Long, Int)] = null
  var cutoffPosAnd : Int = Int.MaxValue
  var cutoffPosOr : Int = Int.MaxValue
  var cutoffValueAnd : Double = 0.0
  var cutoffValueOr : Double = 0.0
  var idIter : Iterator[Long] = null
  var subBeamDone : Boolean = false
  private def prepareNextSubBeam = {
    cid = idIter.next
    carray = dataArrays(cid)
    cpos = 0
    cutoffPosOr = ((carray.length * minFraction).ceil.toInt max minLength)  - 1
    cutoffPosAnd = ((carray.length * maxFraction).ceil.toInt min maxLength) - 1
    val maxVal = carray(0)._1
    if (maxVal.abs < 1e-2) {
      cutoffValueOr =
        if (minMultiple == 1.0) 0.0
        else -minMultiple
      cutoffValueAnd = -maxMultiple
    } else if (maxVal > 0) {
      cutoffValueOr = maxVal / minMultiple
      cutoffValueAnd =
        if (maxMultiple < Double.MaxValue) maxVal / maxMultiple
        else Double.MinValue
    } else {
      cutoffValueOr = maxVal * minMultiple
      cutoffValueAnd =
        if (maxMultiple < Double.MaxValue) maxVal * maxMultiple
        else Double.MinValue
    }
///    Log.logln(s"Config: ${carray.length} $maxVal $minFraction $maxFraction $minLength $maxLength $minMultiple $maxMultiple")
///    Log.logln(s"$cutoffPosOr or $cutoffValueOr or ($cutoffPosAnd and $cutoffValueAnd)")
///    Log.logln(s"Set up subbeam with ${carray.length}, uusing $cutoffPosOr or $cutoffValueOr or ($cutoffPosAnd and $cutoffValueAnd)")
    subBeamDone = false
  }
  override def prepareToIterate(ids: Long*) = {
    for ((id, data) <- dataArrays) {
      dataArrays(id) = data.sortWith(_._1 > _._1)
      for ((item, index) <- dataArrays(id).zipWithIndex)
        maps(id).put(item._2, index)
    }
    idIter =
      if (ids.length == 0) dataArrays.keysIterator.filter(dataArrays(_).length > 0)
      else ids.filter(dataArrays.contains).iterator.filter(dataArrays(_).length > 0)
    if (idIter.hasNext) {
      prepareNextSubBeam
      cpos = -1
    } else cpos = Int.MaxValue
  }
  override def hasNext = {
    subBeamDone =
      ! ( cpos < Int.MaxValue && cpos < (carray.length - 1) &&
          ( cpos < cutoffPosOr ||
            (cpos < 0 || carray(cpos)._1 > cutoffValueOr) ||
            (cpos < cutoffPosAnd && (cpos < 0 || carray(cpos)._1 > cutoffValueAnd))))

    (! subBeamDone) || idIter.hasNext
  }

  override def next = {
    cpos += 1
    if (subBeamDone) prepareNextSubBeam
    carray(cpos)._3
  }

  override def reset(hard: Boolean = false) = {
    if (hard) {
      maps = new HashMap[Long, LongIntMap]
      dataArrays = new HashMap[Long, ArrayBuffer[(Double, Long, Int)]]
    } else {
      for ((id, map) <- maps) map.clear
      for ((id, data) <- dataArrays) data.clear
    }
  }

  override def toString() = {
    val arrayStrings =
      for ((id, data) <- dataArrays)
        yield s"$id Data       $data\n"

    s"${maps.size} sub-beams\n"+
    maps.map{ case (id, map) => s"$id Map        $map" }.mkString("\n") +"\n"+
      arrayStrings.mkString("\n")
  }
  override def stats(prefix: String) = prefix + "not calculated"
}

/** Store all, then use a partial sorting to get the results fast.
  *  + Allows all three kinds of pruning
  *  + O(n + k log k)
  *  - Inserts everything into the chart
  *
  * The O(n) relies on qselect with a random pivot. Could also consider the
  * Floyd-Rivest algorithm.
  */
class ArrayBeam(
  minLength: Int,
  maxLength: Int,
  minMultiple: Double,
  maxMultiple: Double,
  minFraction: Double,
  maxFraction: Double
) extends Beam(
  minLength, maxLength, minMultiple, maxMultiple, minFraction, maxFraction
) {
  // First hashtable maps from beam ID to the components of the beam:
  //  - A mapping from keys to positions in the actual beam storage.
  //  - The array storing the beam.
  //  - A boolean indicating if it has been sorted.
  var maps = new HashMap[Long, (LongIntMap, ArrayBuffer[(Double, Long, Int)], Boolean)]
  var maxes = new LongDoubleMap(0.5, INIT_MAP_SIZE, -1)
  var maxPos = new LongIntMap(0.5, INIT_MAP_SIZE, -1)
  var ids = UnboxedArrayBufferLong()
  var idSet = new LongSet(0.5, INIT_MAP_SIZE)
  var firsts = new LongDoubleMap(0.5, INIT_MAP_SIZE, -1)

  def idIterator = maps.keys.iterator
  def hasID(id: Long) : Boolean = idSet.contains(id)
  override def numberOfIDs : Int = idSet.size

  override def getMaxPos(id: Long) : Int = maxPos.getOrElse(id, -1)

  def addSubBeam(id: Long) = {
    ids.append(id)
    idSet.add(id)
    maps(id) = (
      new LongIntMap(0.5, 8, NOT_PRESENT),
      ArrayBuffer[(Double, Long, Int)](),
      false
    )
    maxes.put(id, Double.MinValue)
    maxPos.put(id, -1)
  }

  private def swap(
    data: ArrayBuffer[(Double, Long, Int)], a: Int, b: Int
  ) = {
    val tmp = data(a)
    data(a) = data(b)
    data(b) = tmp
  }
  @tailrec private def qselect(
    data: ArrayBuffer[(Double, Long, Int)], start: Int, end: Int,
    multipleOr: Double, multipleAnd: Double, lengthOr: Int,
    lengthAnd: Int, rand: Random = new Random
  ) : Int = {
    // Select a random pivot and place it at the end
    val pivot = rand.nextInt(end - start) + start
    val pivotScore = data(pivot)._1
    swap(data, pivot, end - 1)

    // Go through the array and place items after the current split if they are
    // smaller than the pivot
    var i = start
    var split = start
    while (i < end - 1) {
      if (data(i)._1 >= pivotScore) {
        swap(data, i, split)
        split += 1
      }
      i += 1
    }
    swap(data, end - 1, split)

    // Detrmine recursion
///    val pivotAllowed =
///      (split < lengthOr ||
///       pivotScore > multipleOr ||
///       (split < lengthAnd && pivotScore > multipleAnd))
    val pivotAllowed = split < lengthAnd

    if (pivotAllowed) {
      if (end - split <= 1) end
      else qselect(data, split + 1, end, multipleOr, multipleAnd, lengthOr,
        lengthAnd, rand)
    } else {
      if (split - start <= 1) split
      else qselect(data, start, split, multipleOr, multipleAnd, lengthOr,
        lengthAnd, rand)
    }
  }
  def compactArray(
    id: Long, data: ArrayBuffer[(Double, Long, Int)], map: LongIntMap,
    max: Double
  ) = {
///    // Determine cutoffs for this array.
///    val (multipleOr, multipleAnd) = multipleCutoffs(max)
///    val lengthOr = ((data.length * minFraction).ceil.toInt max minLength) min data.length
///    val lengthAnd = (data.length * maxFraction).ceil.toInt min maxLength

///    // Use quickselect to find the top K defined by the cutoffs, and in the
///    // process, partition the list.
///    var split =
///      if (data.length < lengthOr ||
///          (data.length < lengthAnd && multipleAnd == Double.MinValue)) {
///        // Testing multiples would require extracting the min element,
///        // since we rarely use them, it's not worth tracking
///        data.length - 1
///      } else {
///        qselect(data, 0, data.length, multipleOr, multipleAnd,
///          lengthOr, lengthAnd)
///      }

///    // Remove those after split
///    var index = split
///    while (index < data.length) {
///      map.put(data(index)._2, NOT_PRESENT)
///      index += 1
///    }

///    // Sort the rest
///    val ndata = data.slice(0, split).sortWith(_._1 > _._1)
///    index = 0
///    while (index < ndata.length) {
///      map.put(ndata(index)._2, index)
///      index += 1
///    }

///    maps(id) = (map, ndata, true)
///    ndata

    // Use quickselect to find the top K defined by the cutoffs, and in the
    // process, partition the list.
    val split =
      if (data.length <= maxLength) {
        data.length
      } else {
        qselect(data, 0, data.length, Double.MaxValue, Double.MinValue, 0,
          data.length.min(maxLength))
      }

    // Remove those after split
    var index = split
    while (index < data.length) {
      map.put(data(index)._2, NOT_PRESENT)
      index += 1
    }

    // Sort the rest
    val ndata = data.slice(0, split).sortWith(_._1 > _._1)
    index = 0
    while (index < ndata.length) {
      map.put(ndata(index)._2, index)
      index += 1
    }

    maps(id) = (map, ndata, true)
    ndata
  }

  override def update(
    id: Long, key: Long, priority: Double, insertValue: Int, forcedAdd: Boolean
  ) = {
    if (! idSet.contains(id)) addSubBeam(id)
    val (map, data, sorted) = maps(id)
    val max = maxes.get(id)

    // Tracking the value of the first item inserted purely for information
    // purposes (this was introduced when doing experiments in preparation for
    // creating the RadixBeam)
    if (! firsts.contains(id)) firsts.put(id, priority)

    var didInsert = false
    var didUpdate = false
    var nValue = insertValue
    val cpos = map.get(key)
    if (cpos >= 0) {
      // Already present
      nValue = data(cpos)._3
      if (!sorted && data(cpos)._1 < priority) {
        data(cpos) = (priority, key, nValue)
        didUpdate = true
      }
    } else {
      // Not present yet
      val add = forcedAdd || checkMaxMultiple(max, priority) || sorted
      if (! add) nValue = NOT_PRESENT
      else {
        map.put(key, data.length)
        data.append( (priority, key, nValue) )
        didInsert = true
      }
    }

    // Update the max for this subbeam
    if (priority > max) {
      maxes.put(id, priority)
      maxPos.put(id, nValue)
    }

    (didInsert, didUpdate, nValue)
  }

  override def get(id: Long, key: Long) =
    if (! idSet.contains(id)) NOT_PRESENT
    else {
      val (map, data, _) = maps(id)
      val cpos = map.get(key)
      if (cpos < 0) cpos
      else data(cpos)._3
    }

  override def size = maps.foldLeft(0)(_ + _._2._2.length)
  override def size(id: Long) = maps.get(id).fold(0)(_._2.length)
  override def sizes = maps.map{ case (k, v) => (k, v._2.length) }
  override def nBuckets = maps.size
  val statLines = ArrayBuffer[String]()
  private def genStats(first: Double, max: Double, data: ArrayBuffer[(Double, Long, Int)]) = {
    var min = Double.MaxValue
    for ((score, _, _) <- data) if (min > score) min = score
    statLines.append(s"$min $max $first ${data.length}")
  }
  override def stats(prefix: String) =
    statLines.map(prefix + _).mkString("\n")
  override def hasStats = statLines.length > 0

  // iterate
  var nextIndexes : UnboxedArrayBuffer = null
  var arrays : ArrayBuffer[ArrayBuffer[(Double, Long, Int)]] = null
  var nextArray : Int = -1
  var cVal : (Double, Long, Int) = null
  private def prepareArray(id: Long) = {
    val (map, data, sorted) = maps(id)

    if (data.length > 0) {
      if (sorted) {
        nextIndexes.append(0)
        arrays.append(data)
      } else {
///        if (Log.check(Log.VBeamStats)) genStats(firsts.get(id), max, data)
        val ndata = compactArray(id, data, map, maxes.get(id))
        if (ndata.length > 0) {
          nextIndexes.append(0)
          arrays.append(ndata)
        }
      }
    }
  }
  // TODO: The current iteration approach is O( n * |ids| ), which is good if
  // |ids| < log(n), but bad otherwise (and for the arc passes it often will be
  // much bigger).
  // Could have both implemented and choose based on |ids|
  // Also looks like this doesn't have a big impact at the moment (all the time
  // is spent in binary composition, which doesn't use this).
  override def prepareToIterate(idsTodo: Long*) = {
    nextIndexes = new UnboxedArrayBuffer(8)
    arrays = new ArrayBuffer[ArrayBuffer[(Double, Long, Int)]]
    nextArray = -1
    cVal = null
    if (idsTodo.length != 0) {
      for (id <- idsTodo) if (idSet.contains(id)) prepareArray(id)
    } else {
      ids.prepareToIterate
      while (ids.hasNext) prepareArray(ids.next)
    }
  }
  override def hasNext = {
    nextArray = -1
    var bestScore = Double.MinValue
    var i = 0
    while (i < nextIndexes.length) {
      val index = nextIndexes(i)
      if (index >= 0) {
        val array = arrays(i)
        val score = array(index)._1
        if (bestScore <= score) {
          bestScore = score
          nextArray = i
        }
      }
      i += 1
    }

    nextArray >= 0
  }

  override def next = {
    val index = nextIndexes(nextArray)
    cVal = arrays(nextArray)(index)
    if (index == arrays(nextArray).length - 1) nextIndexes(nextArray) = -1
    else nextIndexes(nextArray) += 1
    cVal._3
  }

  override def reset(hard: Boolean = false) = {
    require(false, "Requires updating for id storage")
    if (hard) {
      maps = new HashMap[Long, (LongIntMap, ArrayBuffer[(Double, Long, Int)], Boolean)]
    } else {
      maps.clear
    }
  }

  override def getData(id: Long) = {
    if (! idSet.contains(id)) new ArrayBuffer[(Double, Long, Int)]
    else {
      val info = maps(id)
      if (info._3 || info._2.length == 0) info._2
      else compactArray(id, info._2, info._1, maxes.get(id))
    }
  }

  override def toString() = {
    s"${maps.size} sub-beams\n"+
    maps.map{ case (id, info) => s"$id Subbeam    $info" }.mkString("\n") +"\n"
  }
}


/** After 2k, sort, reduce, and define a cutoff.
  */
class CompactingBeam(
  minLength: Int,
  maxLength: Int,
  minMultiple: Double,
  maxMultiple: Double,
  minFraction: Double,
  maxFraction: Double
) extends ArrayBeam(
  minLength, maxLength, minMultiple, maxMultiple, minFraction, maxFraction
) {
  var lowerBounds = new LongDoubleMap(0.5, INIT_MAP_SIZE, -1)

  override def addSubBeam(id: Long) = {
    super.addSubBeam(id)
    lowerBounds.put(id, Double.MinValue)
  }

  override def update(
    id: Long, key: Long, priority: Double, insertValue: Int, forcedAdd: Boolean
  ) = {
    if (! maps.contains(id)) addSubBeam(id)
    val (map, data, sorted) = maps(id)
    val max = maxes.get(id)
    val min = lowerBounds.get(id)

    if (! firsts.contains(id)) firsts.put(id, priority)

    var didInsert = false
    var didUpdate = false
    var nValue = insertValue
    val cpos = map.get(key)
    if (cpos >= 0) {
      // Already present
      nValue = data(cpos)._3
      if (!sorted && data(cpos)._1 < priority) {
        data(cpos) = (priority, key, nValue)
        didUpdate = true
      }
    } else {
      // Not present yet
      val add = forcedAdd || sorted || (checkMaxMultiple(max, priority) && priority > min)
      if (! add) nValue = NOT_PRESENT
      else {
        map.put(key, data.length)
        data.append( (priority, key, nValue) )
        didInsert = true
      }
    }
    if (priority > max) maxes.put(id, priority)

    if (data.length > maxLength * 2) {
      val ndata = compactArray(id, data, map, max)
      val lowest = ndata.last._1
      lowerBounds.put(id, lowest)
    }

    (didInsert, didUpdate, nValue)
  }
}

/** Adjusts the above approach to have the arrays be heaps, maintained while
  * running.  Does not support min/max fraction, and has more costly
  * insertions, but allows online pruning.
  */
class HeapBeam(
  minLength: Int,
  maxLength: Int,
  minMultiple: Double,
  maxMultiple: Double
) extends ArrayBeam(minLength, maxLength, minMultiple, maxMultiple, 0.0, 1.0) {
  // TODO: Consider a scheme where the mapping from the heap back into the
  // hashtable is maintained, that way we don't have to recompute the hash
  // every time we move something.
  @tailrec private def swapDown(
    heap: ArrayBuffer[(Double, Long, Int)], map: LongIntMap, cpos: Int,
    item: (Double, Long, Int)
  ) : Unit = {
    val nposL = cpos * 2 + 1
    val nposR = cpos * 2 + 2
    val npos =
      if (nposL < heap.length && item._1 > heap(nposL)._1) {
        if (heap.length == nposR || heap(nposL)._1 < heap(nposR)._1) nposL
        else nposR
      } else if (nposR < heap.length && item._1 > heap(nposR)._1) nposR
      else cpos
    if (npos == cpos) {
      heap(cpos) = item
      map.put(item._2, cpos)
    } else {
      val oitem = heap(npos)
      heap(cpos) = oitem
      map.put(oitem._2, cpos)
      swapDown(heap, map, npos, item)
    }
  }
  @tailrec private def swapUp(
    heap: ArrayBuffer[(Double, Long, Int)], map: LongIntMap, cpos: Int,
    item: (Double, Long, Int)
  ) : Unit = {
    val npos = (cpos - 1) / 2
    if (cpos == 0 || heap(npos)._1 < item._1) {
      heap(cpos) = item
      map.put(item._2, cpos)
    } else {
      val oitem = heap(npos)
      heap(cpos) = oitem
      map.put(oitem._2, cpos)
      swapUp(heap, map, npos, item)
    }
  }
  private def peek(heap: ArrayBuffer[(Double, Long, Int)]) : Double = heap(0)._1

  override def update(
    id: Long, key: Long, priority: Double, insertValue: Int, forcedAdd: Boolean
  ) = {
    if (! maps.contains(id)) addSubBeam(id)
    val (map, data, sorted) = maps(id)
    var max = maxes.get(id)

    var didInsert = false
    var didUpdate = false
    var nValue = insertValue
    val cpos = map.get(key)
    if (cpos >= 0) {
      // Already present
      nValue = data(cpos)._3
      if (!sorted && data(cpos)._1 < priority) {
        data(cpos) = (priority, key, nValue)
        swapDown(data, map, cpos, data(cpos))
        didUpdate = true
      }
    } else {
      // Not present yet
      val add =
        if (forcedAdd || data.length < minLength) true
        else {
          if (checkMinMultiple(max, priority)) true
          else if (data.length < maxLength && checkMaxMultiple(max, priority)) true
          else if (!sorted) peek(data) < priority
          else true
        }
      if (! add) nValue = NOT_PRESENT
      else {
        val nitem = (priority, key, nValue)
        map.put(key, data.length)
        data.append(nitem)
        if (!sorted) swapUp(data, map, data.length - 1, nitem)
        didInsert = true
      }
    }

    // Update max
    if (priority > max) {
      max = priority
      maxes.put(id, priority)
    }
    // Remove items
    if (!sorted) {
      val cutoff = multipleCutoffs(max)._2
      while (data.length > maxLength || (data.length > 1 && peek(data) < cutoff)) {
        map.put(data(0)._2, NOT_PRESENT)
        // TODO: This is linear in |data|, a hacky way to handle it would be
        // to track lengths myself and insert nulls when removing items
        data(0) = data.remove(data.length - 1)
        swapDown(data, map, 0, data(0))
      }
    }

    (didInsert, didUpdate, nValue)
  }
}

/** Similar to the array but with some online pruning possible, and fewer
  * guarantees. Track across sentences what the range of values on our beams
  * are, then have a radix-sort like set up, where items are inserted into
  * buckets within that range. This effectively sorts values, as long as there
  * isn't significant clumping.
  *  + Not inserting as much, so smaller charts
  *  + O(1) maybe...
  *
  * TODO: Can we use the second insertion productively? (e.g. to determine the range)
  */
class RadixBeam(
  minLength: Int,
  maxLength: Int,
  minMultiple: Double,
  maxMultiple: Double,
  minFraction: Double,
  maxFraction: Double
) extends Beam(
  minLength, maxLength, minMultiple, maxMultiple, minFraction, maxFraction
) {
  // First hashtable maps from beam ID to the components of the beam:
  //  - A mapping from keys to positions in the actual beam storage.
  //  - The array storing the beam.
  //  - A boolean indicating if it has been sorted.
  var maps = new HashMap[Long, (LongLongMap, ArrayBuffer[ArrayBuffer[(Double, Long, Int)]], ArrayBuffer[Int], Boolean)]
  var maxes = new LongDoubleMap(0.5, INIT_MAP_SIZE, -1)
  var ids = UnboxedArrayBufferLong()
  var idSet = new LongSet(0.5, INIT_MAP_SIZE)
  // Buckets are defined as:
  // ___ f+w*((b/2)-1) ... ___ f+w ___ f ___ f-w ___ ... f-w*((b/2)-1) ___
  var firsts = new LongDoubleMap(0.5, INIT_MAP_SIZE, -1)
///  val buckets = Config.beamBuckets + (if (Config.beamBuckets % 2 == 1) 1 else 0)
///  val range = Config.beamRange
  val buckets = 20 + (if (20 % 2 == 1) 1 else 0)
  val range = 20.0
  val bucketWidth = range / buckets

  def idIterator = maps.keys.iterator
  def hasID(id: Long) : Boolean = idSet.contains(id)

  def addSubBeam(id: Long) = {
    ids.append(id)
    idSet.add(id)
    val array = new ArrayBuffer[ArrayBuffer[(Double, Long, Int)]]
    val nulls = new ArrayBuffer[Int]
    for (i <- 0 until buckets) {
      array.append(new ArrayBuffer[(Double, Long, Int)])
      nulls.append(0)
    }
    maps(id) = (
      new LongLongMap(0.5, INIT_MAP_SIZE, NOT_PRESENT),
      array,
      nulls,
      false
    )
    maxes.put(id, Double.MinValue)
  }

  def priorityToBucket(first: Double, priority: Double) = {
    val diff = first - priority
    val bucket : Int = (diff / bucketWidth).floor.toInt + (buckets / 2)
    ((bucket max 0) min (buckets - 1))
  }
  def bucketToMaxPriority(first: Double, bucket: Int) =
    if (bucket == 0) Double.MaxValue
    else ((buckets / 2) - bucket) * bucketWidth + first
  def bucketToMinPriority(first: Double, bucket: Int) =
    if (bucket == buckets - 1) Double.MinValue
    else ((buckets / 2) - bucket - 1) * bucketWidth + first

  override def update(
    id: Long, key: Long, priority: Double, insertValue: Int,
    forcedAdd: Boolean
  ) = {
    if (! idSet.contains(id)) {
      addSubBeam(id)
      firsts.put(id, priority)
    }
    val (map, data, nulls, sorted) = maps(id)
    val max = maxes.get(id)
    val first = firsts.get(id)

    var didInsert = false
    var didUpdate = false
    var nValue = insertValue
    val cpos = map.getPair(key)
    if (cpos._1 >= 0) {
      // Already present
      assert(cpos._2 < data(cpos._1).length, s"$id $key $priority $insertValue $cpos")
      val item = data(cpos._1)(cpos._2)
      nValue = item._3
      if (!sorted && item._1 < priority) {
        val npos = priorityToBucket(first, priority)
        if (npos != cpos._1) {
          data(cpos._1)(cpos._2) = null
          assert(! sorted)
          nulls(cpos._1) += 1
          map.put(key, npos, data(npos).length)
          data(npos).append((priority, key, nValue))
        } else {
          data(cpos._1)(cpos._2) = (priority, key, nValue)
        }
        didUpdate = true
      }
    } else {
      // Not present yet
      val add = forcedAdd || checkMaxMultiple(max, priority)
      // TODO: Check if the position is already below threshold for top-k
      // Though that means no top % is possible
      if (! add) nValue = NOT_PRESENT
      else {
        val npos = priorityToBucket(first, priority)
        map.put(key, npos, data(npos).length)
        data(npos).append((priority, key, nValue))
        didInsert = true
      }
    }
    if (priority > max) maxes.put(id, priority)

    (didInsert, didUpdate, nValue)
  }

  override def get(id: Long, key: Long) =
    if (! idSet.contains(id)) NOT_PRESENT
    else {
      val (map, data, _, _) = maps(id)
      val cpos = map.getPair(key)
      if (cpos._1 < 0) cpos._1
      else data(cpos._1)(cpos._2)._3
    }

  override def size = maps.foldLeft(0){
    _ + _._2._2.foldLeft(0)(_ + _.length)
  }
  override def size(id: Long) = maps.get(id).fold(0){
    _._2.foldLeft(0)(_ + _.length)
  }
  override def sizes = maps.map{ case (k, v) =>
    (k, v._2.foldLeft(0)(_ + _.length))
  }
  val statLines = ArrayBuffer[String]()
  override def stats(prefix: String) =
    statLines.map(prefix + _).mkString("\n")
  override def hasStats = statLines.length > 0

  // iterate
  // next indexes is a list of items, one per array in arrays, indicating the
  // index we are up to in that array
  var nextIndexes : UnboxedArrayBuffer = null
  var arrays : ArrayBuffer[ArrayBuffer[(Double, Long, Int)]] = null
  var nextArray : Int = -1
  var cVal : (Double, Long, Int) = null
  private def swap(
    data: ArrayBuffer[(Double, Long, Int)], a: Int, b: Int
  ) = {
    val tmp = data(a)
    data(a) = data(b)
    data(b) = tmp
  }
  @tailrec private def qselect(
    data: ArrayBuffer[(Double, Long, Int)], start: Int, end: Int,
    multipleOr: Double, multipleAnd: Double, lengthOr: Int,
    lengthAnd: Int, rand: Random = new Random
  ) : Int = {
    // Select a random pivot and place it at the end
    var pivot = rand.nextInt(end - start) + start
    var allNull = (pivot, false)
    while (data(pivot) == null && !allNull._2) {
      pivot += 1
      if (pivot == end) pivot = start
      if (pivot == allNull._1) allNull = (pivot, true)
    }
    if (allNull._2) start
    else {
      val pivotScore = data(pivot)._1
      swap(data, pivot, end - 1)

      // Go through the array and place items after the current split if
      // they are smaller than the pivot
      var i = start
      var split = start
      while (i < end - 1) {
        if (data(i) != null && data(i)._1 > pivotScore) {
          swap(data, i, split)
          split += 1
        }
        i += 1
      }
      swap(data, end - 1, split)

      // Detrmine recursion
      val pivotAllowed =
        (split < lengthOr ||
         pivotScore > multipleOr ||
         (split < lengthAnd && pivotScore > multipleAnd))

      if (pivotAllowed) {
        if (end - split <= 1) end
        else qselect(data, split + 1, end, multipleOr, multipleAnd, lengthOr,
          lengthAnd, rand)
      } else {
        if (split - start <= 1) split
        else qselect(data, start, split, multipleOr, multipleAnd, lengthOr,
          lengthAnd, rand)
      }
    }
  }
  private def prepareArray(id: Long) = {
    val (map, data, nulls, sorted) = maps(id)
    val max = maxes.get(id)
    val first = firsts.get(id)

    if (sorted && data(0).length > 0) {
      nextIndexes.append(0)
      for (item <- data(0)) assert(item != null)
      arrays.append(data(0))
    } else {
      var totalLength = 0
      var i = 0
      while (i < data.length) {
        val nonNull = data(i).length - nulls(i)
        totalLength += nonNull
        i += 1
      }
      // Determine cutoffs for this array.
      val (multipleOr, multipleAnd) = multipleCutoffs(max)
      val lengthOr = ((totalLength * minFraction).ceil.toInt max minLength) min totalLength
      val lengthAnd = (totalLength * maxFraction).ceil.toInt min maxLength

      // Use quickselect to find the top K defined by the cutoffs, and in
      // the process, partition the list.
      i = 0
      val ndata = new ArrayBuffer[(Double, Long, Int)]
      var done = false
      while (i < data.length) {
        val array = data(i)
        if (done) {
          // Remove those after split
          var index = 0
          while (index < array.length) {
            val item = array(index)
            if (item != null) {
              map.put(item._2, NOT_PRESENT)
            }
            index += 1
          }
        } else {
          val nLength = ndata.length + array.length - nulls(i)
          val minValue = bucketToMinPriority(first, i)
          if (nLength < lengthOr ||
              minValue > multipleOr ||
              (nLength < lengthAnd && minValue > multipleAnd)) {
            // Everything in this bucket is fine, sort them and add them to ndata
            val tdata = array.sortWith{ (a, b) =>
              if (a == null && b == null) false
              else if (a == null) false
              else if (b == null) true
              else a._1 > b._1
            }
            var index = 0
            while (index < tdata.length) {
              val item = tdata(index)
              if (item != null) {
                map.put(item._2, 0, ndata.length)
                ndata.append(item)
              }
              index += 1
            }
          } else {
            // This bucket fails one of our tests, and so contains the bottom
            // of our list.
            val split = qselect(array, 0, array.length, multipleOr,
              multipleAnd, lengthOr - ndata.length, lengthAnd - ndata.length)

            // Remove those after split
            var index = split + 1
            while (index < array.length) {
              val item = array(index)
              if (item != null) {
                map.put(item._2, NOT_PRESENT)
              }
              index += 1
            }

            // Sort the rest
            val tdata = array.slice(0, split + 1).sortWith(_._1 > _._1)
            index = 0
            while (index < tdata.length) {
              val item = tdata(index)
              assert(item != null)
              map.put(item._2, 0, ndata.length)
              ndata.append(item)
              index += 1
            }

            done = true
          }
        }
        i += 1
      }

      data(0) = ndata
      maps(id) = (map, data, nulls, true)
      if (ndata.length > 0) {
        nextIndexes.append(0)
        for (item <- data(0)) assert(item != null)
        arrays.append(ndata)
      }
    }
  }
  override def prepareToIterate(idsTodo: Long*) = {
    nextIndexes = UnboxedArrayBuffer()
    arrays = ArrayBuffer[ArrayBuffer[(Double, Long, Int)]]()
    nextArray = -1
    cVal = null
    if (idsTodo.length != 0) {
      for (id <- idsTodo) if (idSet.contains(id)) prepareArray(id)
    } else {
      ids.prepareToIterate
      while (ids.hasNext) prepareArray(ids.next)
    }
  }
  override def hasNext = {
    nextArray = -1
    var bestScore = Double.MinValue
    var i = 0
    while (i < nextIndexes.length) {
      val index = nextIndexes(i)
      if (index >= 0) {
        val score = arrays(i)(index)._1
        if (bestScore <= score) {
          bestScore = score
          nextArray = i
        }
      }
      i += 1
    }

    nextArray >= 0
  }

  override def next = {
    val index = nextIndexes(nextArray)
    cVal = arrays(nextArray)(index)
    if (index == arrays(nextArray).length - 1) nextIndexes(nextArray) = -1
    else nextIndexes(nextArray) += 1
    cVal._3
  }

  override def reset(hard: Boolean = false) = {
    require(false, "Needs updating for new id storage")
    if (hard) {
      maps = new HashMap[Long, (LongLongMap, ArrayBuffer[ArrayBuffer[(Double, Long, Int)]], ArrayBuffer[Int], Boolean)]
    } else {
      maps.clear
    }
  }

  override def getData(id: Long) = {
    val ans = ArrayBuffer[(Double, Long, Int)]()
    prepareToIterate(id)
    while (hasNext) {
      next
      ans.append(cVal)
    }
    ans
  }

  override def toString() = {
    s"${maps.size} sub-beams\n"+
    maps.map{ case (id, info) => s"$id Subbeam    $info" }.mkString("\n") +"\n"
  }
}

class PruningCube(
  val leftBeam: Beam,
  val middleBeam: Beam,
  val rightBeam: Beam,
  val leftID: Long,
  val middleID: Long,
  val rightID: Long,
  val span: (Int, Int),
  val split1: Int,
  val split2: Int,
  val chart: Chart
) {
  val leftItems = leftBeam.getData(leftID)
  val middleItems =
    if (middleBeam != null) middleBeam.getData(middleID)
    else null
  val rightItems = rightBeam.getData(rightID)
  val left = UnboxedArrayBuffer()
  val middle =
    if (middleItems != null) UnboxedArrayBuffer()
    else null
  val right = UnboxedArrayBuffer()
  val priorities = UnboxedArrayBufferDouble()
  // Using a longset means we assume we won't go through more than 2^21 in any
  // one beam
  val added = new LongSet(Config.loadFactor, 16)
  if (leftItems.length > 0 && rightItems.length > 0) {
    if (middleItems == null) push(0, -1, 0)
    else if (middleItems.length > 0) push(0, 0, 0)
  }

  @tailrec private def swapUp(
    cpos: Int, lItemPos: Int, mItemPos: Int, rItemPos: Int, priority: Double
  ) : Unit = {
    val npos = (cpos - 1) / 2
    if (cpos == 0 || priorities(npos) > priority) {
      priorities(cpos) = priority
      left(cpos) = lItemPos
      if (middle != null) middle(cpos) = mItemPos
      right(cpos) = rItemPos
    } else {
      priorities(cpos) = priorities(npos)
      left(cpos) = left(npos)
      if (middle != null) middle(cpos) = middle(npos)
      right(cpos) = right(npos)
      swapUp(npos, lItemPos, mItemPos, rItemPos, priority)
    }
  }

  @tailrec private def swapDown(
    cpos: Int, lItemPos: Int, mItemPos: Int, rItemPos: Int, priority: Double
  ) : Unit = {
    val nposL = cpos * 2 + 1
    val nposR = cpos * 2 + 2
    val npos =
      if (nposL < priorities.length && priority < priorities(nposL)) {
        if (priorities.length == nposR ||
            priorities(nposL) > priorities(nposR)) nposL
        else nposR
      } else if (nposR < priorities.length &&
          priority < priorities(nposR)) nposR
      else cpos
    if (npos == cpos) {
      priorities(cpos) = priority
      left(cpos) = lItemPos
      if (middle != null) middle(cpos) = mItemPos
      right(cpos) = rItemPos
    } else {
      priorities(cpos) = priorities(npos)
      left(cpos) = left(npos)
      if (middle != null) middle(cpos) = middle(npos)
      right(cpos) = right(npos)
      swapDown(npos, lItemPos, mItemPos, rItemPos, priority)
    }
  }

  private def push(
    lItemPos: Int, mItemPos: Int, rItemPos: Int, fromTop: Boolean = false
  ) = {
    added.add(lItemPos, mItemPos, rItemPos)
    val lItem = leftItems(lItemPos)._3
    val mItem = if (middle == null) -1 else middleItems(mItemPos)._3
    val rItem = rightItems(rItemPos)._3
    val priority = chart.combiner(span._1, span._2, split1, split2, lItem,
      mItem, rItem)
    if (fromTop) {
      swapDown(0, lItemPos, mItemPos, rItemPos, priority)
    } else {
      left.append(lItemPos)
      if (middle != null) middle.append(mItemPos)
      right.append(rItemPos)
      priorities.append(priority)
      swapUp(priorities.length - 1, lItemPos, mItemPos, rItemPos, priority)
    }
  }

  def hasNext : Boolean = priorities.length > 0

  def priorityPeek : Double = priorities(0)

  def next : (Int, Int, Int, Double, Int, Int) = {
    val lItemPos = left(0)
    val mItemPos = if (middle != null) middle(0) else -1
    val rItemPos = right(0)
    val lItem = leftItems(lItemPos)._3
    val mItem = if (middle != null) middleItems(mItemPos)._3 else -1
    val rItem = rightItems(rItemPos)._3
    val ans = (lItem, mItem, rItem, priorities(0), split1, split2)

    val extendLeft =
      (lItemPos < leftItems.length - 1) &&
      !added.contains(lItemPos + 1, mItemPos, rItemPos)
    val extendMiddle =
      middle != null &&
      (mItemPos < middleItems.length - 1) &&
      !added.contains(lItemPos, mItemPos + 1, rItemPos)
    val extendRight =
      (rItemPos < rightItems.length - 1) &&
      !added.contains(lItemPos, mItemPos, rItemPos + 1)

    // Extend, adding the first item from the top
    if (extendLeft) push(lItemPos + 1, mItemPos, rItemPos, true)
    if (extendMiddle) push(lItemPos, mItemPos + 1, rItemPos, !extendLeft)
    if (extendRight)
      push(lItemPos, mItemPos, rItemPos + 1, ! (extendLeft || extendMiddle))

    // If nothing was extended, fill in the gap at the top
    if (! (extendLeft || extendMiddle || extendRight)) {
      val l = left.removeLast
      val m = if (middle != null) middle.removeLast else -1
      val r = right.removeLast
      val p = priorities.removeLast
      if (priorities.length > 0) swapDown(0, l, m, r, p)
    }
    ans
  }

  override def toString() =
    s"Done $added, $leftID $middleID $rightID $span $split1 $split2\n"+
    s"Left        $left\n"+
    s"Middle      $middle\n"+
    s"Right       $right\n"+
    s"Priorities  $priorities\n"+
    s"$leftItems + $middleItems + $rightItems"

  def toShortString() =
    s"Done $added, $leftID $middleID $rightID $span $split1 $split2   "+
    s"[$left]  "+
    s"[$middle]  "+
    s"[$right]"
}

/** Use:
  *  - Add each pruning cube
  *  - Run prepare
  *  - get next while hasNext
  */
class MetaPruningCube(
  val cubeDepth: Int = (Int.MaxValue >> 2)
) {
  var beams = new ArrayBuffer[(Double, PruningCube)] // A heap of beams
  var beamsLength = 0
  var minPriority = Double.MinValue

  @tailrec private def swapDown(
    cpos: Int, cube: PruningCube, priority: Double
  ) : Unit = {
    val nposL = cpos * 2 + 1
    val nposR = cpos * 2 + 2
    val npos =
      if (nposL < beamsLength && priority < beams(nposL)._1) {
        if (beamsLength == nposR || beams(nposL)._1 > beams(nposR)._1) nposL
        else nposR
      } else if (nposR < beamsLength && priority < beams(nposR)._1) nposR
      else cpos
    if (npos == cpos) {
      beams(npos) = (priority, cube)
    } else {
      beams(cpos) = beams(npos)
      swapDown(npos, cube, priority)
    }
  }

  def addBeamPair(
    beamLeft: Beam, beamMiddle: Beam, beamRight: Beam, idLeft: Long,
    idMiddle: Long, idRight: Long, span: (Int, Int), split1: Int, split2: Int,
    chart: Chart
  ) = {
    // Get the max value from each array
    val lItem = beamLeft.getMaxPos(idLeft)
    val mItem =
      if (beamMiddle != null) beamMiddle.getMaxPos(idMiddle)
      else -1
    val rItem = beamRight.getMaxPos(idRight)

    if (lItem < 0 || rItem < 0 || (beamMiddle != null && mItem < 0)) {
      // One of the beams is empty
    } else {
      // Get the first priority
      val priority = chart.combiner(span._1, span._2, split1, split2, lItem,
        mItem, rItem)

      // If good enough, construct the cube and add it
      if (priority > minPriority) {
        val cube = new PruningCube(beamLeft, beamMiddle, beamRight, idLeft,
          idMiddle, idRight, span, split1, split2, chart)
        beams.append((priority, cube))
        if (beams.length > cubeDepth && minPriority == Double.MinValue) {
          prepare
          minPriority = beams.last._1
        } else if (beams.length > cubeDepth * 2) {
          prepare
          beams = beams.slice(0, cubeDepth)
          minPriority = beams.last._1
        }
      }
    }
  }

  def prepare = {
    beams = beams.sortWith( _._1 > _._1 )
    beamsLength = beams.length
  }

  def hasNext = beamsLength > 0

  // These are for debugging
  var tmpIDs = (-1L, -1L, -1L)
  def next = {
    tmpIDs = (beams(0)._2.leftID, beams(0)._2.middleID, beams(0)._2.rightID)
    val ans = beams(0)._2.next
    if (beams(0)._2.hasNext) {
      // Update the position of the beam based on the priority of its next item
      swapDown(0, beams(0)._2, beams(0)._2.priorityPeek)
    } else if (beamsLength > 1) {
      // The beam is exhausted, move the last beam to the top and swap it down
      beamsLength -= 1
      val (priority, beam) = beams(beamsLength)
      swapDown(0, beam, priority)
    } else {
      // All of the beams are exhausted
      beamsLength -= 1
    }
///    Log.logln(s"New beam states:")
///    for ((priority, cube) <- beams)
///      Log.logln("    "+ cube.toShortString)
    ans
  }

  override def toString() = {
    beams.map{ case (p, s) => s"$p $s" }.mkString("\n")
  }
}

object Beam {
  def apply(
    minLength: Int, maxLength: Int, minMultiple: Double, maxMultiple: Double,
    minFraction: Double, maxFraction: Double
  ) = new ArrayBeam(minLength, maxLength, minMultiple, maxMultiple,
    minFraction, maxFraction)
}

object OldBeam {
  def apply(
    minLength: Int, maxLength: Int, minMultiple: Double, maxMultiple: Double,
    minFraction: Double, maxFraction: Double
  ) = new OldBeam(minLength, maxLength, minMultiple, maxMultiple, minFraction,
    maxFraction)
}

object ArrayBeam {
  def apply(
    minLength: Int, maxLength: Int, minMultiple: Double, maxMultiple: Double,
    minFraction: Double, maxFraction: Double
  ) = new ArrayBeam(minLength, maxLength, minMultiple, maxMultiple,
    minFraction, maxFraction)
}

object CompactingBeam {
  def apply(
    minLength: Int, maxLength: Int, minMultiple: Double, maxMultiple: Double,
    minFraction: Double, maxFraction: Double
  ) = new CompactingBeam(minLength, maxLength, minMultiple, maxMultiple,
    minFraction, maxFraction)
}

object HeapBeam {
  def apply(
    minLength: Int, maxLength: Int, minMultiple: Double, maxMultiple: Double,
    minFraction: Double, maxFraction: Double
  ) = new HeapBeam(minLength, maxLength, minMultiple, maxMultiple)
}

object RadixBeam {
  def apply(
    minLength: Int, maxLength: Int, minMultiple: Double, maxMultiple: Double,
    minFraction: Double, maxFraction: Double
  ) = new RadixBeam(minLength, maxLength, minMultiple, maxMultiple,
    minFraction, maxFraction)
}

object BeamTest extends App {
}
