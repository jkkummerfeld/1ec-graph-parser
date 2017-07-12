// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

/**
 * This code is based on the public domain hashmap written by Mikhail Vorontsov:
 * https://github.com/mikvor/hashmapTest
 *
 * Note, it is not threadsafe in general. However, synchronous operations
 * that only inspect state should be fine.
 */

/** Design nodes:
 *
 * allowResize - setting this to false is useful when we want to have a map
 * that can be updated in parallel to accesses. Calls to put still need to be
 * synchronized, but they can occur while calls to get are also run in
 * parallel.
 */

// TODO Add option at creation for pre-hashed keys

// TODO Create 128 bit maps and sets, using 2 64 bit values to store the key.

// TODO: Save an integer by combining size and threshold, instead only track the number till next expansion. Threshold is then when that value is 0 and size is ((spaces * fill ratio) - number till update). Careful with off by one errors (as fill ratio is a double).

// TODO: changeOrPut may make more sense as AddElsePut

package edu.berkeley.nlp.graphparser

import scala.annotation.tailrec

object MapConsts {
  val FREE_KEY = 0
}

@SerialVersionUID(1L)
class LongDoubleMap(
  val fillFactor: Double,
  val initSize: Int,
  val NO_VALUE: Double = Double.PositiveInfinity,
  val allowResize: Boolean = true
) extends Serializable {
  require(fillFactor > 0 || fillFactor < 1, "FillFactor must be in (0, 1)")
  require(initSize > 0, "Size must be positive")

  // Do we have 'free' key in the map?
  // The value is stored at data(0)
  var hasFreeKey = false
  // Keys and values
  // keys in odd positions, values following
  var data : Array[Long] = null
  // We will resize a map once it reaches this size
  var threshold = 0
  // Mask to calculate the original position
  var mask = 0
  var mask2 = 0
  // Current map size
  var size = 0

  clear

  def foreachPair(fn: (Long, Double) => Unit) = {
    if (hasFreeKey)
      fn(MapConsts.FREE_KEY, java.lang.Double.longBitsToDouble(data(0)))
    var i = 1
    while (i < data.length) {
      if (data(i) != MapConsts.FREE_KEY)
        fn(data(i), java.lang.Double.longBitsToDouble(data(i + 1)))
      i += 2
    }
  }

  def clear = {
    var capacity = 1
    var n = initSize
    while (n > 0) {
      capacity <<= 1
      n >>= 1
    }

    // Create new space and update relevant variables
    data = new Array[Long](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
  }

  private def rehash : Unit = {
    require(allowResize)

    // Increase capacity
    // (this calculates it as data.length = cur_capacity * 2 + 1
    val capacity = data.length - 1

    // Create new space and update relevant variables
    val oldData = data
    data = new Array[Long](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
    if (hasFreeKey) {
      size += 1
      data(0) = oldData(0)
    }

    // Insert current data into the new array
    var i = 1
    while (i < oldData.length) {
      val oldKey = oldData(i)
      if (oldKey != MapConsts.FREE_KEY) putInternal(oldKey, oldData(i + 1))
      i += 2
    }
  }

  // Note: Assumes it is not being called with the free key
  @tailrec final def probeForKey(key: Long, pos: Int) : Int = {
    val k = data(pos)
    if (k == MapConsts.FREE_KEY) -pos
    else if (k == key) pos
    else probeForKey(key, (pos + 2) & mask2)
  }
  @inline private def getPos(key: Long) : Int = {
    val start = (Hash.hashLongToInt(key) & mask) + 1
    probeForKey(key, start)
  }

  def get(key: Long) : Double = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) NO_VALUE
      else java.lang.Double.longBitsToDouble(data(0))
    } else {
      val pos = getPos(key)
      if (pos < 0) NO_VALUE
      else java.lang.Double.longBitsToDouble(data(pos + 1))
    }
  }

  def getOrElse(key: Long, default: Double) : Double = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) default
      else java.lang.Double.longBitsToDouble(data(0))
    } else {
      val pos = getPos(key)
      if (pos < 0) default
      else java.lang.Double.longBitsToDouble(data(pos + 1))
    }
  }

  def getOrElseUpdate(key: Long, value: Double) : Double = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) java.lang.Double.longBitsToDouble(data(0))
      else {
        size += 1
        hasFreeKey = true
        data(0) = java.lang.Double.doubleToRawLongBits(value)
        value
      }
    } else {
      val pos = getPos(key)
      if (pos > 0) java.lang.Double.longBitsToDouble(data(pos + 1))
      else {
        size += 1
        val npos = -pos
        data(npos) = key
        data(npos + 1) = java.lang.Double.doubleToRawLongBits(value)
        if (size >= threshold) rehash
        value
      }
    }
  }

  var mostRecent = -2
  var mostRecentKey = 0L
  def getAndNote(key: Long) : Double = {
    mostRecentKey = key
    if (key == MapConsts.FREE_KEY) {
      mostRecent = -1
      if (!hasFreeKey) NO_VALUE
      else java.lang.Double.longBitsToDouble(data(0))
    } else {
      mostRecent = getPos(key)
      if (mostRecent < 0) NO_VALUE
      else java.lang.Double.longBitsToDouble(data(mostRecent + 1))
    }
  }

  private def putInternal(key: Long, value: Long) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) size += 1
      hasFreeKey = true
      data(0) = value
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1) = value
      else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  def putNoted(dvalue: Double) : Unit = {
    val value = java.lang.Double.doubleToRawLongBits(dvalue)
    if (mostRecentKey == MapConsts.FREE_KEY) {
      hasFreeKey = true
      data(0) = value
    } else {
      if (mostRecent > 0) data(mostRecent + 1) = value
      else {
        val npos = -mostRecent
        data(npos) = mostRecentKey
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  def put(key: Long, value: Double) =
    putInternal(key, java.lang.Double.doubleToRawLongBits(value))

  def changeOrPut(key: Long, delta: Double) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) {
        val nval = delta + java.lang.Double.longBitsToDouble(data(0))
        data(0) = java.lang.Double.doubleToRawLongBits(nval)
      } else {
        hasFreeKey = true
        data(0) = java.lang.Double.doubleToRawLongBits(delta)
      }
    } else {
      val pos = getPos(key)
      if (pos >= 0) {
        val nval = delta + java.lang.Double.longBitsToDouble(data(pos + 1))
        data(pos + 1) = java.lang.Double.doubleToRawLongBits(nval)
      } else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = java.lang.Double.doubleToRawLongBits(delta)
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  def contains(key: Long) : Boolean =
    if (key == MapConsts.FREE_KEY) hasFreeKey
    else getPos(key) >= 0

  def get(key: (Int, Int)) : Double =
    get(Hash.combineInLong(key) )
  def get(key: (Int, Int, Int)) : Double =
    get(Hash.combineInLong(key) )
  def get(key1: Int, key2: Int) : Double =
    get(Hash.combineInLong(key1, key2) )
  def get(key1: Int, key2: Int, key3: Int) : Double =
    get(Hash.combineInLong(key1, key2, key3) )
  def getOrElse(key: (Int, Int), default: Double) : Double =
    getOrElse(Hash.combineInLong(key), default)
  def getOrElse(key: (Int, Int, Int), default: Double) : Double =
    getOrElse(Hash.combineInLong(key), default)
  def getOrElse(key1: Int, key2: Int, default: Double) : Double =
    getOrElse(Hash.combineInLong(key1, key2), default)
  def getOrElse(key1: Int, key2: Int, key3: Int, default: Double) : Double =
    getOrElse(Hash.combineInLong(key1, key2, key3), default)
  def put(key: (Int, Int), value: Double) : Unit =
    put(Hash.combineInLong(key), value)
  def put(key: (Int, Int, Int), value: Double) : Unit =
    put(Hash.combineInLong(key), value)
  def put(key1: Int, key2: Int, value: Double) : Unit =
    put(Hash.combineInLong(key1, key2), value)
  def put(key1: Int, key2: Int, key3: Int, value: Double) : Unit =
    put(Hash.combineInLong(key1, key2, key3), value)
}

// TODO: A freeze method that checks for the largest element, and if it is not
// too big, restructures to be an array rather than a hash (avoiding the need
// for hashing in future lookups). For 'too big', it can be twice the current
// size of the map for free (no need to store keys anymore).
@SerialVersionUID(1L)
class IntIntMap(
  val fillFactor: Double,
  val initSize: Int,
  val NO_VALUE: Int = 0,
  val allowResize: Boolean = true
) extends Serializable {
  require(fillFactor > 0 || fillFactor < 1, "FillFactor must be in (0, 1)")
  require(initSize > 0, "Size must be positive")

  // Do we have 'free' key in the map?
  // The value is stored at data(0)
  var hasFreeKey = false
  // Keys and values
  // keys in odd positions, values following
  var data : Array[Int] = null
  // We will resize a map once it reaches this size
  var threshold = 0
  // Mask to calculate the original position
  var mask = 0
  var mask2 = 0
  // Current map size
  var size = 0

  clear

  def foreachPair(fn: (Int, Int) => Unit) = {
    if (hasFreeKey) fn(MapConsts.FREE_KEY, data(0))
    var i = 1
    while (i < data.length) {
      if (data(i) != MapConsts.FREE_KEY) fn(data(i), data(i+1))
      i += 2
    }
  }

  def clear = {
    var capacity = 1
    var n = initSize
    while (n > 0) {
      capacity <<= 1
      n >>= 1
    }

    // Create new space and update relevant variables
    data = new Array[Int](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
  }

  private def rehash : Unit = {
    require(allowResize)

    // Increase capacity
    // (this calculates it as data.length = cur_capacity * 2 + 1
    val capacity = data.length - 1

    // Create new space and update relevant variables
    val oldData = data
    data = new Array[Int](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
    if (hasFreeKey) {
      size += 1
      data(0) = oldData(0)
    }

    // Insert current data into the new array
    var i = 1
    while (i < oldData.length) {
      val oldKey = oldData(i)
      // The size update will be handled by put
      if (oldKey != MapConsts.FREE_KEY) put(oldKey, oldData(i + 1))
      i += 2
    }
  }

  // Note: Assumes it is not being called with the free key
  var probeCount = 0
  @tailrec final def probeForKey(key: Int, pos: Int) : Int = {
///    probeCount += 1
    val k = data(pos)
    if (k == MapConsts.FREE_KEY) -pos
    else if (k == key) pos
    else probeForKey(key, (pos + 2) & mask2)
  }
  var queryCount = 0
  @inline private def getPos(key: Int) : Int = {
///    queryCount += 1
    val start = (Hash.hashN(key) & mask) + 1
    probeForKey(key, start)
  }

  def contains(key: Int) : Boolean =
    if (key == MapConsts.FREE_KEY) hasFreeKey
    else getPos(key) >= 0

  def get(key: Int) : Int = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) NO_VALUE
      else data(0)
    } else {
      val pos = getPos(key)
      if (pos < 0) NO_VALUE
      else data(pos + 1)
    }
  }

  def getOrElse(key: Int, default: Int) : Int = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) default
      else data(0)
    } else {
      val pos = getPos(key)
      if (pos < 0) default
      else data(pos + 1)
    }
  }

  def changeOrPut(key: Int, delta: Int) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) data(0) += delta
      else {
        hasFreeKey = true
        data(0) = delta
      }
    } else {
      val pos = getPos(key)
      if (pos >= 0) data(pos + 1) += delta
      else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = delta
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  var mostRecent = -2
  var mostRecentKey = 0
  def getAndNote(key: Int) : Int = {
    mostRecentKey = key
    if (key == MapConsts.FREE_KEY) {
      mostRecent = -1
      if (!hasFreeKey) NO_VALUE
      else data(0)
    } else {
      val pos = getPos(key)
      mostRecent = pos
      if (pos < 0) NO_VALUE
      else data(pos + 1)
    }
  }

  def putNoted(value: Int) : Unit = {
    if (mostRecentKey == MapConsts.FREE_KEY) {
      hasFreeKey = true
      data(0) = value
    } else {
      if (mostRecent > 0) data(mostRecent + 1) = value
      else {
        val npos = -mostRecent
        data(npos) = mostRecentKey
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }


  def put(key: Int, value: Int) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) size += 1
      hasFreeKey = true
      data(0) = value
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1) = value
      else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  def getOrElseUpdate(key: Int, value: Int) : Int = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) data(0).toInt
      else {
        size += 1
        hasFreeKey = true
        data(0) = value
        value
      }
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1)
      else {
        size += 1
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        if (size >= threshold) rehash
        value
      }
    }
  }
}

@SerialVersionUID(1L)
class LongIntMap(
  val fillFactor: Double,
  val initSize: Int,
  val NO_VALUE: Int = 0,
  val allowResize: Boolean = true
) extends Serializable {
  require(fillFactor > 0 || fillFactor < 1, "FillFactor must be in (0, 1)")
  require(initSize > 0, "Size must be positive")

  // Do we have 'free' key in the map?
  // The value is stored at data(0)
  var hasFreeKey = false
  // Keys and values
  // keys in odd positions, values following
  // Set to null now because we immediately create it below
  var data : Array[Long] = null
  // We will resize a map once it reaches this size
  var threshold = 0
  // Mask to calculate the original position
  var mask = 0
  var mask2 = 0
  // Current map size (ie, number of inserted values)
  var size = 0

  clear

  def foreachPair(fn: (Long, Int) => Unit) = {
    if (hasFreeKey) fn(MapConsts.FREE_KEY, data(0).toInt)
    var i = 1
    while (i < data.length) {
      if (data(i) != MapConsts.FREE_KEY) fn(data(i), data(i+1).toInt)
      i += 2
    }
  }

  def clear = {
    var capacity = 1
    var n = initSize
    while (n > 0) {
      capacity <<= 1
      n >>= 1
    }

    // Create new space and update relevant variables
    data = new Array[Long](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
  }

  private def rehash : Unit = {
    require(allowResize)

    // Increase capacity
    // (this calculates it as data.length = cur_capacity * 2 + 1
    val capacity = data.length - 1

    // Create new space and update relevant variables
    val oldData = data
    data = new Array[Long](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
    if (hasFreeKey) {
      size += 1
      data(0) = oldData(0)
    }

    // Insert current data into the new array
    var i = 1
    while (i < oldData.length) {
      val oldKey = oldData(i)
      if (oldKey != MapConsts.FREE_KEY) putInternal(oldKey, oldData(i + 1))
      i += 2
    }
  }

  // Note: Assumes it is not being called with the free key
  @tailrec final def probeForKey(key: Long, pos: Int) : Int = {
    val k = data(pos)
    if (k == MapConsts.FREE_KEY) -pos
    else if (k == key) pos
    else probeForKey(key, (pos + 2) & mask2)
  }
  @inline private def getPos(key: Long) : Int = {
    val start = (Hash.hashLongToInt(key) & mask) + 1
    probeForKey(key, start)
  }

  def contains(key: Long) : Boolean =
    if (key == MapConsts.FREE_KEY) hasFreeKey
    else getPos(key) >= 0

  def get(key: Long) : Int = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) NO_VALUE
      else data(0).toInt
    } else {
      val pos = getPos(key)
      if (pos < 0) NO_VALUE
      else data(pos + 1).toInt
    }
  }

  def getOrElse(key: Long, default: Int) : Int = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) default
      else data(0).toInt
    } else {
      val pos = getPos(key)
      if (pos < 0) default
      else data(pos + 1).toInt
    }
  }

  var mostRecent = -2
  var mostRecentKey = 0L
  def getAndNote(key: Long) : Int = {
    mostRecentKey = key
    if (key == MapConsts.FREE_KEY) {
      mostRecent = -1
      if (!hasFreeKey) NO_VALUE
      else data(0).toInt
    } else {
      val pos = getPos(key)
      mostRecent = pos
      if (pos < 0) NO_VALUE
      else data(pos + 1).toInt
    }
  }

  private def putInternal(key: Long, value: Long) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) size += 1
      hasFreeKey = true
      data(0) = value
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1) = value
      else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  def putNoted(ivalue: Int) : Unit = {
    val value = ivalue.toLong
    if (mostRecentKey == MapConsts.FREE_KEY) {
      hasFreeKey = true
      data(0) = value
    } else {
      if (mostRecent > 0) data(mostRecent + 1) = value
      else {
        val npos = -mostRecent
        data(npos) = mostRecentKey
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  private def getOrElseUpdateInternal(key: Long, value: Long) = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) data(0).toInt
      else {
        size += 1
        hasFreeKey = true
        data(0) = value
        value.toInt
      }
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1).toInt
      else {
        size += 1
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        if (size >= threshold) rehash
        value.toInt
      }
    }
  }

  def put(key: Long, value: Int) = putInternal(key, value.toLong)

  def getOrElseUpdate(key: Long, value: Int) =
    getOrElseUpdateInternal(key, value.toLong)

  def changeOrPut(key: Long, delta: Int) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) data(0) += delta
      else {
        hasFreeKey = true
        data(0) = delta
      }
    } else {
      val pos = getPos(key)
      if (pos >= 0) data(pos + 1) += delta
      else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = delta
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

///  public int remove(final int key)
///  {
///      if (key == MapConsts.FREE_KEY)
///      {
///          if (!m_hasFreeKey)
///              return NO_VALUE;
///          m_hasFreeKey = false;
///          --size;
///          return m_freeValue; //value is not cleaned
///      }

///      int ptr = (Tools.phiMix(key) & m_mask) << 1;
///      int k = m_data[ ptr ];
///      if (k == key) //we check FREE prior to this call
///      {
///          final int res = m_data[ ptr + 1 ];
///          shiftKeys(ptr);
///          --size;
///          return res;
///      }
///      else if (k == MapConsts.FREE_KEY)
///          return NO_VALUE;  //end of chain already
///      while (true)
///      {
///          ptr = (ptr + 2) & m_mask2; //that's next index calculation
///          k = m_data[ ptr ];
///          if (k == key)
///          {
///              final int res = m_data[ ptr + 1 ];
///              shiftKeys(ptr);
///              --size;
///              return res;
///          }
///          else if (k == MapConsts.FREE_KEY)
///              return NO_VALUE;
///      }
///  }

///  private int shiftKeys(int pos)
///  {
///      // Shift entries with the same hash.
///      int last, slot;
///      int k;
///      final int[] data = this.m_data;
///      while (true)
///      {
///          pos = ((last = pos) + 2) & m_mask2;
///          while (true)
///          {
///              if ((k = data[pos]) == MapConsts.FREE_KEY)
///              {
///                  data[last] = MapConsts.FREE_KEY;
///                  return last;
///              }
///              slot = (Tools.phiMix(k) & m_mask) << 1; //calculate the starting slot for the current key
///              if (last <= pos ? last >= slot || slot > pos : last >= slot && slot > pos) break;
///              pos = (pos + 2) & m_mask2; //go to the next entry
///          }
///          data[last] = k;
///          data[last + 1] = data[pos + 1];
///      }
///  }
}


@SerialVersionUID(1L)
class LongLongMap(
  val fillFactor: Double,
  val initSize: Int,
  val NO_VALUE: Long = 0,
  val allowResize: Boolean = true
) extends Serializable {
  require(fillFactor > 0 || fillFactor < 1, "FillFactor must be in (0, 1)")
  require(initSize > 0, "Size must be positive")

  // Do we have 'free' key in the map?
  // The value is stored at data(0)
  var hasFreeKey = false
  // Keys and values
  // keys in odd positions, values following
  var data : Array[Long] = null
  // We will resize a map once it reaches this size
  var threshold = 0
  // Mask to calculate the original position
  var mask = 0
  var mask2 = 0
  // Current map size
  var size = 0

  clear

  def foreachPair(fn: (Long, Long) => Unit) = {
    if (hasFreeKey) fn(MapConsts.FREE_KEY, data(0))
    var i = 1
    while (i < data.length) {
      if (data(i) != MapConsts.FREE_KEY) fn(data(i), data(i+1))
      i += 2
    }
  }

  def clear = {
    var capacity = 1
    var n = initSize
    while (n > 0) {
      capacity <<= 1
      n >>= 1
    }

    // Create new space and update relevant variables
    data = new Array[Long](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
  }

  private def rehash : Unit = {
    require(allowResize)

    // Increase capacity
    // (this calculates it as data.length = cur_capacity * 2 + 1
    val capacity = data.length - 1

    // Create new space and update relevant variables
    val oldData = data
    data = new Array[Long](capacity * 2 + 1)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity*2 - 2
    mask2 = capacity*2 - 1
    size = 0
    if (hasFreeKey) {
      size += 1
      data(0) = oldData(0)
    }

    // Insert current data into the new array
    var i = 1
    while (i < oldData.length) {
      val oldKey = oldData(i)
      if (oldKey != MapConsts.FREE_KEY) putInternal(oldKey, oldData(i + 1))
      i += 2
    }
  }

  // Note: Assumes it is not being called with the free key
  var probeCount = 0
  @tailrec final def probeForKey(key: Long, pos: Int) : Int = {
///    probeCount += 1
    val k = data(pos)
    if (k == MapConsts.FREE_KEY) -pos
    else if (k == key) pos
    else probeForKey(key, (pos + 2) & mask2)
  }
  var queryCount = 0
  @inline private def getPos(key: Long) : Int = {
///    queryCount += 1
    val start = (Hash.hashLongToInt(key) & mask) + 1
    probeForKey(key, start)
  }

  def contains(key: Long) : Boolean =
    if (key == MapConsts.FREE_KEY) hasFreeKey
    else getPos(key) >= 0

  def get(key: Long) : Long = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) NO_VALUE
      else data(0).toInt
    } else {
      val pos = getPos(key)
      if (pos < 0) NO_VALUE
      else data(pos + 1)
    }
  }

  def getOrElse(key: Long, default: Long) : Long = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) default
      else data(0).toInt
    } else {
      val pos = getPos(key)
      if (pos < 0) default
      else data(pos + 1)
    }
  }

  var mostRecent = -2
  var mostRecentKey = 0L
  def getAndNote(key: Long) : Long = {
    mostRecentKey = key
    if (key == MapConsts.FREE_KEY) {
      mostRecent = -1
      if (!hasFreeKey) NO_VALUE
      else data(0)
    } else {
      val pos = getPos(key)
      mostRecent = pos
      if (pos < 0) NO_VALUE
      else data(pos + 1)
    }
  }

  private def putInternal(key: Long, value: Long) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) size += 1
      hasFreeKey = true
      data(0) = value
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1) = value
      else {
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  def putNoted(ivalue: Long) : Unit = {
    val value = ivalue
    if (mostRecentKey == MapConsts.FREE_KEY) {
      hasFreeKey = true
      data(0) = value
    } else {
      if (mostRecent > 0) data(mostRecent + 1) = value
      else {
        val npos = -mostRecent
        data(npos) = mostRecentKey
        data(npos + 1) = value
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  private def getOrElseUpdateInternal(key: Long, value: Long) = {
    if (key == MapConsts.FREE_KEY) {
      if (hasFreeKey) data(0)
      else {
        size += 1
        hasFreeKey = true
        data(0) = value
        value.toInt
      }
    } else {
      val pos = getPos(key)
      if (pos > 0) data(pos + 1)
      else {
        size += 1
        val npos = -pos
        data(npos) = key
        data(npos + 1) = value
        if (size >= threshold) rehash
        value
      }
    }
  }

  def put(key: Long, value: Long) = putInternal(key, value)

  def getOrElseUpdate(key: Long, value: Long) =
    getOrElseUpdateInternal(key, value)

  // Convenience methods for storing a pair of ints rather than a long
  def put(key: Long, value1: Int, value2: Int) : Unit = {
    val lvalue1 = value1.toLong & 0xffffffffL
    val lvalue2 = value2.toLong & 0xffffffffL
    put(key, (lvalue1 << 32) + lvalue2)
  }
  def getPair(key: Long) = {
    val num = get(key)
    ((num >>> 32).toInt, (num & 0xffffffffL).toInt)
  }

  def get(key: (Int, Int)) : Long =
    get(Hash.combineInLong(key))
  def get(key: (Int, Int, Int)) : Long =
    get(Hash.combineInLong(key))
  def get(key: (Int, Int, Int, Int)) : Long =
    get(Hash.combineInLong(key))
  def get(key1: Int, key2: Int) : Long =
    get(Hash.combineInLong(key1, key2))
  def get(key1: Int, key2: Int, key3: Int) : Long =
    get(Hash.combineInLong(key1, key2, key3))
  def get(key1: Int, key2: Int, key3: Int, key4: Int) : Long =
    get(Hash.combineInLong(key1, key2, key3, key4))
  def get2Ints(key: Long) : (Int, Int) =
    Hash.splitFromLong2(get(key))
  def get2Ints(key: (Int, Int)) : (Int, Int) =
    Hash.splitFromLong2(get(Hash.combineInLong(key)))
  def get2Ints(key1: Int, key2: Int) : (Int, Int) =
    Hash.splitFromLong2(get(Hash.combineInLong(key1, key2)))
  def get2Ints(key1: Int, key2: Int, key3: Int, key4: Int) : (Int, Int) =
    Hash.splitFromLong2(get(Hash.combineInLong(key1, key2, key3, key4)))
  def getOrElse(key: (Int, Int), default: Long) : Long =
    getOrElse(Hash.combineInLong(key), default)
  def getOrElse(key: (Int, Int, Int), default: Long) : Long =
    getOrElse(Hash.combineInLong(key), default)
  def getOrElse(key: (Int, Int, Int, Int), default: Long) : Long =
    getOrElse(Hash.combineInLong(key), default)
  def getOrElse(key1: Int, key2: Int, default: Long) : Long =
    getOrElse(Hash.combineInLong(key1, key2), default)
  def getOrElse(key1: Int, key2: Int, key3: Int, default: Long) : Long =
    getOrElse(Hash.combineInLong(key1, key2, key3), default)
  def getOrElse(key1: Int, key2: Int, key3: Int, key4: Int, default: Long) : Long =
    getOrElse(Hash.combineInLong(key1, key2, key3, key4), default)
  def get2IntsOrElse(key: (Int, Int), default: (Int, Int)) : (Int, Int) =
    Hash.splitFromLong2(getOrElse(Hash.combineInLong(key), Hash.combineInLong(default)))
  def get2IntsOrElse(key1: Int, key2: Int, default: (Int, Int)) : (Int, Int) =
    Hash.splitFromLong2(getOrElse(Hash.combineInLong(key1, key2), Hash.combineInLong(default)))
  def get2IntsOrElse(key1: Int, key2: Int, key3: Int, key4: Int, default: (Int, Int)) : (Int, Int) =
    Hash.splitFromLong2(getOrElse(Hash.combineInLong(key1, key2, key3, key4), Hash.combineInLong(default)))
  def put(key: (Int, Int), value: Long) : Unit =
    put(Hash.combineInLong(key), value)
  def put(key: (Int, Int, Int), value: Long) : Unit =
    put(Hash.combineInLong(key), value)
  def put(key: (Int, Int, Int, Int), value: Long) : Unit =
    put(Hash.combineInLong(key), value)
  def put(key1: Int, key2: Int, value: Long) : Unit =
    put(Hash.combineInLong(key1, key2), value)
  def put(key1: Int, key2: Int, key3: Int, value: Long) : Unit =
    put(Hash.combineInLong(key1, key2, key3), value)
  def put(key1: Int, key2: Int, key3: Int, key4: Int, value: Long) : Unit =
    put(Hash.combineInLong(key1, key2, key3, key4), value)
  def put2Ints(key: (Int, Int), value: (Int, Int)) : Unit =
    put(Hash.combineInLong(key), Hash.combineInLong(value))
  def put2Ints(key1: Int, key2: Int, value: (Int, Int)) : Unit =
    put(Hash.combineInLong(key1, key2), Hash.combineInLong(value))
  def put2Ints(key1: Int, key2: Int, key3: Int, key4: Int, value: (Int, Int)) : Unit =
    put(Hash.combineInLong(key1, key2, key3, key4), Hash.combineInLong(value))
}

@SerialVersionUID(1L)
class LongSet(
  val fillFactor: Double,
  val initSize: Int,
  val allowResize: Boolean = true
) extends Serializable {
  require(fillFactor > 0 || fillFactor < 1, "FillFactor must be in (0, 1)")
  require(initSize > 0, "Size must be positive")

  // Do we have 'free' key in the set?
  var hasFreeKey = false
  // Keys
  var data : Array[Long] = null
  // We will resize a map once it reaches this size
  var threshold = 0
  // Mask to calculate the original position
  var mask = 0
  // Current map size
  var size = 0

  clear

  override def toString() = {
    val start = s"LongSet[$fillFactor, ${data.length}]("
    val freePart = if (hasFreeKey) s"${MapConsts.FREE_KEY}," else ""
    val rest = data.filter(_ != MapConsts.FREE_KEY).map(_.toString).mkString(",")
    val end =")"
    start + freePart + rest + end
  }
  def longToIntPair(num: Long) = ((num >>> 32).toInt, (num & 0xffffffffL).toInt)
  def toStringPairs = {
    val start = s"LongSet[$fillFactor, ${data.length}]("
    val freePair = longToIntPair(MapConsts.FREE_KEY)
    val freePart = if (hasFreeKey) s"$freePair," else ""
    val rest = data.filter(_ != MapConsts.FREE_KEY).map(longToIntPair(_).toString).mkString(",")
    val end =")"
    start + freePart + rest + end
  }

  def foreachVal(fn: Long => Unit) = {
    if (hasFreeKey) fn(MapConsts.FREE_KEY)
    var i = 0
    while (i < data.length) {
      if (data(i) != MapConsts.FREE_KEY) fn(data(i))
      i += 1
    }
  }

  def clear = {
    var capacity = 1
    var n = initSize
    while (n > 0) {
      capacity <<= 1
      n >>= 1
    }

    // Create new space and update relevant variables
    data = new Array[Long](capacity)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity - 1
    size = 0
  }

  private def rehash : Unit = {
    require(allowResize)

    // Increase capacity
    val capacity = data.length * 2

    // Create new space and update relevant variables
    val oldData = data
    data = new Array[Long](capacity)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity - 1
    size = 0
    if (hasFreeKey) size += 1

    // Insert current data into the new array
    var i = 0
    while (i < oldData.length) {
      val oldKey = oldData(i)
      if (oldKey != MapConsts.FREE_KEY) add(oldKey)
      i += 1
    }
  }

  // Note: Assumes it is not being called with the free key
  @tailrec final def probeForKey(key: Long, pos: Int) : Int = {
    val k = data(pos)
    if (k == MapConsts.FREE_KEY) {
      if (pos > 0) -pos
      else Int.MinValue // This is necessary to distinguish +0 and -0
    } else if (k == key) pos
    else probeForKey(key, (pos + 1) & mask)
  }
  @inline private def getPos(key: Long) : Int = {
    val start = (Hash.hashLongToInt(key) & mask)
    probeForKey(key, start)
  }

  def contains(key: Long) : Boolean =
    if (key == MapConsts.FREE_KEY) hasFreeKey
    else getPos(key) >= 0

  def add(key: Long) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) {
        size += 1
        hasFreeKey = true
      }
    } else {
      val pos = getPos(key)
      if (pos < 0) {
        if (pos == Int.MinValue)
          data(0) = key
        else {
          val npos = -pos
          data(npos) = key
        }
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  // Convenience methods for storing multiple ints rather than a long
  // Of course, beyond two they are not fully represented
  def add(key: (Int, Int)) : Unit =
    add( Hash.combineInLong(key) )
  def add(key: (Int, Int, Int)) : Unit =
    add( Hash.combineInLong(key) )
  def add(key: (Int, Int, Int, Int)) : Unit =
    add( Hash.combineInLong(key) )
  def add(key1: Int, key2: Int) : Unit =
    add( Hash.combineInLong(key1, key2) )
  def add(key1: Int, key2: Int, key3: Int) : Unit =
    add( Hash.combineInLong(key1, key2, key3) )
  def add(key1: Int, key2: Int, key3: Int, key4: Int) : Unit =
    add( Hash.combineInLong(key1, key2, key3, key4) )
  def contains(key: (Int, Int)) : Boolean =
    contains( Hash.combineInLong(key) )
  def contains(key: (Int, Int, Int)) : Boolean =
    contains( Hash.combineInLong(key) )
  def contains(key: (Int, Int, Int, Int)) : Boolean =
    contains( Hash.combineInLong(key) )
  def contains(key1: Int, key2: Int) : Boolean =
    contains( Hash.combineInLong(key1, key2) )
  def contains(key1: Int, key2: Int, key3: Int) : Boolean =
    contains( Hash.combineInLong(key1, key2, key3) )
  def contains(key1: Int, key2: Int, key3: Int, key4: Int) : Boolean =
    contains( Hash.combineInLong(key1, key2, key3, key4) )
}

@SerialVersionUID(1L)
class IntSet(
  val fillFactor: Double,
  val initSize: Int,
  val allowResize: Boolean = true
) extends Serializable {
  require(fillFactor > 0 || fillFactor < 1, "FillFactor must be in (0, 1)")
  require(initSize > 0, "Size must be positive")

  // Do we have 'free' key in the set?
  var hasFreeKey = false
  // Keys
  var data : Array[Int] = null
  // We will resize a map once it reaches this size
  var threshold = 0
  // Mask to calculate the original position
  var mask = 0
  // Current map size
  var size = 0

  clear

  def foreachVal(fn: Int => Unit) = {
    if (hasFreeKey) fn(MapConsts.FREE_KEY)
    var i = 0
    while (i < data.length) {
      if (data(i) != MapConsts.FREE_KEY) fn(data(i))
      i += 1
    }
  }

  def clear = {
    var capacity = 1
    var n = initSize
    while (n > 0) {
      capacity <<= 1
      n >>= 1
    }

    // Create new space and update relevant variables
    data = new Array[Int](capacity)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity - 1
    size = 0
  }

  private def rehash : Unit = {
    require(allowResize)

    // Increase capacity
    val capacity = data.length * 2

    // Create new space and update relevant variables
    val oldData = data
    data = new Array[Int](capacity)
    threshold = (capacity * fillFactor).floor.toInt
    mask = capacity - 1
    size = 0
    if (hasFreeKey) size += 1

    // Insert current data into the new array
    var i = 0
    while (i < oldData.length) {
      val oldKey = oldData(i)
      if (oldKey != MapConsts.FREE_KEY) add(oldKey)
      i += 1
    }
  }

  // Note: Assumes it is not being called with the free key
///  var probeCount = 0
  @tailrec final def probeForKey(key: Int, pos: Int) : Int = {
///    probeCount += 1
    val k = data(pos)
    if (k == MapConsts.FREE_KEY) {
      if (pos > 0) -pos
      else Int.MinValue // This is necessary to distinguish +0 and -0
    } else if (k == key) pos
    else probeForKey(key, (pos + 1) & mask)
  }
///  var queryCount = 0
  @inline private def getPos(key: Int) : Int = {
///    queryCount += 1
    val start = (Hash.hashN(key) & mask)
    probeForKey(key, start)
  }

  def contains(key: Int) : Boolean =
    if (key == MapConsts.FREE_KEY) hasFreeKey
    else getPos(key) >= 0

  def add(key: Int) : Unit = {
    if (key == MapConsts.FREE_KEY) {
      if (!hasFreeKey) {
        size += 1
        hasFreeKey = true
      }
    } else {
      val pos = getPos(key)
      if (pos < 0) {
        if (pos == Int.MinValue)
          data(0) = key
        else {
          val npos = -pos
          data(npos) = key
        }
        size += 1
        if (size >= threshold) rehash
      }
    }
  }

  // Convenience methods for storing multiple values
  // Of course, they are not fully represented
  def add(key: (Int, Int)) : Unit =
    add( Hash.combineInInt(key) )
  def add(key: (Int, Int, Int)) : Unit =
    add( Hash.combineInInt(key) )
  def add(key: (Int, Int, Int, Int)) : Unit =
    add( Hash.combineInInt(key) )
  def add(key1: Int, key2: Int) : Unit =
    add( Hash.combineInInt(key1, key2) )
  def add(key1: Int, key2: Int, key3: Int) : Unit =
    add( Hash.combineInInt(key1, key2, key3) )
  def add(key1: Int, key2: Int, key3: Int, key4: Int) : Unit =
    add( Hash.combineInInt(key1, key2, key3, key4) )
  def contains(key: (Int, Int)) : Boolean =
    contains( Hash.combineInInt(key) )
  def contains(key: (Int, Int, Int)) : Boolean =
    contains( Hash.combineInInt(key) )
  def contains(key: (Int, Int, Int, Int)) : Boolean =
    contains( Hash.combineInInt(key) )
  def contains(key1: Int, key2: Int) : Boolean =
    contains( Hash.combineInInt(key1, key2) )
  def contains(key1: Int, key2: Int, key3: Int) : Boolean =
    contains( Hash.combineInInt(key1, key2, key3) )
  def contains(key1: Int, key2: Int, key3: Int, key4: Int) : Boolean =
    contains( Hash.combineInInt(key1, key2, key3, key4) )
}
