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
import scala.util.Sorting

/** A dynamically resizable array that doesn't box
  *
  * Starting with explicit variants, TODO: look into specialized, miniboxed, or
  * even just a base class that takes some of the common code out
  */
@SerialVersionUID(1L)
class UnboxedArrayBuffer(
  val initSpace: Int
) extends Serializable {
  var storage = Array.ofDim[Int](initSpace)
  var length : Int = 0

  def fillWithZero =
    length = storage.length

  def extend(max: Int, filler: Int) = {
    var cur = length
    var nlength = storage.length
    while (nlength <= max) nlength *= 2
    if (nlength > storage.length) {
      val nStorage = Array.ofDim[Int](nlength)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    if (filler != 0) {
      while (cur <= max) {
        storage(cur) = filler
        cur += 1
      }
    }
    if (length <= max) length = max + 1
  }

  def resize(last: Int) = {
    if (last + 1 != storage.length) {
      val nStorage = Array.ofDim[Int](last + 1)
      storage.copyToArray(nStorage)
      if (nStorage.length < length)
        length = nStorage.length
      storage = nStorage
    }
  }

  def compact = resize(length - 1)

  def copy = {
    val ans = new UnboxedArrayBuffer(length)
    storage.copyToArray(ans.storage)
    ans
  }

  @tailrec final def contains(num: Int, pos: Int = 0) : Boolean =
    if (pos >= length) false
    else if (storage(pos) == num) true
    else contains(num, pos + 1)

  override def toString() = {
    val storageStrings = for (i <- 0 until length) yield storage(i).toString
    s"$length/${storage.length}   "+ storageStrings.mkString(" ")
  }

  def apply(pos: Int) = storage(pos)
  def checkedApply(pos: Int) = {
///  def apply(pos: Int) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    apply(pos)
///    storage(pos)
  }

  def +=(pos: Int, num: Int) = storage(pos) += num
  def update(pos: Int, num: Int) = storage(pos) = num
  def checkedUpdate(pos: Int, num: Int) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    update(pos, num)
  }

  def removeLast = {
    length -= 1
    storage(length)
  }

  def last = storage(length - 1)

  def append(num: Int) = {
    if (length == storage.length) {
      val nStorage = Array.ofDim[Int](storage.length * 2)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    storage(length) = num
    length += 1
  }

  def reset(hard: Boolean = false) = {
    length = 0
    if (hard) storage = Array.ofDim[Int](initSpace)
  }

  var cpos = Int.MaxValue
  def prepareToIterate = cpos = -1
  def hasNext = cpos < (length - 1)
  def next = {
    cpos += 1
    storage(cpos)
  }
}

object UnboxedArrayBuffer {
  def apply(initSpace: Int = 2) = new UnboxedArrayBuffer(initSpace)
}

@SerialVersionUID(1L)
class UnboxedArrayBufferLong(
  val initSpace: Int
) extends Serializable {
  var storage = Array.ofDim[Long](initSpace)
  var length : Int = 0

  def fillWithZero =
    length = storage.length

  def extend(max: Int, filler: Long) = {
    var cur = length
    var nlength = storage.length
    while (nlength <= max) nlength *= 2
    if (nlength > storage.length) {
      val nStorage = Array.ofDim[Long](nlength)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    while (cur <= max) {
      storage(cur) = filler
      cur += 1
    }
    if (length <= max) length = max + 1
  }

  def resize(last: Int) = {
    val nStorage = Array.ofDim[Long](last + 1)
    storage.copyToArray(nStorage)
    if (nStorage.length < length)
      length = nStorage.length
    storage = nStorage
  }

  def compact = resize(length - 1)

  def copy = {
    val ans = new UnboxedArrayBufferLong(length)
    var i = 0
    while (i < length) {
      ans(i) = storage(i)
      i += 1
    }
    ans
  }

  @tailrec final def contains(num: Long, pos: Int = 0) : Boolean =
    if (pos >= length) false
    else if (storage(pos) == num) true
    else contains(num, pos + 1)

  override def toString() = {
    val storageStrings = for (i <- 0 until length) yield storage(i).toString
    s"$length/${storage.length}   "+ storageStrings.mkString(" ")
  }

  def apply(pos: Int) = storage(pos)
  def checkedApply(pos: Int) = {
///  def apply(pos: Int) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    apply(pos)
///    storage(pos)
  }

  def +=(pos: Int, num: Long) = storage(pos) += num
  def update(pos: Int, num: Long) = storage(pos) = num
  def checkedUpdate(pos: Int, num: Long) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    update(pos, num)
  }

  def removeLast = {
    length -= 1
    storage(length)
  }

  def last = storage(length - 1)

  def append(num: Long) = {
    if (length == storage.length) {
      val nStorage = Array.ofDim[Long](storage.length * 2)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    storage(length) = num
    length += 1
  }

  def reset(hard: Boolean = false) = {
    length = 0
    if (hard) storage = Array.ofDim[Long](initSpace)
  }

  var cpos = Int.MaxValue
  def prepareToIterate = cpos = -1
  def hasNext = cpos < (length - 1)
  def next = {
    cpos += 1
    storage(cpos)
  }
}

object UnboxedArrayBufferLong {
  def apply(initSpace: Int = 2) = new UnboxedArrayBufferLong(initSpace)
}

@SerialVersionUID(1L)
class UnboxedArrayBufferDouble(
  val initSpace: Int
) extends Serializable {
  var storage = Array.ofDim[Double](initSpace)
  var length = 0

  def fillWithZero =
    length = storage.length

  def extend(max: Int, filler: Double) = {
    var cur = length
    var nlength = storage.length
    while (nlength <= max) nlength *= 2
    if (nlength > storage.length) {
      val nStorage = Array.ofDim[Double](nlength)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    while (cur <= max) {
      storage(cur) = filler
      cur += 1
    }
    if (length <= max) length = max + 1
  }

  def resize(last: Int) = {
    val nStorage = Array.ofDim[Double](last + 1)
    storage.copyToArray(nStorage)
    if (nStorage.length < length)
      length = nStorage.length
    storage = nStorage
  }

  def compact = resize(length - 1)

  def copy = {
    val ans = new UnboxedArrayBufferDouble(length)
    var i = 0
    while (i < length) {
      ans(i) = storage(i)
      i += 1
    }
    ans
  }

  @tailrec final def contains(num: Double, pos: Int = 0) : Boolean =
    if (pos >= length) false
    else if ((storage(pos) - num).abs < 1e-6) true
    else contains(num, pos + 1)

  override def toString() = {
    val storageStrings = for (i <- 0 until length) yield storage(i).toString
    s"$length/${storage.length}   "+ storageStrings.mkString(" ")
  }

  def apply(pos: Int) = storage(pos)
  def checkedApply(pos: Int) = {
///  def apply(pos: Int) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    apply(pos)
///    storage(pos)
  }

  def +=(pos: Int, num: Int) = storage(pos) += num
  def update(pos: Int, num: Double) = storage(pos) = num
  def checkedUpdate(pos: Int, num: Double) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    update(pos, num)
  }

  def removeLast = {
    length -= 1
    storage(length)
  }

  def last = storage(length - 1)

  def append(num: Double) = {
    if (length == storage.length) {
      val nStorage = Array.ofDim[Double](storage.length * 2)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    storage(length) = num
    length += 1
  }

  def reset(hard: Boolean = false) = {
    length = 0
    if (hard) storage = Array.ofDim[Double](initSpace)
  }

  var cpos = Int.MaxValue
  def prepareToIterate = cpos = -1
  def hasNext = cpos < (length - 1)
  def next = {
    cpos += 1
    storage(cpos)
  }
}

object UnboxedArrayBufferDouble {
  def apply(initSpace: Int = 2) = new UnboxedArrayBufferDouble(initSpace)
}

@SerialVersionUID(1L)
class UnboxedArrayBufferFloat(
  val initSpace: Int
) extends Serializable {
  var storage = Array.ofDim[Float](initSpace)
  var length = 0

  def fillWithZero =
    length = storage.length

  def sort(reverse: Boolean = false) = {
    compact
    Sorting.quickSort(storage)
    if (reverse) {
      var i = 0
      val last = length / 2
      while (i < last) {
        val tmp = storage(i)
        storage(i) = storage(length - i - 1)
        storage(length - i - 1) = tmp
        i += 1
      }
    }
  }

  def extend(max: Int, filler: Float) = {
    var cur = length
    var nlength = storage.length
    while (nlength <= max) nlength *= 2
    if (nlength > storage.length) {
      val nStorage = Array.ofDim[Float](nlength)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    while (cur <= max) {
      storage(cur) = filler
      cur += 1
    }
    if (length <= max) length = max + 1
  }

  def resize(last: Int) = {
    val nStorage = Array.ofDim[Float](last + 1)
    storage.copyToArray(nStorage)
    if (nStorage.length < length)
      length = nStorage.length
    storage = nStorage
  }

  def compact = resize(length - 1)

  def copy = {
    val ans = new UnboxedArrayBufferFloat(length)
    var i = 0
    while (i < length) {
      ans(i) = storage(i)
      i += 1
    }
    ans
  }

  @tailrec final def contains(num: Float, pos: Int = 0) : Boolean =
    if (pos >= length) false
    else if ((storage(pos) - num).abs < 1e-6) true
    else contains(num, pos + 1)

  override def toString() = {
    val storageStrings = for (i <- 0 until length) yield storage(i).toString
    s"$length/${storage.length}   "+ storageStrings.mkString(" ")
  }

  def apply(pos: Int) = storage(pos)
  def checkedApply(pos: Int) = {
///  def apply(pos: Int) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    apply(pos)
///    storage(pos)
  }

  def +=(pos: Int, num: Int) = storage(pos) += num
  def update(pos: Int, num: Float) = storage(pos) = num
  def checkedUpdate(pos: Int, num: Float) = {
    assert(pos < length, s"$pos (pos) must be < $length (length)")
    assert(pos >= 0, s"$pos (pos) must be >= 0")
    update(pos, num)
  }

  def removeLast = {
    length -= 1
    storage(length)
  }

  def last = storage(length - 1)

  def append(num: Float) = {
    if (length == storage.length) {
      val nStorage = Array.ofDim[Float](storage.length * 2)
      storage.copyToArray(nStorage)
      storage = nStorage
    }
    storage(length) = num
    length += 1
  }

  def reset(hard: Boolean = false) = {
    length = 0
    if (hard) storage = Array.ofDim[Float](initSpace)
  }

  var cpos = Int.MaxValue
  def prepareToIterate = cpos = -1
  def hasNext = cpos < (length - 1)
  def next = {
    cpos += 1
    storage(cpos)
  }
}

object UnboxedArrayBufferFloat {
  def apply(initSpace: Int = 2) = new UnboxedArrayBufferFloat(initSpace)
}
