// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.util.hashing.MurmurHash3

import java.lang.Long.{ rotateLeft => rotl64 }
import java.lang.Integer.{ rotateLeft => rotl }

/** A few useful functions for computing hash values.
  */
object Hash {
  // Based on the public domain MurmurHash3_x64_128:
  // https://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp
  final val c1 : Long = 0x87c37b91114253d5L
  final val c2 : Long = 0x4cf5ad432745937fL

  @inline final def fmix64(oh: Long) = {
    var h = oh ^ (oh >>> 33);
    h *= 0xff51afd7ed558ccdL
    h = h ^ (h >>> 33);
    h *= 0xc4ceb9fe1a85ec53L
    h ^ (h >>> 33);
  }

  @inline final def hashToLong(nums: Int*) : Long = {
    val len : Long = nums.length * 8
    var h1 : Long = 0L
    var h2 : Long = 1L

    // body
    var i = 1
    while (i < nums.length) {
      var k1 : Long = nums(i - 1)
      var k2 : Long = nums(i)
      i += 2

      k1 *= c1
      k1 = rotl64(k1,31)
      k1 *= c2
      h1 = h1 ^ k1

      h1 = rotl64(h1,27)
      h1 += h2
      h1 = h1*5+0x52dce729

      k2 *= c2
      k2 = rotl64(k2,33)
      k2 *= c1
      h2 = h2 ^ k2

      h2 = rotl64(h2,31)
      h2 += h1
      h2 = h2*5+0x38495ab5
    }

    // tail
    if ((nums.length & 1) == 1) {
      var k1 : Long = nums.last
      k1 *= c1
      k1 = rotl64(k1,31)
      k1 *= c2
      h1 = h1 ^ k1
    }

    // finalization
    h1 = h1 ^ len
    h2 = h2 ^ len
    h1 += h2
    h2 += h1
    h1 = fmix64(h1)
    h2 = fmix64(h2)
    h1 += h2
    h2 += h1

    h2
  }

  @inline final def hashLongsToLong(nums: Long*) : Long = {
    val len : Long = nums.length * 16
    var h1 : Long = 0L
    var h2 : Long = 1L

    // body
    var i = 1
    while (i < nums.length) {
      var k1 : Long = nums(i) & 0xffffffffL
      var k2 : Long = nums(i) >>> 32
      i += 1

      k1 *= c1
      k1 = rotl64(k1,31)
      k1 *= c2
      h1 = h1 ^ k1

      h1 = rotl64(h1,27)
      h1 += h2
      h1 = h1*5+0x52dce729

      k2 *= c2
      k2 = rotl64(k2,33)
      k2 *= c1
      h2 = h2 ^ k2

      h2 = rotl64(h2,31)
      h2 += h1
      h2 = h2*5+0x38495ab5
    }

    // finalization
    // TODO: Are these XORs correct? Seems very odd
    h1 = h1 ^ len
    h2 = h2 ^ len
    h1 += h2
    h2 += h1
    h1 = fmix64(h1)
    h2 = fmix64(h2)
    h1 += h2
    h2 += h1

    h2
  }

  @inline final def hashLongToInt(num: Long) = {
    val data1 : Int = (num & 0xffffffffL).toInt
    val data2 : Int  = (num >>> 32).toInt

    // Mix data1
    var k : Int = data1
    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593
    var h : Int = k
    h = rotl(h, 13)
    h = h * 5 + 0xe6546b64

    // Mix data2
    k = data2
    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593
    h ^= k
    h = rotl(h, 13)
    h = h * 5 + 0xe6546b64

    // Finalize
    h ^= 2
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16

    h
  }

  // Int hash, using scala built-ins

  final val initHash = 83007 // == "Seq".hashCode
  @inline final def progressHash(num: Int, curHash: Int) =
    MurmurHash3.mix(curHash, num)
  @inline final def finishHash(curHash: Int, curHashCount: Int) =
    MurmurHash3.finalizeHash(curHash, curHashCount)

  @inline final def hash(nums: Int*) = {
    var curHash = initHash
    var count = 0
    while (count < nums.length) {
      curHash = progressHash(nums(count), curHash)
      count += 1
    }
    finishHash(curHash, count)
  }

  @inline final def hashN(num0: Int) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    finishHash(curHash, 1)
  }
  @inline final def hashN(num0: Int, num1: Int) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    finishHash(curHash, 2)
  }
  @inline final def hashN(num0: Int, num1: Int, num2: Int) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    finishHash(curHash, 3)
  }
  @inline final def hashN(num0: Int, num1: Int, num2: Int, num3: Int) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    curHash = progressHash(num3, curHash)
    finishHash(curHash, 4)
  }
  @inline final def hashN(
    num0: Int, num1: Int, num2: Int, num3: Int, num4: Int
  ) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    curHash = progressHash(num3, curHash)
    curHash = progressHash(num4, curHash)
    finishHash(curHash, 5)
  }
  @inline final def hashN(
    num0: Int, num1: Int, num2: Int, num3: Int, num4: Int, num5: Int
  ) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    curHash = progressHash(num3, curHash)
    curHash = progressHash(num4, curHash)
    curHash = progressHash(num5, curHash)
    finishHash(curHash, 6)
  }
  @inline final def hashN(
    num0: Int, num1: Int, num2: Int, num3: Int, num4: Int, num5: Int, num6: Int
  ) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    curHash = progressHash(num3, curHash)
    curHash = progressHash(num4, curHash)
    curHash = progressHash(num5, curHash)
    curHash = progressHash(num6, curHash)
    finishHash(curHash, 7)
  }
  @inline final def hashN(
    num0: Int, num1: Int, num2: Int, num3: Int, num4: Int, num5: Int, num6: Int,
    num7: Int
  ) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    curHash = progressHash(num3, curHash)
    curHash = progressHash(num4, curHash)
    curHash = progressHash(num5, curHash)
    curHash = progressHash(num6, curHash)
    curHash = progressHash(num7, curHash)
    finishHash(curHash, 8)
  }
  @inline final def hashN(
    num0: Int, num1: Int, num2: Int, num3: Int, num4: Int, num5: Int, num6: Int,
    num7: Int, num8: Int
  ) = {
    var curHash = initHash
    curHash = progressHash(num0, curHash)
    curHash = progressHash(num1, curHash)
    curHash = progressHash(num2, curHash)
    curHash = progressHash(num3, curHash)
    curHash = progressHash(num4, curHash)
    curHash = progressHash(num5, curHash)
    curHash = progressHash(num6, curHash)
    curHash = progressHash(num7, curHash)
    curHash = progressHash(num8, curHash)
    finishHash(curHash, 9)
  }

  // Convenience methods for combining (possibly with loss) multiple values in
  // an int or long
  // Of course, they are not fully represented
  @inline final def combineInLong(key1: Int, key2: Int) = {
    val lkey1 = key1.toLong & 0xffffffffL
    val lkey2 = key2.toLong & 0xffffffffL
    (lkey1 << 32) | lkey2
  }
  @inline final def combineInLong(key1: Int, key2: Int, key3: Int) = {
    val lkey1 = key1.toLong & 0x1fffffL
    val lkey2 = key2.toLong & 0x1fffffL
    val lkey3 = key3.toLong & 0x1fffffL
    (lkey1 << 42) | (lkey2 << 21) | lkey3
  }
  @inline final def combineInLong(key1: Int, key2: Int, key3: Int, key4: Int) = {
    val lkey1 = key1.toLong & 0xffffL
    val lkey2 = key2.toLong & 0xffffL
    val lkey3 = key3.toLong & 0xffffL
    val lkey4 = key4.toLong & 0xffffL
    (lkey1 << 48) | (lkey2 << 32) | (lkey3 << 16) | lkey4
  }
  @inline final def combineInLong(key: (Int, Int)) = {
    val lkey1 = key._1.toLong & 0xffffffffL
    val lkey2 = key._2.toLong & 0xffffffffL
    (lkey1 << 32) | lkey2
  }
  @inline final def combineInLong(key: (Int, Int, Int)) = {
    val lkey1 = key._1.toLong & 0x1fffffL
    val lkey2 = key._2.toLong & 0x1fffffL
    val lkey3 = key._3.toLong & 0x1fffffL
    (lkey1 << 42) | (lkey2 << 21) | lkey3
  }
  @inline final def combineInLong(key: (Int, Int, Int, Int)) = {
    val lkey1 = key._1.toLong & 0xffffL
    val lkey2 = key._2.toLong & 0xffffL
    val lkey3 = key._3.toLong & 0xffffL
    val lkey4 = key._4.toLong & 0xffffL
    (lkey1 << 48) | (lkey2 << 32) | (lkey3 << 16) | lkey4
  }
  @inline final def combineInInt(key1: Int, key2: Int) = {
    val lkey1 = key1 & 0xffff
    val lkey2 = key2 & 0xffff
    (lkey1 << 16) | lkey2
  }
  @inline final def combineInInt(key1: Int, key2: Int, key3: Int) = {
    val lkey1 = key1 & 0x3ff
    val lkey2 = key2 & 0x3ff
    val lkey3 = key3 & 0x3ff
    (lkey1 << 20) | (lkey2 << 10) | lkey3
  }
  @inline final def combineInInt(key1: Int, key2: Int, key3: Int, key4: Int) = {
    val lkey1 = key1 & 0xff
    val lkey2 = key2 & 0xff
    val lkey3 = key3 & 0xff
    val lkey4 = key4 & 0xff
    (lkey1 << 24) | (lkey2 << 16) | (lkey3 << 8) | lkey4
  }
  @inline final def combineInInt(key: (Int, Int)) = {
    val lkey1 = key._1 & 0xffff
    val lkey2 = key._2 & 0xffff
    (lkey1 << 16) | lkey2
  }
  @inline final def combineInInt(key: (Int, Int, Int)) = {
    val lkey1 = key._1 & 0x3ff
    val lkey2 = key._2 & 0x3ff
    val lkey3 = key._3 & 0x3ff
    (lkey1 << 20) | (lkey2 << 10) | lkey3
  }
  @inline final def combineInInt(key: (Int, Int, Int, Int)) = {
    val lkey1 = key._1 & 0xff
    val lkey2 = key._2 & 0xff
    val lkey3 = key._3 & 0xff
    val lkey4 = key._4 & 0xff
    (lkey1 << 24) | (lkey2 << 16) | (lkey3 << 8) | lkey4
  }

  // Corresponding methods for reversing the process
  @inline final def splitFromLong2(num: Long) = {
    val num1 = (num >> 32).toInt
    val num2 = (num & 0xffffffffL).toInt
    (num1, num2)
  }
  @inline final def splitFromLong3(num: Long) = {
    val num1 = (num >> 42).toInt
    val num2 = ((num >> 21) & 0x1fffffL).toInt
    val num3 = (num & 0x1fffffL).toInt
    (num1, num2, num3)
  }
  @inline final def splitFromLong4(num: Long) = {
    val num1 = (num >> 48).toInt
    val num2 = ((num >> 32) & 0xffffL).toInt
    val num3 = ((num >> 16) & 0xffffL).toInt
    val num4 = (num & 0xffffL).toInt
    (num1, num2, num3, num4)
  }
  @inline final def splitFromInt2(num: Int) = {
    val num1 = (num >> 16).toInt
    val num2 = (num & 0xffffL).toInt
    (num1, num2)
  }
  @inline final def splitFromInt3(num: Int) = {
    val num1 = (num >> 20).toInt
    val num2 = ((num >> 10) & 0x3ffL).toInt
    val num3 = (num & 0x3ffL).toInt
    (num1, num2, num3)
  }
  @inline final def splitFromInt4(num: Int) = {
    val num1 = (num >> 24).toInt
    val num2 = ((num >> 16) & 0xffL).toInt
    val num3 = ((num >> 8) & 0xffL).toInt
    val num4 = (num & 0xffL).toInt
    (num1, num2, num3, num4)
  }
}

