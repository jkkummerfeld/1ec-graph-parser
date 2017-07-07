// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

@SerialVersionUID(1L)
class IntIndexer(
  val maxSize: Int,
  val lowercase: Boolean = false
) extends Serializable {
  require(maxSize > 0, s"$maxSize is invalid for maxSize")
  val objects = ArrayBuffer(Model.objectZero)
  val map = HashMap(Model.objectZero -> 0)

  override def toString() = objects.zipWithIndex.map{ w =>
    f"${w._2}% 5d  ${w._1}"
  }.mkString("\n")
  def apply(o0: String) = synchronized {
    val o = if (lowercase) o0.toLowerCase else o0
    map.getOrElseUpdate(o, {
      objects.append(o)
      require(objects.length - 1 <= maxSize, s"${objects.length} more objects in map than user allows ($maxSize)")
      (objects.length - 1)
    })
  }
  def get(o: String) = synchronized { map(o) }
  def getOrElse(o: String, other: Int) = synchronized {
    map.getOrElse(o, other)
  }
  def value(i: Int) = synchronized { objects(i) }
  def size = objects.length
  def contains(o: String) = synchronized { map.contains(o) }
  def contains(i: Int) = i < size
}
object IntIndexer {
  def apply(maxSize: Int = 0x7fffffff) = new IntIndexer(maxSize)
  def apply(filename: String) = {
    val obj = ObjectReader.load(filename)
    import scala.reflect.ClassTag
    obj match {
      case index: IntIndexer => index
      case a =>
        require(false, s"Object not an IntIndexer - ${ClassTag(a.getClass)} ${a.getClass}")
        null
    }
  }
}

@SerialVersionUID(1L)
class ArgumentIndexer(
  maxSize: Int,
  val formalism: Formalism.Value
) extends IntIndexer(maxSize) with Serializable {
  def conv(o: String) = {
    if (formalism == Formalism.AMR) {
      if (o.length < 2 || o.startsWith("ARG")) o
      else {
        val ending = o.last.toInt
        if (ending > 57 || ending < 50) o
        else o.slice(0, o.length - 1) + "N"
      }
    } else o
  }
  override def apply(o: String) = super.apply(conv(o))
  override def get(o: String) = super.get(conv(o))
  override def getOrElse(o: String, other: Int) = super.getOrElse(conv(o), other)
}
object ArgumentIndexer {
  def apply(formalism: Formalism.Value) =
    new ArgumentIndexer(0x3fff, formalism)
  def apply(filename: String) = {
    val obj = ObjectReader.load(filename)
    import scala.reflect.ClassTag
    obj match {
      case index: ArgumentIndexer => index
      case a =>
        require(false, s"Object not an ArgumentIndexer - ${ClassTag(a.getClass)} ${a.getClass}")
        null
    }
  }
}

@SerialVersionUID(1L)
class NonTerminalIndexer(
  maxSize: Int,
  val formalism: Formalism.Value
) extends IntIndexer(maxSize) with Serializable {
  val literals = ArrayBuffer[Boolean]()
  val punct = HashSet[Int]()
  val verb = HashSet[Int]()
  val coord = HashSet[Int]()
  val preterminals = HashSet[Int]()

  override def apply(o: String) = apply(o, false)

  def apply(o: String, preTerminal: Boolean) = {
    val ans = super.apply(o)
    // AMR literal tracking
    if (formalism == Formalism.AMR && ans >= literals.length)
      literals.append(o.startsWith("LITERAL_"))
    // Parsing symbol type
    if (o.length < 6 && o.length > 0 && o(0) != '_' && !o(0).isLetter)
      punct.add(ans)
    else if (o.length > 0 && o(0) == 'V' && ! o.contains("_"))
      verb.add(ans)
    else if (o == "CC")
      coord.add(ans)
    if (preTerminal) preterminals.add(ans)
    ans
  }
  def isLiteral(i: Int) = formalism == Formalism.AMR && literals(i)
  def isPunct(i: Int) = punct.contains(i)
  def isVerb(i: Int) = verb.contains(i)
  def isCoord(i: Int) = coord.contains(i)
  def isPreterminal(s: String) = preterminals.contains(apply(s))
  def isPreterminal(i: Int) = preterminals.contains(i)
  override def toString() = objects.zipWithIndex.map{ case (s, i) =>
    f"${i}% 5d  ${s}"+
    (if (isLiteral(i)) " literal" else " -") +
    (if (isPunct(i)) " punct" else " -") +
    (if (isVerb(i)) " verb" else " -") +
    (if (isPreterminal(s)) " preterminal" else " -")
  }.mkString("\n")
}
object NonTerminalIndexer {
  def apply(formalism: Formalism.Value) =
    new NonTerminalIndexer(0x7fffffff, formalism)
  def apply(filename: String) = {
    val obj = ObjectReader.load(filename)
    import scala.reflect.ClassTag
    obj match {
      case index: NonTerminalIndexer => index
      case a =>
        require(false, s"Object not a NonTerminalIndexer - ${ClassTag(a.getClass)} ${a.getClass}")
        null
    }
  }
}
