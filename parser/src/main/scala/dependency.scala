// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue, Stack}

class Dependency(
  val child: Int,
  val parent: Int,
  val label: String
) {
  override def toString() = s"$child --${label}-> $parent"
}

object Dependency {
  def apply(child: Int, parent: Int, label: String) =
    new Dependency(child, parent, label)
}

class DepParse(
  sentence: String,
  tokens: Vector[String],
  tags: Vector[String],
  val deps: Vector[Dependency]
) extends Parse(sentence, tokens, tags) {
  override def toString() = super.toString() +
    s"# Graph   Tree $isTree   Projective $isProjective   1EC $is1EC\n" +
    (deps mkString "\n") +"\n"

  def isTree = {
    val children = HashSet[Int]()
    val single_parent =
      deps.forall{ dep =>
        if (children.contains(dep.child)) false
        else {
          children.add(dep.child)
          true
        }
      }
    single_parent && tokens.length == deps.length
  }

  /** For projectivity, check that no pair of dependencies cross
    */
  def isProjective = {
    ! deps.exists{ dep1 =>
      val (l1, r1) =
        if (dep1.child < dep1.parent) (dep1.child, dep1.parent)
        else (dep1.parent, dep1.child)
      deps.exists{ dep2 =>
        val (l2, r2) =
          if (dep2.child < dep2.parent) (dep2.child, dep2.parent)
          else (dep2.parent, dep2.child)
        (l1 < l2 && l2 < r1 && r1 < r2) ||
        (l2 < l1 && l1 < r2 && r2 < r1)
      }
    }
  }

  /** For one-endpoint crossing, check that for each edge that is crossed, all
    * the edges crossing it share an endpoint.
    */
  def is1EC = {
    val endpoints = HashMap[(Int, Int), (HashSet[Int], HashSet[Int])]()
    for (dep1 <- deps) {
      val e1 =
        if (dep1.child < dep1.parent) (dep1.child, dep1.parent)
        else (dep1.parent, dep1.child)
      for (dep2 <- deps) {
        val e2 =
          if (dep2.child < dep2.parent) (dep2.child, dep2.parent)
          else (dep2.parent, dep2.child)
        if (e1._1 < e2._1 && e2._1 < e1._2 && e1._2 < e2._2) {
          if (! endpoints.contains(e1)) endpoints(e1) = (HashSet[Int](), HashSet[Int]())
          endpoints(e1)._1.add(e2._1)
          endpoints(e1)._2.add(e2._2)
        } else if (e2._1 < e1._1 && e1._1 < e2._2 && e2._2 < e1._2) {
          if (! endpoints.contains(e1)) endpoints(e1) = (HashSet[Int](), HashSet[Int]())
          endpoints(e1)._1.add(e2._2)
          endpoints(e1)._2.add(e2._1)
        }
      }
    }
    endpoints.forall{ case (k, v) =>
      v._1.size <= 1 || v._2.size <= 1
    }
  }

  def toConllx = {
    /* Example:
    1 Ms. _ NNP _ _ 2 TITLE _ _
    2 Haag  _ NNP _ _ 3 SBJ _ _
    3 plays _ VBZ _ _ 0 ROOT  _ _
    4 Elianti _ NNP _ _ 3 OBJ _ _
    5 . _ . _ _ 3 P _ _
    */
    val depMap = HashMap[Int, Dependency]()
    for (dep <- deps) depMap(dep.child) = dep

    var ans = ""
    for (i <- 0 until tokens.length) {
      // TODO: Fix terrible backup
      val dep =
        if (depMap.contains(i)) depMap(i)
        else if (i == 0) Dependency(i+1, i, "ROOTDEP")
        else Dependency(i+1, i, "SBJ")
      val tag =
        if (tags.length != 0) tags(i)
        else "NNP"
      val parent =
        if (dep.parent == tokens.length) 0
        else dep.parent + 1
      val token = tokens(i)
      ans += s"${i+1} $token _ $tag _ _ $parent ${dep.label} _ _\n"
    }
    ans
  }
}

object DepParse {
  def apply(sentence: String, tokens: Vector[String], tags: Vector[String],
            deps: Vector[Dependency]) =
    new DepParse(sentence, tokens, tags, deps)

  def readConllx(lines: ArrayBuffer[String]) = {
    /* Example:
    1 Ms. _ NNP _ _ 2 TITLE _ _
    2 Haag  _ NNP _ _ 3 SBJ _ _
    3 plays _ VBZ _ _ 0 ROOT  _ _
    4 Elianti _ NNP _ _ 3 OBJ _ _
    5 . _ . _ _ 3 P _ _
    */
    val tokens = ArrayBuffer[String]()
    val deps = ArrayBuffer[Dependency]()
    val tags = ArrayBuffer[String]()
    for (line <- lines) {
      val fields = line.split('\t')
      val pos = fields(0).toInt - 1
      tokens.append(fields(1))
      tags.append(fields(3))

      val parent =
        if (fields(6) == "0") lines.length
        else fields(6).toInt - 1
      val label =
        if (fields(6) == "0") "ROOTDEP"
        else fields(7)
      deps.append(Dependency(pos, parent, label))
    }

    apply(tokens mkString " ", tokens.toVector, tags.toVector, deps.toVector)
  }

  def readConllu(lines: ArrayBuffer[String]) = readConllx(lines)
}

