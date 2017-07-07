// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{HashMap, HashSet}

/** Evaluation metrics
  */
object Evaluate {
  def apply(auto: Parse, gold: Parse) : Vector[(Int, Int, String)] =
    auto match {
      case a : DepParse =>
        gold match {
          case g : DepParse => depScore(a, g)
          case _ => throw new IllegalArgumentException
        }
      case a : psg.Graph =>
        gold match {
          case g : psg.Graph => psgScore(a, g)
          case _ => throw new IllegalArgumentException
        }
      case a : AMR =>
        gold match {
          case g : AMR => Vector()
          case _ => throw new IllegalArgumentException
        }
      case _ => throw new IllegalArgumentException
    }

  def f(p: Double, r: Double) = {
    if (p + r < 1e-5) 0.0
    else 2 * p * r / (p + r)
  }

  def psgScore(auto: psg.Graph, gold: psg.Graph) = {
    var spine_match = 0
    for ((spine, index) <- auto.spines.zipWithIndex)
      if (gold.spines(index) == spine) spine_match += 1

    var edge_match = 0
    val doneG = new HashSet[psg.Edge]
    for (aedge <- auto.edges) {
      for (gedge <- gold.edges) {
        if (! doneG.contains(gedge) &&
            aedge.equals(gedge, true)) {
          edge_match += 1
          doneG.add(gedge)
        }
      }
    }

    var labeled_edge_match = 0
    for {aedge <- auto.edges
         gedge <- gold.edges
         if aedge == gedge
    } {
      labeled_edge_match += 1
    }

    val goldSpans = gold.toSpans
    val autoSpans = auto.toSpans
    val matchingSpans = autoSpans.intersect(goldSpans).size

    val goldTriples = gold.toTriples.toSet
    val autoTriples = auto.toTriples.toSet
///    for (triple <- gold.toTriples) println(s"g: $triple")
///    for (triple <- auto.toTriples) println(s"a: $triple")

    val nParents = new HashMap[Int, Int]
    for (triple <- goldTriples) {
      val src = triple._1
      if (! nParents.contains(src)) nParents(src) = 0
      nParents(src) += 1
    }
    val multiParentsGold = goldTriples.filter(v => nParents(v._1) > 1)
    val multiParentsAuto = autoTriples.filter(v => nParents(v._1) > 1)

    val matchingTriples = autoTriples.intersect(goldTriples).size
    val matchingTriplesMulti = multiParentsAuto.intersect(multiParentsGold).size

    Vector(
      (spine_match, gold.spines.length, "Spines"),
      (edge_match, gold.edges.length, "Unlabeled Edges"),
      (labeled_edge_match, gold.edges.length, "Labeled Edges"),
      (matchingSpans, autoSpans.size, "Span P"),
      (matchingSpans, goldSpans.size, "Span R"),
      (matchingTriples, autoTriples.size, "Triples P"),
      (matchingTriples, goldTriples.size, "Triples R"),
      (matchingTriplesMulti, multiParentsAuto.size, "Multi-triples P"),
      (matchingTriplesMulti, multiParentsGold.size, "Multi-triples R")
    )
  }

  def depScore(auto: DepParse, gold: DepParse) = {
    // Labeled
    val auto_deps = HashSet[(Int, Int, String)]()
    for (dep <- auto.deps) auto_deps.add((dep.child, dep.parent, dep.label))
    val gold_deps = HashSet[(Int, Int, String)]()
    for (dep <- gold.deps) gold_deps.add((dep.child, dep.parent, dep.label))
    val matching = auto_deps.intersect(gold_deps).size
    val ldeps = (matching, auto_deps.size, "Labeled Deps")

    // Unlabeled
    auto_deps.clear()
    gold_deps.clear()
    for (dep <- auto.deps) auto_deps.add((dep.child, dep.parent, ""))
    for (dep <- gold.deps) gold_deps.add((dep.child, dep.parent, ""))
    val umatching = auto_deps.intersect(gold_deps).size
    val udeps = (umatching, auto_deps.size, "Unlabeled Deps")
    Vector(udeps, ldeps)
  }
}

