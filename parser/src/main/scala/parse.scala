// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

abstract class Parse(
  val sentence: String,
  val tokens: Vector[String],
  val tags: Vector[String]
) extends Serializable {
  override def toString() =
    "# Sentence "+ sentence +"\n"+
    "# Tokens   "+ (tokens mkString " ") +"\n"+
    "# Tags     "+ (tags mkString " ") +"\n"

  def compareString(
    other: Parse, tagsOnly: Boolean = false, edgesOnly: Boolean = false, tracesOnly: Boolean = false
  ) = "Compare not implemented for these two parses"
}

