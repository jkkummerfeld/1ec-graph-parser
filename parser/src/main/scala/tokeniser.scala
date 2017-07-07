// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{ArrayBuffer, HashSet}

object Tokeniser {
  /** Split tokens on:
    *  - Leading punctuation
    *  - Trailing punctuation (except for acronyms)
    *  - Possessives
    *  - '-' unless they are in the provided literal set
    * TODO:
    * Handle "food...garbage." as "food ... garbage ."
    * Handle 't
    * "Holy S***!"
    * YYYYMMDD
    * speech\thought,
    * develop,you
    */
  def tokenise(sentence: String, literals: HashSet[String] = new HashSet[String]) = {
    val tokens = new ArrayBuffer[String]
    for (token <- sentence.split(" ")) {
      var ctoken = token

      // Handle possessives
      val poss = token.endsWith("'s") && token.length > 2
      if (poss) ctoken = ctoken.stripSuffix("'s")

      // Handle leading and trailing punctuation
      val punctExpr = "([^a-zA-Z0-9]+)".r
      punctExpr.findPrefixOf(ctoken) match {
        case Some(punct) if punct.length != ctoken.length && ctoken != "'s" =>
          tokens.append(punct)
          ctoken = ctoken.stripPrefix(punct)
        case _ =>
      }
      val toAdd = punctExpr.findPrefixOf(ctoken.reverse) match {
        case Some(punct) if punct.length != ctoken.length =>
          val suffix = punct.reverse
          // Handle acronyms, 'U.S.' should not be split
          if (! ctoken.stripSuffix(suffix).contains(suffix)) {
            ctoken = ctoken.stripSuffix(punct.reverse)
            Some(suffix)
          } else None
        case _ =>
          None
      }

      if (literals.contains(ctoken.toLowerCase()) || ctoken.split("-").length < 2)
        tokens.append(ctoken)
      else
        tokens ++= ctoken.split("-").mkString(" - ").split(" ")

      for (suffix <- toAdd) tokens.append(suffix)

      // Add the possessive
      if (poss)
        tokens.append("'s")
    }
    tokens
  }
}

