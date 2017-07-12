// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

///import scala.collection.Set
///import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Stack, Queue}
///import scala.util.hashing.MurmurHash3

/**
  */

object Oracle {
///  sealed abstract class AnchoredRule {
///    val span: (Int, Int)
///    val variable: String
///  }
///  case class Terminal(span: (Int, Int), variable: String)
///                      extends AnchoredRule
///  case class Unary(span: (Int, Int), variable: String, arg: String,
///                   child: String) extends AnchoredRule
///  sealed abstract class Binary extends AnchoredRule {
///    val split: Int
///  }
///  case class BinaryNull(span: (Int, Int), variable: String, split: Int,
///                        leftIsParent: Boolean) extends Binary
///  case class BinaryArg(span: (Int, Int), variable: String, split: Int,
///                       child: String, arg: String, leftIsParent: Boolean)
///                      extends Binary

///  /** Converts an AMR into a set of anchored rules.
///    */
///  def simplify(iamr: AMR, log: Boolean = true)
///      : (Boolean, AMR, ArrayBuffer[AnchoredRule]) = {
///    val rawAlignments = HeuristicAligner.alignment(iamr)
///    val wordAlignments = rawAlignments.map{ case (i, c) => (i -> c.variable) }
///    val oamr = removeUnaligned(iamr, wordAlignments)
///    val amr = oamr.getOrElse(AMR(iamr.sentence, iamr.tokens, Concept("a", "amr-unknown")))
///    if (oamr.isEmpty) return (false, amr, ArrayBuffer[AnchoredRule]())
///    val alignedVariables = HashSet() ++ wordAlignments.values
///    val targetSpans = HashMap[String, (Int, Int)]()
///    amr.focus.apply(c =>
///      targetSpans(c.variable) = HeuristicAligner.getSpan(c, rawAlignments))
///    def isInside(cur: (Int, Int), target: (Int, Int)) =
///      target._1 <= cur._1 && cur._1 <= cur._2 && cur._2 <= target._2
///    val chart = HashMap[(Int, Int), ArrayBuffer[(AnchoredRule, Int)]]()
///    val nTokens = amr.tokens.length
///    val concepts = HashMap[String, Concept]()
///    amr.focus.apply(c => concepts(c.variable) = c)

///    // Insert words
///    for (position <- 0 until nTokens) {
///      val span = (position, position + 1)
///      val variable = wordAlignments.getOrElse(position, SNullVar)
///      chart(span) = ArrayBuffer((Terminal(span, variable), 1))
///    }

///    // Build structure
///    for {length <- 1 to nTokens
///         start <- 0 until nTokens
///         end = start + length
///         if end <= nTokens
///         span = (start, end)} {
///      // Binaries
///      for (split <- start + 1 until end) {
///        val toFill = ArrayBuffer[(AnchoredRule, Int)]()
///        for {(left, lscore) <- chart.getOrElse((start, split), Nil)
///             (right, rscore) <- chart.getOrElse((split, end), Nil)} {
///          val tscore = lscore + rscore
///          (left, right) match {
///            // null + null-end -> null
///            case (Terminal(_, SNullVar), Terminal((_, `nTokens`), SNullVar)) =>
///              toFill.append((BinaryNull(span, SNullVar, split, false), tscore))

///            // null + null otherwise not allowed
///            case (Terminal(_, SNullVar), Terminal(_, SNullVar)) =>

///            // a binary null cannot consume right after consuming left
///            case (BinaryNull(_, _, _, false), _) =>

///            // null + thing -> thing
///            case (Terminal(_, SNullVar), _) =>
///              toFill.append((BinaryNull(span, right.variable, split, false), tscore))

///            // thing + null-end -> thing
///            case (_, Terminal((_, `nTokens`), SNullVar)) |
///                 (_, BinaryNull((_, `nTokens`), SNullVar, _, _)) =>
///              toFill.append((BinaryNull(span, left.variable, split, true), tscore))

///            // block all other combining with null to the right
///            case (_, Terminal(_, SNullVar)) |
///                 (_, BinaryNull(_, SNullVar, _, _)) =>

///            case _ =>
///              val lvar = left.variable
///              val rvar = right.variable
///              val lconcept = concepts(lvar)
///              val rconcept = concepts(rvar)
///              def getArg(rule: AnchoredRule) : Option[String] = {
///                rule match {
///                  case Terminal(_, _) => None
///                  case Unary(_, _, _, child) => Some(child)
///                  case BinaryNull(_, _, _, _) => None
///                  case BinaryArg(_, _, _, child, _, _) => Some(child)
///                }
///              }
///              val lpchild = getArg(left)
///              val rpchild = getArg(right)
///              if (lconcept.parents.contains(rconcept) &&
///                  rpchild.forall(_ != lvar) &&
///                  isInside(targetSpans(lvar), (start, split))) {
///                // right -> left
///                val arg = rconcept.args.filter(_._2 == lconcept)(0)._1
///                val nRule = BinaryArg(span, rvar, split, lvar, arg, false)
///                toFill.append((nRule, tscore + 1))
///              } else if (rconcept.parents.contains(lconcept) &&
///                         lpchild.forall(_ != rvar) &&
///                         isInside(targetSpans(rvar), (split, end))) {
///                // left -> right
///                // Constrained so that this cannot be used if left already has
///                // arguments to its left (ie enforce binarisation that
///                // processes right args first)
///                // Some unary constraint
///                (left, right) match {
///                  case (BinaryArg(_, _, _, _, _, false), _) =>
///                  case (Unary(_, _, _, _), _) =>
///                  case _ =>
///                    val arg = lconcept.args.filter(_._2 == rconcept)(0)._1
///                    val nRule = BinaryArg(span, lvar, split, rvar, arg, true)
///                    toFill.append((nRule, tscore + 1))
///                }
///              }
///          }
///        }
///        if (toFill.length > 0) {
///          chart(span) = toFill
///        }
///      }

///      // Unaries
///      val done = HashSet[(String, String)]()
///      def addUnary(cell: ArrayBuffer[(AnchoredRule, Int)], index: Int) : Unit = {
///        val (rule, score) = cell(index)
///        rule match {
///          case r: BinaryNull =>
///          case _ =>
///            val variable = rule.variable
///            if (variable != SNullVar &&
///                isInside(targetSpans(variable), span)) {
///              val cur = concepts(variable)
///              for (parent <- cur.parents) {
///                val pair = (parent.variable, variable)
///                if (! done.contains(pair) &&
///                    ! alignedVariables.contains(parent.variable)) {
///                  val arg = parent.args.filter(_._2.variable == variable)(0)._1
///                  cell.append((Unary(span, parent.variable, arg, variable), score+1))
///                  done += pair
///                }
///              }
///            }
///            if (index < cell.length - 1) addUnary(cell, index + 1)
///        }
///      }
///      chart.get(span).foreach(addUnary(_, 0))
///    }

///    // Determine the chart cells that are part of the complete derivation
///    val inTree = ArrayBuffer[AnchoredRule]()
///    var tree = AMR(amr.sentence, amr.tokens, Concept("a", "amr-unknown"))
///    val success = chart.get((0, nTokens)).exists{
///      topRules =>
///        val (rule, score) = topRules.reduce((a, b) => if (a._2 > b._2) a else b)
///        val nFocus = Concept(rule.variable, concepts(rule.variable).label)
///        inTree.append(BinaryArg((rule.span._1, rule.span._2 + 1), SRoot, rule.span._2, nFocus.variable, SRootArg, false))
///        inTree.append(Terminal((rule.span._2, rule.span._2 + 1), SRoot))
///        val nConceptMap = HashMap(nFocus.variable -> nFocus)
///        val stack = Stack(((0, nTokens), rule.variable))
///        def addConcept(variable: String, parent: String, arg: String) = {
///          val nConcept = nConceptMap.getOrElse(variable,
///                         Concept(variable, concepts(variable).label))
///          val pConcept = nConceptMap(parent)
///          pConcept.args.append((arg, nConcept))
///          nConcept.parents.append(pConcept)
///          nConceptMap(variable) = nConcept
///        }
///        while (! stack.isEmpty) {
///          val (span, cvar) = stack.pop()
///          chart.get(span).foreach{
///            rules =>
///              val (rule, score) =
///                rules.filter(_._1.variable == cvar)
///                     .reduce((a, b) => if (a._2 > b._2) a else b)
///              inTree.append(rule)
///              rule match {
///                case r: Terminal =>
///                case r: Unary =>
///                  stack.push((span, r.child))
///                  addConcept(r.child, r.variable, r.arg)
///                case r: BinaryNull =>
///                  if (r.leftIsParent) {
///                    stack.push(((span._1, r.split), r.variable))
///                    stack.push(((r.split, span._2), SNullVar))
///                  } else {
///                    stack.push(((span._1, r.split), SNullVar))
///                    stack.push(((r.split, span._2), r.variable))
///                  }
///                case r: BinaryArg =>
///                  if (r.leftIsParent) {
///                    stack.push(((span._1, r.split), r.variable))
///                    stack.push(((r.split, span._2), r.child))
///                    addConcept(r.child, r.variable, r.arg)
///                  } else {
///                    stack.push(((span._1, r.split), r.child))
///                    stack.push(((r.split, span._2), r.variable))
///                    addConcept(r.child, r.variable, r.arg)
///                  }
///              }
///          }
///        }
///        tree = AMR(amr.sentence, amr.tokens, nFocus)
///        true
///    }
///    if ((verbosity & VGrammarExtraction) > 0) println{ if (success) "Tree formed" else "Unable to form tree" }
///    if ((verbosity & VGrammarExtraction) > 0) for (rule <- inTree) println(rule)

///    if ((verbosity & VGrammarExtraction) > 0) {
///      // Print the derivation in the chart
///      val longest = 3
///      val inTreeSet = inTree.toSet
///      for (start <- 0 until nTokens) {
///        for (end <- 0 to nTokens) {
///          val span = (start, end)
///          val (content, length) =
///            if (end <= nTokens && chart.contains(span)) {
///              val (text, length) = chart(span).map{
///                  case (r, s) =>
///                    if (inTreeSet.contains(r))
///                      ("\u001b[01;36m"+ r.variable +"\u001b[00m", r.variable.length)
///                    else
///                      (r.variable, r.variable.length)
///                }.reduce[(String, Int)]{
///                  case (a, b) =>
///                    (a._1 + b._1, a._2 + b._2)
///                }
///              if (text == "") (".", 1)
///              else (text, length)
///            } else if (start < end) (".", 1)
///            else ("", 0)
///          val padding = " " * (longest - length + 1)
///          print(padding + content)
///        }
///        println()
///      }

///      // Print each rule in the chart
///      for (start <- 0 until nTokens) {
///        for (end <- 0 to nTokens) {
///          val span = (start, end)
///          chart.get(span).foreach{ case rules =>
///            if (rules.length > 0) print(f"$start%2d $end%2d :")
///            rules.foreach{ case (rule, score) =>
///              val ruletext = rule match {
///                case t: Terminal =>
///                  s"""Terminal(${t.variable})"""
///                case u: Unary =>
///                  s"""Unary(${u.variable} :${u.arg} ${u.child})"""
///                case b: BinaryNull =>
///                  s"""BinaryNull(${b.variable} ${b.leftIsParent})"""
///                case b: BinaryArg =>
///                  s"""BinaryArg(${b.variable} :${b.arg} ${b.child})"""
///              }
///              val text =
///                if (inTreeSet.contains(rule)) "\u001b[01;36m"+ ruletext +"\u001b[00m"
///                else ruletext
///              print("  "+ text)
///            }
///            if (rules.length > 0) println()
///          }
///        }
///      }
///    }
///    (success, tree, inTree)
///  }

  /** Prints the best this parser could produce for this sentence.
    * This means
    *   - Concepts that are not aligned and not internal are removed
    *   - Trees with non-projective edges are excluded
    */
  def amr(amr: AMR, alignedParts: Boolean = false, simplified: Boolean = true) = {
///    assert(alignedParts ^ simplified)
///    if (alignedParts) {
///      val wordAlignments = HeuristicAligner.alignment(amr).map{
///        case (i, c) => (i -> c.variable)
///      }
///      val aligned = removeUnaligned(amr, wordAlignments)
///      (aligned.isDefined,
///       aligned.getOrElse(AMR(amr.sentence, amr.tokens, Concept("a", "amr-unknown"))))
///    } else {
///      val simplified = simplify(amr)
///      (simplified._1, simplified._2)
///    }
  }
}

