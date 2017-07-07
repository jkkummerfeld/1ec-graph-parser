// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue, Stack}

// Note, the arguments are stored as an array because they can be repeated (e.g. multiple ARG0-of)
@SerialVersionUID(1L)
class Concept(
  val variable: String,
  val label: String,
  val args: ArrayBuffer[Tuple2[String, Concept]] = new ArrayBuffer[Tuple2[String, Concept]],
  val parents: ArrayBuffer[Concept] = new ArrayBuffer[Concept]
) extends Serializable {

  // TODO: Rename this to be consistent with the collections library, and
  // implement the relevant trait
  def apply(function: Concept => Unit, processed: HashSet[Concept] = new HashSet[Concept]) : Unit = {
    if (! processed.contains(this)) {
      processed += this
      function(this)
      for (arg <- args)
        arg._2.apply(function, processed)
    }
  }

  def dfsDownApply(function: Concept => Unit) : Unit = apply(function)
  def bfsUpApply(function: Concept => Unit) : Unit = {
    val fringe = new Queue[Concept]
    val seen = new HashSet[Concept]
    fringe += this
    seen += this
    while (! fringe.isEmpty) {
      val next = fringe.dequeue
      function(next)
      for (parent <- next.parents) {
        if (! seen.contains(parent)) {
          fringe += parent
          seen += parent
        }
      }
    }
  }

  def labelToString = {
    if (variable(0) != 'W')
      label
    else if (label(0) == '-' || label == "interrogative" ||
          label == "imperative" || label == "expressive" ||
          (parents.length > 0 &&
           parents(0).label != "name" && parents(0).label != "url-entity" &&
           label(0).isDigit && (! label.contains(':')) && (! label.contains('/'))))
      label
    else
        "\""+ label +"\""
  }

  def toString(processed: HashSet[String], toTree: Boolean) : String = {
    if (processed.contains(variable)) {
      if (toTree) null else variable
    } else {
      processed += variable
      val parts = new ArrayBuffer[String]
      for (arg <- args) {
        val subpart = arg._2.toString(processed, toTree)
        if (subpart != null)
          parts.append("\n      :"+ arg._1 +" "+ (subpart split '\n' mkString "\n      "))
      }
      if (variable(0) == 'W') {
        labelToString
      } else
        "("+ variable +" / "+ label + (parts mkString "") +")"
    }
  }

  def toString(toTree: Boolean) : String = toString(new HashSet[String], toTree)

  override def toString() = toString(false)

  def checkForCycle(seen: HashSet[String]) : Boolean =
    if (seen contains variable) {
      true
    } else {
      seen += variable
      val ans = args.exists(_._2.checkForCycle(seen))
      seen -= variable
      ans
    }

  def containsCycle : Boolean = checkForCycle(HashSet[String]())

  def variableToConceptMap : HashMap[String, Concept] = {
    val concepts = HashMap[String, Concept]()
    apply(c => concepts(c.variable) = c)
    concepts
  }
}

object Concept {
  def apply(variable: String,
            label: String,
            args: ArrayBuffer[Tuple2[String, Concept]] = ArrayBuffer[Tuple2[String, Concept]](),
            parents: ArrayBuffer[Concept] = ArrayBuffer[Concept]()) : Concept =
    new Concept(variable, label, args, parents)

  def fromText(text: String, index: Int,
                      allConcepts: HashMap[String, Concept]
                     ) : (Int, Concept, ArrayBuffer[Tuple2[Concept, Int]]) = {
    // Construct a concept from raw text, where index points into text at the
    // start of this concept.  Note that this will generate new variables to
    // fix repeated use (errors in the corpus) and also to give a standard
    // structure that holds for literals too (which get the variable W[0-9]*)

    def nextMatchingChar(index: Int, predicate: Int => Boolean, inQuote: Boolean = false) : Int =
      if (index == text.length || (!inQuote && predicate(index)))
        index
      else {
        val quoteState = if (text(index) != '"') inQuote else !inQuote
        nextMatchingChar(index + 1, predicate, quoteState)
      }
    def tokenEnd(index: Int) =
      nextMatchingChar(index, x => " \n)".contains(text(x)))
    def nextNonWhitespace(index: Int) =
      nextMatchingChar(index, x => ! " \n".contains(text(x)))

    def getVariable(variable: String, index: Int = 1) : String =
      if (! allConcepts.contains(variable)) variable
      else getVariable(variable(0) + index.toString(), index + 1)

    def getArgument(index: Int) : (String, (Int, Concept, ArrayBuffer[Tuple2[Concept, Int]])) = {
      assert(text(index) == ':')
      val argEnd = tokenEnd(index + 1)
      val argLabel = text.slice(index + 1, argEnd)
      val next = argEnd + 1
      if (text(next) != '(') {
        val textEnd = tokenEnd(next)
        val end = if (text(textEnd) != ' ') textEnd else nextNonWhitespace(textEnd)
        val variable = text.slice(next, textEnd)
        if (allConcepts.contains(variable)) {
          // Return the concept being referenced
          (argLabel, (end, allConcepts(variable), new ArrayBuffer))
        } else {
          if (variable(0) == '"' || variable(0) == '-' || variable(0).isDigit ||
              variable == "imperative" || variable == "expressive" ||
              variable == "interrogative") {
            // Construct a new concept for this string literal
            val nLabel = if (variable(0) == '"') variable.slice(1, variable.length - 1) else variable
            val nVariable = getVariable("W")
            val nConcept = new Concept(nVariable, nLabel)
            allConcepts += (nVariable -> nConcept)
            (argLabel, (end, nConcept, new ArrayBuffer))
          } else {
            // In some cases a variable will be used before it is seen
            val nConcept = new Concept(variable, argLabel)
            (argLabel, (end, nConcept, ArrayBuffer(Tuple2(nConcept, -1))))
          }
        }
      } else {
        // Construct the structure beneath
        (argLabel, fromText(text, next, allConcepts))
      }
    }

    assert(text(index) == '(')

    val variableEnd = tokenEnd(index + 1)
    val variable = getVariable(text.slice(index + 1, variableEnd))

    val labelStart = variableEnd + 3 // skip over the '/'
    val labelEnd = tokenEnd(labelStart)
    val label = text.slice(labelStart, labelEnd)

    var cIndex = nextNonWhitespace(labelEnd)
    val nConcept = new Concept(variable, label)
    allConcepts += (variable -> nConcept)
    val incomplete = new ArrayBuffer[Tuple2[Concept, Int]]
    while (text(cIndex) != ')') {
      val (arg, (end, concept, subIncomplete)) = getArgument(cIndex)
      concept.parents.append(nConcept)
      for (pair <- subIncomplete) {
        if (pair._2 != -1) incomplete += pair
        else incomplete += Tuple2(pair._1, nConcept.args.length)
      }
      if (! subIncomplete.map(_._1).contains(concept))
        nConcept.args += Tuple2(arg, concept)
      cIndex = nextNonWhitespace(end)
    }
    val stillIncomplete = new ArrayBuffer[Tuple2[Concept, Int]]
    for (pair <- incomplete) {
      val subConcept = pair._1
      val subReference = allConcepts.getOrElse(subConcept.variable, null)
      if (subReference != null) {
        subConcept.parents(0).args.insert(pair._2, Tuple2(subConcept.label, subReference))
        subReference.parents.append(subConcept.parents(0))
      } else stillIncomplete += pair
    }
    (cIndex + 1, nConcept, stillIncomplete)
  }

  def fromText(text: String) : Concept =
    fromText(text, 0, new HashMap[String, Concept])._2
}

/** Iterates over all concepts that can be reached by following arguments.
  */
class ConceptIterator(
  concept: Concept
) extends Iterator[Concept] {
  val processed = new HashSet[Concept]
  val stack = Stack[Concept](concept)

  def next : Concept = {
    val ans = stack.pop()
    processed += ans

    def addArgs(c: Concept, i: Int) : Unit =  {
      stack.push(c.args(i)._2)
      if (i > 0) addArgs(c, i - 1)
    }
    if (ans.args.length > 0) addArgs(ans, ans.args.length - 1)

    // Ensure that after next has run the top of the stack is a valid next
    while ((!stack.isEmpty) && processed.contains(stack.head))
      stack.pop()
    ans
  }

  def hasNext : Boolean = ! stack.isEmpty
}

class AMR(
  sentence: String,
  tokens: Vector[String],
  val focus: Concept
) extends Parse(sentence, tokens, Vector[String]()) {
  def toString(toTree: Boolean) = super.toString() +
    (focus.toString(new HashSet[String], toTree)) + "\n"

  override def toString() = toString(false)

  def copy(concept: Concept, toExclude: Set[String],
           done: HashMap[String, Concept] = HashMap[String, Concept](),
           cycleDone: HashSet[String] = HashSet[String]()) : Option[Concept] = {
    val cvar = concept.variable
    if (toExclude.contains(cvar) || cycleDone.contains(cvar)) {
      None
    } else {
      done.get(cvar).fold{
        val nConcept = Concept(cvar, concept.label)
        done(cvar) = nConcept
        cycleDone += cvar
        for (arg <- concept.args) {
          copy(arg._2, toExclude, done, cycleDone) foreach { c =>
            nConcept.args.append((arg._1, c))
            c.parents.append(nConcept)
          }
        }
        cycleDone -= cvar
        Some(nConcept)
      }{
        Some(_)
      }
    }
  }

  /** Starts from concepts aligned to words and follows all non-overlapping
    * paths to the root. Any concept that is not on such a path is removed.
    */
  def removeUnaligned(wordAlignments: HashMap[Int, String]) : Option[AMR] = {
    val concepts = HashMap[String, Concept]()
    focus.apply(c => concepts(c.variable) = c)
    val alignedConcepts = HashSet[String]()
    for (pair <- wordAlignments) {
      val cur = concepts(pair._2)
      val seen = HashSet[String]()
      def DFS(concept: Concept) : Boolean = {
        val variable = concept.variable
        val use =
          if (seen.contains(variable)) {
            false
          } else if (concept.parents.length == 0 || concept == focus) {
            true
          } else {
            seen += variable
            concept.parents.map(DFS).reduce(_ || _)
          }
        if (use) alignedConcepts += variable
        else seen -= variable
        use
      }
      DFS(cur)
    }
    val unaligned = concepts.keySet.diff(alignedConcepts)

    copy(focus, unaligned) match {
      case Some(f) =>
        Some(AMR(sentence, tokens, f))
      case None =>
        System.err.println("No Alignment for:\n"+ toString())
        None
    }
  }

}

object AMR {
  def apply(sentence: String, tokens: Vector[String], focus: Concept) : AMR =
    new AMR(sentence, tokens, focus)

  def fromText(sentence: String, tokens: Vector[String], text: String) : AMR =
    new AMR(sentence, tokens, Concept.fromText(text))

  def findMatching(amr: AMR, predicate: Concept => Boolean,
                   matching: ArrayBuffer[Concept]) : Unit =
    amr.focus.apply(x => if (predicate(x)) matching.append(x))

  def measureDistances(amr: AMR) = {
    def measure(concept: Concept) = {
      if (concept.parents.length > 1) {
        println(concept.label)
        // Print what types of arguments this is
        val args = for {parent <- concept.parents
                        child <- parent.args
                        if child._2 == concept} yield child._1
        println("multi-parent-args "+ (args.sorted.mkString(" ")))

        // Work out if these are implicit or explicit arguments
        def getBottom(concept: Concept) : Concept =
          if (concept.args.length != 1) concept
          else if (concept.args(0)._1.endsWith("-of")) concept
          else getBottom(concept.args(0)._2)

        val bottom = getBottom(concept)
        val predicateRegEx = "^(.*)-[0-9]*$".r
        val bottomLabel = bottom.label match {
          case predicateRegEx(group) => group
          case _ => bottom.label
        }
        // How many words could match this?
        var matches = 0
        for (word <- amr.tokens)
          if (word.toLowerCase().startsWith(bottomLabel.toLowerCase()))
            matches += 1
        val explicit = (matches == concept.parents.length)

        // Find LCA
        val parents = concept.parents.length
        val scores = new HashMap[Concept, Int] withDefaultValue 0
        var lca: Concept = null
        def updateScores(concept: Concept) : Unit =
          if (lca == null) {
            scores(concept) += 1
            if (scores(concept) == parents)
              lca = concept
          }
        for (parent <- concept.parents)
          parent.bfsUpApply(updateScores(_))

        // Work out distances
        def getDist(concept: Concept, lca: Concept) : Int = {
          val fringe = new Queue[Tuple2[Concept, Int]]
          val seen = new HashSet[Concept]
          fringe += new Tuple2(concept, 0)
          seen += concept
          while (! fringe.isEmpty) {
            val next = fringe.dequeue
            if (next._1 == lca)
              return next._2
            for (tparent <- next._1.parents) {
              if (! seen.contains(tparent)) {
                seen += tparent
                fringe += Tuple2(tparent, next._2 + 1)
              }
            }
          }
          -1
        }
        if (lca == null) println(lca)
        else println(lca.variable)
        print(s"""${explicit} """)
        for (parent <- concept.parents) {
          print(getDist(parent, lca))
          print(" ")
        }
        println()
      }
    }
    amr.focus.apply(measure)
  }
}

