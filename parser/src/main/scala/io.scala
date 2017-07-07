// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.io.Source
import java.io.PrintWriter
import java.io.StringWriter
import java.io.File
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue}

object Formalism extends Enumeration {
  type Formalism = Value
  val AMR, PSG, DEP = Value
}

object DEPFormat extends Enumeration {
  type DEPFormat = Value
  val Conllx, Conllu = Value
}

object Log {
  // TODO: Add indentation and timing
  var logFile = System.out

  val VParseExtraction = 1
  val VGrammarExtraction = 2
  val VDetailedStats = 4
  val VMaking = 8
  val VParseChart = 16
  val VAdding = 32
  val VFeatures = 64
  val VCellSizes = 128
  val VTrace = 256
  val VMarginals = 512
  val VBeamStats = 1024
  val VParseChartWithGold = 2048
  val VMemoryStats = 4096

  var toFlush = ArrayBuffer[String]()

  @inline final def check(verboseFlag: Int, verbosity: Int) =
    verboseFlag == 0 || (verbosity & verboseFlag) > 0

  def logLater(obj: => Any = "") = synchronized {
    toFlush.append(obj.toString)
  }
  def flush = synchronized {
    for (line <- toFlush)
      logFile.print(line)
    toFlush.clear
  }

  def log(obj: => Any = "") = synchronized {
    logFile.print(obj.toString)
  }

  def logln(obj: => Any = "") = synchronized {
    logFile.println(obj.toString)
  }

  @inline final def logToErr(obj: => Any = "") = synchronized {
    System.err.println(obj.toString)
  }

  @inline final def stacktrace(length: Int = Int.MaxValue) = synchronized {
    try {
      throw new Exception
    } catch {
      case e: Exception =>
        val sw = new StringWriter();
        val pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        val lines = sw.toString().split("\n")
        logln(lines.slice(2, (lines.length - 20) min (length + 2)).mkString("\n") +"\n");
    }
  }
}

object ObjectWriter {
  def save[T <: Serializable](obj: T, filename: String) = {
    val file = new BufferedOutputStream(new FileOutputStream(filename))
    val out = new ObjectOutputStream(new GZIPOutputStream(file))
    out.writeObject(obj)
    out.close()
  }
}

object ObjectReader {
  def load(filename: String): Object = {
    val file = new BufferedInputStream(new FileInputStream(filename))
    val in = new ObjectInputStream(new GZIPInputStream(file))
    val obj = in.readObject()
    in.close()
    obj
  }
}

class PSGReader(
  val srcFile: String
) extends Iterator[(Int, psg.Graph)] {
  val src = Source.fromFile(srcFile).getLines

  def hasNext : Boolean = src.hasNext

  def next : (Int, psg.Graph) = {
    val lines = new ArrayBuffer[String]
    var done = false
    var sentID = -1
    while (src.hasNext && ! done) {
      val line = src.next()
      if (Config.printInput) println(line.trim)
      if (line.length == 0) done = true
      else if (line(0) != '#') lines.append(line)
      else if (line.startsWith("# SentID")) {
        sentID = line.trim.split(" ")(2).toInt
      }
    }
    (sentID, psg.Graph.fromText(lines))
  }
}

class DEPReader(
  val srcFile: String,
  val format: DEPFormat.Value = DEPFormat.Conllx
) extends Iterator[(Int, DepParse)] {
  val src = Source.fromFile(srcFile).getLines

  def hasNext : Boolean = src.hasNext

  def next : (Int, DepParse) = {
    val lines = new ArrayBuffer[String]
    var sentID = -1
    var done = false
    while (src.hasNext && ! done) {
      val line = src.next()
      if (line.startsWith("# SentID")) {
        sentID = line.trim.split(" ")(2).toInt
      } else if (line.length > 0) lines.append(line)
      else done = true
    }
    format match {
      case DEPFormat.Conllx => (sentID, DepParse.readConllx(lines))
      case DEPFormat.Conllu => (sentID, DepParse.readConllu(lines))
    }
  }
}

class AMRReader(
  val srcFile: String,
  val splitLiterals: Boolean = true
) extends Iterator[(Int, AMR)] {
  val src = Source.fromFile(srcFile).getLines

  def hasNext : Boolean = src.hasNext

  def next : (Int, AMR) = {
    val lines = new ArrayBuffer[String]
    var sentID = -1
    var sentence = ""
    var done = false
    while (src.hasNext && ! done) {
      val line = src.next()
      if (line.startsWith("# ::snt ")) sentence = line.stripPrefix("# ::snt ")
      else if (! line.startsWith("#")) {
        if (line.startsWith("# SentID")) {
          sentID = line.trim.split(" ")(2).toInt
        } else if (line.length > 0) lines.append(line)
        else if (lines.length > 0) done = true
      }
    }

    val literals = new HashSet[String]
    for (line <- lines) {
      if (line.contains('"')) {
        val words = line.split("\"")
        for (i <- 1 until words.length by 2)
          literals += words(i).toLowerCase()
      }
    }
    val tokens =
      if (splitLiterals) Tokeniser.tokenise(sentence)
      else Tokeniser.tokenise(sentence, literals)

    (sentID, AMR.fromText(sentence, tokens.toVector, lines mkString " "))
  }
}

class ParseWriter(
  val destFile: String
) {
  val dest = new PrintWriter(new File(destFile))
  def write(parse: Parse) =
    parse match {
      case d: DepParse => dest.println(d.toConllx)
      case a: AMR => dest.println(a)
      case p: psg.Graph => dest.println(p)
      case _ =>
    }
  def close() = dest.close()
}

object AMRDotWriter {
  // Methods for generating output in the format of graphviz dot
  def dotNode(variable: String, label: String, extra: String = "") =
    s"""  ${variable} [${extra}label="${label.split('"').mkString("\\\"")}"]"""

  def dotEdge(from: String, to: String, label: String, reverseArgOf: Boolean,
              extra: String = "") =
    if (reverseArgOf && label.endsWith("-of"))
      s"""  ${to} -> ${from} [${extra}label="${label}_r"]"""
    else
      s"""  ${from} -> ${to} [${extra}label="${label}"]"""

  def dotSentence(tokens: Vector[String]) = {
    var length = 0
    val ans = for (word <- tokens) yield {
      length += word.length
      if (length > 20) {
        length = 0
        word + '\n'
      } else {
        word
      }
    }
    ans mkString " " split "\"" mkString "'"
  }

  def dotWrap(tokens: Vector[String],
              nodes: ArrayBuffer[String],
              edges: ArrayBuffer[String]) = {
    "digraph G {\n"+
    "  SENTENCE [label=\""+ dotSentence(tokens) +"\"]\n"+
    (nodes mkString "\n") +"\n"+
    (edges mkString "\n") +"\n"+
    "}"
  }

  def toDotGraph(amr: AMR, reverseArgOf: Boolean) : String = {
    val nodes = new ArrayBuffer[String]()
    val edges = new ArrayBuffer[String]()

    def conceptToStrings(concept: Concept) = {
      nodes.append(dotNode(concept.variable, concept.label))
      for (arg <- concept.args)
        edges.append(dotEdge(concept.variable, arg._2.variable, arg._1, reverseArgOf))
    }
    amr.focus.apply(conceptToStrings(_))
    dotWrap(amr.tokens, nodes, edges)
  }

  def amrComparison(gold: AMR, other: AMR, reverseArgOf: Boolean) : String = {
    // Find exact common concepts
    def addConcept(concept: Concept,
                   map: HashMap[String, ArrayBuffer[Concept]]) = {
      map.getOrElseUpdate(concept.label, new ArrayBuffer[Concept]) += concept
    }
    val goldConcepts = new HashMap[String, ArrayBuffer[Concept]]
    gold.focus.apply(addConcept(_: Concept, goldConcepts))
    val otherConcepts = new HashMap[String, ArrayBuffer[Concept]]
    other.focus.apply(addConcept(_: Concept, otherConcepts))
    val conceptMap = new HashMap[Concept, Concept]
    val fringe = new Queue[Concept]
    for (pair <- goldConcepts
         if pair._2.length == 1 &&
            otherConcepts.contains(pair._1) &&
            otherConcepts(pair._1).length == 1
        ) {
      conceptMap += (pair._2(0) -> otherConcepts(pair._1)(0))
      conceptMap += (otherConcepts(pair._1)(0) -> pair._2(0))
      fringe += pair._2(0)
    }

    // BFS to find other common nodes
    while (! fringe.isEmpty) {
      val cur = fringe.dequeue
      val omatch = conceptMap(cur)
      for (gArg <- cur.args
           if ! conceptMap.contains(gArg._2);
           oArg <- omatch.args
           if oArg._1 == gArg._1 &&
              oArg._2.label == gArg._2.label &&
              ! conceptMap.contains(gArg._2) // Do not add it twice
          ) {
        conceptMap += (gArg._2 -> oArg._2)
        conceptMap += (oArg._2 -> gArg._2)
        fringe += gArg._2
      }
    }

    // Identify the common edges
    val goldEdges = new HashSet[Tuple3[Concept, String, Concept]]
    val commonEdges = new HashSet[Tuple3[Concept, String, Concept]]
    def getCommonEdges(concept: Concept, isGold: Boolean = false) = {
      if (isGold) {
        for (arg <- concept.args)
          goldEdges += new Tuple3[Concept, String, Concept](concept, arg._1, arg._2)
      } else {
        for (arg <- concept.args
             if conceptMap.contains(arg._2) &&
                conceptMap.contains(concept)) {
          val edge = new Tuple3[Concept, String, Concept](conceptMap(concept), arg._1, conceptMap(arg._2))
          if (goldEdges.contains(edge)) {
            commonEdges += edge
            commonEdges += new Tuple3[Concept, String, Concept](concept, arg._1, arg._2)
          }
        }
      }
    }
    gold.focus.apply(getCommonEdges(_, true))
    other.focus.apply(getCommonEdges(_))

    // Print to coloured dot
    val nodes = new ArrayBuffer[String]()
    val edges = new ArrayBuffer[String]()
    val commonColour = "black"
    val extraColour = "red"
    val missingColour = "blue"
    def conceptToStrings(concept: Concept, isGold: Boolean = false) = {
      val commonConcept = conceptMap.contains(concept)

      // Create this node
      val extra =
        if (commonConcept) s"""color="${commonColour}" """
        else if (isGold) s"""color="${missingColour}" """
        else s"""color="${extraColour}" """

      val variable =
        if (isGold) concept.variable
        else if (commonConcept) conceptMap(concept).variable
        else 'o' + concept.variable

      if (isGold || (! commonConcept))
        nodes.append(dotNode(variable, concept.label, extra))

      // Create edges to its arguments
      for (arg <- concept.args) {
        val commonArgument = conceptMap.contains(arg._2)

        val ovariable =
          if (isGold) arg._2.variable
          else if (commonArgument) conceptMap(arg._2).variable
          else 'o' + arg._2.variable

        val edge = new Tuple3[Concept, String, Concept](concept, arg._1, arg._2)
        val commonEdge = commonEdges.contains(edge)

        val extra =
          if (commonEdge) s"""color="${commonColour}" """
          else if (isGold) s"""color="${missingColour}" """
          else s"""color="${extraColour}" """

        if (isGold || (! commonEdge))
          edges.append(dotEdge(variable, ovariable, arg._1, reverseArgOf, extra))
      }
    }
    gold.focus.apply(conceptToStrings(_, true))
    other.focus.apply(conceptToStrings(_))
    dotWrap(gold.tokens, nodes, edges)
  }

  def toAlignedDotGraph(amr: AMR, optAlignment: Option[HashMap[Int, Concept]], reverseArgOf: Boolean) : String = {
    val alignment = optAlignment match {
      case Some(a) => a
      case None => HeuristicAligner.alignment(amr)
    }
    val seen = new HashSet[Concept]
    for (i <- 0 until amr.tokens.length) {
      print("# "+ amr.tokens(i) + " "*(20 - amr.tokens(i).length))
      if (alignment.contains(i)) {
        print(" "+ alignment(i).label + " (" + alignment(i).variable +")")
        seen += alignment(i)
      }
      println()
    }
    def printUnseen(concept: Concept) =
      if (! seen.contains(concept))
        println("# Unaligned:           "+ concept.label + " (" + concept.variable +")")
    amr.focus.apply(printUnseen(_))

    val extraNodes = new ArrayBuffer[String]
    val extraEdges = new ArrayBuffer[String]
    val otherEdges = new ArrayBuffer[String]
    for (i <- 0 until amr.tokens.length) {
      val token = amr.tokens(i) match {
        case "\"" => "\\\""
        case a => a
      }
      extraNodes += s"""    word${i} [color="grey" label="${token}"]\n"""
      if (i != 0)
        extraEdges += s"""    word${i - 1} -> word${i} [style="invis"]\n"""
      if (alignment.contains(i))
        otherEdges += s"""  ${alignment(i).variable} -> word${i} [color="grey"]\n"""
    }

    val nodes = new ArrayBuffer[String]()
    val edges = new ArrayBuffer[String]()

    def conceptToStrings(concept: Concept) = {
      nodes.append(dotNode(concept.variable, concept.label))
      for (arg <- concept.args)
        edges.append(dotEdge(concept.variable, arg._2.variable, arg._1, reverseArgOf))
    }
    amr.focus.apply(conceptToStrings(_))

    "digraph G {\n"+
    "  rankdir=TB\n"+
    "  subgraph {\n"+
    "    rank = sink\n"+
    (extraNodes mkString "") +
    (extraEdges mkString "") +
    "  }\n"+
    "  SENTENCE [label=\""+ dotSentence(amr.tokens) +"\"]\n"+
    (nodes mkString "\n") +"\n"+
    (edges mkString "\n") +"\n"+
    (otherEdges mkString "") +
    "}"
  }

  def write(
    destFile: String, amr: AMR, other: AMR = null, aligned: Boolean = true,
    optAlignment: Option[HashMap[Int, Concept]] = None,
    reverseArgOf: Boolean = true
  ) = {
    val dest = new PrintWriter(new File(destFile))
    val text = (other, aligned) match {
      case (null, true) => toAlignedDotGraph(amr, optAlignment, reverseArgOf)
      case (null, false) => toDotGraph(amr, reverseArgOf)
      case (_, true) => amrComparison(amr, other, reverseArgOf) // TODO: Implement this
      case (_, false) => amrComparison(amr, other, reverseArgOf)
    }
    dest.write(text)
    dest.close()
  }
}
