// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser.psg

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue}

import edu.berkeley.nlp.graphparser.Config
import edu.berkeley.nlp.graphparser.Parse

/** A single node in a parse. The instance is defined relative to a spine,
 * since it coontains the count and nullDepth (which depend on the surrounding
 * structure).
 *
 * @param symbol the core non-terminal string (e.g. NP)
 * @param functions any function tags (e.g. -LOC-CLR)
 * @param count to distinguish repetition of the same node in a spine
 * @param nullDepth the depth within null spans (e.g. 0 for non-null)
 */
@SerialVersionUID(1L)
class Node(
  val symbol: String,
  val functions: Vector[String],
  val count: Int,
  val nullDepth: Int
) extends Serializable {
  @transient lazy val isNull = nullDepth > 0

  @transient lazy val symbolToString = symbol + functions.map("-"+ _).mkString("")

  override def toString() = s"$symbolToString,$nullDepth.$count"

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Node]

  def equals(other: Any, ignoreNullDepth: Boolean): Boolean =
    other match {
      case that: Node =>
        (that canEqual this) &&
        that.symbol == symbol &&
        that.functions == functions &&
        that.count == count &&
        (ignoreNullDepth || that.nullDepth == nullDepth)
      case _ => false
    }

  override def equals(other: Any): Boolean = equals(other, false)
}

object Node {
  val None = new Node("NO_TAG", Vector[String](), 0, 0)
  val Root = new Node("ROOT", Vector[String](), 0, 0)

  def apply(
    symbol: String, functions: Vector[String], count: Int, nullDepth: Int
  ) = new Node(symbol, functions, count, nullDepth)
}

/** A single edge in a parse.
 *
 * @param src index of the child word/spine
 * @param srcNode symbol of the child
 * @param target index of the parent word/spine
 * @param targetNode symbol of the parent
 * @param trace the trace type on this edge
 */
@SerialVersionUID(1L)
class Edge(
  val src: Int,
  val srcNode: Node,
  val target: Int,
  val targetNode: Node,
  val trace: String
) extends Serializable {
  // Non-traces may not link to/from null elements
  require(trace != "_" || (! srcNode.isNull && ! targetNode.isNull))

  @transient lazy val isTrace = trace != "_"
  @transient lazy val isStructural = ! isTrace
  override def toString() = {
    val targetString = targetNode.symbolToString
    if (isStructural) s"$target ${targetString}_${targetNode.count}"
    else {
      val targetNull = targetNode.isNull.toString()(0).toUpper
      val srcNull = srcNode.isNull.toString()(0).toUpper
      val srcString = srcNode.symbolToString

      target.toString +
      s" ${targetString}_${targetNode.count} $targetNull" +
      s" ${srcString}_${srcNode.count} $srcNull" +
      s" $trace"
    }
  }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Edge]

  override def equals(other: Any): Boolean = equals(other, false)

  def equals(other: Any, ignoreLabel: Boolean): Boolean =
    other match {
      case that: Edge =>
        (that canEqual this) &&
        that.src == src &&
        that.target == target &&
        (ignoreLabel || that.srcNode == srcNode) &&
        (ignoreLabel || that.targetNode == targetNode) &&
        (ignoreLabel || that.trace == trace)
      case _ => false
    }

  def crosses(other: Edge) = {
    val thisLeft = src.min(target)
    val thisRight = src.max(target)
    val thatLeft = other.src.min(other.target)
    val thatRight = other.src.max(other.target)

    (thisLeft < thatLeft &&
     thatLeft < thisRight &&
     thisRight < thatRight) ||
    (thatLeft < thisLeft &&
     thisLeft < thatRight &&
     thatRight < thisRight)
  }
}

object Edge {
  def apply(
    src: Int, srcNode: Node, target: Int, targetNode: Node, trace: String
  ) = new Edge(src, srcNode, target, targetNode, trace)
}

/*
 * The traces vector covers self-edges: edges that go from one point in the
 * spine to another.
 */
@SerialVersionUID(1L)
class Spine(
  val nodes: Vector[Node],
  val traces: Vector[Edge]
) extends Serializable {
  def withTrace(trace: Edge) = new Spine(nodes, traces :+ trace)

  def posInNodes(
    symbol: String, functions: Vector[String], count: Int, isNull: Boolean
  ) : Int = {
    if (nodes.length == 0) -1
    else nodes.indexWhere{ s =>
      s.symbol == symbol &&
      s.functions == functions &&
      s.count == count &&
      s.isNull == isNull
    }
  }
  def posInNodes(node: Node) : Int =
    posInNodes(node.symbol, node.functions, node.count, node.isNull)

  def getMatchingNode(
    symbol: String, functions: Vector[String], count: Int, isNull: Boolean
  ) = {
    val pos = posInNodes(symbol, functions, count, isNull)
    if (pos == -1) null
    else nodes(pos)
  }

  def getMatchingNode(node: Node) : Node =
    getMatchingNode(node.symbol, node.functions, node.count, node.isNull)

  @transient lazy val nodesToString = {
    if (nodes.length == 0) "_"
    else {
      var depth = 0
      var ans = ""
      for (node <- nodes) {
        // Add the closing brackets for the previous node
        if (depth > 0) {
          for (i <- node.nullDepth until depth) ans += ")"
          if (node.nullDepth > 0 && node.nullDepth <= depth) ans += ")"
        }
        if (ans.length > 0) ans += "_"
        if (node.nullDepth > 0) ans += "("
        ans += node.symbolToString
        depth = node.nullDepth
      }
      ans
    }
  }

  @transient lazy val tracesToString : String =
    traces.map{ t =>
      s"${t.srcNode} ${t.targetNode} ${t.trace}"
    }.mkString(" ")

  override def toString() : String =
    nodesToString +" "+ tracesToString

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Spine]

  override def equals(other: Any): Boolean =
    equals(other, false)

  def equals(other: Any, ignoreTraceEdges: Boolean): Boolean =
    other match {
      case that: Spine =>
        (that canEqual this) &&
        that.nodes == nodes &&
        (ignoreTraceEdges || that.traces == traces)
      case _ => false
    }
}

object Spine {
  def fromText(text: String) = {
    // Example:
    // VP_VP_(NP-SBJ_(*))_S_VP_VP_S
    val counts = HashMap[(String, Vector[String], Boolean), Int]()
    val nodes = ArrayBuffer[Node]()
    if (text != "_") {
      var depth = 0
      for (part <- text.split("_")) {
        // Track depth of traces
        if (part.contains("(")) depth += 1

        if (Config.keepTraces || depth == 0) {
          val symbolParts = part.filter(c => c != '(' && c != ')').split("-")
          val symbol = symbolParts.head
          val functions = symbolParts.tail.toVector
          val key = (symbol, functions, depth > 0)
          val count = counts.getOrElse(key, -1) + 1
          counts(key) = count
          nodes.append(new Node(symbol, functions, count, depth))
        }

        // There can only be one open bracket, but there could be multiple
        // close brackets. For example:
        // (NP-SBJ_(*))
        for (char <- part) if (char == ')') depth -= 1
      }
    }
    new Spine(nodes.toVector, Vector[Edge]())
  }
}

class Graph(
  sentence: String,
  tokens: Vector[String],
  tags: Vector[String],
  val spines: Vector[Spine],
  val edges: Vector[Edge]
) extends Parse(sentence, tokens, tags) {
  def genStringLines(
    spinesOnly: Boolean, arcsOnly: Boolean, tracesOnly: Boolean
  ) = {
    val maxes = Array(0, 0, 0, 0, 0, 0)
    val lines = for (position <- 0 until tags.length) yield {
      val number = (position+1).toString
      maxes(0) = maxes(0).max(number.length)

      val token = tokens(position)
      maxes(1) = maxes(1).max(token.length)

      val tag = tags(position)
      maxes(2) = maxes(2).max(tag.length)

      val spine =
        if (arcsOnly || tracesOnly) "___"
        else spines(position).nodesToString
      maxes(3) = maxes(3).max(spine.length)

      val structuralEdge = {
        val parent =
          if (position == 0) spines.length
          else position - 1
        val options = edges.filter{ edge =>
          edge.isStructural && edge.src == position
        }

        if (options.length > 0) options.head
        else new Edge(position, Node.None, parent, Node.None, "?")
      }

      val parent =
        if (structuralEdge.target == spines.length) "0"
        else (structuralEdge.target.toInt + 1).toString
      maxes(4) = maxes(4).max(parent.length)

      val structuralSymbol =
        if (structuralEdge.target == spines.length) "ROOT_0"
        else structuralEdge.toString.split(" ")(1)
      maxes(5) = maxes(5).max(structuralSymbol.length)

      val traceEdges =
        if (spinesOnly) "___"
        else {
          val combinedTraces =
            spines(position).traces ++
            edges.filter{ edge =>
              edge.isTrace && edge.src == position
            }

          combinedTraces.map{ edge =>
            new Edge(edge.src + 1, edge.srcNode, edge.target + 1,
              edge.targetNode, edge.trace)
          }.map(" " + _.toString).mkString("").trim
        }

      (number, token, tag, spine, parent, structuralSymbol, traceEdges)
    }

    val fieldFormat = s"%${maxes(0)}s %-${maxes(1)}s %-${maxes(2)}s %-${maxes(3)}s %${maxes(4)}s %-${maxes(5)}s %s"
    lines.map{ parts =>
      String.format(fieldFormat, parts._1, parts._2, parts._3, parts._4,
        parts._5, parts._6, parts._7)
    }
  }
  @transient lazy val stringLines = genStringLines(false, false, false)
  @transient lazy val stringLinesArcs = genStringLines(false, true, false)
  @transient lazy val stringLinesTraces = genStringLines(false, false, true)
  @transient lazy val stringLinesSpines = genStringLines(true, false, false)
  override def toString() = {
    val base = super.toString()
    val graphType = s"# crossing: $crossingType selfEdge: $hasSelfEdge doubleEdge: $hasDoubleEdge cycle: $cycleType\n"
    base + graphType + stringLines.mkString("\n") + "\n"
  }

  override def compareString(
    other: Parse, spinesOnly: Boolean = false, arcsOnly: Boolean = false,
    tracesOnly: Boolean = false
  ) = {
    other match {
      case psg: Graph =>
        val thisLines =
          if (spinesOnly) stringLinesSpines
          else if (tracesOnly) stringLinesTraces
          else if (arcsOnly) stringLinesArcs
          else stringLines
        val maxLength = thisLines.map(_.length).max + 2
        val otherLines =
          if (spinesOnly) psg.stringLinesSpines
          else if (tracesOnly) psg.stringLinesTraces
          else if (arcsOnly) psg.stringLinesArcs
          else psg.stringLines
        ( for ((thisLine, otherLine) <- thisLines.zip(otherLines))
          yield {
            val thisLineSimple = thisLine.split("  *").mkString(" ")
            val otherLineSimple = otherLine.split("  *").mkString(" ")

            if (thisLineSimple == otherLineSimple) thisLine
            else {
              // TODO: Break them up into tokens and colour the tokens that
              // differ
              val fmt = "%-"+ maxLength.toString +"s %s"
              String.format(fmt, thisLine, otherLine)
            }
          }
        ).mkString("\n") + "\n"
      case _ =>
        super.compareString(other)
    }
  }

  /* Projective - check that no pair of dependencies cross.
   * 1 Endpoint Crossing - check that for each edge that is crossed, all
   * the edges crossing it share an endpoint.
   * Other - All remaining structures
   */
  @transient lazy val crossingType = {
    var projective = true
    var oneEC = true
    for (edge0 <- edges) {
      val left0 = edge0.src.min(edge0.target)
      val right0 = edge0.src.max(edge0.target)
      val inside = HashSet[Int]()
      val outside = HashSet[Int]()
      for (edge1 <- edges) {
        val left1 = edge1.src.min(edge1.target)
        val right1 = edge1.src.max(edge1.target)
        if (left0 < left1 && left1 < right0 && right0 < right1) {
          inside.add(left1)
          outside.add(right1)
        } else if (left1 < left0 && left0 < right1 && right1 < right0) {
          inside.add(right1)
          outside.add(left1)
        }
      }
      if (inside.size != 0 || outside.size != 0) projective = false
      if (inside.size > 1 && outside.size > 1) oneEC = false
    }
    (projective, oneEC)
  }

  /* Is there an edge between a point and itself, such as:
    # Parse (ROOT
    # Parse   (FRAG
    # Parse     (NP
    # Parse       (NP (NNS Slides) )
    # Parse       (SBAR
    # Parse         (WHNP-1 (-NONE- 0) )
    # Parse           (S
    # Parse             (NP (-NONE- *T*-1) )
    # Parse               (VP (TO to)
    # Parse                 (VP (VB illustrate)
    # Parse                   (NP (NNP Shostakovich) (NNS quartets) ))))))
    # Parse     (. ?) ))
    # Sentence   Slides to illustrate Shostakovich quartets ?
    # Tokens     1 Slides  2 to  3 illustrate  4 Shostakovich  5 quartets  6 ?
    # Identity   1 (1, 1) WHNP-1 True
    # Reference  1 (1, 1) *T*-1
    # Empty      (1, 1) 0
    # Graph type  proj graph no-cycle no-cycle-alt has-cycle1 no-double
    1 Slides       NNS NP_NP_FRAG                         0 ROOT_0
    2 to           TO  _                                  3 VP_1
    3 illustrate   VB  VP_VP_(NP_(*T*))_S_(WHNP_(0))_SBAR 1 NP_1   3 WHNP_0 T NP_0 T *T*
    4 Shostakovich NNP _                                  5 NP_0
    5 quartets     NNS NP                                 3 VP_0
    6 ?            .   _                                  1 FRAG_0
   */
  @transient lazy val hasSelfEdge =
    edges.exists{ e => e.src == e.target } ||
    spines.exists{ _.traces.length > 0 }

  /* Same edge multiple times:
    # Parse (ROOT
    # Parse   (S
    # Parse     (S-1
    # Parse       (NP (DT The) (NNP SEC) )
    # Parse       (VP (MD will)
    # Parse         (ADVP (RB probably) )
    # Parse         (VP (VB vote)
    # Parse           (PP (IN on)
    # Parse             (NP (DT the) (NN proposal) ))
    # Parse           (NP (RB early) (JJ next) (NN year) ))))
    # Parse     (, ,)
    # Parse     (NP (PRP he) )
    # Parse     (VP (VBD said)
    # Parse       (SBAR (-NONE- 0)
    # Parse         (S (-NONE- *T*-1) )))
    # Parse     (. .) ))
    # Sentence   The SEC will probably vote on the proposal early next year , he said .
    # Tokens     1 The  2 SEC  3 will  4 probably  5 vote  6 on  7 the  8 proposal  9 early  10 next  11 year  12 ,  13 he  14 said  15 .
    # Identity   1 (0, 11) S-1 False
    # Reference  1 (14, 14) *T*-1
    # Empty      (14, 14) 0
    # Graph type  proj graph no-cycle no-cycle-alt no-cycle1 has-double
     1 The      DT  _                          2 NP_0
     2 SEC      NNP NP                         5 S_0
     3 will     MD  _                          5 VP_1
     4 probably RB  ADVP                       5 VP_1
     5 vote     VB  VP_VP_S                   14 S_0    14 S_0 T S_0 F *T*
     6 on       IN  PP                         5 VP_0
     7 the      DT  _                          8 NP_0
     8 proposal NN  NP                         6 PP_0
     9 early    RB  _                         11 NP_0
    10 next     JJ  _                         11 NP_0
    11 year     NN  NP                         5 VP_0
    12 ,        ,   _                         14 S_0
    13 he       PRP NP                        14 S_0
    14 said     VBD (SBAR_(0)_(S_(*T*)))_VP_S  0 ROOT_0
    15 .        .   _                         14 S_0
   */
  @transient lazy val hasDoubleEdge =
    edges.exists{ e =>
      edges.exists{ f =>
        e.src == f.src && e.target == f.target && e != f
      }
    }

  /* Tree / DAG / Other
   * Return true/false for Tree, and max(shortest cycle for each word)
    # Parse (ROOT
    # Parse   (S-1 (CC But)
    # Parse     (ADVP (RB lately) )
    # Parse     (PRN (, ,)
    # Parse       (S
    # Parse         (NP (NNS retailers) )
    # Parse         (VP (VBP say)
    # Parse           (SBAR (-NONE- 0)
    # Parse             (S (-NONE- *T*-1) ))))
    # Parse       (, ,) )
    # Parse     (NP (JJ fake) )
    # Parse     (VP (VBZ has)
    # Parse       (VP (VBN become)
    # Parse         (ADJP (RBR more) (JJ fashionable) )))
    # Parse     (. .) ))
    # Sentence   But lately , retailers say , fake has become more fashionable .
    # Tokens     1 But  2 lately  3 ,  4 retailers  5 say  6 ,  7 fake  8 has  9 become  10 more  11 fashionable  12 .
    # Identity   1 (0, 12) S-1 False
    # Reference  1 (5, 5) *T*-1
    # Empty      (5, 5) 0
    # Graph type  proj graph has-cycle has-cycle-alt no-cycle1 no-double
     1 But         CC  S                              0 ROOT_0 5 S_0 T S_0 F *T*
     2 lately      RB  ADVP                           1 S_0
     3 ,           ,   _                              5 PRN_0
     4 retailers   NNS NP                             5 S_0
     5 say         VBP (SBAR_(0)_(S_(*T*)))_VP_S_PRN  1 S_0
     6 ,           ,   _                              5 PRN_0
     7 fake        JJ  NP                             1 S_0
     8 has         VBZ _                              9 VP_1
     9 become      VBN VP_VP                          1 S_0
    10 more        RBR _                             11 ADJP_0
    11 fashionable JJ  ADJP                           9 VP_0
    12 .           .   _                              1 S_0
  */
  @transient lazy val cycleType = {
    val tree = edges.length == tokens.length

    val edgeMap = HashMap[Int, HashSet[Int]]()
    for (edge <- edges) {
      val map = edgeMap.getOrElseUpdate(edge.src, new HashSet[Int])
      map.add(edge.target)
    }
    val dag =
      if (edges.length <= tokens.length) -1
      else (0 until tokens.length).map{ start =>
          val queue = Queue((start, 0))
          val seen = HashSet(start)
          var cycleLength = -1
          while (queue.length > 0) {
            val (cur, dist) = queue.dequeue
            if (cur < tokens.length && edgeMap.contains(cur)) {
              for (parent <- edgeMap(cur)) {
                if (parent == start && cycleLength < 0) cycleLength = dist + 1
                if (! seen.contains(parent)) {
                  queue.enqueue((parent, dist+1))
                  seen.add(parent)
                }
              }
            }
          }
          cycleLength
        }.max
    (tree, dag)
  }

  // The idea here is to provide a set of edges that do not contain a loop or
  // 1ec violation, cutting a small number of edges out if necessary
  @transient lazy val dag1ecEdges = {

    val excluded = new HashSet[Edge]
    val edgeMap = HashMap[Int, HashSet[Edge]]()
    val traceEdges = new HashSet[Edge]
    for (edge <- edges) {
      edgeMap.getOrElseUpdate(edge.src, new HashSet[Edge]).add(edge)
      if (edge.isTrace) traceEdges.add(edge)
    }

    // Remove edges one at a time until we are acyclic
    var done = false
    while (! done) {
      // Identify all trace edges that are part of a 1ec violation (note those
      // that are doing the crossing, and those that are multi-crossed).
      val problemEdges = new HashMap[Edge, (Int, Int)]

      for (edge <- edges
           if ! excluded.contains(edge)) {
        val left = edge.src.min(edge.target)
        val right = edge.src.max(edge.target)

        var inside = new HashSet[Int]
        var outside = new HashSet[Int]
        edges.filter{ e => e.crosses(edge) && ! excluded.contains(e) }.foreach{ e =>
          val (cin, cout) =
            if (left < e.src && e.src < right) (e.src, e.target)
            else (e.target, e.src)
          inside.add(cin)
          outside.add(cout)
        }
        if (inside.size > 1 && outside.size > 1) {
          if (edge.isTrace) {
            val count = problemEdges.getOrElse(edge, (0, 0))
            problemEdges(edge) = (count._1 + 1, 0)
          } else {
            edges.filter{ e =>
              e.crosses(edge) &&
              e.isTrace &&
              ! excluded.contains(e)
            }.foreach{ e =>
              val count = problemEdges.getOrElse(e, (0, 0))
              problemEdges(e) = (count._1 + 1, 0)
            }
          }
        }
      }

      // Start from those edges, and see if they are part of cycles.
      for (edge <- traceEdges
           if ! excluded.contains(edge)) {
        // Do a BFS
        val queue = Queue(edge.target)
        val seen = HashSet(edge.src)
        var found = 0 // approximately tracks number of cycles (not the actual amount, which is non-trivial to calculate)
        while (queue.length > 0) {
          val cur = queue.dequeue
          // Don't go to the root
          if (cur < tokens.length) {
            // Consider each edge leaving here
            for (edge2 <- edgeMap(cur)) {
              if (! excluded.contains(edge2)) {
                val parent = edge2.target
                if (parent == edge.src) found += 1
                if (! seen.contains(parent)) {
                  queue.enqueue(parent)
                  seen.add(parent)
                }
              }
            }
          }
        }
        if (found > 0) {
          val count = problemEdges.getOrElse(edge, (0, 0))
          problemEdges(edge) = (count._1, found)
        }
      }

      // Remove the worst edge (part of a cycle and/or still 1ec involved)
      if (problemEdges.size == 0) done = true
      else {
        val maxCount = problemEdges.map( _._2._1 ).max
        val maxFound = problemEdges.map( _._2._2 ).max
        var worst : (Edge, Double) = (null, 0.0)
        for ((edge, (count, found)) <- problemEdges) {
          val countScore =
            if (maxCount == 0) 1.0
            else count.toDouble / maxCount
          val foundScore =
            if (maxFound == 0) 1.0
            else found.toDouble / maxFound
          val overall = countScore + foundScore
          if (overall > worst._2) worst = (edge, overall)
        }

        excluded.add(worst._1)
      }
    }

    edges.filter( ! excluded.contains(_) )
  }

  @transient lazy val toTriples = {
    def edgeToTriple(edge: Edge) = {
      val siblingString =
        if (edge.target == tokens.length) ""
        else {
          val siblingPos = spines(edge.target).posInNodes(edge.targetNode) - 1
          if (siblingPos < 0) "NO_TAG"
          else spines(edge.target).nodes(siblingPos).toString
        }

      (edge.src, edge.target, edge.srcNode.toString,
        edge.targetNode.toString, siblingString, edge.trace)
    }

    edges.map{ edgeToTriple(_) } ++
    spines.flatMap{ _.traces.map{ edgeToTriple(_) } }
  }

  @transient lazy val toSpans = {
    val spans = HashMap[(Int, Node), (Int, Int)]()

    def getSpan(pos: Int, node: Node) : (Int, Int) = {
      spans.getOrElseUpdate((pos, node), {
        var min = pos
        var max = pos + 1
        // Look at all edges leading here, get the span of their top symbol and
        // use that to work out what words this position spans.
        for (edge <- edges) {
          if (edge.target == pos && edge.targetNode == node) {
            val span = getSpan(edge.src, edge.srcNode)
            if (span._1 < min) min = span._1
            if (span._2 > max) max = span._2
          }
        }
        // Look at symbols beneath this one in the spine, use their span to
        // update the span as well.
        spines(pos).nodes.forall{ subnode =>
          if (subnode == node) false
          else if (subnode.isNull) true
          else {
            val span = getSpan(pos, subnode)
            if (span._1 < min) min = span._1
            if (span._2 > max) max = span._2
            true
          }
        }

        (min, max)
      })
    }

    // First do regular non-terminal nodes
    for {(spine, index) <- spines.zipWithIndex
      node <- spine.nodes
      if ! node.isNull
    } spans((index, node)) = getSpan(index, node)

    // Now do null elements
    for {(spine, index) <- spines.zipWithIndex
      (node, cur) <- spine.nodes.zipWithIndex
      if node.isNull
    } {
      // Go up in the spine until a regular non-terminal is reached. Use it's
      // span to place the null element. For simplicity, always put it on the
      // far left (in practise the rules are much more complex).
      ((cur + 1) until spine.nodes.length).exists{ pos =>
        if (spine.nodes(pos).isNull) false
        else {
          val cspan = getSpan(index, spine.nodes(pos))
          spans((index, node)) = (cspan._1, cspan._1)
          true
        }
      }
    }

    val ans = HashSet[(String, Int, Int)]()
    for (((index, node), (left, right)) <- spans) {
      // This is necessary because spines like "_" get a span stored for
      // convenience.
      if (spines(index).nodes.length > 0)
        ans.add( (node.symbolToString, left, right) )
    }
    ans
  }
}

object Graph {
  def apply(sentence: String, tokens: Vector[String], tags: Vector[String],
            spines: Vector[Spine], edges: Vector[Edge]) =
    new Graph(sentence, tokens, tags, spines, edges)

  def fromText(lines: ArrayBuffer[String]) = {
    /* Example:
    1 We          PRP NP-SBJ                       5 S_1    5 NP-SBJ_0 T NP-SBJ_0 F *
    2 're         VBP _                            5 VP_3
    3 about       IN  _                            5 VP_2
    4 to          TO  _                            5 VP_1
    5 see         VB  VP_VP_(NP-SBJ_(*))_S_VP_VP_S 0 ROOT_0
    6 if          IN  _                            8 SBAR_0
    7 advertising NN  NP-SBJ                       8 S_0
    8 works       VBZ VP_S_SBAR                    5 VP_0
    9 .           .   _                            5 S_1

    * Note, we are guaranteed that the last symbol in a spine is either _ or a
    * regular non-terminal node (not a null element). Null elements are placed
    * before the node they are under.
    */
    val tokens = new ArrayBuffer[String]
    val edges = new ArrayBuffer[Edge]
    val spines = new ArrayBuffer[Spine]
    val tags = new ArrayBuffer[String]

    // Make spines
    val sentenceLength = lines.length
    for (line <- lines) {
      val parts = line.trim().split("  *")
      tokens.append(parts(1))
      tags.append(parts(2))
      val spine = Spine.fromText(parts(3))
      spines.append(spine)
    }

    // Make structural edges
    for ((line, srcPos) <- lines.zipWithIndex) {
      val parts = line.trim().split("  *")
      val spine = spines(srcPos)
      val targetPos =
        if (parts(4) == "0") sentenceLength
        else parts(4).toInt - 1

      val srcNode =
        if (spine.nodes.length == 0) Node.None
        else spine.nodes.last
      val targetNode =
        if (targetPos == sentenceLength) Node.Root
        else {
          val symbol = parts(5).split("_").head.split("-").head
          val functions = parts(5).split("_").head.split("-").tail.toVector
          val count = parts(5).split("_").last.toInt
          spines(targetPos).getMatchingNode(symbol, functions, count, false)
        }

      edges.append(new Edge(srcPos, srcNode, targetPos, targetNode, "_"))
    }

    // Add traces
    // Note - Other parts of the system assume that these edges are added
    // after all the structural edges (so they appear later in the edge list).
    // TODO: Should this be pushed into pre-processing of data? (ie. we always
    // do traces, some data just doesn't have them)
    if (Config.keepTraces) {
      for ((line, srcPos) <- lines.zipWithIndex) {
        val parts = line.trim().split("  *")
        for (pos <- (6 until parts.length by 6)) {
          val targetPos = parts(pos).toInt - 1
          val targetNode = {
            val subparts = parts(pos + 1).split("_")
            val symbol = subparts.head.split("-").head
            val functions = subparts.head.split("-").tail.toVector
            val count = subparts.last.toInt
            val isNull = parts(pos + 2) == "T"
            spines(targetPos).getMatchingNode(symbol, functions, count, isNull)
          }
          val srcNode = {
            val subparts = parts(pos + 3).split("_")
            val symbol = subparts.head.split("-").head
            val functions = subparts.head.split("-").tail.toVector
            val count = subparts.last.toInt
            val isNull = parts(pos + 4) == "T"
            spines(srcPos).getMatchingNode(symbol, functions, count, isNull)
          }
          val trace = parts(pos + 5)
          edges.append(new Edge(srcPos, srcNode, targetPos, targetNode, trace))
        }
      }
    }

    // Move loop edges from the edge list into their spines
    edges.filter{ e => e.src == e.target }.foreach{ e =>
      spines(e.src) = spines(e.src).withTrace(e)
    }
    val finalEdges = edges.filter{ v => v.src != v.target }

    apply(tokens.mkString(" "), tokens.toVector, tags.toVector,
      spines.toVector, finalEdges.toVector)
  }

  def symbolToString(symbol: (String, Vector[String])) =
    symbol._1 + symbol._2.map("-"+ _).mkString("")
}

