// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, PriorityQueue, Set}

object ArcPassConstraint extends Enumeration {
  type ArcPassConstraint = Value
  val BOTH, EITHER, NEITHER, PARENT, CHILD = Value
}

// TODO: Merge / define terminology: arc, arg, edge
// Also, child / src, parent / target

/**
  */
@SerialVersionUID(1L)
abstract class Model(
  val formalism: Formalism.Value,
  val wordIndex: IntIndexer,
  val argIndex: ArgumentIndexer,
  val nonTerminalIndex: NonTerminalIndexer,
  val filterByTag: Boolean
) extends Serializable {
  val includePOSInSpine = Config.includePOSInSpine
  val includePOSInArg = Config.includePOSInArg
  val featureDev = Config.featureDev
  val edgeHasTargetGrandParent = Config.edgeHasTargetGrandParent
  val edgeHasSourceGrandParent = Config.edgeHasSourceGrandParent
  val edgeHasTargetParent = Config.edgeHasTargetParent
  val edgeHasSourceParent = Config.edgeHasSourceParent
  val edgeHasTargetChild = Config.edgeHasTargetChild
  val edgeHasSourceChild = Config.edgeHasSourceChild
  val edgeHasTargetGrandChild = Config.edgeHasTargetGrandChild
  val edgeHasSourceGrandChild = Config.edgeHasSourceGrandChild
  val ruleStructuralFeatureOnly = Config.ruleStructuralFeatureOnly
  val ternaryTraces = Config.ternaryTraces

  val spineFeatParts = Config.spineFeatParts
  val spineFeatComplete = Config.spineFeatComplete
  val spineFeatAllButTrace = Config.spineFeatAllButTrace
  val spineFeatAllButNull = Config.spineFeatAllButNull

  val useCoarseArcScore = Config.useCoarseArcScore
  val useCoarseTraceScore = Config.useCoarseTraceScore
  val useCoarseSpineScore = Config.useCoarseSpineScore

  // Not in use - idea is that we could have chain scores fire features on
  // regular traces too
///  val useMergedChainScore = Config.useMergedChainScore

  // Tag -> Spine IDs
  val tagToSpines = new HashMap[Int, HashSet[Int]]
  // Token -> Spine IDs
  val tokenToSpines = new HashMap[Int, HashSet[Int]]
  // Spine ID -> Symbol IDs
  val spines = new ArrayBuffer[(Vector[((Int, Vector[Int]), Int, Int)], Vector[((Int, Vector[Int]), Int, Boolean, (Int, Vector[Int]), Int, Boolean, Int)])]
  val spineParts = new ArrayBuffer[Vector[Int]]
  val spinesOriginal = new ArrayBuffer[psg.Spine]
  val denseSpineMap = new ArrayBuffer[Vector[Int]]
  val spineSymbolMap = new IntIntMap(Config.loadFactor, 256, -1)

  // Maps from the argIndex value to the set of properties
  // Vector has: (symbol, count number, trace depth, index in vector)
  // Booleans are: structural edge, chain edge, a non-trace trace (used in trace scorer)
  val edges = new ArrayBuffer[(Vector[(Int, Vector[Int], Int, Int)], Boolean, Boolean, Boolean, Vector[Int])]
  val denseArgMap = new ArrayBuffer[Vector[Int]]
  val chainTraceEdges = new IntSet(Config.loadFactor, 100)
  // Edge ID -> Internal ID for weights (selected to give more compact storage)
  val argSymbolMap = new IntIntMap(Config.loadFactor, 256, -1)

  // Mapping from (childTag, parentTag, childSpineID, parentSpineID) to Args
  @transient lazy val argMap = new LongLongMap(Config.loadFactor, 1 << 22, -1,
    false)
  @transient lazy val argMapVals = new ArrayBuffer[UnboxedArrayBuffer]

  // Tokens and token suffixes -> Frequency in training
  val tokenCounts = new IntIntMap(Config.loadFactor, 2 << 17, -1)
  val tokenMap = new IntIntMap(Config.loadFactor, 2 << 17, -1)

  // Note - order of creation is exploited in ArcGenerator
  val NonArg = argIndex(Model.SNon)
  val RootArg = argIndex(Model.SRootArg)
  val NullArg = argIndex(Model.SNull)

  val NonNT = nonTerminalIndex(Model.SNon)
  val NULL_SPINE = NonNT
  val NULL_SPINE_SET = HashSet(NonNT)
  val NullNT = nonTerminalIndex(Model.SNull)
  val RootNT = nonTerminalIndex(Model.SRootNT)
  val GoldNT = nonTerminalIndex(Model.SGoldNT)
  val UnderscoreNT = nonTerminalIndex("_")
  val RootTag = nonTerminalIndex(Model.SRootTag, true)
  val GoldTag = nonTerminalIndex(Model.SGoldTag, true)
  val StarTag = nonTerminalIndex(Model.SNoTag, true)
  val NonTrace = nonTerminalIndex(Model.SNonTrace)

  val NonWord = wordIndex(Model.SNon)
  val NullWord = wordIndex(Model.SNull)
  val RootWord = wordIndex(Model.SRootWord)

  val RootSpine = {
    val rootNode = psg.Node(Model.SRootNT, Vector[String](), 0, 0)
    new psg.Spine(Vector(rootNode), Vector[psg.Edge]())
  }
  val RootSpineID =
    if (formalism == Formalism.PSG) addSpinePSG(Model.SRootTag, RootSpine)
    else 0
  tagToSpines(RootTag) = HashSet(RootSpineID)
  tokenToSpines(RootWord) = HashSet(RootSpineID)
  val GoldSpine = {
    val goldNode = psg.Node(Model.SGoldNT, Vector[String](), 0, 0)
    new psg.Spine(Vector(goldNode), Vector[psg.Edge]())
  }
  val BackupSpineID =
    if (formalism == Formalism.PSG)
      addSpinePSG(Model.SGoldTag, GoldSpine)
    else 0
  val EmptySpine = new psg.Spine(Vector[psg.Node](), Vector[psg.Edge]())
  val EmptySpineID =
    if (formalism == Formalism.PSG)
      addSpinePSG(Model.SGoldTag, EmptySpine)
    else 0

  def isTreeEdge(arg: Int) = edges(arg)._2
  def isChainTraceEdge(arg: Int) = edges(arg)._3
  def getChildSymbolForArg(arg: Int) = (0, 0)

  // For the ArcParser. Maps from a pair of tag IDs to the set of args they
  // can have. At training all options are constructed (even though only one
  // will be used). This allows any option to be used at eval (though of course
  // the pruning at training will mean arcs may not get enough weight to avoid
  // pruning).
  val allowedArgLists = new HashMap[Int, UnboxedArrayBuffer]
  def allowedArgList(
    constraint: ArcPassConstraint.Value, child: Int, parent: Int
  ) = allowedArgLists.synchronized {
    val id = constraint match {
      case ArcPassConstraint.NEITHER => 0
      case ArcPassConstraint.EITHER => ((child + (parent * 100)) << 3) + 1
      case ArcPassConstraint.PARENT => (parent << 3) + 2
      case ArcPassConstraint.CHILD => (child << 3) + 3
      case ArcPassConstraint.BOTH => (child + (parent * 100)) << 4
    }
    allowedArgLists.getOrElseUpdate(id, new UnboxedArrayBuffer(8))
  }
  def addAllowedArg(
    constraint: ArcPassConstraint.Value, child: Int, parent: Int, id: Int
  ) = {
    val allowedList = allowedArgList(constraint, child, parent)
    if (! allowedList.contains(id)) allowedList.append(id)
  }

  @transient lazy val edgeCounts = new IntIntMap(Config.loadFactor, 1 << 12, -1)
  def mapToDenseArg(features: Vector[Int]) = {
    features.map{ v => argSymbolMap.getOrElse(v, -1) }
  }
  def mapToDenseSpine(features: Vector[Int]) = {
    features.map{ v => spineSymbolMap.getOrElse(v, -1) }
  }
  def updateEdgeCounts(label: Int) = {
    val structuralFeatures = getPartsForPSG(label, false)
    if (ruleStructuralFeatureOnly) {
      edgeCounts.changeOrPut(structuralFeatures.last, 1)
      if (featureDev.length > 4 && featureDev(4) == 'T') {
        val sym = structuralFeatures(structuralFeatures.length - 2)
        edgeCounts.changeOrPut(sym, 1)
      }
    } else {
      for (feat <- structuralFeatures) edgeCounts.changeOrPut(feat, 1)
    }
  }
  def createReorderingMaps() = {
    if (spineSymbolMap.size == 0) {
      Log.logln("Creating an internal spine mapping")
      val counts = ArrayBuffer[(Int, Int)]()
      spineCounts.foreachPair{ (label, count) =>
        counts.append((count, label))
      }
      for (((_, label), i) <- counts.sorted.reverse.zipWithIndex)
        spineSymbolMap.put(label, i)
      Log.logln(s"spineSymbolMap has ${spineSymbolMap.size} entries")
      // Make the dense mapping for spines (the system does not create novel
      // oens later, unlike edges). However, we can have things in our count
      // that aren't spines (e.g. sub-parts) so we should not create values
      // for all of them.
      for (label <- (0 until spinesOriginal.length)) {
        if (spinesOriginal(label) != null)
          denseSpineMap(label) = mapToDenseSpine(spineParts(label))
      }
    }
    if (argSymbolMap.size == 0) {
      Log.logln("Creating an internal edge mapping")
      val counts = ArrayBuffer[(Int, Int)]()
      edgeCounts.foreachPair{ (label, count) =>
        counts.append((count, label))
      }
      for (((_, label), i) <- counts.sorted.reverse.zipWithIndex)
        argSymbolMap.put(label, i)
      Log.logln(s"argSymbolMap has ${argSymbolMap.size} entries")

      denseArgMap(BackupLabelStructural) = mapToDenseArg(edges(BackupLabelStructural)._5)
      denseArgMap(BackupLabelTrace) = mapToDenseArg(edges(BackupLabelTrace)._5)
      denseArgMap(BackupLabelChain) = mapToDenseArg(edges(BackupLabelChain)._5)
    }
  }

  val traceMap = new HashMap[(String, String, String), Int]
  def getTraceEdgeID(
    trace: String, srcTag: String, targetTag: String, useCache: Boolean = false
  ) = traceMap.synchronized {
    val comb = (trace, srcTag, targetTag)
    if (useCache && traceMap.contains(comb)) traceMap(comb)
    else {
      val mtrace =
        if (! ternaryTraces) trace
        else if (trace == "_" || trace == "=" || trace == "*NON*") trace
        else "*"

      val ans = addEdgePSG(0, EmptySpine, nonTerminalIndex(srcTag, true), 0,
        EmptySpine, nonTerminalIndex(targetTag, true), mtrace, true)
      if (useCache) traceMap(comb) = ans
      ans
    }
  }
  def getPartsForPSG(sym: Int, isSpine: Boolean = false) = {
    if (isSpine) {
      if (sym < 0) Vector[Int]()
      else if (denseSpineMap.size < sym || denseSpineMap(sym) == null)
        spineParts(sym)
      else denseSpineMap(sym)
    } else {
      if (sym < 0) Vector[Int]()
      else if (denseArgMap.size < sym || denseArgMap(sym) == null)
        edges(sym)._5
      else denseArgMap(sym)
    }
  }
  val BackupLabelStructural = addEdgePSG(0, GoldSpine, GoldTag, 0, GoldSpine,
    GoldTag, "_", true)
  updateEdgeCounts(BackupLabelStructural)
  // TODO: next two are not quite right, since the trace edge goes between
  // non-trace positions.
  val BackupLabelTrace = addEdgePSG(0, GoldSpine, GoldTag, 0, GoldSpine,
    GoldTag, "NO_TAG", true)
  updateEdgeCounts(BackupLabelTrace)
  val BackupLabelChain = addEdgePSG(0, GoldSpine, GoldTag, 0, GoldSpine,
    GoldTag, "NO_TAG_chain", true)
  updateEdgeCounts(BackupLabelChain)
  def combinePartAndID(part: Int, id: Int) = (part << 5) + (id << 1)
  def getPartForEdge(
    spine: psg.Spine, pos: Int, delta: Int, tag: Int, id: Int,
    features: ArrayBuffer[Int],
    parts: ArrayBuffer[(Int, Vector[Int], Int, Int)] = null
  ) = {
    var npos = pos
    var shift = 0
    while (shift < delta.abs) {
      if (delta < 0) npos -= 1
      else npos += 1
      if (npos < 0 || npos >= spine.nodes.length ||
          ! spine.nodes(npos).isNull) shift += 1
    }

    if (npos == -1 && includePOSInArg) {
      if (parts != null) parts.append((tag, Vector[Int](), 0, 0))
      if (features != null) features.append(combinePartAndID(tag, id))
    } else if (npos == -1 && delta == 0) {
      if (parts != null) parts.append((StarTag, Vector[Int](), 0, 0))
      if (features != null) features.append(combinePartAndID(StarTag, id))
    } else if (spine.nodes.length <= npos || npos < 0) {
      // TODO: Should we just have no feature in this case?
      if (parts != null)
        parts.append((nonTerminalIndex("_"), Vector[Int](), 0, 0))
      if (features != null)
        features.append(combinePartAndID(nonTerminalIndex("_"), id))
    } else {
      val cur = spine.nodes(npos)
      val symbol = nonTerminalIndex(cur.symbol)
      val functions = cur.functions.map(nonTerminalIndex(_))
      if (parts != null)
        parts.append((symbol, functions, cur.count, cur.nullDepth))
      if (features != null) {
        features.append(combinePartAndID(symbol, id))
        cur.functions.foreach{ funcStr =>
          val func = nonTerminalIndex(funcStr)
          features.append(combinePartAndID(func, id))
        }
        val symbolCount = (symbol << 5) + cur.count
        val symbolCountNull = (symbolCount << 3) + cur.nullDepth
        val symbolNull = (symbolCount << 3) + cur.nullDepth
        features.append(combinePartAndID(symbolCount << 2, id) + 1)
        features.append(combinePartAndID(symbolCountNull << 2 + 1, id) + 1)
        features.append(combinePartAndID(symbolNull << 2 + 2, id) + 1)
        features.append(combinePartAndID(cur.nullDepth << 2 + 3, id) + 1)
      }
    }
  }
  def getEdgeSrc(
    edge: (Vector[(Int, Vector[Int], Int, Int)], Boolean, Boolean, Boolean, Vector[Int])
  ) = edge._1(0)
  def getEdgeTarget(
    edge: (Vector[(Int, Vector[Int], Int, Int)], Boolean, Boolean, Boolean, Vector[Int])
  ) = edge._1(1)
  def getEdgeTrace(
    edge: (Vector[(Int, Vector[Int], Int, Int)], Boolean, Boolean, Boolean, Vector[Int])
  ) : String = nonTerminalIndex.value(edge._1(2)._1)
  def addEdgePSG(
    srcSpinePos: Int, srcSpine: psg.Spine, srcTagID: Int,
    targetSpinePos: Int, targetSpine: psg.Spine, targetTagID: Int,
    trace: String, add: Boolean
  ) : Int = {
    val parts = ArrayBuffer[(Int, Vector[Int], Int, Int)]()
    val features = ArrayBuffer[Int]()

    // TODO: Think about how to allow different levels to have different
    // amounts of detail. See note below on a challenge for this.

    // These firat three must be in this order (used for access elsewhere)
    features.append(combinePartAndID(nonTerminalIndex(trace), 2))
    getPartForEdge(srcSpine, srcSpinePos, 0, srcTagID, 0, features, parts)
    getPartForEdge(targetSpine, targetSpinePos, 0, targetTagID, 1, features,
      parts)
    parts.append((nonTerminalIndex(trace), Vector[Int](), 0, 0))

    if (trace != "_" && featureDev.length > 6 && featureDev(6) == 'T') {
      // Option to skip the refined definition for traces
    } else {
      if (edgeHasSourceParent)
        getPartForEdge(srcSpine, srcSpinePos, 1, srcTagID, 4, features)
      if (edgeHasSourceGrandParent)
        getPartForEdge(srcSpine, srcSpinePos, 2, srcTagID,5, features)
      if (edgeHasSourceChild)
        getPartForEdge(srcSpine, srcSpinePos, -1, srcTagID, 6, features)
      if (edgeHasSourceGrandChild)
        getPartForEdge(srcSpine, srcSpinePos, -2, srcTagID, 7, features)
      if (edgeHasTargetParent)
        getPartForEdge(targetSpine, targetSpinePos, 1, targetTagID,  8,
          features)
      if (edgeHasTargetGrandParent)
        getPartForEdge(targetSpine, targetSpinePos, 2, targetTagID, 9,
          features)
      if (edgeHasTargetChild)
        getPartForEdge(targetSpine, targetSpinePos, -1, targetTagID, 10,
          features)
      if (edgeHasTargetGrandChild)
        getPartForEdge(targetSpine, targetSpinePos, -2, targetTagID, 11,
          features)
    }

    // Add the complete string that is a minimal representation of the rule
    val text = features.map(_.toString).mkString(" ")
    if (featureDev.length > 4 && featureDev(4) == 'T') {
      val completeRuleNFunc = parts.map{ v =>
        nonTerminalIndex.value(v._1) +"_"+ v._3 +
        (if (v._4 > 0) "T" else "F")
      }.mkString("::")
      features.append(combinePartAndID(nonTerminalIndex(completeRuleNFunc), 12))
    }
    features.append(combinePartAndID(nonTerminalIndex(text), 3))

    if (! add) {
      val id = argIndex.getOrElse(text, -1)

      if (id < 0 || id >= edges.length || edges(id) == null) {
        // Note - for arcParser novel args cannot occur, so this is never
        // triggered. For localParser it could be, since anywhere linking two
        // spines could be considered. However, if the edge definition of the
        // two parsers are the same, the arcParser will prune this edge from
        // the localParser, because it is unseen, so it doesn't matter.
        // If in the future we allow different levels of granularity at
        // different passes, then there could be an issue (a more specific
        // edge in local is unseen, but it's more general definition in arc
        // is).
        -1
      } else {
        if (denseArgMap(id) == null && argSymbolMap.size > 0)
          denseArgMap(id) = mapToDenseArg(edges(id)._5)

        id
      }
    } else {
      val id = argIndex(text)

      // Make space if necessary
      while (id >= edges.length) {
        edges.append(null)
        denseArgMap.append(null)
      }

      // Create the edge if necessary
      if (edges(id) == null) {
        val structural = trace == "_"
        val isNonTrace = trace == Model.SNonTrace
        val chain = trace.contains("chain")
        edges(id) = (parts.toVector, structural, chain, isNonTrace,
          features.toVector)
      }

      // Mapping for denser models
      if (denseArgMap(id) == null && argSymbolMap.size > 0)
        denseArgMap(id) = mapToDenseArg(edges(id)._5)

      if (filterByTag) {
        for (other <- 0 until 100) {
          addAllowedArg(ArcPassConstraint.EITHER, other, targetTagID, id)
          addAllowedArg(ArcPassConstraint.EITHER, srcTagID, other, id)
        }
        addAllowedArg(ArcPassConstraint.NEITHER, srcTagID, targetTagID, id)
        addAllowedArg(ArcPassConstraint.PARENT, srcTagID, targetTagID, id)
        addAllowedArg(ArcPassConstraint.CHILD, srcTagID, targetTagID, id)
        addAllowedArg(ArcPassConstraint.BOTH, srcTagID, targetTagID, id)
      }

      id
    }
  }
  def addEdgePSG(
    edge: psg.Edge, srcSpine: psg.Spine, srcTagID: Int, targetSpine: psg.Spine,
    targetTagID: Int, add: Boolean
  ) : Int = {
    val srcSpinePos =
      if (edge.srcNode == psg.Node.None) -1
      else srcSpine.posInNodes(edge.srcNode)
    val targetSpinePos = targetSpine.posInNodes(edge.targetNode)
///    Log.logln(s"Adding Edge: ($edge, ${edge.targetNode}, ${edge.srcNode}) $srcSpine $srcTagID $targetSpine $targetTagID $add => $srcSpinePos $targetSpinePos")

    addEdgePSG(srcSpinePos, srcSpine, srcTagID, targetSpinePos, targetSpine,
      targetTagID, edge.trace, add)
  }

  def addSpinePSG(
    tag: String, spine: psg.Spine, add: Boolean = true
  ) = {
    val baseSymbol = if (includePOSInSpine) tag else Model.SNoTag
    val text = baseSymbol +"_"+ spine.toString
    if (! add) {
      val id = nonTerminalIndex.getOrElse(text, -1)
      if (id < 0 || spines.length <= id) -1
      else {
        if (denseSpineMap(id) == null && spineSymbolMap.size > 0) {
          val parts = spineParts(id)
          if (parts == null) -1
          else {
            denseSpineMap(id) = mapToDenseSpine(parts)
            id
          }
        } else id
      }
    } else {
      val id = nonTerminalIndex(text)

      // Make space if necessary
      while (id >= spines.length) {
        spines.append(null)
        spineParts.append(null)
        spinesOriginal.append(null)
        denseSpineMap.append(null)
      }

      // Create the spine if necessary
      if (spines(id) == null) {
        val traces = spine.traces.map{ e =>
          val srcFunctions = e.srcNode.functions.map(nonTerminalIndex(_))
          val targetFunctions = e.targetNode.functions.map(nonTerminalIndex(_))
          (
            (nonTerminalIndex(e.targetNode.symbol), targetFunctions),
            e.targetNode.count, e.targetNode.isNull,
            (nonTerminalIndex(e.srcNode.symbol), srcFunctions),
            e.srcNode.count, e.srcNode.isNull,
            nonTerminalIndex(e.trace)
          )
        }
        val base = ((nonTerminalIndex(baseSymbol), Vector[Int]()), 0, 0)
        val symbols = spine.nodes.map{ node =>
          val functions = node.functions.map(nonTerminalIndex(_))
          ((nonTerminalIndex(node.symbol), functions), node.count,
            node.nullDepth)
        }

        spines(id) = (base +: symbols, traces)
        spinesOriginal(id) = spine

        // TODO: Allow variation in whether the number is included and
        // likewise for the boolean indicating null-ness.  Also allow
        // variation in how the trace edges on the spine are handled.
        val parts = new ArrayBuffer[Int]
        if (spineFeatComplete) parts.append(id)
        if (spineFeatParts) {
          parts.append(nonTerminalIndex(s"${baseSymbol}_0_false"))
          spine.nodes.map{ node =>
            val sym = s"${node.symbolToString}_${node.count}_${node.nullDepth}"
            parts.append(nonTerminalIndex(sym))
          }
          spine.traces.map{ t =>
            parts.append(nonTerminalIndex(t.toString()))
          }
        }
        if (spineFeatAllButTrace) {
          parts.append(nonTerminalIndex(
            s"${baseSymbol}_0_false" +
            spine.nodes.map{ node =>
              s"${node.symbolToString}_${node.count}_${node.nullDepth}"
            }.mkString(" ")
          ))
        }
        if (spineFeatAllButNull) {
          parts.append(nonTerminalIndex(
            s"${baseSymbol}_0_false" +
            spine.nodes.filter( _.nullDepth == 0 ).map{ node =>
              s"${node.symbolToString}_${node.count}_${node.nullDepth}"
            }.mkString(" ")
          ))
        }
        if (featureDev.length > 4 && featureDev(4) == 'T') {
          parts.append(nonTerminalIndex(
            s"${baseSymbol}_0_0"+
            spine.nodes.map{ node =>
              s" ${node.symbol}_${node.count}_${node.nullDepth}"
            }.mkString("")
          ))
        }
        spineParts(id) = parts.toVector

        // Mapping for denser models
        if (spineSymbolMap.size > 0)
          denseSpineMap(id) = mapToDenseSpine(spineParts(id))
      }
      id
    }
  }
  @transient lazy val spineCounts = new IntIntMap(Config.loadFactor, 1 << 12, -1)
  def addSpineOptionPSG(
    token: String, tag: String, spine: psg.Spine, add: Boolean
  ) = {
    val id = addSpinePSG(tag, spine)
    if (id >= 0) {
      val structuralFeatures = getPartsForPSG(id, true)
      for (feat <- structuralFeatures) spineCounts.changeOrPut(feat, 1)
    }
    if (id >= 0 && add) {
      val tokenID = wordIndex(token)
      tokenToSpines.getOrElseUpdate(tokenID, HashSet[Int]()).add(id)
      val tagID = nonTerminalIndex(tag)
      tagToSpines.getOrElseUpdate(tagID, HashSet[Int]()).add(id)
    }
    id
  }

  def addSpineOption(
    pos: Int, tag: String, parse: Parse, add: Boolean = true
  ) = parse match {
    case d: DepParse =>
    case p: psg.Graph =>
      addSpineOptionPSG(parse.tokens(pos), tag, p.spines(pos), add)
    case a: AMR =>
  }

  def simplifyToken(token: String) = {
    // Note - compressing successive digits (ie. 000.0 -> 0.0) does not give
    // much further reduction.
    // Also, trying:
    //    token.split("[0-9]").mkString("0")
    // Has unintuitive behaviour, for example:
    //   scala> "34.56".split("[0-9]")
    //   res10: Array[String] = Array("", "", .)
    val mergeNums = token.map{ v =>
      if ("1234567890".contains(v)) "0"
      else v
    }.mkString

    // Taking headlines and other rare uses of words with more than just
    // isolated letters capitalised, and lowercasing them:
    val caseChanged = "[A-Z][A-Z]".r.findFirstMatchIn(mergeNums).fold{
      mergeNums
      }{ _ =>
      mergeNums.toLowerCase
    }

    caseChanged
  }

  val suffix = if (Config.distinguishSuffix) "_SUFFIX" else ""
  def addToken(token: String) = {
    // Insert this token into the word index
    val tokenID = wordIndex(token)

    // Also insert the simplified version, and use that for the counting
    val modToken = simplifyToken(token)
    val modTokenID = wordIndex(modToken)
    tokenCounts.changeOrPut(modTokenID, 1)

    // The simple way of doing a suffix-based word representation.
    // The idea is we store all observed suffixes, and later will map a word
    // to the suffix that has occurred at least N times.
    // However! This does not behave as we might expect. For example, if the
    // word 'qwerty' occurs 1,000 times and then word 'werty' occurs once,
    // this will map both words to themselves. If our goal is to densify the
    // feature space this is unfortunate, as 'werty' will actually only be
    // seen once.
    // For the PTB, after mapping all digits to 0, there are 10,295 tokens
    // that fall into this case mapping to 2,051 suffixes, and the tokens
    // together occur 54,928 times.
    // The alternative approach, below, will reduce these values to 3364, 782,
    // and 14406 respectively.
    if (Config.simpleSuffixMap) {
      for (i <- (0 until (modToken.length - 1))) {
        val part = modToken.slice(i, modToken.length) + suffix
        val partID = wordIndex(part)
        tokenCounts.changeOrPut(partID, 1)
      }
    }
  }

  def createTokenMapping() = {
    if (Config.simpleSuffixMap) {
		  tokenCounts.foreachPair{ (id, count) =>
        val token = wordIndex.value(id)
        if (suffix == "" || ! token.endsWith(suffix)) {
          if (count >= Config.suffixMapCutoff) tokenMap.put(id, id)
          else {
            // Start at the smallest and work up to find the longest suffix
            // with the required frequency
            val mappedID =
              ((token.length - 2) until -1 by -1).foldLeft{
                val len = token.length
                val part = token.slice(len - 2, len) + suffix
                wordIndex(part)
              }{ (cur, index) =>
                val part = token.slice(index, token.length) + suffix
                val partID = wordIndex.getOrElse(part, -1)
                if (partID >= 0 &&
                    tokenCounts.getOrElse(partID, 0) >= Config.suffixMapCutoff)
                  partID
                else cur
              }
            tokenMap.put(id, mappedID)
          }
        }
      }
    } else {
      val tokensByLength = new ArrayBuffer[HashSet[String]]
		  tokenCounts.foreachPair{ (id, _) =>
        val token = wordIndex.value(id)
        while (tokensByLength.length <= token.length)
          tokensByLength.append(new HashSet[String])
        tokensByLength(token.length).add(token)
      }

      // Gradually combine counts by going through words from longest to
      // shortest, accumulating counts.
      for (length <- (tokensByLength.length - 1 until -1 by -1)) {
        for (word <- tokensByLength(length)) {
          // Check if a word or word suffix meets the cutoff
          val count = tokenCounts.get(wordIndex(word))
          if (count < Config.suffixMapCutoff && length > 2) {
            // If it doesn't, insert its next shorter version
            // Note, as a side effectm we will end up also mapping all
            // intermediate lengths, i.e. For qwertyuiop,
            //  qwertyuiop -> op
            //   wertyuiop -> op
            //    ertyuiop -> op
            // etc
            val modword =
              if (word.endsWith(suffix)) word.slice(1, word.length)
              else word.slice(1, word.length) + suffix
            tokenCounts.changeOrPut(wordIndex(modword), count)
            tokensByLength(length - 1).add(modword)
          }
        }
      }

      for (length <- (0 until tokensByLength.length)) {
        for (word <- tokensByLength(length)) {
          val wordID = wordIndex(word)
          val count = tokenCounts.get(wordID)
          if (count >= Config.suffixMapCutoff ||
              word.stripSuffix(suffix).length <= 2) {
            tokenMap.put(wordID, wordID)
          } else {
            // Because of ordering, we will only need to go one step down
            val modword =
              if (word.endsWith(suffix)) word.slice(1, word.length)
              else word.slice(1, word.length) + suffix
            val modwordID = wordIndex(modword)
            val mappedID = tokenMap.get(modwordID)
            tokenMap.put(wordID, mappedID)
          }
        }
      }
    }
  }

  def idsForTokens(tokens: Vector[String]) = {
    tokens.map{ token =>
      wordIndex.getOrElse(token, 0)
    }
  }

  def featuresForTokens(tokens: Vector[String]) = {
    tokens.map{ rawToken =>
      val token = simplifyToken(rawToken)
      val tokenID = wordIndex.getOrElse(token, -1)
      if (tokenID >= 0) tokenMap.get(tokenID)
      else {
        // Handling unseen tokens
        val length = token.length
        ((length - 2) until -1 by -1).foldLeft{
          val part = token.slice(length - 2, length) + suffix
          val partID = wordIndex(part)
          tokenMap.getOrElseUpdate(partID, partID)
        }{ (cur, index) =>
          val part = token.slice(index, length) + suffix
          val partID = wordIndex.getOrElse(part, -1)
          if (partID >= 0) tokenMap.get(partID)
          else cur
        }
      }
    }
  }


  // ======================================================
  // Scoring

  def getInitScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int,
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ): Double
  def getArcScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, direction: Int, length: Int,
    scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit,
    useCache: Boolean
  ): Double
  def getInitScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int, 
    wIndex: Int, spine: Int, countOnGet: (Int, Double) => Unit
  ): Double
  def getArcScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    chidlIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, direction: Int, length: Int, scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit, useCache: Boolean, edgeSpace: Int
  ) : Double

  def getSpineOptionsByToken(token: Int) =
    tokenToSpines.getOrElse(token, HashSet(EmptySpineID))
  def getSpineOptionsByTag(tag: Int) =
    tagToSpines.getOrElse(tag, HashSet(EmptySpineID))

  def getInitScore(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], sentID: Int,
    wIndex: Int, spine: Int, gold: Boolean, countOnGet: (Int, Double) => Unit
  ) = {
    if (formalism == Formalism.DEP)
      getInitScoreDep(words, prefixes, tags, sentID, wIndex, spine, countOnGet)
    else if (formalism == Formalism.PSG)
      getInitScorePSG(words, prefixes, tags, sentID, wIndex, spine, countOnGet)
    else 1.0
  }

  def getArcScore(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int],
    childIndex: Int, parentIndex: Int, childSpine: Int, parentSpine: Int,
    arg: Int, length: Int, gold: Boolean, scoreCache: Array[Double],
    surfaceFeatureLists: Array[UnboxedArrayBuffer],
    countOnGet: (Int, Double) => Unit, useCache: Boolean, edgeSpace: Int
  ) = {
    val direction = Model.arcToDirection(childIndex, parentIndex)
    if (formalism == Formalism.DEP)
      getArcScoreDep(words, prefixes, tags, childIndex, parentIndex,
        childSpine, parentSpine, arg, direction, length,
        scoreCache, surfaceFeatureLists, countOnGet, useCache)
    else if (formalism == Formalism.PSG)
      getArcScorePSG(words, prefixes, tags, childIndex, parentIndex,
        childSpine, parentSpine, arg, direction, length,
        scoreCache, surfaceFeatureLists, countOnGet, useCache, edgeSpace)
    else 1.0
  }

  def getArcCoarseScore(
    score: Double, childIndex: Int, parentIndex: Int, arg: Int,
    source: ParserType.Value, countOnGet: (Int, Double) => Unit
  ) : Double
  def getSpineCoarseScore(
    score: Double, index: Int, sentenceLength: Int, spine: Int,
    source: ParserType.Value, countOnGet: (Int, Double) => Unit
  ) : Double

  def getBinaryScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, stateLeft: Int, stateMiddle: Int,
    stateRight: Int, spineLeft: Int, spineSplit1: Int, spineSplit2: Int,
    spineRight: Int, spineXL: Int, spineXM: Int, spineXR: Int, sentenceID: Int,
    scoreCache: Array[Double], countOnGet: (Int, Double) => Unit,
    useCache: Boolean
  ) : Double

  def getBinaryScore(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, stateLeft: Int, stateMiddle: Int,
    stateRight: Int, spineLeft: Int, spineSplit1: Int, spineSplit2: Int,
    spineRight: Int, spineXL: Int, spineXM: Int, spineXR: Int, sentenceID: Int,
    scoreCache: Array[Double], countOnGet: (Int, Double) => Unit,
    useCache: Boolean, lossType: LossType.Value
  ) = {
    if (formalism == Formalism.DEP) 0.0
    else if (formalism == Formalism.PSG)
      getBinaryScorePSG(words, prefixes, tags, left, split1, split2,
        right, stateLeft, stateMiddle, stateRight, spineLeft,
        spineSplit1, spineSplit2, spineRight, spineXL, spineXM, spineXR,
        sentenceID, scoreCache, countOnGet, useCache)
    else 0.0
  }

  def getFullBinaryScoreDep(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit
  ) : Double

  def getFullBinaryScorePSG(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit
  ) : Double

  def getFullBinaryScore(
    words: Vector[Int], prefixes: Vector[Int], tags: Vector[Int], left: Int,
    split1: Int, split2: Int, right: Int, focus: Int,
    lStructArray: UnboxedArrayBuffer, lStructPos: Int,
    mStructArray: UnboxedArrayBuffer, mStructPos: Int,
    rStructArray: UnboxedArrayBuffer, rStructPos: Int,
    countOnGet: (Int, Double) => Unit,
    lossType: LossType.Value
  ) =
    if (formalism == Formalism.DEP)
      getFullBinaryScoreDep(words, prefixes, tags, left, split1, split2,
        right, focus, lStructArray, lStructPos, mStructArray,
        mStructPos, rStructArray, rStructPos, countOnGet)
    else if (formalism == Formalism.PSG)
      getFullBinaryScorePSG(words, prefixes, tags, left, split1, split2,
        right, focus, lStructArray, lStructPos, mStructArray,
        mStructPos, rStructArray, rStructPos, countOnGet)
    else 0.0

  // ============================================
  // Gold state tracking

  def setGoldDep(
    parse: DepParse, deps: UnboxedArrayBuffer, tags: ArrayBuffer[Int],
    add: Boolean, stage: Stage
  ) = {
    for (tag <- parse.tags.map(nonTerminalIndex(_)))
      tags.append(tag)
    tags.append(RootNT)
    val sentenceLength = parse.tokens.length + 1
    deps.extend(sentenceLength * sentenceLength, -1)
    for (dep <- parse.deps) {
      val pos = dep.child + (dep.parent * sentenceLength)
      if (deps(pos) >= 0) throw new Exception("Same dep twice")
      deps(pos) = argIndex(dep.label)
    }
  }

  // TODO: Can we reduce redundancy by using parseToArrays here?
  // Note - This assumes edges are ordered with structural ones first.
  def setGoldPSG(
    parse: psg.Graph, goldEdges: UnboxedArrayBuffer,
    goldSpines: ArrayBuffer[Int], add: Boolean, stage: Stage
  ) = {
    if (goldSpines != null) {
      if (stage.parserType == ParserType.ARC ||
          stage.parserType == ParserType.TSCORER) {
        for (i <- 0 to parse.tokens.length)
          goldSpines.append(NULL_SPINE)
      } else {
        for (i <- 0 until parse.tokens.length) {
          val tag = parse.tags(i)
          val spine = parse.spines(i)
          val id = addSpinePSG(tag, spine, add)
          if (id >= 0) goldSpines.append(id)
          else goldSpines.append(BackupSpineID)
        }
        goldSpines.append(RootSpineID)
      }
    }

    if (goldEdges != null) {
      val sentenceLength = parse.tokens.length + 1
      goldEdges.extend(sentenceLength * sentenceLength * 3, -1)
      if (stage.parserType == ParserType.TSCORER) {
        // The first three edges between a given pair (child, parent) are
        // saved. Note - they are saved in no particular order, breaking
        // assumptions in the checking code, but that's resolved in the
        // traceScorer.
        val done = new HashMap[(Int, Int), Int]
        val doneTypes = new HashMap[(Int, Int), HashSet[Int]]
        for (edge <- parse.edges) {
          if (edge.trace != "_") {
            val child = edge.src
            val parent = edge.target
            val count = done.getOrElse((child, parent), 0)
            if (count == 0) doneTypes((child, parent)) = new HashSet[Int]()
            if (count < 3) {
              val parentTag = parse.tags(parent)
              val childTag = parse.tags(child)
              val arg = getTraceEdgeID(edge.trace, childTag, parentTag)
              if (! doneTypes((child, parent)).contains(arg)) {
                val pos = (child + (parent * sentenceLength)) * 3 + count
                goldEdges(pos) = arg
                done((child, parent)) = count + 1
                doneTypes((child, parent)).add(arg)
              }
            }
          }
        }

        var child = 0
        while (child < sentenceLength - 1) {
          var parent = 0
          while (parent < sentenceLength - 1) {
            if (child != parent && done.getOrElse((child, parent), 0) == 0) {
              val parentTag = parse.tags(parent)
              val childTag = parse.tags(child)
              val arg = getTraceEdgeID(Model.SNonTrace, childTag, parentTag)
              val pos = (child + (parent * sentenceLength)) * 3
              goldEdges(pos) = arg
              done((child, parent)) = 1
            }
            parent += 1
          }
          child += 1
        }
      } else {
        val seenT = new IntSet(Config.loadFactor, 20)
        val seenC = new IntSet(Config.loadFactor, 20)
        for (edge <- parse.dag1ecEdges) {
          val child = edge.src
          val parent = edge.target
          val childSpine = parse.spines(child)
          val parentSpine =
            if (parent < parse.spines.length) parse.spines(parent)
            else RootSpine
          val parentTag =
            if (parent < parse.tags.length)
              nonTerminalIndex(parse.tags(parent), true)
            else RootTag
          val childTag = nonTerminalIndex(parse.tags(child), true)
          val tlabel = addEdgePSG(edge, childSpine, childTag, parentSpine,
            parentTag, add)
          val label =
            if (tlabel >= 0) tlabel
            else {
              Log.logln(s"Backing off to backup for $edge ($child, $parent) $tlabel $childTag $parentSpine $parentTag")
              if (edge.trace == "_") BackupLabelStructural
              else if (edge.trace.contains("chain")) BackupLabelChain
              else BackupLabelTrace
            }
          val edgeType = getEdgeType(edge.trace == "_", edge.trace.contains("chain"))
          val pos = (child + (parent * sentenceLength)) * 3 + edgeType
          if (edgeType == 0) {
            goldEdges(pos) = label
          } else if (edgeType == 1) {
            if (! seenC.contains(child, parent)) {
              seenC.add(child, parent)
              goldEdges(pos) = label
            }
          } else {
            if (! seenT.contains(child, parent)) {
              seenT.add(child, parent)
              goldEdges(pos) = label
            }
          }
        }
      }
    }
  }

  def setGoldParse(
    parse: Parse, goldEdges: UnboxedArrayBuffer,
    goldSpines: ArrayBuffer[Int], add: Boolean, stage: Stage
  ) = parse match {
    case d: DepParse => setGoldDep(d, goldEdges, goldSpines, add, stage)
    case p: psg.Graph => setGoldPSG(p, goldEdges, goldSpines, add, stage)
    case a: AMR => // TODO
  }

  def getEdgeType(structural: Boolean, chain: Boolean) = {
    if (structural) 0
    else if (chain) 1
    else 2
  }

  def checkArcIsGold(
    parent: Int, child: Int, goldEdges: UnboxedArrayBuffer,
    sentenceLength: Int, arg: Int, structural: Boolean, chain: Boolean,
    checkArg: Boolean = true
  ) = {
    if (goldEdges == null || goldEdges.length < 3) false
    else {
      val edgeType = getEdgeType(structural, chain)
      val pos = (child + (parent * sentenceLength)) * 3 + edgeType
      val gold = goldEdges(pos)
      (
        (gold >= 0 && (!checkArg || gold == arg)) ||
        (gold < 0 && arg < 0)
      )
    }
  }
  def goldEdgesToTraces(
    goldEdges: UnboxedArrayBuffer, sentenceLength: Int,
    parserType: ParserType.Value
  ) = {
    // TODO: Speed this up
    val traces = new HashSet[(Int, Int, Boolean)]
    val counts = new LongLongMap(Config.loadFactor, 1024)
    val insideCounts = new LongLongMap(Config.loadFactor, 1024)
    def updateCounts(left: Int, right: Int, isStructural: Boolean) = {
      var inside = left + 1
      while (inside < right) {
        val curL = counts.get2IntsOrElse(left, inside, (0, 0))
        if (isStructural) counts.put2Ints(left, inside, (curL._1 + 1, curL._2))
        else counts.put2Ints(left, inside, (curL._1, curL._2 + 1))
        val curR = counts.get2IntsOrElse(right, inside, (0, 0))
        if (isStructural) counts.put2Ints(right, inside, (curR._1 + 1, curR._2))
        else counts.put2Ints(right, inside, (curR._1, curR._2 + 1))

        var outsideLeft = 0
        while (outsideLeft < left) {
          var outsideRight = right + 1
          while (outsideRight < sentenceLength) {
            var inside2 = inside
            while (inside2 < right) {
              val cur = insideCounts.get2IntsOrElse(outsideLeft, inside,
                inside2, outsideRight, (0, 0))
              if (isStructural)
                insideCounts.put2Ints( outsideLeft, inside, inside2,
                  outsideRight, (cur._1 + 1, cur._2))
              else
                insideCounts.put2Ints( outsideLeft, inside, inside2,
                  outsideRight, (cur._1, cur._2 + 1))
              inside2 += 1
            }
            outsideRight += 1
          }
          outsideLeft += 1
        }

        inside += 1
      }
    }
    if (
      parserType != ParserType.TSCORER &&
      parserType != ParserType.SSCORER &&
      goldEdges != null && goldEdges.length >= 3
    ) {
      var child = 0
      while (child < sentenceLength) {
        var parent = 0
        while (parent < sentenceLength) {
          if (child != parent) {
            val left = child.min(parent)
            val right = child.max(parent)
            val pos = (child + (parent * sentenceLength)) * 3
            if (goldEdges(pos) >= 0) updateCounts(left, right, true)
            if (goldEdges(pos + 1) >= 0) {
              traces.add((left, right, true))
              updateCounts(left, right, false)
            }
            if (goldEdges(pos + 2) >= 0) {
              traces.add((left, right, false))
              updateCounts(left, right, false)
            }
          }
          parent += 1
        }
        child += 1
      }
    }
    (traces, counts, insideCounts)
  }

  def spineIsGold(pos: Int, spine: Int, goldSpines: ArrayBuffer[Int]) =
    goldSpines != null && goldSpines.length > pos && goldSpines(pos) == spine

  def stateIsGold(
    cell: Int, state: Int, lSpine: Int, rSpine: Int, xSpine: Int,
    goldStates: Set[(Int, Int, Int, Int, Int)]
  ) = goldStates != null &&
    goldStates.contains((cell, state, lSpine, rSpine, xSpine))

  // ============================================
  // Conversion to/from chart representation

  def parseToArraysDep(parse: DepParse, stage: Stage) = {
    val arcs = ArrayBuffer[(Int, Int, Int)]()
    val terminals = ArrayBuffer[(Int, Int)]()
    for ((tag, index) <- parse.tags.zipWithIndex) {
      terminals.append((index, NULL_SPINE))
    }
    terminals.append((terminals.length, NULL_SPINE))

    for (dep <- parse.deps) {
      val arg = argIndex(dep.label)
      val arc = (dep.child, dep.parent, arg)
      arcs.append(arc)
    }
    (terminals, arcs)
  }

  // Note - This assumes edges are ordered with structural ones first.
  def parseToArraysPSG(parse: psg.Graph, add: Boolean, stage: Stage) = {
    val arcs = ArrayBuffer[(Int, Int, Int)]()
    val terminals = ArrayBuffer[(Int, Int)]()

    val tags = parse.tags ++ Vector(Model.SRootTag)
    for ((spine, index) <- parse.spines.zipWithIndex) {
      val tag = tags(index)
      if (stage.parserType == ParserType.ARC ||
          stage.parserType == ParserType.TSCORER)
        terminals.append((index, NULL_SPINE))
      else {
        val id = addSpinePSG(tag, spine, add)
        if (id >= 0) terminals.append((index, id))
        else terminals.append((index, BackupSpineID))
      }
    }
    if (stage.parserType == ParserType.ARC ||
        stage.parserType == ParserType.TSCORER)
      terminals.append((terminals.length, NULL_SPINE))
    else
      terminals.append((terminals.length, RootSpineID))

    // Add edges
    if (stage.parserType == ParserType.TSCORER) {
      val done = new HashMap[(Int, Int), Int]
      val doneTypes = new HashMap[(Int, Int), HashSet[Int]]
      for (edge <- parse.edges) {
        if (edge.trace != "_") {
          val child = edge.src
          val parent = edge.target
          val count = done.getOrElse((child, parent), 0)
          if (count == 0) doneTypes((child, parent)) = new HashSet[Int]
          if (count < 3) {
            val parentTag = parse.tags(parent)
            val childTag = parse.tags(child)
            val arg = getTraceEdgeID(edge.trace, childTag, parentTag)
            if (! doneTypes((child, parent)).contains(arg)) {
              val arc =
                if (arg >= 0) (child, parent, arg)
                else (child, parent, BackupLabelTrace)

              arcs.append(arc)
              done((child, parent)) = count + 1
              doneTypes((child, parent)).add(arg)
            }
          }
        }
      }

      val sentenceLength = parse.tokens.length + 1
      var child = 0
      while (child < sentenceLength - 1) {
        var parent = 0
        while (parent < sentenceLength - 1) {
          if (child != parent && done.getOrElse((child, parent), 0) == 0) {
            val parentTag = parse.tags(parent)
            val childTag = parse.tags(child)
            val arg = getTraceEdgeID(Model.SNonTrace, childTag, parentTag)
            done((child, parent)) = 1
            arcs.append((child, parent, arg))
          }
          parent += 1
        }
        child += 1
      }
    } else {
      // Filter for producibility (mostly done in the parse.dag1ecEdges call):
      //   - only double+ edges of types (TC, ST, SC, STC)
      val seenT = new IntSet(Config.loadFactor, 20)
      val seenC = new IntSet(Config.loadFactor, 20)
      for (edge <- parse.dag1ecEdges) {
        val child = edge.src
        val parent = edge.target
        val childSpine = parse.spines(child)
        val parentSpine =
          if (parent < parse.spines.length) parse.spines(parent)
          else RootSpine
        val parentTag =
          if (parent < parse.tags.length)
            nonTerminalIndex(parse.tags(parent), true)
          else RootTag
        val childTag = nonTerminalIndex(parse.tags(child), true)
        val arg = addEdgePSG(edge, childSpine, childTag, parentSpine,
          parentTag, add)
        val arc =
          if (arg >= 0) (child, parent, arg)
          else (child, parent, BackupLabelStructural)

        // At the moment we just take the first edge of each type.
        if (edge.trace == "_") {
          arcs.append(arc)
        } else if (edge.trace.contains("chain")) {
          if (! seenC.contains(child, parent)) {
            seenC.add(child, parent)
            arcs.append(arc)
          }
        } else {
          if (! seenT.contains(child, parent)) {
            seenT.add(child, parent)
            arcs.append(arc)
          }
        }
      }
    }

    (terminals, arcs)
  }

  def parseToArraysAMR(parse: AMR, stage: Stage) = {
    val arcs = ArrayBuffer[(Int, Int, Int)]()
    val terminals = ArrayBuffer[(Int, Int)]()
      // TODO
    (terminals, arcs)
  }

  // Convert to the internal chart representation. Add is used to control
  // whether the model is updated in the process (convenient in training, but
  // not allowed if doing evaluation internally).
  def parseToArrays(parse: Parse, add: Boolean, stage: Stage) =
    parse match {
      case d: DepParse => parseToArraysDep(d, stage)
      case p: psg.Graph => parseToArraysPSG(p, add, stage)
      case a: AMR => parseToArraysAMR(a, stage)
    }

  def alignSpineSides(
    leftPairs: ArrayBuffer[(Int, String)],
    rightPairs: ArrayBuffer[(Int, String)], top: String
  ) = {
    // This cannot be perfect, since the arcs can be inconsistent (e.g. a top
    // that is the POS, but an arc points to a value). Use a simple greedy
    // approach.
    val symbols = ArrayBuffer[String]()

    // Insert all of left, ignoring duplicates
    for ((_, symbol) <- leftPairs) {
      if (symbols.length == 0 || symbols.last != symbol)
        symbols.append(symbol)
    }

    // Insert right as needed
    var cpos = -1
    for ((_, symbol) <- rightPairs) {
      var npos = cpos
      var matched = false
      while (npos < symbols.length && !matched) {
        if (npos >= 0 && symbols(npos) == symbol) matched = true
        else npos += 1
      }
      if (matched) cpos = npos
      else {
        cpos += 1
        symbols.insert(cpos, symbol)
      }
    }

    // Insert top if needed, it could be null because the arc parser may have
    // created arcs leading here and so indicating there is are non-terminals,
    // but the departing structural arc may be starting at the POS tag.
    if (top != null && (symbols.length == 0 || symbols.last != top))
      symbols.append(top)

    symbols
  }

  def arraysToParsePSG(
    arcArray: ArrayBuffer[(Int, Int, Int)],
    spineArray: ArrayBuffer[(Int, Int)], sentence: String,
    tokens: Vector[String], inputTags: Vector[String]
  ) = {
///    Log.logln(s"Debug: $sentence\nDebug: $inputTags\nDebug: $spineArray\nDebug: $arcArray\n")
    val tags = inputTags.toArray
    val curSpines = Array.ofDim[psg.Spine](tokens.length)

    // If all spines have been collapsed, e.g. in the arcParser, construct
    // spines based on the arc labels.
    var spineless = true
    for ((pos, spine) <- spineArray)
      if (spine != NULL_SPINE)
        spineless = false

    if (!spineless) {
      for ((pos, spineID) <- spineArray) {
        if (pos < spineArray.length - 1) { // Don't record a root spine
          val original = spinesOriginal(spineID)
          val nodes = original.nodes
          val traces = original.traces.map{ e =>
            new psg.Edge(pos, e.srcNode, pos, e.targetNode, e.trace)
          }
          curSpines(pos) = new psg.Spine(nodes, traces)
        }
      }
    } else {
      // Backup
      for (pos <- 0 until curSpines.length)
        curSpines(pos) = psg.Spine.fromText("_")
      // Collect information from arcs:
      //  - The top of each spine (indicated by the arg leading away from it)
      //  - Labels in the spine (indicated by arcs linking to it)
      val prespines = new HashMap[Int, ArrayBuffer[(Int, String)]]
      val tops = new HashMap[Int, String]
      val ensurePresent = new HashMap[Int, ArrayBuffer[String]]
      for ((child, parent, label) <- arcArray) {
        val edge = edges(label)
        val srcInfo =
          nonTerminalIndex.value(getEdgeSrc(edge)._1) +
          getEdgeSrc(edge)._2.map{ func =>
            "-"+ nonTerminalIndex.value(func)
          }.mkString
        val srcIsNull = getEdgeSrc(edge)._4 > 0
        val targetInfo =
          nonTerminalIndex.value(getEdgeTarget(edge)._1) +
          getEdgeTarget(edge)._2.map{ func =>
            "-"+ nonTerminalIndex.value(func)
          }.mkString
        val targetIsNull = getEdgeTarget(edge)._4 > 0
        val trace = getEdgeTrace(edge).stripSuffix("_chain")

        // Add to info for child
        if (trace == "_") {
          if (srcInfo != "_" && srcInfo != "NO_TAG") tops(child) = srcInfo

          // Add to the parent (skipping root)
          if (parent < tokens.length - 1 && targetInfo != "NO_TAG") {
            prespines.getOrElseUpdate(parent,
              new ArrayBuffer[(Int, String)]
            ).append((parent, targetInfo))
          }
        } else {
          val childEnsure =
            ensurePresent.getOrElseUpdate(child, new ArrayBuffer[String])
          val parentEnsure =
            ensurePresent.getOrElseUpdate(parent, new ArrayBuffer[String])

          val pSym =
            if (! targetIsNull) targetInfo
            else "("+ targetInfo +"_("+ trace +"))"
          val cSym =
            if (! srcIsNull) srcInfo
            else if (! targetIsNull) "("+ srcInfo +"_(" + trace + "))"
            else srcInfo
          childEnsure.append(cSym)
          parentEnsure.append(pSym)

          // Ensure these get considered
          prespines.getOrElseUpdate(child, new ArrayBuffer[(Int, String)])
          prespines.getOrElseUpdate(parent, new ArrayBuffer[(Int, String)])
        }
      }
      for ((pos, prespine) <- prespines) {
        val left = prespine.filter(_._1 < pos).sortWith(_._1 > _._1)
        val right = prespine.filter(_._1 >= pos).sorted
        val top = tops.getOrElse(pos, null)
        val spineText = alignSpineSides(left, right, top)
///        Log.logln(s"$pos $spineText from $prespines $top")
        val text =
          ensurePresent.get(pos).fold(spineText){ extraSymbols =>
            if (spineText.length == 0) extraSymbols
            else
              spineText.slice(0, spineText.length - 1) ++
              extraSymbols.filter(!spineText.contains(_)) ++
              ArrayBuffer(spineText.last)
          }.mkString("_")
///        Log.logln(s"$pos $text")
        curSpines(pos) = psg.Spine.fromText(text)
      }
    }

    val finalEdges = ArrayBuffer[psg.Edge]()
    for ((src, target, label) <- arcArray) {
      val edge = edges(label)
      if (! edge._4) {
        val srcNode = {
          val info = getEdgeSrc(edge)
          val symbol = nonTerminalIndex.value(info._1)
          val functions = info._2.map(nonTerminalIndex.value(_))
          new psg.Node(symbol, functions, info._3, info._4)
        }
        val targetNode = {
          val info = getEdgeTarget(edge)
          val symbol = nonTerminalIndex.value(info._1)
          val functions = info._2.map(nonTerminalIndex.value(_))
          new psg.Node(symbol, functions, info._3, info._4)
        }
        val trace = getEdgeTrace(edge)
        finalEdges.append(
          new psg.Edge(src, srcNode, target, targetNode, trace))
      }
    }

    psg.Graph(sentence, tokens, tags.toVector, curSpines.toVector,
      finalEdges.toVector)
  }

  def arraysToParseDep(
    arcs: ArrayBuffer[(Int, Int, Int)],
    terminals: ArrayBuffer[(Int, Int)], sentence: String,
    tokens: Vector[String], inputTags: Vector[String]
  ) = {
    val deps = ArrayBuffer[Dependency]()
    for ((child, parent, label) <- arcs)
      deps.append(Dependency(child, parent, argIndex.value(label)))
    val tags = Array.ofDim[String](tokens.length)

    var spineless = true
    for ((pos, unaryID) <- terminals)
      if (unaryID >= 0) spineless = false
    if (!spineless)
      for ((pos, tag) <- terminals)
        tags(pos) = nonTerminalIndex.value(tag)

    DepParse(sentence, tokens, tags.toVector,
      deps.sortWith(_.child < _.child).toVector)
  }

  def arraysToParseAMR(
    arcs: ArrayBuffer[(Int, Int, Int)],
    terminals: ArrayBuffer[(Int, Int)], sentence: String,
    tokens: Vector[String], inputTags: Vector[String]
  ) = new AMR(sentence, tokens, new Concept("a", "amr-unknown"))

  def arraysToParse(
    arcs: ArrayBuffer[(Int, Int, Int)],
    terminals: ArrayBuffer[(Int, Int)], sentence: String,
    tokens: Vector[String], parseType: Formalism.Value,
    inputTags: Vector[String]
  ) = {
    if (parseType == Formalism.AMR)
      arraysToParseAMR(arcs, terminals, sentence, tokens, inputTags)
    else if (parseType == Formalism.DEP)
      arraysToParseDep(arcs, terminals, sentence, tokens, inputTags)
    else
      arraysToParsePSG(arcs, terminals, sentence, tokens, inputTags)
  }

  // ======================================================

  def sentString = ""

  override def toString() = {
    val ans = ArrayBuffer("Model Values:\n")

    ans.append(s"\nModel spine options for words: (${tokenToSpines.size} total)")
    for ((word, options) <- tokenToSpines) {
      val sword = wordIndex.value(word)
      ans.append(s"Spine_Opt_Tok ${word}_$sword ${options.mkString(" ")}")
    }
    ans.append(s"\nModel spine options for tags: (${tagToSpines.size} total)")
    for ((tag, options) <- tagToSpines) {
      val stag = nonTerminalIndex.value(tag)
      ans.append(s"Spine_Opt_Tag ${tag}_$stag ${options.mkString(" ")}")
    }

    ans.append(s"\nModel Edges: (${edges.size} total), (child, parent, trace)")
    for ((vals, id) <- edges.zipWithIndex) {
      if (vals != null) {
        val text =
          vals._1.map{ case (symbol, functions, count, nullDepth) =>
            nonTerminalIndex.value(symbol) +
            functions.map("-"+ nonTerminalIndex.value(_)).mkString("") +
            s"_$count.$nullDepth"
          }.mkString(" ") +
          (if (vals._2) " Struct" else " Trace") +
          (if (vals._3) "-Chain" else "") +
          (if (vals._4) " NonTrace" else "")

        val rule = argIndex.value(id) // This will capture the features (._5)
        val denseRule = mapToDenseArg(edges(id)._5)

        ans.append(f"Edge $id% 5d , $text , $rule dense: $denseRule")
      }
    }

    ans.append(s"\nModel Spines: (${spines.filter(_ != null).size} total)")
    for ((info, id) <- spines.zipWithIndex) {
      if (info != null) {
        val parts = spineParts(id).map( nonTerminalIndex.value(_) )
        val dense = denseSpineMap(id)
        ans.append(f"Spine $id% 5d ${nonTerminalIndex.value(id)} with $parts $dense")
      }
    }

    ans.append(s"\nModel Allowed Args: (id, #allowed)")
    for ((id, vals) <- allowedArgLists)
      ans.append(f"Allowed $id% 5d  ${vals.length}")

    ans mkString "\n"
  }

  def writeToText(filename: String) = {
    val modelOut = new PrintWriter(new File(filename))
    modelOut.println(toString())
    modelOut.close()
  }
}

object Model {
  // Always used as the 0 index of the indexers
  val objectZero = "__SPECIAL_ZERO_CASE__"
  // A special case for non-words and non-args
  val SNon = "__NON__"
  // AMR specific - used for words that don't map to a concept
  val SNull = "__NULL__"
  // Artificial root word
  val SRootWord = "ROOTWORD"
  val SRootTag = "ROOTTAG"
  val SRootNT = "ROOT"
  val SRootArg = "ROOTDEP"
  // A special case to use when inserting a gold structure (but not wanting to
  // adjust the model)
  val SGoldTag = "GOLD_TAG"
  val SGoldNT = "GOLD_NT"
  val SNoTag = "NO_TAG"
  // Special symbol for an arc that does not exist (for trace scoring)
  // An underscore cannot be used as we wish to be able to distinguish the two.
  val SNonTrace = "*NON*"

  final def arcToDirection(child: Int, parent: Int) =
    if (child < parent) 1
    else 0

  def apply(filename: String) = {
    val obj = ObjectReader.load(filename)
    import scala.reflect.ClassTag
    obj match {
      case model: Model => model
      case a =>
        require(false, "Invalid model type in file "+
          s"$filename ${ClassTag(a.getClass)} ${a.getClass}")
        null
    }
  }
}

