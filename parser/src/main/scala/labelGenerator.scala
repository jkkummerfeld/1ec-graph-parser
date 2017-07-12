// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.HashSet

// Selection of valid arc labels
class LabelGenerator(
  model: Model
) {
  // When using tags to constrain edges
  val EMPTY = new UnboxedArrayBuffer(0)
  var curPSGArgs : UnboxedArrayBuffer = null
  var curPSGArgsPos : Int = 0
  var nextLabel : Int = -1
  var nextMustBeTrace = false
  var mustBeTrace = false

  // Work out the possible args for this spine pair
  val traceOptions = HashSet("=", "*", "*?*", "*EXP*", "*ICH*", "*NOT*",
    "*PPA*", "*RNR*", "*T*", "*U*", "0")
  private def getPSGArgOptions(
    childSpineID: Int, parentSpineID: Int, childTag: Int, parentTag: Int
  ) = {
    val childSpine = model.spinesOriginal(childSpineID)
    val parentSpine = model.spinesOriginal(parentSpineID)

    val ans = new UnboxedArrayBuffer(parentSpine.nodes.length + 1)

    var parentPos = 0
    while (parentPos < parentSpine.nodes.length) {
      // Try adding structural, going from child top
      val option = model.addEdgePSG(childSpine.nodes.length - 1, childSpine,
        childTag, parentPos, parentSpine, parentTag, "_", false)
      if (option >= 0) ans.append(option)

      // Try adding traces
      var childPos = 0
      while (childPos < childSpine.nodes.length) {
        // Determine trace
        // Source null, get the next symbol in the chain
        // Target null, get the next symbol in the chain
        // Neither null, but this has been seen with '=', allow '='
        val trace =
          if (childSpine.nodes(childPos).isNull)
            childSpine.nodes(childPos + 1).symbol
          else if (parentSpine.nodes(parentPos).isNull)
            parentSpine.nodes(parentPos + 1).symbol
          else "="

        // The code above will select symbols that are not always traces, for
        // example:
        // The S in:  ADJP_(NP_(*T*))_S
        // The S in:  (SBAR_(0)_(S_(*T*)))_VP_S
        if (traceOptions.contains(trace)) {
          val option = model.addEdgePSG(childPos, childSpine, childTag,
            parentPos, parentSpine, parentTag, trace, false)
          if (option >= 0) ans.append(-option)
          if (trace == "*" || trace == "*T*") {
            val traceOption = model.addEdgePSG(childPos, childSpine, childTag,
              parentPos, parentSpine, parentTag, trace +"_chain", false)
            if (traceOption >= 0) ans.append(-traceOption)
          }
        }
        childPos += 1
      }
      parentPos += 1
    }

    ans
  }

  def prepare(
    childTag: Int, parentTag: Int, childSpineID: Int, parentSpineID: Int,
    span: (Int, Int), sentenceLength: Int, parentIsRoot: Boolean,
    arcPassConstraint: ArcPassConstraint.Value
  ) = {
///    Log.logln(s"Arc Gen: $childTag $parentTag $childSpineID $parentSpineID $span $sentenceLength $parentIsRoot ${model.EmptySpineID}")
    nextLabel = -1
    mustBeTrace = false
    if (model.formalism == Formalism.PSG) {
      // Get the list os options
      if (model.filterByTag) {
        curPSGArgs = model.allowedArgList(arcPassConstraint, childTag,
          parentTag)
      } else {
        val key = (childTag, parentTag, childSpineID, parentSpineID)
        // These don't need to be synchronised as we made the argMap without
        // allowing resizing.
        val argPos = model.argMap.getOrElse(key, -1).toInt
        curPSGArgs = EMPTY
        if (argPos >= 0) {
          curPSGArgs = model.argMapVals(argPos)
        } else if (argPos == -1) {
          curPSGArgs = getPSGArgOptions(childSpineID, parentSpineID, childTag,
            parentTag)
          val npos =
            if (curPSGArgs.length == 0) -2
            else {
              model.argMapVals.synchronized {
                model.argMapVals.append(curPSGArgs)
                model.argMapVals.length - 1
              }
            }
          model.argMap.synchronized {
            model.argMap.put(key, npos.toLong)
          }
        }
      }

      // Prepare the first
      curPSGArgsPos = -1
      next
    } else if (model.formalism == Formalism.DEP) {
      if (span._2 == sentenceLength - 1) {
        if (parentIsRoot) nextLabel = model.RootArg
      } else nextLabel = model.NullArg + 1
    } else {
      // If direction is with parent to the right, and the spine is the null
      // spine, set to NullArg
      // If parent is a literal, set to argIndex.size
      if (span._2 == sentenceLength - 1) {
        if (parentIsRoot) nextLabel = model.RootArg
      } else nextLabel = model.NullArg + 1
    }
  }

  def hasNext = nextLabel >= 0
  def next = {
    // Answer
    val ans = nextLabel
    mustBeTrace = nextMustBeTrace

    // Prepare for the next call
    if (model.formalism == Formalism.PSG) {
      curPSGArgsPos += 1
      if (curPSGArgsPos >= curPSGArgs.length) nextLabel = -1
      else {
        val option = curPSGArgs(curPSGArgsPos)
        nextLabel = option.abs
        nextMustBeTrace = option < 0
      }
    } else if (model.formalism == Formalism.DEP) {
      if (nextLabel == model.RootArg) nextLabel = -1
      else if (nextLabel >= model.argIndex.size - 1) nextLabel = -1
      else nextLabel += 1
    } else {
      // If arg == NullArg, set to -1
      if (nextLabel == model.RootArg) nextLabel = -1
      else nextLabel += 1
    }

    // Return the saved answer
    ans
  }
}
