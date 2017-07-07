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
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap}

@SerialVersionUID(1L)
class PositionReference(
  val position: Char,
  val item: Char
) extends Serializable {
  def getValue(
    goal: (Int, Int, Int), items: List[(Int, Int, Int)], cur: Int
  ) = {
    if (item == '_' || item == '-') -1
    else if (item == '+') items(cur)._1 + 1
    else {
      val reference =
        if (item == '0') items(0)
        else if (item == '1') items(1)
        else if (item == 'P') goal
        else throw new IllegalStateException(s"$item invalid in rules.txt")
      if (position == 'i') reference._1
      else if (position == 'j') reference._2
      else if (position == 'x') reference._3
      else throw new IllegalStateException(s"$position invalid in rules.txt")
    }
  }
}

class TernaryRuleIterator {
  var rules : ArrayBuffer[(Int, Int, Int, Int, Int, Int, Int)] = null
  var span : (Int, Int) = null
  var split1 : Int = -1

  var rulePos : Int = -1
  var idMiddleNext = -1
  var idRightNext = -1
  var idGoalNext = -1
  var idMiddle = -1
  var idRight = -1
  var idGoal = -1
  def next = {
    idMiddle = idMiddleNext
    idRight = idRightNext
    idGoal = idGoalNext
    prepareNext
  }
  def hasNext = idMiddleNext != -1

  @inline final def infoToID(coreID: Int, external: Int, arc: Int) =
    arc << 25 | coreID | (external << 16)

  def prepareNext = {
    if (rulePos < rules.length) {
      val rule = rules(rulePos)
      val external0 =
        if (rule._2 == 10) Chart.EXTERNAL_NONE
        else if (rule._2 == 11) span._1
        else if (rule._2 == 12) span._2
        else throw new Exception(s"Invalid ternary $rule part 2")
      val external1 =
        if (rule._5 == 10) Chart.EXTERNAL_NONE
        else if (rule._5 == 13) split1
        else throw new Exception(s"Invalid ternary $rule part 5")
      idMiddleNext = infoToID(rule._1, external0, rule._3)
      idRightNext = infoToID(rule._4, external1, rule._6)
      idGoalNext = rule._7
      rulePos += 1
    } else {
      idMiddleNext = -1
      idRightNext = -1
      idGoal = -1
    }
  }

  def prepareToIterate(
    rules: ArrayBuffer[(Int, Int, Int, Int, Int, Int, Int)], span: (Int, Int),
    split1: Int
  ) = {
    this.rules = rules
    this.span = span
    this.split1 = split1
    rulePos = 0
    prepareNext
  }
}

class BinaryRuleIterator {
  var rules : ArrayBuffer[(Int, Int, Int, Int)] = null
  var span : (Int, Int) = (-1, -1)
  var split : Int = 0
  var xmax : Int = 0
  var leftX : Int = 0

  var external = -1
  var externalExtreme = -1
  var rule : (Int, Int, Int, Int) = null
  var rulePos = -1
  var idRight = -1
  var idRightNext = -1
  var idGoal = -1
  def next = {
    idRight = idRightNext
    prepareNext
  }
  def hasNext = idRightNext != -1

  @inline final def infoToID(coreID: Int, external: Int, arc: Int) =
    arc << 25 | coreID | (external << 16)
  @inline final def infoToGoal(coreID: Int, external: Int) : Int =
    coreID | (external << 16)

  def prepareNext = {
    var done = false
    if (external >= 0 && external != externalExtreme) {
      if (external < externalExtreme)
        external += 1
      else
        external -= 1
      idRightNext = infoToID(rule._1, external, rule._3)
      idGoal = infoToGoal(rule._4, external)
      done = true
    } else {
      while (rulePos < rules.length && !done) {
        rule = rules(rulePos)
        if (rule._2 == 5) {
          idRightNext = infoToID(rule._1, Chart.EXTERNAL_NONE, rule._3)
          idGoal = infoToGoal(rule._4, leftX)
          done = true
        } else if (rule._2 == 6) {
          idRightNext = infoToID(rule._1, span._1, rule._3) // i
          idGoal = infoToGoal(rule._4, leftX)
          done = true
        } else if (rule._2 == 7) {
          idRightNext = infoToID(rule._1, leftX, rule._3) // == left ext
          idGoal = infoToGoal(rule._4, leftX)
          done = true
        } else if (rule._2 == 8 && span._1 > 0) { // < i
          external = 0
          externalExtreme = span._1 - 1
          idRightNext = infoToID(rule._1, external, rule._3)
          idGoal = infoToGoal(rule._4, external)
          done = true
        } else if (rule._2 == 9 && span._2 < xmax) { // > j
          external = xmax
          externalExtreme = span._2 + 1
          idRightNext = infoToID(rule._1, external, rule._3)
          idGoal = infoToGoal(rule._4, external)
          done = true
        }
        rulePos += 1
      }
    }

    if (!done) {
      idRightNext = -1
      idGoal = -1
    }
  }

  def prepareToIterate(
    rules: ArrayBuffer[(Int, Int, Int, Int)], span: (Int, Int), split: Int,
    xmax: Int, leftX: Int
  ) = {
    this.rules = rules
    this.span = span
    this.split = split
    this.xmax = xmax
    this.leftX = leftX
    rulePos = 0
    external = -1
    prepareNext
  }
}

@SerialVersionUID(1L)
class Rules(
  val doGraphs: Boolean,
  val doCrossing: Boolean
) extends Serializable {
  val originalRuleTextBinary = new HashMap[Long, String]
  val originalRuleTextArc1 = new HashMap[Long, ArrayBuffer[String]]
  val originalRuleTextArc2 = new HashMap[Long, ArrayBuffer[String]]

  // Arcs - Given a state, return a list of arcs that can be created.
  // Key is a state int, with hasFinal == false and
  val arcRulesFirst = new HashMap[Int, ArrayBuffer[(String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value])]]
  val arcRulesSecond = new HashMap[Int, ArrayBuffer[(String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value])]]
  val arcRulesFirstAllowed = new HashMap[Int, ArrayBuffer[(String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value])]]
  val arcRulesSecondAllowed = new HashMap[Int, ArrayBuffer[(String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value])]]

  // Combines - Given a set of states, return the representations for possible
  // extensions.
  val combineRules = new HashMap[Long, (Int, List[(Int, (Int, Int, Int), PositionReference)], Boolean, Boolean, String, Boolean)]
  val combineTopDown = new HashMap[Long, (Int, List[(Int, (Int, Int, Int), PositionReference)], Boolean, Boolean, String, Boolean)]

  // Mapping from the combineRules ID to the arguments required to run
  // addToAllowedSubbeams
  val toConsiderAllowing = new HashMap[Long, ((Int, (Int, Int, Int), PositionReference), (Int, (Int, Int, Int), PositionReference), (Int, (Int, Int, Int), PositionReference), Int, Boolean, Boolean)]
  val subbeamToExtensionsBinary = new HashMap[Int, ArrayBuffer[(Int, Int, Int, Int)]]
  val subbeamToExtensionsTernary = new HashMap[Int, ArrayBuffer[(Int, Int, Int, Int, Int, Int, Int)]]

  val ruleIDs = new LongIntMap(0.5, 1024, 0)
  val arcRuleIDs = new IntIntMap(0.5, 1024, 0)
  def getAndCountRuleID(id: Long) = {
    val ans = combineRules.getOrElse(id, null)
    if (ans != null && subbeamToExtensionsBinary.size == 0) {
      ruleIDs.synchronized{
        val count = ruleIDs.getAndNote(id)
        ruleIDs.putNoted(count + 1)
      }
    }
    ans
  }
  def getAndCountArcID(id: Int, secondStage: Boolean) = {
    val ans =
      if (secondStage) arcRulesSecond(id)
      else arcRulesFirst(id)
    if (arcRulesFirstAllowed.size == 0) {
      arcRuleIDs.synchronized{
        val count = arcRuleIDs.getAndNote(id)
        arcRuleIDs.putNoted(count + 1)
      }
    }
    ans
  }
  def setAllowedRules(cutoff: Int) = {
    if (subbeamToExtensionsBinary.size == 0) {
      for ((id, params) <- toConsiderAllowing) {
        if (
          cutoff <= ruleIDs.getOrElse(id, 0) &&
          (!params._5 || doGraphs) &&
          (!params._6 || doCrossing)
        ) addToAllowedSubbeams(params._1, params._2, params._3, params._4)
      }
      for ((id, params) <- arcRulesFirst) {
        if (cutoff <= arcRuleIDs.getOrElse(id, 0))
          arcRulesFirstAllowed(id) = params
      }
      if (doGraphs || doCrossing) {
        for ((id, params) <- arcRulesSecond) {
          if (cutoff <= arcRuleIDs.getOrElse(id, 0))
            arcRulesSecondAllowed(id) = params
        }
      }
    }
  }

  def getAllowedExtensionsBinary(
    subbeamIDFull: Long, span: (Int, Int), split: Int, xmax: Int,
    iterator: BinaryRuleIterator
  ) = {
    val subbeamID = subbeamIDFull & 0xffffffffL
    val leftBase =
      ((subbeamID >> 1) & 0x7fff) +
      ((subbeamID >> 9) & 0x8000)
    val leftX = Chart.externalPosFromState(subbeamID.toInt)
    val leftXmod =
      if (leftX == Chart.EXTERNAL_NONE) 0
      else if (leftX < span._1) 1
      else if (leftX > span._2) 2
      else if (leftX == span._2) 3
      else if (split < leftX && leftX < span._2) 4
      else -1
    if (leftXmod < 0) null
    else {
      val leftKey = (leftBase << 3).toInt + leftXmod
      val options = subbeamToExtensionsBinary.getOrElse(leftKey, null)
      if (options != null)
        iterator.prepareToIterate(options, span, split, xmax, leftX)
    }
  }
  def getAllowedExtensionsTernary(
    subbeamIDFull: Long, span: (Int, Int), split1: Int,
    iterator: TernaryRuleIterator
  ) = {
    val subbeamID = subbeamIDFull & 0xffffffffL
    val leftBase =
      ((subbeamID >> 1) & 0x7fff) +
      ((subbeamID >> 9) & 0x8000)
    val leftX = Chart.externalPosFromState(subbeamID.toInt)
    val leftXmod =
      if (leftX == Chart.EXTERNAL_NONE) 0
      else if (leftX < span._1) 1
      else if (leftX > span._2) 2
      else if (leftX == span._2) 3
      else if (split1 < leftX && leftX < span._2) 4
      else -1
    if (leftXmod < 0) null
    else {
      val leftKey = (leftBase << 3).toInt + leftXmod
      val options = subbeamToExtensionsTernary.getOrElse(leftKey, null)
      if (options != null)
        iterator.prepareToIterate(options, span, split1)
    }
  }
  def addToAllowedSubbeams(
    item0: (Int, (Int, Int, Int), PositionReference),
    item1: (Int, (Int, Int, Int), PositionReference),
    item2: (Int, (Int, Int, Int), PositionReference),
    goal: Int
  ) = {
    val leftBase =
      ((item0._1 >> 1) & 0x7fff) +
      ((item0._1 >> 9) & 0x8000)
    val leftX =
      if (item0._3.position == 'j' && item0._3.item == '1') {
        if (item1 == null) 3
        else 4
      } else if (item0._3.position == 'x' && item0._3.item == 'P') {
        if (Chart.externalPosFromState(item0._1) == Chart.EXTERNAL_LEFT) 1
        else 2
      } else 0

    val leftKey = (leftBase << 3) + leftX

    if (item1 == null) {
      val binaryOptions = subbeamToExtensionsBinary.getOrElseUpdate(leftKey,
        new ArrayBuffer[(Int, Int, Int, Int)])
      val rightBase = item2._1 & 0x100fffe
      val rightX =
        if (item2._3.position == 'i' && item2._3.item == '0') 6
        else if (item2._3.position == 'x' && item2._3.item == 'P') {
          if (leftX == 1 || leftX == 2) 7
          else if (Chart.externalPosFromState(item2._1) == Chart.EXTERNAL_LEFT) 8
          else 9
        } else 5
      val nDirect = Chart.numDirectEdgesFromState(item2._1)
      val goalBase = goal & 0x100fffe
      val option = (rightBase, rightX, nDirect, goalBase)
      if (binaryOptions.contains(option))
        throw new Exception(s"Option already present: $option $item0 $item2")
      binaryOptions.append(option)
    } else {
      val ternaryOptions = subbeamToExtensionsTernary.getOrElseUpdate(leftKey,
        new ArrayBuffer[(Int, Int, Int, Int, Int, Int, Int)])
      val middleBase = item1._1 & 0x100fffe
      val middleX =
        if (item1._3.position == 'i' && item1._3.item == '0') 11
        else if (item1._3.position == 'j' && item1._3.item == '2') 12
        else 10
      val rightBase = item2._1 & 0x100fffe
      val rightX =
        if (item2._3.position == 'j' && item2._3.item == '0') 13
        else 10
      val nDirectM = Chart.numDirectEdgesFromState(item1._1)
      val nDirectR = Chart.numDirectEdgesFromState(item2._1)
      val goalBase = goal & 0x100fffe
      val option = (middleBase, middleX, nDirectM, rightBase, rightX,
        nDirectR, goalBase)
      if (ternaryOptions.contains(option))
        throw new Exception(s"Option already present: $option $item0 $item1 $item2")
      ternaryOptions.append(option)
    }
  }

  def positionForArcSymbol(
    symbol: String, left: Int, right: Int, external: Int
  ) = {
    if (symbol == "i") left
    else if (symbol == "j") right
    else external
  }
  def positionForCombinationSymbol(symbol: String, external: Int) = {
    if (symbol == "__") -1
    else if (symbol == "i0") 11 // This is important for the i+1 case
    else if (symbol == "j0") 12
    else if (symbol == "i1") 12
    else if (symbol == "j1") 14
    else if (symbol == "i2") 14
    else if (symbol == "j2") 16
    else if (symbol == "xP" && external == Chart.EXTERNAL_LEFT) 1
    else if (symbol == "xP" && external == Chart.EXTERNAL_RIGHT) 21
    else if (symbol == "i+") -1
    else 0
  }

  // Fill in the rule sets
  {
    def getExternalLocation(str: String) =
      if (str(1) == '.') Chart.EXTERNAL_LEFT
      else if (str(2) == '.') Chart.EXTERNAL_RIGHT
      else Chart.EXTERNAL_NONE

    val input = getClass.getResourceAsStream("/rules.txt")
    val rule_text = Source.fromInputStream(input).getLines
    for (rule <- rule_text) {
      val parts = rule.split(" [|] ")
      val onlyGraphs = parts(5) == "graph"
      val onlyCrossing = parts(6) == "1ec"
      if (parts(0) == "arc") {
        // For example:
        // arc | = -_.L jxFXFF | jxFxFF | p: x | c: j | graph | 1ec
        val parentState = parts(2).map(ArgState.charToValue(_))
        val goalParent = parts(1).split(" ")(2).map(ArgState.charToValue(_))
        val state = Chart.stateIntToArcInt(Chart.genStateMiscInt(
          CrossingState.charToValue(parts(1)(5)),
          getExternalLocation(parts(1).split(" ")(1)), !parts(1).contains("-"),
          false, false, false, false, false, parentState(0), parentState(1),
          parentState(2), parentState(3), parentState(4), parentState(5)), 10)
        val parent = parts(3).split(" ")(1)
        val child = parts(4).split(" ")(1)
        val secondStage = parentState.exists(_ == ArgState.Direct)
        val pencilworthy = parts(1)(0) != '-'
        val nlist = new ArrayBuffer[(String, String, Boolean, Boolean, Boolean, IndexedSeq[ArgState.Value])]
        val cur =
          if (secondStage) arcRulesSecond.getOrElseUpdate(state, nlist)
          else arcRulesFirst.getOrElseUpdate(state, nlist)
        cur.append((parent, child, pencilworthy, onlyGraphs, onlyCrossing,
          goalParent))

        if (secondStage) originalRuleTextArc2.getOrElseUpdate(state, new ArrayBuffer[String]).append(rule)
        else originalRuleTextArc1.getOrElseUpdate(state, new ArrayBuffer[String]).append(rule)

///        println(s"$state for $rule")
      } else if (parts(0) == "combination") {
        // binary or ternary rule
        // combination | ___I F_F___ | ___I ___I | F_F___ j_F___ | i+ __ | all | all
        // combination | =_.E FFFFiF | -_.N -_.R | FFIFiF FFFFIF | xP xP | graph | 1ec
        // combination | ___I F_F___ | -_.R ___I =._L | FFFFIF F_F___ JFFFFJ | j1 __ j0 | graph | 1ec
        val goalParent = parts(1).split(" ")(1).map(ArgState.charToValue(_))
        val goalExternal = getExternalLocation(parts(1).split(" ")(0))
        val goalCrossing = CrossingState.charToValue(parts(1)(3))
        val goal = Chart.genStateMiscInt(goalCrossing, goalExternal,
          !parts(1).contains("-"), false, false, false, false, false,
          goalParent(0), goalParent(1), goalParent(2), goalParent(3),
          goalParent(4), goalParent(5)
        )
        val spans = parts(2).split(" ")
        val parents = parts(3).split(" ")
        val positions = parts(4).split(" ")
        var num = 0
        val items = List(spans, parents, positions).transpose.map{ item =>
          val span = item(0)
          val parent = item(1)
          val pos = item(2)
          val parentState = parent.map(ArgState.charToValue(_))
          val external = getExternalLocation(span)
          val state = Chart.genStateMiscInt(
            CrossingState.charToValue(span(3)), external, !span.contains("-"),
            false, false, false, false, false, parentState(0), parentState(1),
            parentState(2), parentState(3), parentState(4), parentState(5))
          val xPos = new PositionReference(pos(0), pos(1))
          val i = positionForCombinationSymbol(s"i$num", external)
          val j = positionForCombinationSymbol(s"j$num", external)
          val x = positionForCombinationSymbol(pos, external)
          num += 1
          (state, (i, j, x), xPos)
        }

        // Add to combine rules
        val id =
          if (items.length == 2)
            Chart.stateIntsToBinaryInt(items(0)._1, items(0)._2, items(1)._1,
              items(1)._2)
          else
            Chart.stateIntsToBinaryInt(items(0)._1, items(0)._2, items(1)._1,
              items(1)._2, items(2)._1, items(2)._2)
        if (combineRules.contains(id)) {
          println(combineRules(id))
          println(s"$rule has a duplicate signature")
          throw new IllegalStateException(s"$rule has a duplicate signature")
        }
        val nonIntervalItems = items.filter{ v =>
          Chart.crossingFromState(v._1) != CrossingState.Interval
        }
        val createsCrossing =
          goalCrossing != CrossingState.Both &&
          nonIntervalItems.length > 1
///        Log.logln(s"nr$id for $rule")
        combineRules(id) = (goal, items, onlyGraphs, onlyCrossing, rule,
          createsCrossing)

        // Add to top down combine rules
        val goalSpan = (
          positionForCombinationSymbol("i0", -1),
          positionForCombinationSymbol("j" + (items.length - 1).toString, -1),
          positionForCombinationSymbol("xP", goalExternal)
        )
        val idTopDown =
          if (items.length == 2)
            Chart.stateIntsToBinaryIntTopDownFromFull(goal, goalSpan,
              items(0)._1, items(0)._2, items(1)._1, items(1)._2)
          else
            Chart.stateIntsToBinaryIntTopDownFromFull(goal, goalSpan,
              items(0)._1, items(0)._2, items(1)._1, items(1)._2, items(2)._1,
              items(2)._2)
        combineTopDown(idTopDown) = (goal, items, onlyGraphs, onlyCrossing,
          rule, createsCrossing)

        // Add to subbeam info
        if (items.length == 2)
          toConsiderAllowing(id) =
            (items(0), null, items(1), goal, onlyGraphs, onlyCrossing)
        else
          toConsiderAllowing(id) =
            (items(0), items(1), items(2), goal, onlyGraphs, onlyCrossing)

        originalRuleTextBinary(id) = rule
      } else throw new IllegalStateException(s"$rule invalid in rules.txt")
    }
    input.close()

    Log.logln("Rules added")
  }

  override def toString() = {
    val lines = new ArrayBuffer[String]
    for ((id, params) <- toConsiderAllowing) {
      val text = originalRuleTextBinary(id)
      val count = ruleIDs.getOrElse(id, 0)
      lines.append(s"$count $text")
    }
    for ((id, params) <- arcRulesFirst) {
      val count = arcRuleIDs.getOrElse(id, 0)
      val text = originalRuleTextArc1(id).mkString(s"\n$count ")
      lines.append(s"$count $text")
    }
    for ((id, params) <- arcRulesSecond) {
      val count = arcRuleIDs.getOrElse(id, 0)
      val text = originalRuleTextArc2(id).mkString(s"\n$count ")
      lines.append(s"$count $text")
    }

    lines.mkString("\n")
  }
}

object Rules {
  def apply(filename: String) = {
    val obj = ObjectReader.load(filename)
    import scala.reflect.ClassTag
    obj match {
      case rules: Rules =>
        Log.logln("Rules read in")
        rules
      case a =>
        throw new IllegalArgumentException("Invalid rules type in file "+
          s"$filename ${ClassTag(a.getClass)} ${a.getClass}"
        )
    }
  }
}
