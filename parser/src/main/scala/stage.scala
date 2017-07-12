// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.io.Source
import scala.collection.mutable.HashMap

object ParserType extends Enumeration {
  type ParserType = Value
  val ARC, SSCORER, TSCORER, LOCAL, FULL = Value

  def id(value: ParserType.Value) = {
    if (value == ARC) 0
    else if (value == SSCORER) 1
    else if (value == TSCORER) 2
    else if (value == LOCAL) 3
    else if (value == FULL) 4
    else -1
  }
}

object SubbeamType extends Enumeration {
  type SubBeamTeam = Value
  val SPINES, STATE = Value
}

// TODO: Long term I want to have a tree of stages.

class Stage(
  val name: String,
  val parent: Option[Stage],
  val doCrossing: Boolean,
  val doGraphs: Boolean,
  val eval: Boolean,
  val useSumMarginals: Boolean,
  val parserType: ParserType.Value,
  val subbeamType: SubbeamType.Value,
  val rules: Rules,
  val ruleCutoff: Int,
  val pruningRatioTrain: Double,
  val pruningRatioEval: Double,
  val pruningRatioEvalInTrain: Double,
  val pruningRankTrain: Double,
  val pruningRankEval: Double,
  val pruningRankEvalInTrain: Double,
  val pruneWithExpScore: Boolean,
  val requireTreeBasis: Boolean,
  // Beam variables
  val cubeDepth: Int,
  val beamMinLength: Int,
  val beamMaxLength: Int,
  val beamMinMultiple: Double,
  val beamMaxMultiple: Double,
  val beamMinFraction: Double,
  val beamMaxFraction: Double,
  val beamType: (Int, Int, Double, Double, Double, Double) => Beam,
  val beamBuckets: Int,
  val beamRange: Double,
  // Model variables
  val model : Model,
  val arcPassConstraint: ArcPassConstraint.Value,
  val verbosityTrain : Int,
  val verbosityEvalInTrain : Int,
  val verbosityEval : Int,
  val doNotPruneGoldSpinesInTrain : Boolean,
  val doNotPruneGoldSpinesInEval : Boolean,
  val doNotPruneGoldArcsInTrain : Boolean,
  val doNotPruneGoldArcsInEval : Boolean,
  // Trace parser
  val traceStage : Option[Stage],
  // Spine parser
  val spineStage : Option[Stage]
) {
  // Set the rules for the parent stage (will do nothing if they are already
  // set)
  parent.foreach{ pstage => pstage.rules.setAllowedRules(0) }
  // If we are accepting all rules, set it here
  if (ruleCutoff <= 0 && rules != null) rules.setAllowedRules(0)

///  val ruleCounters = new HashMap[Int, LongLongMap]
///  for (i <- (0 until 250)) ruleCounters(i) = new LongLongMap(0.5, 2 << 8)

  def generateParser(
    training: Boolean, evalInTrain: Boolean, eval: Boolean,
    top: Boolean = true
  ) : Parser = {
    val parentParser = parent.map(
      _.generateParser(training, evalInTrain, eval, false)
    )
    val traceParser = traceStage.map(
      _.generateParser(training, evalInTrain, eval, false)
    )
    val spineParser = spineStage.map(
      _.generateParser(training, evalInTrain, eval, false)
    )
    val pruningRatio =
      if (training) pruningRatioTrain
      else if (evalInTrain) pruningRatioEvalInTrain
      else pruningRatioEval
    val pruningRank =
      if (training) pruningRankTrain
      else if (evalInTrain) pruningRankEvalInTrain
      else pruningRankEval
    val verbosity =
      if (training) verbosityTrain
      else if (evalInTrain) verbosityEvalInTrain
      else verbosityEval
    val (keepGoldArcs, keepGoldSpines) =
      if (training) (doNotPruneGoldArcsInTrain, doNotPruneGoldSpinesInTrain)
      else (doNotPruneGoldArcsInEval, doNotPruneGoldSpinesInEval)

    if (parserType == ParserType.ARC)
      new ArcParser(this, parentParser, traceParser, spineParser,
        pruningRatio, pruningRank, verbosity, keepGoldArcs, keepGoldSpines,
        training && top)
    else if (parserType == ParserType.TSCORER)
      new TraceScorer(this, pruningRatio, pruningRank, verbosity,
        keepGoldArcs, keepGoldSpines, training && top)
    else if (parserType == ParserType.SSCORER)
      new SpineScorer(this, pruningRatio, pruningRank, verbosity,
        keepGoldArcs, keepGoldSpines, training && top)
    else if (parserType == ParserType.LOCAL)
      new LocalParser(this, parentParser, traceParser, spineParser,
        pruningRatio, pruningRank, verbosity, keepGoldArcs, keepGoldSpines,
        training && top)
    else
      new FullStructureParser(this, parentParser, traceParser, spineParser,
        pruningRatio, pruningRank, verbosity, keepGoldArcs, keepGoldSpines,
        training && top)
  }
}

object Stage {
  def getBoolean(arg: Option[String], default: Boolean = false) = {
    arg.fold{ default }{
      case "default" => default
      case "true" => true
      case "True" => true
      case "t" => true
      case "T" => true
      case "false" => false
      case "False" => false
      case "f" => false
      case "F" => false
      case a => throw new IllegalArgumentException(s"Unknown boolean '$a'")
    }
  }
  def getEitherBoolean(
    arg0: Option[String], arg1: Option[String], default: Boolean = false
  ) = {
    if (arg0.isEmpty && arg1.isEmpty) {
      default
    } else if (arg0.isEmpty && arg1.isDefined) {
      getBoolean(arg1, default)
    } else if (arg0.isDefined && arg1.isEmpty) {
      getBoolean(arg0, default)
    } else {
      val arg0b = getBoolean(arg0, default)
      val arg1b = getBoolean(arg1, default)
      if (arg0b != arg1b) Log.logln("Conflicting getEitherBoolean options")
      arg1b
    }
  }
  def getInt(arg: Option[String], default: Int) =
    arg.fold{ default }{
      case "default" => default
      case a => a.toInt
    }
  def getDouble(arg: Option[String], default: Double) =
    arg.fold{ default }{
      case "default" => default
      case a => a.toDouble
    }
  def getString(arg: Option[String], default: String) =
    arg.fold{ default }{
      case "default" => default
      case a => a
    }
  def getBeamType(
    arg: Option[String],
    default: (Int, Int, Double, Double, Double, Double) => Beam
  ) = arg.fold{ default }{
      case "default" => default
      case "array" => ArrayBeam.apply _
      case "sorted" => HeapBeam.apply _
      case "old" => OldBeam.apply _
      case "radix" => RadixBeam.apply _
      case "compacting" => CompactingBeam.apply _
      case _ => throw new IllegalArgumentException("Beam type")
    }
  def getArcPassConstraint(
    arg: Option[String], default: ArcPassConstraint.Value
  ) = arg.fold{ default }{
      case "default" => default
      case "both" => ArcPassConstraint.BOTH
      case "either" => ArcPassConstraint.EITHER
      case "neither" => ArcPassConstraint.NEITHER
      case "child" => ArcPassConstraint.CHILD
      case "parent" => ArcPassConstraint.PARENT
      case _ => throw new IllegalArgumentException("Arc pass constraint")
    }

  def apply(
    filename: String, parent: Option[Stage], wordIndex: IntIndexer,
    argIndex: ArgumentIndexer, ntIndex: NonTerminalIndexer, number: Int
  ) : Stage = {
    val fields = new HashMap[String, String]
    for (line <- Source.fromFile(filename).getLines) {
      if (line.trim.length > 0) {
        val parts = line.trim.split(" ")
        val name = parts(0)
        val contents = parts.tail.mkString(" ")
        fields(name) = contents
      }
    }

    val name = getString(fields.get("name"), s"stage$number")
    val parserType =
      fields.getOrElse("parserType", "default") match {
        case "arc" | "default" => ParserType.ARC
        case "trace" => ParserType.TSCORER
        case "spine" => ParserType.SSCORER
        case "local" => ParserType.LOCAL
        case "full" => ParserType.FULL
        case _ => throw new IllegalArgumentException("Parser type")
      }
    val subbeamType =
      fields.getOrElse("subbeamType", "default") match {
        case "state" | "default" => SubbeamType.STATE
        case "spines" => SubbeamType.SPINES
        case _ => throw new IllegalArgumentException("Subbeam ID type")
      }
    val doCrossing = getBoolean(fields.get("doCrossing"), false)
    val doGraphs =  getBoolean(fields.get("doGraphs"), false)
    val rules =
      fields.get("rules").fold{
        new Rules(doCrossing, doGraphs)
      }{ filename =>
        Rules(filename)
      }
    val filterByTag =
      parserType == ParserType.ARC ||
      parserType == ParserType.TSCORER
    val model =
      fields.get("modelFilename").fold{
        fields.get("modelType").fold{
          throw new IllegalArgumentException("No modelFilename or modelType")
        }{
          case "discriminative" | "default" =>
            new DiscriminativeModel(Config.parseType, wordIndex, argIndex,
              ntIndex, filterByTag)
          case "generative" =>
            new GenerativeModel(Config.parseType, wordIndex, argIndex,
              ntIndex, filterByTag)
          case content =>
            if (content.startsWith("external")) {
              new ExternalModel(Config.parseType, wordIndex, argIndex,
                ntIndex, filterByTag, content)
            } else throw new IllegalArgumentException("Model type")
        }
      }{ filename => Model(filename) }

    val traceRatio = getDouble(fields.get("traceRatio"), -1.0)
    val traceRank = getDouble(fields.get("traceRank"), 10000000)
    val traceRatioEval = getDouble(fields.get("traceRatioEval"), traceRatio)
    val traceRankEval = getDouble(fields.get("traceRankEval"), traceRank)
    val traceStage =
      fields.get("traceModel").map{ filename =>
        new Stage("trace" + name, None, true, true, false, false,
          ParserType.TSCORER, SubbeamType.STATE, null, 0, traceRatio,
          traceRatioEval, traceRatioEval, traceRank, traceRankEval,
          traceRankEval, false, false, 0, 0, 0, 0.0, 0.0, 0.0, 0.0,
          ArrayBeam.apply _, 0, 0.0, Model(filename),
          ArcPassConstraint.EITHER, 0, 0, 0, false, false, false, false, None,
          None)
      }

    val spineRatio = getDouble(fields.get("spineRatio"), -1.0)
    val spineRatioEval = getDouble(fields.get("spineRatioEval"), spineRatio)
    val spineStage =
      fields.get("spineModel").map{ filename =>
        val (expPrune, model) =
          if (filename.startsWith("external")) {
            (false, new ExternalModel(Config.parseType, wordIndex, argIndex,
              ntIndex, filterByTag, filename))
          } else (true, Model(filename))
        new Stage("spine" + name, None, true, true, false, false,
          ParserType.SSCORER, SubbeamType.STATE, null, 0, spineRatio,
          spineRatioEval, spineRatioEval, 0.0, 0.0, 0.0, expPrune, false, 0,
          0, 0, 0.0, 0.0, 0.0, 0.0, ArrayBeam.apply _, 0, 0.0, model,
          ArcPassConstraint.EITHER, 0, 0, 0, false, false, false, false, None,
          None)
      }

    new Stage(
      name,
      parent,
      doCrossing,
      doGraphs,
      getBoolean(fields.get("eval"), false),
      getBoolean(fields.get("useSumMarginals"), false),
      parserType,
      subbeamType,
      rules,
      getInt(fields.get("ruleCutoff"), 0),
      getDouble(fields.get("pruningRatioTrain"), -1.0),
      getDouble(fields.get("pruningRatioEval"), -1.0),
      getDouble(fields.get("pruningRatioEvalInTrain"), -1.0),
      getDouble(fields.get("pruningRankTrain"), 10000000),
      getDouble(fields.get("pruningRankEval"), 10000000),
      getDouble(fields.get("pruningRankEvalInTrain"), 10000000),
      getBoolean(fields.get("pruneWithExpScore"), false),
      getBoolean(fields.get("requireTreeBasis"), true),
      getInt(fields.get("cubeDepth"), 1 << 28), // Cannot be Int.MaxValue because of how it is used in beam
      getInt(fields.get("beamMinLength"), 0),
      getInt(fields.get("beamMaxLength"), Int.MaxValue),
      getDouble(fields.get("beamMinMultiple"), 0.5),
      getDouble(fields.get("beamMaxMultiple"), Double.MaxValue),
      getDouble(fields.get("beamMinFraction"), 0.0),
      getDouble(fields.get("beamMaxFraction"), 1.0),
      getBeamType(fields.get("beamType"), Beam.apply _),
      getInt(fields.get("beamBuckets"), 20),
      getDouble(fields.get("beamRange"), 20.0),
      model,
      getArcPassConstraint(fields.get("arcPassConstraint"),
        ArcPassConstraint.EITHER),
      getInt(fields.get("verbosityTrain"), 0),
      getInt(fields.get("verbosityEvalInTrain"), 0),
      getInt(fields.get("verbosityEval"), 0),
      getEitherBoolean(fields.get("doNotPruneGoldInTrain"),
        fields.get("doNotPruneGoldSpinesInTrain"), true),
      getEitherBoolean(fields.get("doNotPruneGoldInEval"),
        fields.get("doNotPruneGoldSpinesInEval"), false),
      getEitherBoolean(fields.get("doNotPruneGoldInTrain"),
        fields.get("doNotPruneGoldArcsInTrain"), true),
      getEitherBoolean(fields.get("doNotPruneGoldInEval"),
        fields.get("doNotPruneGoldArcsInEval"), false),
      traceStage,
      spineStage
    )
  }
}

