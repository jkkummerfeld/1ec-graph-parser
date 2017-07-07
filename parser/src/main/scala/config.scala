// Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a non-commercial use license.  See the
// LICENSE.txt file in the top-level directory of this distribution or at
// https://github.com/jkkummerfeld/graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

object Config {
  // Data
  var minLength : Int = 0
  var maxLength : Int = 999
  var maxTrainSents : Int = 999999
  var maxEvalSents : Int = 999999
  var trainIn : Option[String] = None
  var trainAutoPOS : Option[String] = None
  var evalIn : Option[String] = None
  var evalAutoPOS : Option[String] = None
  var goldPOS : Boolean = false
  var trainGoldPOS : Boolean = false // Will default to goldPOS
  var evalGoldPOS : Boolean = false // Will default to goldPOS
  var parseType : Formalism.Value = Formalism.PSG
  var keepTraces : Boolean = false
  var printInput : Boolean = false
  var simpleSuffixMap : Boolean = false
  var distinguishSuffix : Boolean = true
  var suffixMapCutoff : Int = 100

  // Misc
  var prefix : String = "experiment"
  var logToStdout : Boolean = false
  var loadFactor : Double = 0.5
  var parallel : Boolean = true
  var printModel : Boolean = false
  var printIndexes : Boolean = false
  var printRuleCounts : Boolean = false
  var indexPrefix : Option[String] = None
  var threads: Option[Int] = None

  // Parser
  var stageConfigs : List[String] = List()
  var cachePrefixRead : Option[String] = None
  var cachePrefixWrite : Option[String] = None
  var cacheWriteArc : Boolean = false
  var cacheWriteLocal : Boolean = false

  // Model
  var train : Boolean = false
  var skipFinalOutside : Boolean = false
  var shuffleTrainData : Boolean = true
  var devEvalFreq : Int = Int.MaxValue
  var earlyIterationStop : Int = Int.MaxValue
  var learnerType : LearnerType.Value = LearnerType.Counting
  var lossWeightSpine : Double = 1.0
  var lossWeightDiffS : Double = 2.0
  var lossWeightDiffT : Double = 2.0
  var lossWeightExtraS : Double = 1.0
  var lossWeightExtraT : Double = 1.0
  var lossWeightMissedS : Double = 1.0
  var lossWeightMissedT : Double = 1.0
  var lossType : LossType.Value = LossType.HAMMING
  var trainIterations : Int = 10
  var averagedLearning : Boolean = false
  var useBetweenFeatures : Boolean = true
  var featureFreqCutoff : Int = 0 // Occurrence in train must be at or above this
  var arcFreqCutoff : Int = 0 // Occurrence in train must be at or above this
  var c : Double = 1e-6
  // this c for primal SVM
  // need other c for n-slack case (where it is not multiplied by the regulariser)
  var delta : Double = 1e-6
  var stepSize : Double = 0.001
  var l2reg : Boolean = false
  var batchSize : Int = 1
  var cachedInit : Boolean = false // This makes a difference because some features (e.g. one end of an arc) are cached and not counted after the first time they are accessed
  var mergeRareAndUnseenArcFeats : Boolean = false
  var mergeRareAndUnseenSurfaceFeats : Boolean = false
  var mergeAllRareOrUnseen : Boolean = true
  var negativeFeaturePower : Int = 0
  var lowercaseWordFeatures : Boolean = false
  var includePOSInSpine : Boolean = false
  var includePOSInArg : Boolean = false
  var edgeHasTargetGrandParent : Boolean = false
  var edgeHasSourceGrandParent : Boolean = false
  var edgeHasTargetParent : Boolean = false
  var edgeHasSourceParent : Boolean = false
  var edgeHasTargetChild : Boolean = false
  var edgeHasSourceChild : Boolean = false
  var edgeHasTargetGrandChild : Boolean = false
  var edgeHasSourceGrandChild : Boolean = false
  var ruleStructuralFeatureOnly : Boolean = true
  var ternaryTraces : Boolean = false
  var spineFeatParts : Boolean = false
  var spineFeatComplete : Boolean = true
  var spineFeatAllButTrace : Boolean = false
  var spineFeatAllButNull : Boolean = false
  var useMergedChainScore : Boolean = true
  var useCoarseArcScore : Boolean = false
  var useCoarseTraceScore : Boolean = false
  var useCoarseSpineScore : Boolean = false

  var newLocalArcFeatures : Boolean = false
  var newLocalBinaryFeatures : Boolean = false
  var featureDev : String = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"

  private var set : Boolean = false

  def setArgs(args: Array[String]) = {
    require(! set, "Setting config more than once")
    set = true

    def getBooleanArg(arg: String, default: Boolean) = {
      if (args.contains(arg)) {
        val pos = args.indexOf(arg) + 1
        if (pos < args.length &&
          (args(pos) == "false" || args(pos) == "False")) false
        else true
      }
      else default
    }
    def getListArg(arg: String, default: List[String]) =
      if (args.contains(arg)) args(args.indexOf(arg) + 1).split(",").toList
      else default
    def getIntArg(arg: String, default: Int) =
      if (args.contains(arg)) args(args.indexOf(arg) + 1).toInt
      else default
    def getDoubleArg(arg: String, default: Double) =
      if (args.contains(arg)) args(args.indexOf(arg) + 1).toDouble
      else default
    def getStringArg(arg: String, default: String) =
      if (args.contains(arg)) args(args.indexOf(arg) + 1)
      else default
    def getOptIntArg(arg: String, default: Option[Int]) : Option[Int] =
      if (args.contains(arg)) Some(args(args.indexOf(arg) + 1).toInt)
      else None
    def getOptStringArg(arg: String, default: Option[String]) : Option[String] =
      if (args.contains(arg)) Some(args(args.indexOf(arg) + 1))
      else None

    // Data options
    minLength = getIntArg("-minLength", minLength)
    maxLength = getIntArg("-maxLength", maxLength)
    maxTrainSents = getIntArg("-maxTrainSents", maxTrainSents)
    maxEvalSents = getIntArg("-maxEvalSents", maxEvalSents)
    trainIn = getOptStringArg("-trainIn", trainIn)
    trainAutoPOS = getOptStringArg("-trainAutoPOS", trainAutoPOS)
    evalIn = getOptStringArg("-evalIn", evalIn)
    evalAutoPOS = getOptStringArg("-evalAutoPOS", evalAutoPOS)
    goldPOS = getBooleanArg("-goldPOS", goldPOS)
    trainGoldPOS = getBooleanArg("-trainGoldPOS", goldPOS)
    evalGoldPOS = getBooleanArg("-evalGoldPOS", goldPOS)
    keepTraces = getBooleanArg("-keepTraces", keepTraces)
    printInput = getBooleanArg("-printInput", printInput)
    simpleSuffixMap = getBooleanArg("-simpleSuffixMap", simpleSuffixMap)
    distinguishSuffix = getBooleanArg("-distinguishSuffix", distinguishSuffix)
    suffixMapCutoff = getIntArg("-suffixMapCutoff", suffixMapCutoff)

    // Misc options
    prefix = getStringArg("-prefix", prefix)
    logToStdout = getBooleanArg("-logToStdout", logToStdout)
    loadFactor = getDoubleArg("-loadFactor", loadFactor)
    parallel = getBooleanArg("-parallel", parallel)
    threads = getOptIntArg("-threads", threads)

    // Parser options
    stageConfigs = getListArg("-stageConfigs", stageConfigs)
///    cachePrefixRead = getOptStringArg("-cachePrefixRead", cachePrefixRead)
///    cachePrefixWrite = getOptStringArg("-cachePrefixWrite", cachePrefixWrite)
///    cacheWriteArc = getBooleanArg("-cacheWriteArc", cacheWriteArc)
///    cacheWriteLocal = getBooleanArg("-cacheWriteLocal", cacheWriteLocal)
    train = getBooleanArg("-train", train)
    skipFinalOutside = getBooleanArg("-skipFinalOutside", skipFinalOutside)
    shuffleTrainData = getBooleanArg("-shuffleTrainData", shuffleTrainData)
    devEvalFreq = getIntArg("-devEvalFreq", devEvalFreq)
    earlyIterationStop = getIntArg("-earlyIterationStop", earlyIterationStop)

    // Model options
    lossWeightSpine = getDoubleArg("-lossWeightSpine", lossWeightSpine)
    lossWeightDiffS = getDoubleArg("-lossWeightDiffS", lossWeightDiffS)
    lossWeightDiffT = getDoubleArg("-lossWeightDiffT", lossWeightDiffT)
    lossWeightExtraS = getDoubleArg("-lossWeightExtraS", lossWeightExtraS)
    lossWeightExtraT = getDoubleArg("-lossWeightExtraT", lossWeightExtraT)
    lossWeightMissedS = getDoubleArg("-lossWeightMissedS", lossWeightMissedS)
    lossWeightMissedT = getDoubleArg("-lossWeightMissedT", lossWeightMissedT)
    printModel = getBooleanArg("-printModel", printModel)
    printIndexes = getBooleanArg("-printIndexes", printIndexes)
    printRuleCounts = getBooleanArg("-printRuleCounts", printRuleCounts)
    indexPrefix = getOptStringArg("-indexPrefix", indexPrefix)
    trainIterations = getIntArg("-trainIterations", trainIterations)
    averagedLearning = getBooleanArg("-averagedLearning", averagedLearning)
    useBetweenFeatures = getBooleanArg("-useBetweenFeatures", useBetweenFeatures)
    featureFreqCutoff = getIntArg("-featureFreqCutoff", featureFreqCutoff)
    arcFreqCutoff = getIntArg("-arcFreqCutoff", arcFreqCutoff)
    c = getDoubleArg("-c", c)
    delta = getDoubleArg("-delta", delta)
    stepSize = getDoubleArg("-stepSize", stepSize)
    l2reg = getBooleanArg("-l2reg", l2reg)
    batchSize = getIntArg("-batchSize", batchSize)
    cachedInit = getBooleanArg("-cachedInit", cachedInit)
    mergeRareAndUnseenArcFeats = getBooleanArg(
      "-mergeRareAndUnseenArcFeats", mergeRareAndUnseenArcFeats)
    mergeRareAndUnseenSurfaceFeats = getBooleanArg(
      "-mergeRareAndUnseenSurfaceFeats", mergeRareAndUnseenSurfaceFeats)
    mergeAllRareOrUnseen = getBooleanArg(
      "-mergeAllRareOrUnseen", mergeAllRareOrUnseen)
    negativeFeaturePower = getIntArg("-negativeFeaturePower", negativeFeaturePower)
    includePOSInSpine = getBooleanArg("-includePOSInSpine", includePOSInSpine)
    includePOSInArg = getBooleanArg("-includePOSInArg", includePOSInArg)
    edgeHasTargetGrandParent = getBooleanArg("-edgeHasTargetGrandParent", edgeHasTargetGrandParent)
    edgeHasSourceGrandParent = getBooleanArg("-edgeHasSourceGrandParent", edgeHasSourceGrandParent)
    edgeHasTargetParent = getBooleanArg("-edgeHasTargetParent", edgeHasTargetParent)
    edgeHasSourceParent = getBooleanArg("-edgeHasSourceParent", edgeHasSourceParent)
    edgeHasTargetChild = getBooleanArg("-edgeHasTargetChild", edgeHasTargetChild)
    edgeHasSourceChild = getBooleanArg("-edgeHasSourceChild", edgeHasSourceChild)
    edgeHasTargetGrandChild = getBooleanArg("-edgeHasTargetGrandChild", edgeHasTargetGrandChild)
    edgeHasSourceGrandChild = getBooleanArg("-edgeHasSourceGrandChild", edgeHasSourceGrandChild)
    ruleStructuralFeatureOnly = getBooleanArg("-ruleStructuralFeatureOnly", ruleStructuralFeatureOnly)
    lowercaseWordFeatures = getBooleanArg("-lowercaseWordFeatures", lowercaseWordFeatures)
    ternaryTraces = getBooleanArg("-ternaryTraces", ternaryTraces)

    newLocalArcFeatures = getBooleanArg("-newLocalArcFeatures", newLocalArcFeatures)
    newLocalBinaryFeatures = getBooleanArg("-newLocalBinaryFeatures", newLocalBinaryFeatures)
    spineFeatParts = getBooleanArg("-spineFeatParts", spineFeatParts)
    spineFeatComplete = getBooleanArg("-spineFeatComplete", spineFeatComplete)
    spineFeatAllButTrace = getBooleanArg("-spineFeatAllButTrace", spineFeatAllButTrace)
    spineFeatAllButNull = getBooleanArg("-spineFeatAllButNull", spineFeatAllButNull)
    useMergedChainScore = getBooleanArg("-useMergedChainScore", useMergedChainScore)
    useCoarseArcScore = getBooleanArg("-useCoarseArcScore", useCoarseArcScore)
    useCoarseTraceScore = getBooleanArg("-useCoarseTraceScore", useCoarseTraceScore)
    useCoarseSpineScore = getBooleanArg("-useCoarseSpineScore", useCoarseSpineScore)

    featureDev = getStringArg("-featureDev", featureDev)

    lossType =
      getStringArg("-lossType", "default") match {
        case "zero" => LossType.ZERO
        case "hamming" => LossType.HAMMING
        case "default" => lossType
        case _ => throw new IllegalArgumentException("Loss type")
      }
    parseType =
      getStringArg("-parseType", "default") match {
        case "amr" => Formalism.AMR
        case "dep" => Formalism.DEP
        case "psg" => Formalism.PSG
        case "default" => parseType
        case _ => throw new IllegalArgumentException("Parse type")
      }
    learnerType =
      getStringArg("-learnerType", "default") match {
        case "counting" => LearnerType.Counting
        case "perceptron" => LearnerType.Perceptron
        case "primalSVM" => LearnerType.PrimalSVM
        case "default" => learnerType
        case _ => throw new IllegalArgumentException("Learner type")
      }
  }

  override def toString() = {
    ""
  }
}
