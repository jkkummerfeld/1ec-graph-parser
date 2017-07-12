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
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, HashMap, Stack, HashSet, Set}
import scala.collection.parallel.immutable.ParVector
import scala.math
import scala.collection.immutable.IndexedSeq
import scala.collection.parallel._

///import org.github.jamm.MemoryMeter

class LearningStats {
  var totalA = 0
  var totalG = 0
  var matchingA = 0
  var matchingG = 0
  var parseTime = 0.0
  var updateTime = 0.0
  var wallTime = 0.0
  var matchingSpans = 0
  var autoSpans = 0
  var goldSpans = 0
  var objective = 0.0
  var regularisation = 0.0
  var totalParses = 0
  var goldPrunedSearchError = 0

  var gradientWaiting = 0.0
  var pruningWaiting = 0.0

  override def toString() = {
    val p =
      if (totalA > 0) 100.0 * matchingA / totalA
      else 0.0
    val r =
      if (totalG > 0) 100.0 * matchingG / totalG
      else 0.0
    val pc =
      if (autoSpans > 0) 100.0 * matchingSpans / autoSpans
      else 0.0
    val rc =
      if (goldSpans > 0) 100.0 * matchingSpans / goldSpans
      else 0.0
    val pTimeString = f"${parseTime / 6e10}%.1f"
    val uTimeString = f"${updateTime / 6e10}%.1f"
    val wTimeString = f"${wallTime / 6e10}%.1f"
    val prunedFrac =
      if (totalParses == 0) "0.0"
      else f"${100 * goldPrunedSearchError / totalParses}%.1f"
    f"$p%.1f $r%.1f  $pc%.1f $rc%.1f  $prunedFrac  "+
    f"$objective%.1f + $regularisation%.1f  " +
    f"I $wTimeString%5s $pTimeString%5s $uTimeString%5s  "+
    f"$matchingA / $totalA, "+
    f"$matchingG / $totalG, "+
    f"$matchingSpans / $autoSpans, "+
    f"$matchingSpans / $goldSpans I"+
    s" $gradientWaiting $pruningWaiting"
  }
}

object LearnerType extends Enumeration {
  type LearnerType = Value
  val Counting, Perceptron, PrimalSVM = Value
}

object LossType extends Enumeration {
  type LossType = Value
  val P, R, ZERO, HAMMING = Value
  // My hamming distance is:
  //   +1 for a missed edge
  //   +1 for an extra edge
  //   +1 for edge match, but different label
  // It is implemented in several places in the chart:
  //   - Scoring arcs
  //   - Scoring spines (scored every time)
  //   - Deciding which parts (S, T, C) of an arc to include
  //   - Binary composition, to handle missed arcs
  //   - Special case if missing an arc spanning the entire sentence (TODO)
}

object Learn {
  val randGen = new Random(0)
  val delta = 1e-2
  val etaC = Config.stepSize * Config.c
  // TODO: Why does changing delta impact perceptron performance?

  // ======================================================
  // Generative

  def depGenerative(
    trainData: ArrayBuffer[(Int, DepParse)],
    devData: ArrayBuffer[(Int, DepParse)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: GenerativeModel
  ) = {
///    val parser = stage.generateParser(true, false, false)
    for (((sentID, parse), atags) <- trainData.zip(trainAutoPOS)) {
      val tags = if (atags.length > 0) atags else parse.tags
      // Generate simple unary mappings
///      for (i <- 0 until parse.tags.length) {
///        val token = parse.tokens(i)
///        val tag = tags(i)
///        val id = model.addSpineOption(i, tag, Vector((Model.SNon, 0, false)))
///        model.incrementInit(tag, id)
///      }
///      val rid = model.addSpineOption(Model.SRootWord, Model.SRootTag, model.RootSpine)
///      model.incrementInit(Model.SRootTag, rid)

      // Update binary scores
      for (dep <- parse.deps) {
        val (parent, ptag) =
          if (dep.parent == parse.tokens.length) (Model.SRootWord, Model.SRootTag)
          else (parse.tokens(dep.parent), tags(dep.parent))
        val child = parse.tokens(dep.child)
        val ctag = tags(dep.child)
        val direction = Model.arcToDirection(dep.child, dep.parent)

        model.dependencyIncrement(child, ctag, dep.label, parent, ptag, direction)
      }
    }
    model.normaliseInit
  }

  def psgGenerative(
    trainData: ArrayBuffer[(Int, psg.Graph)],
    devData: ArrayBuffer[(Int, psg.Graph)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: GenerativeModel
  ) = {
    Log.logln("Training PSG generative model")
///    val parser = stage.generateParser(true, false, false)
    for (((sentID, parse), atags) <- trainData.zip(trainAutoPOS)) {
      val tokens = parse.tokens
///      val tags = if (atags.length > 0) atags else parse.tags
      // Generate simple unary mappings
///      for (i <- 0 until parse.tags.length) {
///        val token = parse.tokens(i)
///        val tag = tags(i)
///        val spine = parse.spines(i)
///        val id = model.addSpineOption(token, tag, spine.symbols)
///        model.incrementInit(tag, id)
///      }
///      val rid = model.addSpineOption(Model.SRootWord, Model.SRootTag, model.RootSpine)
///      model.incrementInit(Model.SRootTag, rid)

      // Update binary scores
///      val rootSpine = new Spine(Vector[(String, Int, Boolean)](),
///        Vector[Edge]())
      for (edge <- parse.edges) {
        if (edge.trace == "_") {
          val target = edge.target
          val src = edge.src
          val wd = tokens(src)
          val td = ""
///            if (parse.spines(src).toString == "_") "*"
///            else "*_"+ parse.spines(src).toString
          val (wh, th) = ("", "")
///            if (target == tokens.length) (Model.SRootWord, "*_"+ Model.SRootWord)
///            else (tokens(target), "*_"+ parse.spines(target).toString)
          val l = ""
///            if (target == tokens.length)
///              model.addEdge(edge, rootSpine, tags(src), Model.SRootTag)
///            else
///              model.addEdge(edge, parse.spines(target), tags(src), tags(target))
          val d = if (src > target) 0 else 1

          model.dependencyIncrement(wd, td, l, wh, th, d)
        }
      }
    }
    model.normaliseInit
  }

  def amrGenerative(
    trainData: ArrayBuffer[(Int, AMR)], devData: ArrayBuffer[(Int, AMR)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: GenerativeModel
  ) = {
    // Binary
      // For each edge in the AMR
        // Increment binary scores

    // Unary
      // Get alignment

      // For each word
        // For each concept it could align to
          // Build a chain by going to parents until ...
          //  - Going to a concept that could align somewhere may be too short
          //    (may have to align the word elsewhere)
          //  - Continuing as long as desired is too far
          //  - Continue to a concept that can only align to a single word?
  }

  // ======================================================
  // Discriminative

  private var updateNumber = 0
  private val sumForAv = UnboxedArrayBufferDouble()
  private val sumSqrGrad = UnboxedArrayBufferDouble()
  private val lastUpdates = UnboxedArrayBuffer(2)

  // Note -- Currently using AdaGrad. Could switch to Adam, but the main
  // advantage it gives is robustness in non-stationary cases, which I'm not
  // facing since I have a convex objective.
  // Also, Adam doesn't have a convenient form for the delayed updates.
  // An approximation does work, by setting epsilon = 0 (instead of 10^-8) and
  // then re-arranging. The result will be an equation with (m / sqrt(u)) in
  // front of a sum of lots of terms that don't collapse nicely. However, those
  // terms are all constants, so the complete value could be calculated for
  // each number of time steps, then treated as a constant to multiply in.
  //
  // The equation without epsilon is:
  // ( alpha * m / sqrt(u) ) *
  // sum of i in 0 to n-1 (
  //     beta1 ^ (n - i) * sqrt( 1 - beta2 ^ (t - i) )
  //    ------------------------------------------------
  //    (1 - beta1 ^ (t - i)) * sqrt( beta2 ^ (n - i) )
  //  )

  def perceptronUpdate(model: DiscriminativeModel, k: Int, v: Double) = {
    if (v != 0) {
      val change = -v * delta
      if (Config.averagedLearning)
        sumForAv(k) += change * updateNumber
      model.weights.update(k, change)
    }
  }

  def opsL1Update(model: DiscriminativeModel, k: Int, v: Double) = {
    if (v != 0) {
      sumSqrGrad(k) += v * v
      val w = model.weights.values(k)
      val sq = math.sqrt(sumSqrGrad(k))
      val d = (w - v * Config.stepSize / sq)
      val d2 = d.abs - (etaC / sq)
      val nw =
        if (d2 < 0) 0.0
        else if (d > 0) d2
        else -d2
      model.weights.set(k, nw)
      lastUpdates(k) = updateNumber
    }
  }

  def opsL1updateOnGet(model: DiscriminativeModel, k: Int) = {
    val uTime = lastUpdates(k)
    if (updateNumber != uTime) {
      // - Read the vars
      // - Calculate the new values
      // - Check if the stored value matches the read values
      //    - Yes, go ahead and set them all (possibly duplicating work)
      //    - No and time is updated, another thread did the work, return
      //    - No and time is not ready, oh well, use the old value potentially
      val w = model.weights.values(k)
      val sq = math.sqrt(sumSqrGrad(k))
      val t = updateNumber - uTime
      val tw = w.abs - etaC * t / sq
      val nw =
        if (tw < 0) 0.0
        else if (w > 0) tw
        else -tw
      if (updateNumber != lastUpdates(k)) {
        // Here we update time first, to ensure that we are never doing an
        // update using an inconsistent version of the weight and time.
        lastUpdates(k) = updateNumber
        model.weights.values(k) = nw
      }
    }
  }

  def opsL2Update(model: DiscriminativeModel, k: Int, v: Double) = {
    if (v != 0) {
      sumSqrGrad(k) += v * v
      val w = model.weights.values(k)
      val sq = math.sqrt(sumSqrGrad(k))
      val nw = (w * sq - Config.stepSize * v) / (etaC + sq)
      model.weights.set(k, nw)
      lastUpdates(k) = updateNumber
    }
  }

  def opsL2updateOnGet(model: DiscriminativeModel, k: Int) = {
    val uTime = lastUpdates(k)
    if (updateNumber != uTime) {
      // - Read the vars
      // - Calculate the new values
      // - Check if the stored value matches the read values
      //    - Yes, go ahead and set them all (possibly duplicating work)
      //    - No and time is updated, another thread did the work, return
      //    - No and time is not ready, oh well, use the old value potentially
      val w = model.weights.values(k)
      val sq = math.sqrt(sumSqrGrad(k))
      val t = updateNumber - uTime
      val nw = w * math.pow(sq / (etaC + sq), t)
      if (updateNumber != lastUpdates(k)) {
        // Here we update time first, to ensure that we are never doing an
        // update using an inconsistent version of the weight and time.
        lastUpdates(k) = updateNumber
        model.weights.values(k) = nw
      }
    }
  }

  def generalLearner(
    trainData: ArrayBuffer[_ <: (Int, Parse)],
    devData: ArrayBuffer[_ <: (Int, Parse)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: DiscriminativeModel, gParse: (Parse, Vector[String]) => Parse,
    update: (Int, Double) => Unit
  ) = {
    def parse(
      sentPos: Int, gradient: LongDoubleMap, stats: LearningStats,
      doingDev: Boolean, pruningStats: PruningStats
    ) = {
      val parser = stage.generateParser(! doingDev, doingDev, false)
      val tags =
        if (doingDev) devAutoPOS(sentPos)
        else trainAutoPOS(sentPos)
      val (sentID, parse) =
        if (tags.length == 0) {
          if (doingDev) devData(sentPos)
          else trainData(sentPos)
        } else {
          val (sentID, parse) =
            if (doingDev) devData(sentPos)
            else trainData(sentPos)
          (sentID, gParse(parse, tags))
        }

      val preParse = System.nanoTime()
      val goldEdges = UnboxedArrayBuffer()
      val goldUnaries = new ArrayBuffer[Int]

      val lossType =
        if (doingDev) LossType.ZERO
        else Config.lossType
      val goldStates =
        if (!doingDev || stage.doNotPruneGoldArcsInEval ||
            stage.doNotPruneGoldSpinesInEval) {
          val gparser = stage.generateParser(!doingDev, doingDev, false)
          gparser.prepare(parse.tokens, tags, sentID, null, null, null,
            lossType)
          val (gterminals, garcs) = model.parseToArrays(parse, !doingDev,
            stage)
          val goldItems = gparser.insertDerivation(gterminals, garcs)
          HashSet(gparser.fineChart.itemsToStates(goldItems):_*)
        } else new HashSet[(Int, Int, Int, Int, Int)]
      if (!doingDev || stage.doNotPruneGoldArcsInEval ||
          stage.doNotPruneGoldSpinesInEval)
        model.setGoldParse(parse, goldEdges, goldUnaries, !doingDev, stage)

///      {
///        val sentenceLength = parse.tokens.length + 1
///        Log.logln("Gold edges are:")
///        var i = 0
///        while (i < goldEdges.length) {
///          if (goldEdges(i) >= 0)
///            Log.logln(s"$i ${goldEdges(i)} with $sentenceLength")
///          i += 1
///        }
///      }

      val (aarcs, aterminals, ascore) = parser.parse(parse.tokens, tags,
        sentID, pruningStats != null, goldEdges, goldUnaries, goldStates,
        lossType)
      val outputParse : Parse =
        if (! doingDev) null
        else model.arraysToParse(aarcs, aterminals, parse.sentence,
          parse.tokens, Config.parseType, tags)
      val postParse = System.nanoTime()

      var prePruningStatsOut = 0L
      var prePruningStatsIn = 0L
      if (pruningStats != null) {
        prePruningStatsOut = System.nanoTime()
        pruningStats.synchronized {
          prePruningStatsIn = System.nanoTime()
          parser.updatePruningStats(pruningStats, parse)
        }
      }

      val chart = parser.fineChart
      val autoItems = parser.extractParseItems._1
      val (gterminals, garcs) = model.parseToArrays(parse, !doingDev, stage)
      val goldItems = parser.insertDerivation(gterminals, garcs)
      val lossG = parser.calculateLoss(gterminals, garcs, aterminals, aarcs,
        lossType)
      // TODO: Fix the issue that this is wrong when we fail to form a parse
      val lossA = parser.calculateLoss(aterminals, aarcs, gterminals, garcs,
        lossType)
      val totalG = parser.calculateTotal(gterminals, garcs)
      val totalA = parser.calculateTotal(aterminals, aarcs)
      val matchingG = parser.calculateMatching(aterminals, aarcs, gterminals, garcs)
      val matchingA = parser.calculateMatching(gterminals, garcs, aterminals, aarcs)
      Log.logln(s"P $matchingA / $totalA and R $matchingG / $totalG with losses $lossA $lossG")
///      Log.logln(parse)

      if (Log.check(Log.VParseExtraction + Log.VParseChart, stage.verbosityTrain)) {
        if (lossA != 0 || lossG != 0) {
          Log.logln(s"aarcs: $aarcs")
          Log.logln(s"aterminals: $aterminals")
          Log.logln(s"garcs: $garcs")
          Log.logln(s"gterminals: $gterminals")
          val aparse : Parse =
            model.arraysToParse(aarcs, aterminals, parse.sentence,
              parse.tokens, Config.parseType, tags)
          Log.logln(s"gold: ${goldItems.sorted}")
          Log.logln(s"auto: ${autoItems.sorted}")
          Log.logln(s"Auto derivation:\n"+
            parser.fineChart.toDenseString(autoItems))
          Log.logln(s"Gold derivation:\n"+
            parser.fineChart.toDenseString(goldItems))
          Log.logln(chart)
          Log.logln(s"Comparing Gold to Auto:\n${parse.compareString(aparse)}")
        } else {
          Log.logln(s"Gold and auto items: ${autoItems.sorted}")
          Log.logln(parse)
          Log.logln(parser.fineChart.toDenseString(autoItems))
          Log.logln(chart)
        }
      }

      var goldPruned = false
      var preGradientOut = 0L
      var preGradientIn = 0L
      if (goldItems.length > 0 ||
          stage.parserType == ParserType.TSCORER ||
          stage.parserType == ParserType.SSCORER
      ) {
        if (lossA != 0 || lossG != 0) {
          // Insert the gold and guess parses, counting feature queries
          // in the process. This can be done in parallel with other
          // threads parsing, but only one thread at a time can update
          // the gradient.
          // Note - this is also executed during doingDev, but with no effect
          // except to show if we are pruning the gold.
          var autoScore = 0.0
          var goldScore = 0.0
          preGradientOut = System.nanoTime()
          gradient.synchronized {
            preGradientIn = System.nanoTime()
            chart.useModelCache = false
            chart.featureCounter = (i: Int, v: Double) => {
///              Log.logln(s"Half update $i ${-v}")
              goldScore += model.weights.values(i) * v
              if (!doingDev) gradient.changeOrPut(i.toLong, -v)
            }
            if (Log.check(Log.VParseExtraction, stage.verbosityTrain))
              Log.logln("Inserting gold derivation")
            parser.insertDerivation(gterminals, garcs)
            if (aarcs.length > 0 || stage.parserType == ParserType.SSCORER) {
              chart.featureCounter = (i: Int, v: Double) => {
///                Log.logln(s"Half update $i $v ${model.weights.values(i)}")
                autoScore += model.weights.values(i) * v
                if (!doingDev) gradient.changeOrPut(i.toLong, v)
              }
              if (Log.check(Log.VParseExtraction, stage.verbosityTrain))
                Log.logln("Inserting auto derivation")
              parser.insertDerivation(aterminals, aarcs)
            } else Log.logln("No parse found, update with gold only.")
          }

          goldPruned = ascore < goldScore
///          Log.logln(s"Gold pruned = $goldPruned based on $goldScore v ($autoScore and $ascore)")
          if (ascore < goldScore) {
            Log.logToErr(s"Gold pruned = $goldPruned based on $goldScore v ($autoScore and $ascore) for ${parse.tokens} (during dev? $doingDev)")
            Log.logln(s"Gold pruned = $goldPruned based on $goldScore v ($autoScore and $ascore) for ${parse.tokens} (during dev? $doingDev)")
          }
///          if (goldPruned) throw new Exception

          if (!doingDev && (lossG - (ascore - autoScore)).abs > 1e-2) {
            Log.logln(s"Bad loss augmentation, loss $lossG for scores $ascore and $autoScore from:\naa $aarcs\nga $garcs\nat $aterminals\ngt $gterminals")
            Log.logln(s"$parse")
            for (edge <- garcs) {
              val arg = edge._3
              val vals = model.edges(arg)
              Log.logln(edge.toString +"  "+
                vals._1.map{ case (symbol, functions, count, nullDepth) =>
                  model.nonTerminalIndex.value(symbol) +
                  functions.map("-"+ model.nonTerminalIndex.value(_)).mkString("") +
                  s"_$count.$nullDepth"
                }.mkString(", ") +
                (if (vals._2) " Struct" else " Trace") +
                (if (vals._3) "-Chain" else "") +
                (if (vals._4) " NonTrace" else "")
              )
            }
          }

          if (Log.check(Log.VParseChartWithGold, stage.verbosityTrain)) {
            if (ascore < goldScore) {
              Log.logln(s"auto: ${autoItems.sorted}")
              Log.logln(s"gold: ${goldItems.sorted}")
              Log.logln(chart.toString() +"\n")

///                val parser = stage.generateParser(!doingDev, doingDev, false)
///                Config.verbosity = 56
///                parser.parse(parse.tokens, tags, sentID,
///                  pruningStats != null,
///                  goldEdges, goldUnaries, !doingDev)
///                Config.verbosity = Config.trainVerbosity
            }
          }

          chart.featureCounter = null
          chart.useModelCache = true
        } else Log.logln("No errors, so no change to gradient.")
      } else Log.logln(s"Failed to insert gold ${parse.tokens.length}")
      val postUpdate = System.nanoTime()

      // Get scores
      var autoS = 0
      var goldS = 0
      var matchingS = 0
      val aarcsUnlabeled = new HashSet[(Int, Int)]
      for (arc <- aarcs) {
        val structural = model.isTreeEdge(arc._3)
        if (structural) {
          autoS += 1
          if (garcs.contains(arc)) matchingS += 1
        } else aarcsUnlabeled.add((arc._1, arc._2))
      }
      val garcsUnlabeled = new HashSet[(Int, Int)]
      for (arc <- garcs) {
        val structural = model.isTreeEdge(arc._3)
        if (structural) goldS += 1
        else garcsUnlabeled.add((arc._1, arc._2))
      }
      if (stage.parserType == ParserType.TSCORER) {
        autoS = 0
        goldS = 0
        matchingS = 0
        for (arc <- aarcs) {
          if (! model.isTreeEdge(arc._3) && ! stage.model.edges(arc._3)._4) {
            autoS += 1
            if (garcs.contains(arc)) matchingS += 1
          }
        }
        for (arc <- garcs) {
          if (! model.isTreeEdge(arc._3) && ! stage.model.edges(arc._3)._4) {
            goldS += 1
          }
        }
      } else if (stage.parserType == ParserType.SSCORER) {
        autoS = 0
        goldS = 0
        matchingS = 0
        for ((gterm, aterm) <- gterminals.zip(aterminals)) {
          autoS += 1
          goldS += 1
          if (aterm == gterm) matchingS += 1
        }
      }

      var autoT = aarcsUnlabeled.size
      var goldT = garcsUnlabeled.size
      var matchingT = aarcsUnlabeled.intersect(garcsUnlabeled).size

      stats.synchronized {
///        stats.totalA += totalA
///        stats.totalG += totalG
///        stats.matchingA += matchingA
///        stats.matchingG += matchingG
///        stats.autoSpans += autoSpans
///        stats.goldSpans += goldSpans
///        stats.matchingSpans += matchingSpans
        stats.totalA += autoS
        stats.totalG += goldS
        stats.matchingA += matchingS
        stats.matchingG += matchingS
        stats.autoSpans += autoT
        stats.goldSpans += goldT
        stats.matchingSpans += matchingT

        stats.totalParses += 1
        if (goldPruned) stats.goldPrunedSearchError += 1
        stats.parseTime += (postParse - preParse)
        stats.updateTime += (postUpdate - postParse)
        stats.objective += lossG
        stats.gradientWaiting += preGradientIn - preGradientOut
        stats.pruningWaiting += prePruningStatsIn - prePruningStatsOut
      }
      (sentID, outputParse)
    }

    var devIter = 0
    var totalParsed = 0
    var nextDev = Config.devEvalFreq
    var iter = 0
    val taskSupport = Config.threads.filter(_ > 0).map{ n =>
      // Need to update to newer scala version for this?
///      new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(n))
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
    }
    while (iter < Config.trainIterations) {
      iter += 1
      val trainStats = new LearningStats
      val trainPruneStats =
        if (Log.check(Log.VMarginals, stage.verbosityTrain))
          new PruningStats()
        else null
      // Shuffle the order of instances
      val order =
        if (! Config.shuffleTrainData) (0 until trainData.length)
        else randGen.shuffle[Int, IndexedSeq](0 until trainData.length)

      var index = 0
      while (index < order.length) {
        val gradient = new LongDoubleMap(Config.loadFactor, 1024)

        // Run a batch
        // TODO: Can we guide the parallel execution to do longer sentences
        // first? / as a balancing indicator
        val preTrain = System.nanoTime()
        val slice =
          if (Config.parallel) {
            val ans = order.slice(index, index + Config.batchSize).par
            taskSupport.map(ans.tasksupport = _)
            ans
          } else order.slice(index, index + Config.batchSize)
        slice.map(parse(_, gradient, trainStats, false, trainPruneStats))
        val postTrain = System.nanoTime()

        trainStats.wallTime += postTrain - preTrain

        // Update the model
        updateNumber += 1
        var deltaO = 0.0
        gradient.foreachPair{ (k, v) =>
          if (v.abs > 0.1) {
            val w = model.weights.values(k.toInt)
            trainStats.objective += w * v
            deltaO += w * v
          }
        }
        Log.logln(s"Objective: $deltaO")
        gradient.foreachPair{ (k, v) =>
///          if (v.abs > 1e-6) Log.logln(s"Update $k $v")
          update(k.toInt, v)
        }
        index += Config.batchSize

        // Parse dev data (if it's time)
        totalParsed += Config.batchSize
        if (totalParsed >= nextDev) {
          nextDev += Config.devEvalFreq

          val devStats = new LearningStats
          val pruneStats =
            if (Log.check(Log.VMarginals, stage.verbosityEvalInTrain))
              new PruningStats()
            else null

          val preDev = System.nanoTime()
          val devSlice =
            if (Config.parallel) {
              val ans = ParVector.range(0, devData.length)
              taskSupport.map(ans.tasksupport = _)
              ans
            } else (0 until devData.length)
          val outputParseVector =
              devSlice.map(parse(_, gradient, devStats, true, pruneStats))
          val postDev = System.nanoTime()

          val outputParses = outputParseVector.toList.sortWith( _._1 < _._1 )
          val filename = Config.prefix +".auto_all.dev"+ devIter.toString
          val autoOut = new ParseWriter(filename)
          for (i <- 0 until outputParses.size)
            autoOut.write(outputParses(i)._2)
          autoOut.close()
          devIter += 1

          devStats.wallTime += postDev - preDev
          Log.logln(f"Trainstats-d $totalParsed%6d  "+ devStats.toString)
          if (Log.check(Log.VMarginals, stage.verbosityEvalInTrain))
            Log.logln(pruneStats)

          // Save intermediate model
          ObjectWriter.save(stage.model.wordIndex, s"${Config.prefix}.dev${devIter}.index.word.gz")
          ObjectWriter.save(stage.model.argIndex, s"${Config.prefix}.dev${devIter}.index.argument.gz")
          ObjectWriter.save(stage.model.nonTerminalIndex, s"${Config.prefix}.dev${devIter}.index.nonterminal.gz")
          ObjectWriter.save(stage.model, s"${Config.prefix}.dev${devIter}.model.gz")
          ObjectWriter.save(stage.rules, s"${Config.prefix}.dev${devIter}.rules.gz")
        }

        if (index > Config.earlyIterationStop) index += order.length

///        for ((length, ruleCounter) <- stage.ruleCounters) {
///          ruleCounter.foreachPair{ case (key, value) =>
///            Log.logln(s"qr$key $value $length")
///          }
///        }
      }
      trainStats.regularisation = Config.c *
        ( if (Config.l2reg) model.weights.squareSum else model.weights.sum )

      Log.logln(f"Trainstats-t $iter%6d  "+ trainStats.toString)
      if (Log.check(Log.VMarginals, stage.verbosityTrain))
        Log.logln(trainPruneStats)

      // This is a good point to garbage collect as none of the parsers (and
      // associated caches) are live anymore.
      System.gc()
    }
  }

  def gParseDep(parse: Parse, tags: Vector[String]) =
    parse match {
      case p: DepParse => DepParse(p.sentence, p.tokens, tags, p.deps)
      case _ => require(false, "Inconsistency") ; null
    }

  def gParsePSG(parse: Parse, tags: Vector[String]) =
    parse match {
      case p: psg.Graph =>
        psg.Graph(p.sentence, p.tokens, tags, p.spines, p.edges)
      case _ => require(false, "Inconsistency") ; null
    }

  def depDiscriminative(
    trainData: ArrayBuffer[(Int, DepParse)],
    devData: ArrayBuffer[(Int, DepParse)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: DiscriminativeModel
  ) = {
    val parser = stage.generateParser(true, false, false)
    // For each sentence, insert the gold parse into the chart (without
    // parsing) and add one for every feature that is triggered in the
    // process. Then use those as counts to prune the feature space.
    // This also ensures that space for all surface feature references is
    // created.
    val featCounts = new IntIntMap(Config.loadFactor, 1 << 18)
    val featureCounter = (i: Int, v: Double) => featCounts.changeOrPut(i, 1)
    for (((sentID, tparse), tags) <- trainData.zip(trainAutoPOS)) {
      val parse =
        if (tags.length == 0) tparse
        else gParseDep(tparse, tags)
      parser.prepare(parse.tokens, parse.tags, sentID, null, null, null,
        Config.lossType)
      parser.fineChart.featureCounter = featureCounter
      parser.fineChart.useModelCache = Config.cachedInit
      val (gterminals, garcs) = model.parseToArrays(parse, true, stage)
      val goldItems = parser.insertDerivation(gterminals, garcs)
    }
    parser.fineChart.featureCounter = null

    model.weights.pruneAndPrepare(Config.featureFreqCutoff,
      Config.arcFreqCutoff, featCounts)
    for (((sentID, tparse), tags) <- devData.zip(devAutoPOS)) {
      val parse =
        if (tags.length == 0) tparse
        else gParseDep(tparse, tags)
      parser.prepare(parse.tokens, parse.tags, sentID, null, null, null,
        Config.lossType)
    }

    Log.logln(s"Model Info ${model.weights.sizes}")

    Config.learnerType match {
      case LearnerType.Counting =>
        featCounts.foreachPair(
          (k: Int, v: Int) => model.weights.update(k, v * delta)
        )
      case LearnerType.Perceptron =>
        Log.logln("Training with perceptron")
        model.weights.updateOnGet = null
        if (Config.averagedLearning)
          sumForAv.extend(model.weights.size, 0.0)

        generalLearner(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          model, gParseDep, perceptronUpdate(model, _: Int, _: Double))

        if (Config.averagedLearning) {
          // Apply average
          var i = 0
          val u = updateNumber + 1
          while (i < sumForAv.length) {
            val w = model.weights.values(i)
            val s = sumForAv(i)
            model.weights.set(i, w - (s / u))
            i += 1
          }
        }
      case LearnerType.PrimalSVM =>
        sumSqrGrad.extend(model.weights.size, Config.delta)
        lastUpdates.extend(model.weights.size, 0)

        if (Config.l2reg) {
          model.weights.updateOnGet =
            if (etaC > 0.0) opsL2updateOnGet(model, _: Int)
            else (a: Int) => { }
          val update = opsL2Update(model, _: Int, _: Double)
          generalLearner(trainData, devData, trainAutoPOS, devAutoPOS, stage,
            model, gParseDep, update)
        } else {
          model.weights.updateOnGet =
            if (etaC > 0.0) opsL1updateOnGet(model, _: Int)
            else (a: Int) => { }
          val update = opsL1Update(model, _: Int, _: Double)
          generalLearner(trainData, devData, trainAutoPOS, devAutoPOS, stage,
            model, gParseDep, update)
        }
    }

    Log.logln(s"Model Info ${model.weights.sizes}")
  }

  def prepareCaches(
    model: DiscriminativeModel, stage: Stage,
    featureCounter: (Int, Double) => Unit, parse: psg.Graph, sentID: Int,
    insertGold: Boolean = true
  ) = {
    val parser = stage.generateParser(true, false, false)
    parser.prepare(parse.tokens, parse.tags, sentID, null, null, null,
      LossType.ZERO)
    if (insertGold) {
      if (Log.check(Log.VParseExtraction, stage.verbosityTrain))  {
        Log.logln(parse)
      }
      parser.fineChart.featureCounter = featureCounter
      parser.fineChart.useModelCache = Config.cachedInit
      val (gterminals, garcs) = model.parseToArrays(parse, true, stage)
      val goldItems = parser.insertDerivation(gterminals, garcs)
///      Log.logln(parser.fineChart.toString())
///      Log.logln(parser.fineChart.toDenseString(goldItems))
    }
  }

  def psgDiscriminative(
    trainData: ArrayBuffer[(Int, psg.Graph)],
    devData: ArrayBuffer[(Int, psg.Graph)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: DiscriminativeModel
  ) = {
    // For each sentence, insert the gold parse into the chart (without
    // parsing) and add one for every feature that is triggered in the
    // process. Then use those as counts to prune the feature space.
    // This also ensures that space for all surface feature references is
    // created.
    Log.logln("Creating positive features")
    val featCounts = new IntIntMap(Config.loadFactor, 1 << 18)
    val featureCounter = (i: Int, v: Double) => featCounts.changeOrPut(i, 1)
    var count = 0
    for (((sentID, tparse), tags) <- trainData.zip(trainAutoPOS)) {
///      if (count % 100 == 0 && Log.check(Log.VMemoryStats, stage.verbosityTrain)) {
///        val meter = new MemoryMeter()
///        Log.logln(s"memory model-count              ${meter.countChildren(model)}")
///        Log.logln(s"memory model                    ${meter.measureDeep(model)}")
///        Log.logln(s"memory allowedArgLists          ${meter.measureDeep(model.allowedArgLists)}")
///        Log.logln(s"memory weights.surfaceFeatToID  ${meter.measureDeep(model.weights.surfaceFeatToID)}")
///        Log.logln(s"memory weights.structureMapping ${meter.measureDeep(model.weights.structureMapping)}")
///        Log.logln(s"memory weights.values           ${meter.measureDeep(model.weights.values)}")
///        Log.logln(s"memory surfaceFeatToID.size ${model.weights.surfaceFeatToID.size}")
///      }
      val parse = if (tags.length == 0) tparse else gParsePSG(tparse, tags)
      prepareCaches(model, stage, featureCounter, parse, sentID)
      if (count % 1000 == 0) Log.logln(s"Processed $count sentences")
      count += 1
    }
    model.weights.pruneAndPrepare(Config.featureFreqCutoff,
      Config.arcFreqCutoff, featCounts)
    stage.rules.setAllowedRules(stage.ruleCutoff)

    if (Config.printRuleCounts) {
      val ruleCountOut = new PrintWriter(new File(Config.prefix +".rules.txt"))
      ruleCountOut.print(stage.rules.toString())
      ruleCountOut.close()
    }

    // Prepare cache for dev data too
    for (((sentID, tparse), tags) <- devData.zip(devAutoPOS)) {
      val parse = if (tags.length == 0) tparse else gParsePSG(tparse, tags)
      prepareCaches(model, stage, featureCounter, parse, sentID, false)
    }

    Log.logln(s"Model Info ${model.weights.sizes}")

    Config.learnerType match {
      case LearnerType.Counting =>
        featCounts.foreachPair(
          (k: Int, v: Int) => model.weights.update(k, v * delta)
        )
      case LearnerType.Perceptron =>
        Log.logln("Training with perceptron")
        model.weights.updateOnGet = null
        if (Config.averagedLearning)
          sumForAv.extend(model.weights.size, 0.0)

        generalLearner(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          model, gParsePSG, perceptronUpdate(model, _: Int, _: Double))

        if (Config.averagedLearning) {
          // Apply average
          var i = 0
          val u = updateNumber + 1
          while (i < sumForAv.length) {
            val w = model.weights.values(i)
            val s = sumForAv(i)
            model.weights.set(i, w - (s / u))
            i += 1
          }
        }
      case LearnerType.PrimalSVM =>
        sumSqrGrad.extend(model.weights.size, Config.delta)
        lastUpdates.extend(model.weights.size, 0)

        if (Config.l2reg) {
          model.weights.updateOnGet =
            if (etaC > 0.0) opsL2updateOnGet(model, _: Int)
            else (a: Int) => { }
          val update = opsL2Update(model, _: Int, _: Double)
          generalLearner(trainData, devData, trainAutoPOS, devAutoPOS,
            stage, model, gParsePSG, update)
        } else {
          model.weights.updateOnGet =
            if (etaC > 0.0) opsL1updateOnGet(model, _: Int)
            else (a: Int) => { }
          val update = opsL1Update(model, _: Int, _: Double)
          generalLearner(trainData, devData, trainAutoPOS, devAutoPOS,
            stage, model, gParsePSG, update)
        }
    }

    Log.logln(s"Model Info ${model.weights.sizes}")
  }

  def amrDiscriminative(
    trainData: ArrayBuffer[(Int, AMR)],
    devData: ArrayBuffer[(Int, AMR)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage,
    model: DiscriminativeModel
  ) = {
    Config.learnerType match {
      case LearnerType.Counting => // TODO
      case LearnerType.Perceptron => // TODO
      case LearnerType.PrimalSVM => // TODO
    }
  }

  def depLearn(
    trainData: ArrayBuffer[(Int, DepParse)],
    devData: ArrayBuffer[(Int, DepParse)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage
  ) = {
    stage.model match {
      case gmodel: GenerativeModel =>
        depGenerative(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          gmodel)
      case dmodel: DiscriminativeModel =>
        depDiscriminative(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          dmodel)
      case _ =>
    }
  }

  def amrLearn(
    trainData: ArrayBuffer[(Int, AMR)],
    devData: ArrayBuffer[(Int, AMR)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage
  ) =
    stage.model match {
      case gmodel: GenerativeModel =>
        amrGenerative(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          gmodel)
      case dmodel: DiscriminativeModel =>
        amrDiscriminative(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          dmodel)
      case _ =>
    }

  def psgLearn(
    trainData: ArrayBuffer[(Int, psg.Graph)],
    devData: ArrayBuffer[(Int, psg.Graph)],
    trainAutoPOS: ArrayBuffer[Vector[String]],
    devAutoPOS: ArrayBuffer[Vector[String]], stage: Stage
  ) = {
///		val meter = new MemoryMeter()
///		Log.logln(
///			s"memory train    ${meter.measureDeep(trainData)}\n"+
///			s"memory train-pos    ${meter.measureDeep(trainAutoPOS)}\n"+
///			s"memory dev    ${meter.measureDeep(devData)}\n"+
///			s"memory dev-pos    ${meter.measureDeep(devAutoPOS)}\n"
///		)
///		val runtime = Runtime.getRuntime
///		Log.logln(
///			s"memory used   ${runtime.totalMemory - runtime.freeMemory}\n"+
///			s"memory free   ${runtime.freeMemory}\n"+
///			s"memory total  ${runtime.totalMemory}\n"+
///			s"memory max    ${runtime.maxMemory}\n"
///		)

    val model = stage.model
    // Prepare counts for all words
    for {
      (sentID, parse) <- trainData
      token <- parse.tokens
    } model.addToken(token)
    model.createTokenMapping()
    // Seed the non-terminal index to ensure bare non-terminals are early
    for {
      (sentID, parse) <- trainData
      tag <- parse.tags
    } model.nonTerminalIndex(tag, true)
    for {
      tags <- trainAutoPOS
      tag <- tags
    } model.nonTerminalIndex(tag, true)
    // In the case of very small training sets we may miss some POS tags that
    // are in the dev set (something we can assume won't happen once at full
    // scale). Make sure they exist in the indices.
    for {
      (sentID, parse) <- devData
      tag <- parse.tags
    } model.nonTerminalIndex(tag, true)
    for {
      tags <- devAutoPOS
      tag <- tags
    } model.nonTerminalIndex(tag, true)

    // Add other non-terminal symbols
    for {
      (sentID, parse) <- trainData
      spine <- parse.spines
      node <- spine.nodes
    } {
      model.nonTerminalIndex(node.symbol)
      node.functions.foreach( model.nonTerminalIndex(_) )
    }

    // Make sure all traces then immediately follow
    for {
      (sentID, parse) <- trainData
      edge <- parse.edges
    } model.nonTerminalIndex(edge.trace)

    // Create and count spines
    if (stage.parserType != ParserType.ARC &&
        stage.parserType != ParserType.TSCORER) {
      for (((sentID, parse), atags) <- trainData.zip(trainAutoPOS)) {
        for (i <- 0 until parse.tokens.length) {
          val tag = if (atags.length > 0) atags(i) else parse.tags(i)
          model.addSpineOption(i, tag, parse)
        }
      }
    }

    // Create and count edge types
    for ((sentID, parse) <- trainData) {
      val goldEdges = UnboxedArrayBuffer()
      val goldSpines = new ArrayBuffer[Int]
      model.setGoldParse(parse, goldEdges, goldSpines, true, stage)
      goldEdges.prepareToIterate
      while (goldEdges.hasNext) {
        val label = goldEdges.next
        if (label >= 0) model.updateEdgeCounts(label)
      }
    }
    model.createReorderingMaps()

    // Train
    model match {
      case gmodel: GenerativeModel =>
        psgGenerative(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          gmodel)
      case dmodel: DiscriminativeModel =>
        psgDiscriminative(trainData, devData, trainAutoPOS, devAutoPOS, stage,
          dmodel)
      case _ =>
    }
  }
}

