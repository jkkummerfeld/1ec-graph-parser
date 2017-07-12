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
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap}
import scala.math.exp
import scala.collection.parallel._

// TODO: Add support for externally defined spines

object JKKMain extends App {
  val margs =
    if (args(0).startsWith("edu.berkeley.nlp.graphparser"))
      args.slice(1, args.length)
    else args
  import Config._
  setArgs(margs)

  if (! logToStdout)
    Log.logFile = new java.io.PrintStream(new File(prefix +".log"))
  Log.logln(s"# Running with:\n# ${margs.mkString(" ")}\n")

  require(! (goldPOS && parseType == Formalism.AMR),
    "Gold POS not available for AMR")
  if (trainGoldPOS && trainAutoPOS.isDefined)
    Log.logln("Given trainAutoPOS and trainGoldPOS, using gold POS")
  if (evalGoldPOS && evalAutoPOS.isDefined)
    Log.logln("Given evalAutoPOS and evalGoldPOS, using gold POS")
  require(stepSize >= 0.0, "Step size must be non-negative")
  require(c >= 0.0, "C must be non-negative")

  (margs(0), margs.length - 1) match {
    case ("-traintest", _) =>

///      verbosity = trainVerbosity

      // Read or create indices
      // TODO: In what ways are the separate indices useful?
      val wordIndex = indexPrefix.fold(
        new IntIndexer(0x7fffffff, Config.lowercaseWordFeatures)
      )(text => IntIndexer(text +"word.gz"))
      val argIndex = indexPrefix.fold(ArgumentIndexer(parseType)){text =>
        ArgumentIndexer(text +"argument.gz")}
      val ntIndex = indexPrefix.fold(NonTerminalIndexer(parseType))(text =>
        NonTerminalIndexer(text +"nonterminal.gz"))

      val stages = new ArrayBuffer[Stage]

      {
        var prev : Option[Stage] = None
        var i = 0
        for (filename <- Config.stageConfigs) {
          val stage = Stage(filename, prev, wordIndex, argIndex, ntIndex, i)
          i += 1
          prev = Option(stage)
          stages.append(stage)
        }
      }

      require(stages.length > 0, "No stages defined")

      // --------
      // Train
      // --------
      // TODO: Clean up to avoid repetition
      if (Config.train) {
        Log.logln(s"Doing training")
        val trainPOSReader =
          if (trainGoldPOS) None
          else trainAutoPOS.map{ filename =>
            Source.fromFile(filename).getLines }
        val evalPOSReader =
          if (evalGoldPOS) None
          else evalAutoPOS.map{ filename =>
            Source.fromFile(filename).getLines }
        trainIn.foreach{filename =>
          if (parseType == Formalism.AMR) {
            val trainData = ArrayBuffer[(Int, AMR)]()
            val reader = new AMRReader(filename)
            val trainAutoPOS = ArrayBuffer[Vector[String]]()
            while (trainData.size < maxTrainSents && reader.hasNext) {
              val (sentID, parse) = reader.next
              val pos =
                trainPOSReader.fold(Vector[String]())(
                  _.next().split(" ").toVector )
              if (parse.tokens.length <= maxLength &&
                  parse.tokens.length >= minLength) {
                trainData.append((sentID, parse))
                trainAutoPOS.append(pos)
              }
            }
            val devData = ArrayBuffer[(Int, AMR)]()
            val devAutoPOS = ArrayBuffer[Vector[String]]()
            Log.logln(s"Training AMR on ${trainData.length} sentences")
            Learn.amrLearn(trainData, devData, trainAutoPOS, devAutoPOS,
              stages.last)
          } else if (parseType == Formalism.DEP) {
            val trainData = ArrayBuffer[(Int, DepParse)]()
            val reader = new DEPReader(filename)
            val trainAutoPOS = ArrayBuffer[Vector[String]]()
            while (trainData.size < maxTrainSents && reader.hasNext) {
              val (sentID, parse) = reader.next
              val pos =
                if (trainGoldPOS) parse.tags
                else trainPOSReader.fold(Vector[String]())(
                  _.next().split(" ").toVector )
              if (parse.tokens.length <= maxLength &&
                  parse.tokens.length >= minLength) {
                trainData.append((sentID, parse))
                trainAutoPOS.append(pos)
              }
            }
            val devData = ArrayBuffer[(Int, DepParse)]()
            val devAutoPOS = ArrayBuffer[Vector[String]]()
            Log.logln(s"Training Dep on ${trainData.length} sentences")
            Learn.depLearn(trainData, devData, trainAutoPOS, devAutoPOS,
              stages.last)
          } else if (parseType == Formalism.PSG) {
            val trainData = ArrayBuffer[(Int, psg.Graph)]()
            val reader = new PSGReader(filename)
            val trainAutoPOS = ArrayBuffer[Vector[String]]()
            while (trainData.size < maxTrainSents && reader.hasNext) {
              val (sentID, parse) = reader.next
              val pos =
                if (trainGoldPOS) parse.tags
                else trainPOSReader.fold(Vector[String]())(
                  _.next().split(" ").toVector )
              if (parse.tokens.length <= maxLength &&
                  parse.tokens.length >= minLength) {
                trainData.append((sentID, parse))
                trainAutoPOS.append(pos)
              }
            }

            val devData = ArrayBuffer[(Int, psg.Graph)]()
            val devAutoPOS = ArrayBuffer[Vector[String]]()
            evalIn.foreach{ filename =>
              val evalReader = new PSGReader(filename)
              while (devData.size < maxEvalSents && evalReader.hasNext) {
                val (sentID, parse) = evalReader.next
                val pos =
                  if (evalGoldPOS) parse.tags
                  else evalPOSReader.fold( Vector[String]()
                  )( _.next().split(" ").toVector )
                if (parse.tokens.length <= maxLength &&
                    parse.tokens.length >= minLength) {
                  devData.append((sentID, parse))
                  devAutoPOS.append(pos)
                }
              }
            }

            Log.logln(s"Training PSG on ${trainData.length} sentences")
            Learn.psgLearn(trainData, devData, trainAutoPOS, devAutoPOS,
              stages.last)
          }
        }
        Log.logln(s"Done training")

        if (printIndexes) {
          val indexOut = new PrintWriter(new File(prefix +".index.txt"))
          indexOut.print(
            s"\nIndex Words: (${wordIndex.size} total)\n"+
            wordIndex.toString.split("\n").map("Word "+ _).mkString("\n"))
          indexOut.print(
            s"\nIndex Non-Terminals: (${ntIndex.size} total)\n"+
            ntIndex.toString.split("\n").map("Non-Terminal "+ _).mkString("\n"))
          indexOut.print(
            s"\nIndex Args: (${argIndex.size} total)\n"+
            argIndex.toString.split("\n").map("Arg "+ _).mkString("\n"))
          indexOut.close()
        }
        if (printModel) stages.last.model.writeToText(prefix +".model.txt")
        if (train) {
          ObjectWriter.save(wordIndex, prefix +".index.word.gz")
          ObjectWriter.save(argIndex, prefix +".index.argument.gz")
          ObjectWriter.save(ntIndex, prefix +".index.nonterminal.gz")
          ObjectWriter.save(stages.last.model, prefix +".model.gz")
          ObjectWriter.save(stages.last.rules, prefix +".rules.gz")
        }
      }

      // --------
      // Evaluate
      // --------
///      verbosity = evalVerbosity

      // Read evaluation set
      val evalData = ArrayBuffer[(Int, Parse, Vector[String], Int)]()
      evalIn.foreach{filename =>
        val evalReader : Iterator[(Int, Parse)] =
          if (parseType == Formalism.AMR) new AMRReader(filename)
          else if (parseType == Formalism.DEP) new DEPReader(filename)
          else new PSGReader(filename)
        val evalPOSReader =
          if (evalGoldPOS) None
          else evalAutoPOS.map{ filename =>
            Source.fromFile(filename).getLines }
        for ((sentID, goldParse) <- evalReader) {
          val autoPOS =
            if (evalGoldPOS) {
              goldParse match {
                case psg: psg.Graph => psg.tags
                case dep: DepParse => dep.tags
                case _ => Vector[String]()
              }
            } else evalPOSReader.fold( Vector[String]() ){
              _.next().split(" ").toVector
            }
          val tokens = goldParse.tokens
          if (tokens.length <= maxLength &&
              tokens.length >= minLength &&
              evalData.length < maxEvalSents)
            evalData.append((evalData.length, goldParse, autoPOS, sentID))
        }
      }

      stages.filter(_.eval).foreach{ stage =>
        val model = stage.model
        val name = stage.name
        Log.logln(s"Evaluating ${stage.name}")

        val stats = new PruningStats()
        val goldAll = HashMap[Int, Parse]()
        val goldParsed = HashMap[Int, Parse]()
        val autoAll = HashMap[Int, Parse]()
        val autoParsed = HashMap[Int, Parse]()
        var missed = 0
        val results = ArrayBuffer[Vector[(Int, Int, String)]]()
        // Pre-processing to ensure parallel execution can run correctly
        evalData.map{ case (sentNo, goldParse, tags, sentID) =>
          val parser = stage.generateParser(false, false, true)
          parser.prepare(goldParse.tokens, tags, sentID, null, null, null,
            LossType.ZERO)
        }
        model match {
          case d: DiscriminativeModel => d.weights.updateOnGet = null
          case _ =>
        }

        val todo = if (Config.parallel) evalData.par else evalData

        todo.map{ case (sentNo, goldParse, tags, sentID) =>
          val parser = stage.generateParser(false, false, true)
          var toLog = ""
          val (sentence, tokens) = (goldParse.sentence, goldParse.tokens)
          val goldEdges = UnboxedArrayBuffer()
          val goldUnaries = new ArrayBuffer[Int]
          val goldStates =
            if (stage.doNotPruneGoldSpinesInEval ||
                stage.doNotPruneGoldArcsInEval) {
              val gparser = stage.generateParser(false, false, true)
              gparser.prepare(goldParse.tokens, tags, sentID, null, null,
                null, LossType.ZERO)
              val (gterminals, garcs) = stage.model.parseToArrays(goldParse,
                false, stage)
              val goldItems = gparser.insertDerivation(gterminals, garcs)
              HashSet(gparser.fineChart.itemsToStates(goldItems):_*)
            } else new HashSet[(Int, Int, Int, Int, Int)]
          if (stage.doNotPruneGoldSpinesInEval ||
              stage.doNotPruneGoldArcsInEval)
            model.setGoldParse(goldParse, goldEdges, goldUnaries, false, stage)
          val doOutside =
            (!skipFinalOutside) ||
            Log.check(Log.VMarginals, stage.verbosityEval)
          val (arcs, terminals, score) =
            parser.parse(tokens, tags, sentID, doOutside, goldEdges,
              goldUnaries, goldStates)

          if (Log.check(Log.VMarginals, stage.verbosityEval))
            stats.synchronized { parser.updatePruningStats(stats, goldParse) }

          val parse : Parse =
            model.arraysToParse(arcs, terminals, sentence, tokens, parseType,
              tags)
          toLog += "Auto:\n"
          toLog += parse.toString +"\n"
          toLog += "Gold:\n"
          toLog += goldParse.toString +"\n"
          toLog += "Direct comparison:\n"
          toLog += goldParse.compareString(parse,
            stage.parserType == ParserType.SSCORER, false,
            stage.parserType == ParserType.TSCORER) + "\n"
          autoAll.synchronized{
            autoAll(sentNo) = parse
            goldAll(sentNo) = goldParse
            if (score > Float.MinValue) {
              autoParsed(sentNo) = parse
              goldParsed(sentNo) = goldParse
            } else missed += 1
          }
          if (score > Float.MinValue) {
            val scoreSet1 = Evaluate(parse, goldParse)

            // This is a more stringent check, the eval ones only look at
            // src and target, this also checks the child at the target
            // (which is part of the arc definition).
            val (gterms, garcs) = model.parseToArrays(goldParse, false, stage)
            var arcMatch = 0
            var terminalMatch = 0
            for (a <- arcs) if (garcs.contains(a)) arcMatch += 1
            for (t <- terminals) if (gterms.contains(t)) terminalMatch += 1
            val scoreSet2 = Vector(
              (arcMatch, arcs.length, "Arcs"),
              (terminalMatch, terminals.length, "Terminals"))

            val scoreSet = scoreSet1 ++ scoreSet2
            toLog += "Eval:"+ scoreSet.toString +"\n"
            results.synchronized{ results.append(scoreSet) }
            if (Log.check(Log.VParseChartWithGold, stage.verbosityEval)) {
              val goldStates = parser.insertDerivation(gterms, garcs)
              val goldSet = HashSet[(Int, Int)](goldStates:_*)
              toLog += "Gold parse states:\n"+
                parser.fineChart.cellsToString(Some(goldSet)) +"\n"
              Log.logln(parser.fineChart)
            }
          }
          Log.log(toLog)
        }

        val goldAllOut = new ParseWriter(s"$prefix.gold_all.$name")
        val goldParsedOut = new ParseWriter(s"$prefix.gold_parsed.$name")
        val autoAllOut = new ParseWriter(s"$prefix.auto_all.$name")
        val autoParsedOut = new ParseWriter(s"$prefix.auto_parsed.$name")

        for (i <- 0 until goldAll.size) {
          goldAll.get(i).foreach{ parse => goldAllOut.write(parse) }
          goldParsed.get(i).foreach{ parse => goldParsedOut.write(parse) }
          autoAll.get(i).foreach{ parse => autoAllOut.write(parse) }
          autoParsed.get(i).foreach{ parse => autoParsedOut.write(parse) }
        }

        goldAllOut.close()
        goldParsedOut.close()
        autoAllOut.close()
        autoParsedOut.close()

        if (Log.check(Log.VMarginals, stage.verbosityEval)) Log.logln(stats)

        Log.logln(s"${results.size} instances parsed (missed $missed)")
        if (results.length > 0) {
          for (i <- 0 until results(0).length) {
            val result = results.map( v =>
              v(i)
            ).reduce( (a, b) =>
              (a._1 + b._1, a._2 + b._2, a._3)
            )
            val percent = 100 * result._1.toDouble / result._2.toDouble
            Log.logln(f"Result $i, $result  $percent%.2f")
          }
        }
      }

      Log.logFile.close()

    // -------------------------------------------------------------

    case ("-runtest", _) =>

      // Read indices
      val wordIndex = indexPrefix.fold(
        throw new Exception("Missing index")
      )(text => IntIndexer(text +"word.gz"))
      val argIndex = indexPrefix.fold(
        throw new Exception("Missing index")
      )(text => ArgumentIndexer(text +"argument.gz"))
      val ntIndex = indexPrefix.fold(
        throw new Exception("Missing index")
      )(text => NonTerminalIndexer(text +"nonterminal.gz"))

      val stages = new ArrayBuffer[Stage]

      {
        var prev : Option[Stage] = None
        var i = 0
        for (filename <- Config.stageConfigs) {
          val stage = Stage(filename, prev, wordIndex, argIndex, ntIndex, i)
          i += 1
          prev = Option(stage)
          stages.append(stage)
        }
      }

      require(stages.length > 0, "No stages defined")

      // Read evaluation set
      val evalData = ArrayBuffer[(Int, String, Vector[String], Vector[String], Int)]()
      evalIn.foreach{filename =>
        val evalPOSReader =
          if (evalGoldPOS) None
          else evalAutoPOS.map{ filename =>
            Source.fromFile(filename).getLines }
        var sentID = -1
        for (line <- Source.fromFile(filename).getLines) {
          if (line.startsWith("# SentID")) {
            sentID = line.trim.split(" ")(2).toInt
          } else {
            val autoPOS =
              evalPOSReader.fold( Vector[String]() ){
                _.next().split(" ").toVector
              }
            val sentence = line.trim
            val tokens = sentence.split(" ").toVector
            if (tokens.length <= maxLength &&
              tokens.length >= minLength && tokens.length > 0 &&
                evalData.length < maxEvalSents)
              evalData.append((evalData.length, sentence, tokens, autoPOS,
                sentID))
          }
        }
      }

      stages.filter(_.eval).foreach{ stage =>
        val model = stage.model
        val name = stage.name
        Log.logln(s"Evaluating ${stage.name}")

        val autoAll = HashMap[Int, Parse]()
        val autoParsed = HashMap[Int, Parse]()
        var missed = 0
        // Pre-processing to ensure parallel execution can run correctly
        evalData.map{ case (sentNo, sentence, tokens, tags, sentID) =>
          val parser = stage.generateParser(false, false, true)
          parser.prepare(tokens, tags, sentID, null, null, null, LossType.ZERO)
        }
        model match {
          case d: DiscriminativeModel => d.weights.updateOnGet = null
        }

        val taskSupport = Config.threads.filter(_ > 0).map{ n =>
          new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(n))
        }

        val todo =
          if (Config.parallel) {
            val ans = evalData.par
            taskSupport.foreach(ans.tasksupport = _)
            ans
          } else evalData

        // TODO: Change this to just map, rather than treating this as a loop
        todo.map{ case (sentNo, sentence, tokens, tags, sentID) =>
          val parser = stage.generateParser(false, false, true)
          var toLog = ""
          val goldEdges = UnboxedArrayBuffer()
          val goldUnaries = new ArrayBuffer[Int]
          val goldStates = new HashSet[(Int, Int, Int, Int, Int)]
          val (arcs, terminals, score) =
            parser.parse(tokens, tags, sentID, false, goldEdges, goldUnaries,
              goldStates)

          val parse : Parse =
            model.arraysToParse(arcs, terminals, sentence, tokens, parseType,
              tags)
          toLog += "Auto:\n"
          toLog += parse.toString +"\n"
          autoAll.synchronized{
            autoAll(sentNo) = parse
            if (score > Float.MinValue) {
              autoParsed(sentNo) = parse
            } else missed += 1
          }
          if (Log.check(Log.VParseChartWithGold, stage.verbosityEval))
            toLog += parser.fineChart.toString +"\n"
          Log.log(toLog)
        }

        val autoAllOut = new ParseWriter(s"$prefix.auto_all.$name")
        val autoParsedOut = new ParseWriter(s"$prefix.auto_parsed.$name")

        for (i <- 0 until todo.size) {
          autoAll.get(i).foreach{ parse => autoAllOut.write(parse) }
          autoParsed.get(i).foreach{ parse => autoParsedOut.write(parse) }
        }

        autoAllOut.close()
        autoParsedOut.close()
      }

      Log.logFile.close()

    case ("-print", _) =>
      if (parseType == Formalism.AMR)
        for ((_, amr) <- new AMRReader(margs(1))) println(amr)
      else if (parseType == Formalism.DEP)
        for ((_, dep) <- new DEPReader(margs(1))) println(dep)
      else if (parseType == Formalism.PSG)
        for ((sentID, parse) <- new PSGReader(margs(1))) {
          if (parse.tokens.length <= Config.maxLength) {
///            val stage = new Stage("printer", None, true, true, false, false,
///              ParserType.LOCAL, SubbeamType.SPINES, new Rules(true, true), 0,
///              0.0, 0.0, 0.0, 0, 0, 0, false, true, 0, 0, 0, 0.0, 0.0, 0.0, 0.0,
///              ArrayBeam.apply _, 0, 0.0,
///              new DiscriminativeModel(Formalism.PSG, IntIndexer(),
///                ArgumentIndexer(Formalism.PSG),
///                NonTerminalIndexer(Formalism.PSG), true),
///              ArcPassConstraint.EITHER, 0, 0, 0, true, false, false, false,
///              false, None, None)
///            val parser = stage.generateParser(true, false, false)
///            parser.prepare(parse.tokens, parse.tags, null, null, null,
///              LossType.ZERO)
///            val (gterminals, garcs) = stage.model.parseToArrays(parse, true,
///              stage)
///            val goldItems = parser.insertDerivation(gterminals, garcs)
///            println(parser.fineChart.toDenseString(goldItems))
            println(parse)
            for (edge <- parse.dag1ecEdges) println(s"Edge: $edge")
          }
        }

    case ("-eval", _) =>
      val goldIn = new PSGReader(margs(1))
      val autoIn = new PSGReader(margs(2))
      val results = new ArrayBuffer[(Array[Int], String)]
      var missed = 0
      var parsed = 0
      while (goldIn.hasNext && autoIn.hasNext) {
        val (_, autoParse) = autoIn.next
        val (_, goldParse) = goldIn.next
        if (goldParse.tokens.length <= Config.maxLength) {
          println(goldParse.compareString(autoParse))

          // Update coverage
          if (autoParse.spines.length > 0) parsed += 1
          else missed += 1

          // Update score counts (when missed, this still works)
          val scoreSet = Evaluate(autoParse, goldParse)
          for (i <- (0 until scoreSet.length)) {
            if (results.length <= i)
              results.append((Array(0, 0), scoreSet(i)._3))
            results(i)._1(0) += scoreSet(i)._1
            results(i)._1(1) += scoreSet(i)._2
          }

          // Print scores for this sentence
          println(scoreSet.map{ v => s"${v._1} / ${v._2}"}.mkString("  "))
          println()
        }
      }
      if (goldIn.hasNext || autoIn.hasNext)
        println("Error - Different number of parses in auto and gold")
      println("Overall:")
      println(s"$parsed instances parsed (missed $missed)")
      for ((result, i) <- results.zipWithIndex) {
        val percent = 100 * result._1(0).toDouble / result._1(1).toDouble
        println(f"Result $i, ${result._1(0)} / ${result._1(1)} $percent%.2f (${result._2}")
      }

    case ("-p", 2) =>
      val prefix = margs(2)
      var cur = 0
      for ((_, amr) <- new AMRReader(margs(1))) {
        println(cur)
        println(amr)
        AMRDotWriter.write(prefix + cur.toString(), amr)
        cur += 1
      }

    case ("-p", _) =>
      val prefix = margs(2)
      val reverse = margs.contains("-reverseArgOf")
      var cur = 0
      for ((_, amr) <- new AMRReader(margs(1))) {
        println(cur)
        println(amr)
        AMR.measureDistances(amr)
        AMRDotWriter.write(prefix + cur.toString(), amr, reverseArgOf = reverse)
        cur += 1
      }

    case ("-compare", 3) =>
      val gold = new AMRReader(margs(1))
      val output = new AMRReader(margs(2))
      val prefix = margs(3)
      var cur = 0
      while (output.hasNext && gold.hasNext) {
        val (_, oAMR) = output.next()
        val (_, gAMR) = gold.next()
        println(oAMR)
        println(gAMR)
        AMRDotWriter.write(prefix + cur.toString() +".gold", gAMR)
        AMRDotWriter.write(prefix + cur.toString() +".auto", oAMR)
        AMRDotWriter.write(prefix + cur.toString() +".both", gAMR, oAMR)
        cur += 1
      }

    case _ =>
      System.err.println("Arguments did not match any configuration")

  }
}
