// 1EC Graph Parser
// Copyright (c) University of California
// Copyright (c) Jonathan Kummerfeld
//
// This software is covered by a license.  See the LICENSE.txt file in the
// top-level directory of this distribution or at
// https://github.com/jkkummerfeld/1ec-graph-parser for the full text of the
// license.

package edu.berkeley.nlp.graphparser

import scala.collection.mutable.{Queue, HashMap, HashSet, ArrayBuffer}

/** A rule based aligner.
  * Returns a hashmap where the token number in the sentence is mapped to a
  * concept. Each concept is used only once and not every token is mapped.
  */
object HeuristicAligner {
  val log = true

  def alignment(amr: AMR) : HashMap[Int, Concept] = {
    // TODO:
    //  - Shift the general model to be aimed at giving a set of options for
    //    each alignment
    //
    //  - Handle cases of multiple words mapping to a single concept
    //  - Handle cases where a word occurs twice, but with different capitolisation
    //  - Consider adding irregular demonyms
    //  - Fix cases like 'terrorism -> terror-01 when terror, is a word'
    //  - Consider adding a special case for one word, one concept examples
    val alignment = new HashMap[Int, Concept]
    def mapConcepts(concept: Concept,
                   map: HashMap[String, ArrayBuffer[Concept]]) = {
      val predicateRegEx = "^(.*)-[0-9]*$".r
      val label = concept.label match {
        case "-" => "NEG"
        case "have-org-role-91" => "ORG_ROLE_SPECIAL"
        case predicateRegEx(group) => group
        case _ => concept.label
      }
      // TODO: rethink this restriction
      if (! (concept.args.length == 1 && concept.args(0)._1 == "name"))
        map.getOrElseUpdate(label, new ArrayBuffer[Concept]) += concept
    }
    val conceptMap = new HashMap[String, ArrayBuffer[Concept]]
    amr.focus.apply(mapConcepts(_: Concept, conceptMap))
    val counts = new HashMap[String, Int]
    for {i <- 0 until amr.tokens.length
         word = amr.tokens(i).toLowerCase()} {
      if (! counts.contains(word)) counts(word) = 0
      counts(word) += 1
    }
    val used = new HashSet[Concept]

    def getNum(s: String) =
      try {
        s.toInt
      } catch {
        case e:Exception => -1
      }
    def isNum(s: String) = s forall (! Character.isLetter(_))

    val optionsTried = HashMap[Int, ArrayBuffer[Concept]]()
    val revOptionsTried = HashMap[Concept, ArrayBuffer[Int]]()
    def addAlignments(predicate: (String, String) => Boolean) = {
      val options = HashMap[Int, ArrayBuffer[Concept]]()
      val revOptions = HashMap[Concept, ArrayBuffer[Int]]()
      for {(label, concepts) <- conceptMap
           concept = concepts(0)
           if ! used.contains(concept)
          } {
        for {i <- 0 until amr.tokens.length
             if ! alignment.contains(i)
             word = amr.tokens(i).toLowerCase()
             if predicate(word, label)
            } {
          if (concepts.length == 1 && counts(word) == 1) {
            options.getOrElseUpdate(i, ArrayBuffer[Concept]()).append(concept)
            revOptions.getOrElseUpdate(concept, ArrayBuffer[Int]()).append(i)
          }
          for (c <- concepts) {
            optionsTried.getOrElseUpdate(i, ArrayBuffer[Concept]()).append(c)
            revOptionsTried.getOrElseUpdate(c, ArrayBuffer[Int]()).append(i)
          }
        }
      }
      for ((token, concepts) <- options) {
        if (concepts.length == 1 && revOptions.getOrElse(concepts(0), Nil).length == 1) {
          used += concepts(0)
          alignment(token) = concepts(0)
          if (log) println(s"""Aligned (${concepts(0).variable} / ${concepts(0).label}) to '${amr.tokens(token)}' (token ${token})""")
        }
      }
    }
///    addAlignments(_.toLowerCase() == _.toLowerCase())
    if (log) println("nums")
    addAlignments((x, y) => getNum(x) == getNum(y) && getNum(x) > 0)
    if (log) println("nums exact")
    addAlignments((x, y) => isNum(x) && isNum(y) && x == y)
    if (log) println("exact match")
    addAlignments((x, y) => !isNum(x) && x.toLowerCase() == y.toLowerCase())
    if (log) println("full prefix")
    addAlignments((x, y) => !isNum(x) && x.toLowerCase().startsWith(y.toLowerCase()) && x.length > 2)
    if (log) println("prefix 6")
    addAlignments((x, y) => !isNum(x) && x.toLowerCase().slice(0, 6) == y.toLowerCase().slice(0, 6))
    if (log) println("prefix 5")
    addAlignments((x, y) => !isNum(x) && x.toLowerCase().slice(0, 5) == y.toLowerCase().slice(0, 5))
    if (log) println("prefix 4")
    addAlignments((x, y) => !isNum(x) && x.toLowerCase().slice(0, 4) == y.toLowerCase().slice(0, 4))
    if (log) println("basic stem")
    addAlignments((x, y) => !isNum(x) && x.toLowerCase().endsWith("ing") &&
      y.toLowerCase().endsWith("e") &&
      x.toLowerCase().stripSuffix("ing") == y.toLowerCase().stripSuffix("e"))
    if (log) println("web")
    addAlignments((x, y) => !isNum(x) && x.startsWith("http://") &&
                            x.stripPrefix("http://") == y)
    if (log) println("special cases")
    val dateMap = HashMap("january" -> "1", "february" -> "2", "march" -> "3",
      "april" -> "4", "may" -> "5", "june" -> "6", "july" -> "7",
      "august" -> "8", "september" -> "9", "october" -> "10",
      "november" -> "11", "december" -> "12")
    addAlignments((x, y) => dateMap.getOrElse(x.toLowerCase(), -1) == y)
    val numMap = HashMap[String, String](
      "one" -> "1", "two" -> "2", "three" -> "3",
      "four" -> "4", "five" -> "5", "six" -> "6", "seven" -> "7",
      "eight" -> "8", "nine" -> "9", "ten" -> "10", "eleven" -> "11",
      "twelve" -> "12", "thiretten" -> "13", "fourteen" -> "14",
      "fifteen" -> "15", "sixteen" -> "16", "seventeen" -> "17",
      "eighteen" -> "18", "nineteen" -> "19", "twenty" -> "20",
      "first" -> "1", "second" -> "2", "third" -> "3", "fourth" -> "4",
      "fifth" -> "5", "sixth" -> "6", "seventh" -> "7", "eighth" -> "8",
      "ninth" -> "9", "tenth" -> "10", "eleventh" -> "11", "twelveth" -> "12",
      "thirteenth" -> "13", "fourteenth" -> "14", "fifteenth" -> "15",
      "sixteenth" -> "16", "seventeenth" -> "17", "eighteenth" -> "18",
      "nineteenth" -> "19", "twentieth" -> "20", "thirtieth" -> "30",
      "fourtieth" -> "40", "fiftieth" -> "50", "sixtieth" -> "60",
      "seventieth" -> "70", "eightieth" -> "80", "nintieth" -> "90",
      "1st" -> "1", "2nd" -> "2", "3rd" -> "3", "4th" -> "4", "5th" -> "5", "6th" -> "6",
      "7th" -> "7", "8th" -> "8", "9th" -> "9", "10th" -> "10", "11th" -> "11", "12th" -> "12",
      "13th" -> "13", "14th" -> "14", "15th" -> "15", "16th" -> "16", "17th" -> "17",
      "18th" -> "18", "19th" -> "19", "20th" -> "20", "21st" -> "21", "22nd" -> "22",
      "23rd" -> "23", "24th" -> "24", "25th" -> "25", "26th" -> "26", "27th" -> "27",
      "28th" -> "28", "29th" -> "29", "30th" -> "30", "31st" -> "31")
    addAlignments((x, y) => numMap.getOrElse(x.toLowerCase(), -1) == y)
    val specialCases = HashMap(
      // edit distance 1
      "chose" -> "choose",
      "held" -> "hold",
      "began" -> "begin",
      "them" -> "they",
      "women" -> "woman",
      "men" -> "man",
      "led" -> "lead",
      "met" -> "meet",
      "lost" -> "lose",
      // further
      "kept" -> "keep",
      "in" -> "include",
      "sold" -> "sell",
      "death" -> "die", "deaths" -> "die",
      "said" -> "say", "according" -> "say",
      "if" -> "interrogative", "whether" -> "interrogative",
      "but" -> "contrast",
      "could" -> "possible", "may" -> "possible", "might" -> "possible",
      "can" -> "possible", "be" -> "possible",
      "must" -> "obligate", "have" -> "obligate",
      "should" -> "recommend",
      "told" -> "tell",
      "not" -> "NEG", "didn't" -> "NEG", "no" -> "NEG", "lack" -> "NEG",
      "don't" -> "NEG", "isn't" -> "NEG", "aren't" -> "NEG", "neither" -> "NEG",
      "nor" -> "NEG", "cannot" -> "NEG", "n't" -> "NEG", "doesn't" -> "NEG",
      "without" -> "NEG",
      "naval" -> "navy",
      "anti" -> "oppose", "anti" -> "counter",
      "let" -> "allow",
      "these" -> "this",
      "those" -> "that",
      "her" -> "she",
      "his" -> "he", "him" -> "he",
      "my" -> "i", "me" -> "i", "i'm" -> "i", "myself" -> "i",
      "their" -> "they",
      "us" -> "we", "our" -> "we",
      "people" -> "person", "those" -> "person",
      "sales" -> "sell",
      "located" -> "be-located-at",
      "necessary" -> "need",
      "birth" -> "bear",
      "left" -> "leave",
      ";" -> "and", "," -> "and", "nor" -> "and",
      "!" -> "expressive",
      "whos" -> "have", "has" -> "have",
      "per" -> "1", "every" -> "1", "a" -> "1",
      "beloved" -> "love",
      "french" -> "france",
      "like" -> "resemble",
      "basis" -> "base",
      "relies" -> "rely",
      "denies" -> "deny",
      "ability" -> "capable",
      "because" -> "cause", "since" -> "cause", "reasons" -> "cause",
      "then" -> "infer",
      "sure" -> "assure",
      "traitors" -> "betray",
      "subs" -> "submarine",
      "according" -> "conform",
      "went" -> "go"
    )
    addAlignments((x, y) => specialCases.getOrElse(x.toLowerCase(), "") == y)
    val specialMultiCases = HashMap(
      "?" -> Array("amr-unknown", "interrogative")
    )
    addAlignments((x, y) =>
      specialMultiCases.getOrElse(x.toLowerCase(), Array[String]()).exists(_ == y))

    if (log) println("greedy matching")
    val pathCosts = HashMap[(Concept, Concept), Int]()
    def addCosts(start: Concept) = {
      val queue = Queue[(Concept, Int)]((start, 0))
      while (! queue.isEmpty) {
        val cur = queue.dequeue
        for (parent <- cur._1.parents) {
          if (! pathCosts.contains((start, parent))) {
            pathCosts((start, parent)) = cur._2 + 1
            queue.enqueue((parent, cur._2 + 1))
            if (parent.parents.length == 1) {
              val parent2 = parent.parents(0)
              if (! pathCosts.contains((start, parent2)) &&
                  parent2.args(0)._1 == "name") {
                pathCosts((start, parent2)) = cur._2 + 1
                queue.enqueue((parent2, cur._2 + 1))
              }
            }
          }
        }
        for ((arg, child) <- cur._1.args) {
          if (! pathCosts.contains((start, child))) {
            pathCosts((start, child)) = cur._2 + 1
            queue.enqueue((child, cur._2 + 1))
            if (child.args.length == 1 && child.args(0)._1 == "name") {
              for ((arg2, child2) <- child.args) {
                if (! pathCosts.contains((start, child2))) {
                  pathCosts((start, child2)) = cur._2 + 1
                  queue.enqueue((child2, cur._2 + 1))
                }
              }
            }
          }
        }
      }
    }
    amr.focus.apply(addCosts(_))
///    for (((c1, c2), cost) <- pathCosts)
///      println(s"(${c1.variable} / ${c1.label}) (${c2.variable} / ${c2.label})  $cost")

    val printed = (HashSet[Int](), HashSet[Concept]())
    val todo = (ArrayBuffer[Int](), ArrayBuffer[Concept]())
    def followLinks(concept: Concept = null, index: Int = -1) : String = {
      var ans = ""
      if (concept != null) {
        for {num <- revOptionsTried(concept)
             if !alignment.contains(num) && ! printed._1.contains(num)} {
          printed._1.add(num)
          todo._1.append(num)
          ans += s" ${amr.tokens(num)},$num" + followLinks(index=num)
        }
      } else {
        for {c <- optionsTried(index)
             if !used.contains(c) && ! printed._2.contains(c)} {
          printed._2.add(c)
          todo._2.append(c)
          ans += s" (${c.variable} / ${c.label})" + followLinks(c)
        }
      }
      ans
    }
    for ((concept, vals) <- revOptionsTried) {
      if (! used.contains(concept) && ! printed._2.contains(concept)) {
        todo._1.clear
        todo._2.clear
///        val str = followLinks(concept)
///        println("Unaligned component:   "+ str)

        // For each word-concept pair, calculate the best score
        val scores = HashMap[(Concept, Int), Double]()
        for {c <- todo._2
             i <- todo._1} {
          var dist = 0
          var best = (-1.0, -1, -1, -1)
          for (j <- (i + 1) until amr.tokens.length) {
            if (alignment.contains(j)) {
              dist += 1
              val cdist = pathCosts((c, alignment(j)))
              val tdist = cdist + dist + (j - i) / 100.0
              if (best._1 < 0 || tdist < best._1)
                best = (tdist, cdist, dist, j - i)
            }
          }
          dist = 0
          for (j <- (i - 1) to 0 by -1) {
            if (alignment.contains(j)) {
              dist += 1
              val cdist = pathCosts((c, alignment(j)))
              val tdist = cdist + dist + (i - j) / 100.0
              if (best._1 < 0 || tdist < best._1)
                best = (tdist, cdist, dist, i - j)
            }
          }
          if (best._1 >= 0) scores((c, i)) = best._1
///            println(s"(${c.variable} / ${c.label}) ${amr.tokens(i)},$i   $best")
        }

        // repeat TODO: Switch to considering all options
        for (i <- 0 until todo._1.length) {
          // get best score
          var best : ((Concept, Int), Double) = null
          for (opt <- scores)
            if (! alignment.contains(opt._1._2) && !used.contains(opt._1._1) &&
                (best == null || best._2 > opt._2)) best = opt
          // set alignment
          if (best != null) {
            alignment(best._1._2) = best._1._1
            used.add(best._1._1)
            val c = best._1._1
            val token = best._1._2
            if (log) println(s"""Aligned (${c.variable} / ${c.label}) to '${amr.tokens(token)}' (token ${token})""")
          }
        }
      }
    }

    // Currently just greedy max of scores, could switch to trying to avoid
    // creating non-projectivity


    if (amr.tokens.length == 1 && amr.focus.label == "amr-empty")
      alignment(0) = amr.focus

    if (log) {
      println("Unaligned words:")
      for {i <- 0 until amr.tokens.length
           if ! alignment.contains(i)
           word = amr.tokens(i).toLowerCase()}
        println(s"$i $word")
      println("Unaligned concepts:")
      amr.focus.apply{ c =>
        if (! used.contains(c)) println(s"${c.variable} / ${c.label}")
      }
      println()
    }

    alignment
  }

  def getSpan(concept: Concept,
              alignment: HashMap[Int, Concept]) : Tuple2[Int, Int] = {
    var min = 1000
    var max = -1
    for {pair <- alignment
         if pair._2.variable == concept.variable} {
      min = pair._1
      max = pair._1 + 1
    }
    for (arg <- concept.args
         if arg._2.parents.length == 1) {
      val span = getSpan(arg._2, alignment)
      if (span._1 != 1000) {
        min = min.min(span._1)
        max = max.max(span._1)
      }
      if (span._2 != -1) {
        min = min.min(span._2)
        max = max.max(span._2)
      }
    }
    (min, max)
  }
}
