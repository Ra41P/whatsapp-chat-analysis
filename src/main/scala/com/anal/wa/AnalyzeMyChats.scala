package com.anal.wa

import scala.io.Source
import scala.util.Try
import java.io.FileWriter
import net.liftweb.json._
import net.liftweb.json.Serialization.write

case class WAMessage(person: String, text: String, date: String, time: String)
case class TextAggLine(text: String, count: Int, data: List[WAMessage])
case class TextBreakDown(text: String, globalCount: Int, personCount: List[TextRow])
case class TextRow(person: String, percTotalUsage: Double)

case class PersonAggLine(person: String, count: Int, data: List[WAMessage])
case class PersonBreakDown(person: String, globalCount: Int, textCount: List[PersonRow])
case class PersonRow(phrase: String, percTotalUsage: Double)

object AnalyzeMyChats extends App {
  val rawLines = Source.fromFile("raw.txt").getLines().toList
  val textFw = new FileWriter("text.txt")
  val personFw = new FileWriter("person.txt")
  println(s"Raw Line length:${rawLines.length}")

  /*
   * Sample line is as
   * 11/08/15, 8:53 PM - FirstName LastName: Message 
   */
  val regexLines = rawLines.map { line =>
    Try {
      val Array(date, other) = line.split(",", 2)
      val Array(time, otherText) = other.split("-", 2)
      val Array(personIfAny, actualText) = otherText.split(":", 2)
      val returnObj = WAMessage(personIfAny.trim, actualText.trim.replaceAll("[^a-zA-Z ]", "").toLowerCase.split("\\s+").mkString(" "), date.trim, time.trim)
      (returnObj)
    }.toOption
  }.flatten
  println(s"Regex-able lines length:${regexLines.length}")

  //----------------------------------------Text based analysis----------------------------------------//

  println("Doing text...........")
  //N-gram tokenize them
  val textNGrammedLines = for {
    chatData <- regexLines
    nGram <- NGramsGenerator.generate(chatData.text, 1, 9).map(_.mkString(" "))
  } yield chatData.copy(text = nGram)
  println(s"N-grammed lines length:${textNGrammedLines.length}")

  val textGroupedData = textNGrammedLines.groupBy(_.text).toList

  val countedData = textGroupedData map { case (text, chatData) => TextAggLine(text, chatData.length, chatData) }

  val textBreakDown = countedData map { countedLine =>
    val personList = countedLine.data
      .groupBy(_.person)
      .toList
      .map {
        case (person, personData) => TextRow(person, (personData.length / countedLine.count.toDouble) * 100)
      }
    val sortedPersonList = personList.sortBy(-_.percTotalUsage)
    TextBreakDown(countedLine.text, countedLine.count, sortedPersonList.take(10))
  }
  val sortedData = textBreakDown.sortBy(-_.globalCount)

  implicit val formats = DefaultFormats
  textFw.write(write(sortedData))
  //sortedData.foreach { line => textFw.write(s"${line.text},${line.globalCount},${line.personCount}\n\n") }

  //----------------------------------------Person based analysis----------------------------------------//
  println("Doing people...........")
  val personNGrammedLines = for {
    chatData <- regexLines
    nGram <- NGramsGenerator.generate(chatData.text, 2, 9).map(_.mkString(" "))
  } yield chatData.copy(text = nGram)
  println(s"N-grammed lines length:${textNGrammedLines.length}")

  val personGroupedData = personNGrammedLines.groupBy(_.person).toList

  val personCountedData = personGroupedData map { case (person, chatData) => PersonAggLine(person, chatData.length, chatData) }

  val personBreakDown = personCountedData map { countedLine =>
    val textList = countedLine.data
      .groupBy(_.text)
      .toList
      .map {
        case (text, textData) => PersonRow(text, (textData.length / countedLine.count.toDouble) * 100)
      }
    val sortedTextList = textList.sortBy(-_.percTotalUsage)
    PersonBreakDown(countedLine.person, countedLine.count, sortedTextList.take(20))
  }

  personFw.write(write(personBreakDown))

  //  personBreakDown.foreach { line => personFw.write(s"${line.person},${line.globalCount},${line.textCount}\n\n") }

  println("Done :)")
}