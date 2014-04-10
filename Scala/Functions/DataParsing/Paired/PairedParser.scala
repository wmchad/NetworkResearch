// scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/wmchad/Code/Scala/Functions/DataParsing/Paired/PairedParser.scala ConvertFile  /ischool/jb/roshan_anon/parsed/2011-11/2011-11-Paired-Call.txt /ischool/jb/roshan_anon/CDR_chad/towerMap-2012-10.txt /ischool/jb/roshan_anon/CDR_chad/paired/2011-11-Paired 

/*** Moorg.apache.bilityRW ***/

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import java.io.File
import java.util.Random
import scala.io.Source

// Parses pre-paired data in various ways
// to pull out wanted data
object PairedParser {

  // Converts the given paired file to the format I would like to
  // work with. The input paired file should have the following fields, pipe-delimited:
  //    From Caller
  //    To Caller
  //    Date (YYMMDD)
  //    Time (HH:MM:SS)
  //    Duration in seconds
  //    From tower id
  //    From location access code (LAC)
  //    To tower id
  //    To location access code (LAC)
  // The output file will have the following fields, pipe-delimited:
  //    Year
  //    Month
  //    Day
  //    Hour
  //    Minute
  //    Second
  //    From tower id (combined)
  //    To tower id (combined)
  //    Duration in seconds
  // Here, the combined tower ids use a single identifier for all tower ids
  // in the same geolocation
  def ConvertPairedFile(sc: SparkContext, pairedFile: String,
    towerLocFile: String, outDir: String) {

    println("Converting paired data from file " + pairedFile)

    // check that the files exist
    var file = new File(pairedFile)
    if (!file.exists()) {
      println(pairedFile + " does not exist")
      return
    }
    file = new File(towerLocFile)
    if (!file.exists()) {
      println(towerLocFile + " does not exist")
      return
    }

    val kvPairs = Source.fromFile(towerLocFile)
      .getLines.map(_.split(","))
      .map(fields => fields(0) -> fields(1)).toList
    val towerMap = sc.broadcast(Map(kvPairs : _*))

    val lines = sc.textFile(pairedFile)
      .map(_.split("\\|"))
      .filter(_.size==9)
      .map ( entry =>
        (Array(entry(2).substring(0,2), entry(2).substring(2,4), entry(2).substring(4,6),
          entry(3).substring(0,2), entry(3).substring(3,5), entry(3).substring(6,8),
          towerMap.value.getOrElse(entry(5), entry(5)),
          towerMap.value.getOrElse(entry(7), entry(7)), entry(4)))
        .reduceLeft{(a,b) => a + "|" + b}
      )
      .saveAsTextFile(outDir)
  }


  // Gets network data for a specified time period
  // The paired file has the following fields (from ConvertPairedFile) - pipe-delimited:
  //    Year
  //    Month
  //    Day
  //    Hour
  //    Minute
  //    Second
  //    From tower id (combined)
  //    To tower id (combined)
  //    Duration in seconds
  // The output file will have the following fields - pipe-delimited:
  //    From tower id (combined)
  //    To tower id (combined)
  //    Number of calls
  def GetNetwork(sc: SparkContext, pairedFile: String,
    year: String, month: String, day: String,
    startHour: Integer, startMin: Integer, startSec: Integer,
    endHour: Integer, endMin: Integer, endSec: Integer,
    outDir: String) {

    println("Pulling network for " + year + "-" + month + "-" + day +
      " from " + startHour + ":" + startMin + ":" + startSec +
      " to " + endHour + ":" + endMin + ":" + endSec)

    // read in file into RDD (Array of strings)
    val file = new File(pairedFile)
    if (!file.exists()) {
      println(pairedFile + " does not exist")
      return
    }

    val lines = sc.textFile(pairedFile)
      .map(_.split("\\|"))
      .filter( x =>
        x(0) == year &&
        x(1) == month &&
        x(2) == day &&
        (x(3).toInt > startHour ||
          (x(3).toInt == startHour && (x(4).toInt > startMin ||
            (x(4).toInt == startMin && x(5).toInt >= startSec)))) &&
        (x(3).toInt < endHour ||
          (x(3).toInt == endHour && (x(4).toInt < endMin ||
            (x(4).toInt == endMin && x(5).toInt <= endSec))))
      )
      .map( x => ((x(6), x(7)), 1) )
      .reduceByKey(_+_)
      .map {
        case ((fromTower, toTower), count) => fromTower + "|" + toTower + "|" + count
      }
      .saveAsTextFile(outDir)
  }

  // Pulls out data for a single tower from a paired file
  // The paired file has the following fields (from ConvertPairedFile) - pipe-delimited:
  //    Year
  //    Month
  //    Day
  //    Hour
  //    Minute
  //    Second
  //    From tower id (combined)
  //    To tower id (combined)
  //    Duration in seconds
  // The output files will have the same format, one for incoming calls
  // and one for outgoing calls
  def GetTowerCalls(sc: SparkContext, pairedFile: String,
    towerId: String, inCallDir: String, outCallDir: String) {

    println("Getting tower calls for tower " + towerId)

    // read in file into RDD (Array of strings)
    val file = new File(pairedFile)
    if (!file.exists()) {
      println(pairedFile + " does not exist")
      return
    }

    val lines = sc.textFile(pairedFile)
    lines.filter( x => x.split("\\|")(6) == towerId )
      .saveAsTextFile(outCallDir)
    lines.filter( x => x.split("\\|")(7) == towerId )
      .saveAsTextFile(inCallDir)
  }

  def main(args: Array[String]) {
    if (args.length < 1) {
      PrintUsage()
      System.exit(1)
    }

    System.setProperty("spark.local.dir","/ischool/jb/roshan_anon/tmp")
    val sc = new SparkContext("local[4]", "PairedParser")
    
    args(0) match {
      case "ConvertFile" =>
        if ( args.length < 4 ) {
          PrintUsage()
          System.exit(1)
        }
        ConvertPairedFile(sc, args(1), args(2), args(3))
      case "GetNetwork" =>
        if ( args.length < 12 ) {
          PrintUsage()
          System.exit(1)
        }
        GetNetwork(sc, args(1), args(2), args(3), args(4),
          args(5).toInt, args(6).toInt, args(7).toInt, args(8).toInt,
          args(9).toInt, args(10).toInt, args(11))
      case "GetTowerCalls" =>
        if ( args.length < 5 ) {
          PrintUsage()
          System.exit(1)
        }
        GetTowerCalls(sc, args(1), args(2), args(3), args(4))
      case _ =>
        PrintUsage()
        System.exit(1)
    }
    println("Success")

    System.exit(0)
  }

  def PrintUsage() {
    System.err.println("Usage: PairedParser ConvertFile <pairedFile> <towerMapFile> <outDirectory>")
    System.err.println("--OR--")
    System.err.println("       PairedParser GetNetwork <pairedFile> <year (YY)> <month (MM)> <day (DD)>")
    System.err.println("                    <startHour (HH)> <startMinute (mm)> <startsecond (ss)>")
    System.err.println("                    <endHour (HH)> <endMinute (mm)> <endsecond (ss)>")
    System.err.println("                    <outDirectory>")
    System.err.println("--OR--")
    System.err.println("       PairedParser GetTowerCalls <pairedFile> <towerId>")
    System.err.println("                    <inCallDirectory> <outCallDirectory>")
  }

}

