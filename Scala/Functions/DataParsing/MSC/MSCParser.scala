// scala -J-Xmx64g -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar /ischool/jb/roshan_anon/scripts/wmchad/parsing/ParseRaw.scala 2011-12 45
/*** Moorg.apache.bilityRW ***/

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import java.io.File
import java.util.Random

object MSCParser {

  // Parses raw MSC cdr data and saves it
  // as calls vs sms with only the desired fields
  def ParseMSCcdr(sc: SparkContext, cdrFile: String,
    year: String, month: String, day: String,
    outFileCall: String, outFileSms: String) {

    println("Parsing MSC CDR's for " + year + "-" + month + "-" + d)

    // read in file into RDD (Array of strings)
    val file = new File(cdrFile)
    if (!file.exists()) {
      println(cdrFile + " does not exist")
      return
    }

    val lines = sc.textFile(cdrFile).map(_.split(";"))
      .filter( entry => entry(0) == "0" || entry(0) == "1" || entry(0) == "6" || entry(0) == "7" )
      .map( entry => Array(entry(0), entry(1), entry(2), entry(3),
        entry(8), entry(9), entry(13), entry(14), entry(15))  ).cache

    lines.filter( entry => entry(0) == "0" || entry(0) == "1" )
      .map {
        case(Array(ct,srvNo,isdn,othNo,t1,t2,day,time,dur)) =>
         ct+";"+srvNo+";"+isdn+";"+othNo+";"+t1+";"+t2+";"+day+";"+time+";"+dur
      }.saveAsTextFile(outFileCall)

    // SMS does not have a duration, so don't include it
    lines.filter( entry => entry(0) == "6" || entry(0) == "7" )
      .map {
        case(Array(ct,srvNo,isdn,othNo,t1,t2,day,time,dur)) =>
          ct+";"+srvNo+";"+isdn+";"+othNo+";"+t1+";"+t2+";"+day+";"+time
      }.saveAsTextFile(outFileSms)
  }

  def main(args: Array[String]) {
    if (args.length < 2) {
      System.err.println("Usage: MSCParser <cdrFile> <year> <month> <day> <outCallFile> <outSmsFile>")
      //      System.exit(1)
    }
    
    System.setProperty("spark.local.dir","/ischool/jb/roshan_anon/tmp")
    val sc = new SparkContext("local[4]", "MSCParser")
    ParseMSCcdr(sc, args(0), args(1), args(2), args(3), args(4), args(5))
    println("Success")
    
    System.exit(0)
p  }
}

