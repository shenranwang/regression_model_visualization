package regressionModel

import java.io.IOException
import java.io.Reader
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.StringReader
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scalafx.scene.control.Alert

/**
 * A parser and string manipulator object.
 */ 
object Parser {
  
 /**
  * Reads CSV and XML files and returns a Data object based on the given data set.
  * The first line must be x;y or y;x. The value separator must be ';', and any commas
  * will be automatically converted to periods. The data set must contain at least two
  * data points in order to create a regression model.
  */
  def loadData(src: Source): Data = {
    // The x and y values to be added to the Data object.  
    val xData = ListBuffer[Double]()
    val yData = ListBuffer[Double]()
    // All data points.
    val lines = src.getLines().toArray.filter(_ != ";")
    var formatted = lines.map(_.replace(',', '.').split(';'))
    
    // Check whether file is *.xml.
    if (formatted.head(0).contains("<?xml version=") && formatted.head(0).contains("?>")) {
      formatted = xml(formatted)
    }
    
    // Check whether there are some empty data points.
    if (formatted.exists(x => x.length != 2)) {
      throwError("The file countains invalid data; not all data points are pairs,\neg. x does not have a corresponding y value.")
    }
    
    // If all data points are not real numbers, throw error.
    if (!formatted.tail.forall(_.forall(checkDouble(_)))) {
      throwError("The file countains invalid data;\nsome data points are not real numbers.")
    }
    
    val values = formatted.tail.map(_.map(_.trim().toDouble)) // all values
    val head = formatted.head.map(_.trim().toLowerCase())     // Array("x", "y") or Array("y", "x")
    
    // Placing x and y values into xData and yData respectively.
    if (head(0) == "x" && head(1) == "y") {
      for (i <- 0 until values.length) { 
        xData += values(i)(0) 
        yData += values(i)(1)
      }
    } else if (head(0) == "y" && head(1) == "x") {
      for (i <- 0 until values.length) { 
        yData += values(i)(0) 
        xData += values(i)(1)
      }
    } else {
      throwError("The file contains invalid data; perhaps\n1) x and y are missing from the beginning of the file, or\n2) the values aren't separated by a semicolon?")
    }
    src.close()
    if (xData.length < 2) {
      throwError("The file has less that two data points; impossible to plot a regression model with it.")
    }
    val data = new Data(xData.toArray, yData.toArray)
    if (data.dataPair.distinct.length < 2) {
      throwError("The file has more than two data points; however, they all have the same coordinates.")
    } else if (data.xValues.distinct.length < 2) {
      throwError("The file has more than two data points; however, all data points belong to the same x value.")
    }
    data
  }
  
  private def xml(formatted: Array[Array[String]]): Array[Array[String]] = {
    var data = formatted.tail.flatten.map(_.trim())
    var dataPoints = Array(Array("x", "y"))
    if (data(0) != "<DataInfo>") {
      throwError("The XML file does not contain <DataInfo>.")
    } else {
      data = data.tail
    }
    
    def xory(s1: String, s2: String): Array[String] = {
      if (s1.contains("<x>") && s1.contains("</x>") && s2.contains("<y>") && s2.contains("</y>")) {
        Array(s1.dropWhile(_ != '>').tail.takeWhile(_ != '<'), s2.dropWhile(_ != '>').tail.takeWhile(_ != '<'))
      } else if (s1.contains("<y>") && s1.contains("</y>") && s2.contains("<x>") && s2.contains("</x>")) {
        Array(s2.dropWhile(_ != '>').tail.takeWhile(_ != '<'), s1.dropWhile(_ != '>').tail.takeWhile(_ != '<'))
      } else {
        throwError("The XML file does not contain x or y.")
      }
    }
    
    while (data(0) != "</DataInfo>") {
      val (current, remainder) = data.splitAt(4)
      if (current(0) == "<Data>" && current(3) == "</Data>") {
        val pair = xory(current(1), current(2))
        dataPoints = dataPoints :+ pair
        data = remainder
      } else {
       throwError("The XML file does not contain Data.")      
      }
    }
    dataPoints
  }

 /**
  * Checks whether the string can be transformed into a positive integer.
  */
  def checkPosInt(s: String): Boolean = {
    if (!s.forall(x => x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9')) {
      false
    } else {
      true
    }
  }

 /**
  * Checks whether the string can be transformed into a real number.
  */  
  def checkDouble(s: String): Boolean = {
    val string = s.replace(',', '.')
    if (!string.forall(x => x == '0' || x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9' || x == '.' || x == '-')) {
      false
    } else {
      val periods = string.count(_ == '.')
      val minuses = string.count(_ == '-')
      if (periods > 1 || minuses > 1) false else true
    }
  }

 /**
  * Throw an alert in case of error while reading a file.
  */   
  def throwError(s: String) = {
    val error = new TaskEnded(s)
      new Alert(Alert.AlertType.Error) {
        this.title = "Error"
        this.headerText = "File Error"
        this.contentText = error.description
      }.showAndWait()   
    throw error  
  }
  
}