package regressionModel

import scalafx.scene.chart._
import scalafx.collections.ObservableBuffer
import scala.collection.mutable.ListBuffer

/**
 * The Graph class calculates all the coordinates of the data set and the regression model. 
 */  
class Graph(model: RegrModel) {
  
  private val screenWidth = java.awt.Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt
  
 /**
  * Rounds the parameters.
  */
  private def rounded(d: Double, decimal: Int) = {
     new java.math.BigDecimal(d).setScale(decimal, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

 /**
  * Checks whether the parameter is negative or positive.
  */
  private def negOrPos(value: Double): String = {
    if (value >= 0) { 
      model match {
        case simple: SimpleModel => "+" + rounded(value, 10).toString() 
        case poly: PolynomialModel => "+" + rounded(value, poly.parameters().length + 8).toString() 
      }    
    } else { 
      model match {
        case simple: SimpleModel => "-" + rounded(value.abs, 10).toString() 
        case poly: PolynomialModel => "-" + rounded(value.abs, poly.parameters().length + 8).toString() 
      }
      
    }
  }
 
 /**
  * Produces exponent for equation.
  */ 
  private def exp(value: Int): String = if (value >= 2) { "x^" +  value } else if (value == 1) { "x" } else {""}
  
 /**
  * Returns the equation of the model as a string.
  */
  def equation(): String = {
    val params = model.parameters()
    val length = params.length
    var equation = "y="
    for (exponent <- 0 until length) {
      equation = equation + negOrPos(params(length - exponent - 1)) + exp(length - exponent - 1)
    }
    equation
  }

 /**
  * Calculates the coordinates with separation dx based on the values of the model's parameter vector. 
  * Essentially, the method calculates the points that belong to the regression model, separated by a distance of deltax.
  * The coordinates form the line/curve of the regression model.
  */  
  private def dataCoordinates(xMin: Double, xMax: Double, yMin: Double, yMax: Double): ListBuffer[(Double, Double)] = {
    val params = model.parameters()
    val regression = ListBuffer[(Double, Double)]()
    
    // Calculates the y coordinate with a given x coordinate based on the parameters of the regression model.
    def calcY(x: Double): Double = {
      var sum: Double = 0
      for (exp <- 0 until params.length) {
        sum += params(exp) * math.pow(x, exp)
      }
      sum
    }
    
    val (xData, yData) = (model.data.xValues, model.data.yValues)
    
    // The smallest difference is dx (xMax, xMin are often the graph bounds).
    val deltax = {
      if (yMin == yMax) {
        // If y values are the same for all data points.
        model match {
          case simple: SimpleModel => 
            xMax.ceil - xMin.floor / screenWidth
          case poly: PolynomialModel => 
            math.min(xMax.ceil - xMin.floor, xData.max.ceil - xData.min.floor) / screenWidth
        }       
      } else {
        model match {
          case simple: SimpleModel => 
            math.min(xMax.ceil - xMin.floor, yMax.ceil - yMin.floor) / screenWidth
          case poly: PolynomialModel => 
            math.min(math.min(xMax.ceil - xMin.floor,
                              xData.max.ceil - xData.min.floor),
                     math.min(yMax.ceil - yMin.floor,
                              yData.max.ceil - yData.min.floor)) / screenWidth
        }

      }
    }
    
    // Calculate all values from the smallest x value to the 
    // largest x value in the data, with x increasing by deltax after each iteration.
    var x = xMin.floor
    while (x <= xMax.ceil) {
      val y = calcY(x)
      regression += ((x, y))
      x += deltax
    }
    regression
  }

 /**
  * Changes both the data points and the regression model points into the right format for the GUI.
  */      
  def drawGraph(xMin: Double, xMax: Double, yMin: Double, yMax: Double) = {
    val regr = dataCoordinates(xMin, xMax, yMin, yMax)
    val (pdata, regressionModel) = (
      XYChart.Series[Number, Number]("Regression Model", ObservableBuffer(regr.map(pair => XYChart.Data[Number, Number](pair._1, pair._2)))),
      XYChart.Series[Number, Number]("Data Points", ObservableBuffer(model.data.dataPair.map(pair => XYChart.Data[Number, Number](pair._1, pair._2)):_*))
    )
    (pdata, regressionModel)
  }
}