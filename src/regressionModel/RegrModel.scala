package regressionModel

import org.ejml._
import org.ejml.simple.SimpleMatrix
import collection.mutable.ListBuffer

/**
 * The base of all regression models. Calculates the parameters of the regression model based on the given data set.
 */ 
abstract class RegrModel(val data: Data) {
  
 /**
  * Creates the design matrix.
  */
  def xMatrix(xData: Seq[Double]): SimpleMatrix
 
 /**
  * Creates the n-by-1 vector of observed values of the dependent variable, where n is the number of data points. 
  */  
  def yVector(yData: Seq[Double]): SimpleMatrix = {
    val yVector = new SimpleMatrix(yData.length, 1)
    for (j <- 0 until yVector.numRows()) {
      yVector.setRow(j, 0, yData(j))
    }
    yVector
  }
  
 /**
  * Calculates the parameters of any given type of regression model.
  */    
  def plotModel(): SimpleMatrix

 /**
  * Returns the parameters of a model in a buffer for other use.
  */  
  def parameters(): ListBuffer[Double] = {
    val paramVector = this.plotModel()
    val params = ListBuffer[Double]()
    for (i <- 0 until paramVector.numRows()) {
      params += paramVector.get(i)
    }
    params
  }
  
}
