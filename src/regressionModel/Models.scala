package regressionModel

import org.ejml._
import org.ejml.simple.SimpleMatrix
import collection.mutable.ListBuffer

/**
 * The SimpleModel class calculates the parameters of a simple, linear regression model based on the give data set.
 */
class SimpleModel(data: Data) extends RegrModel(data) {

 /**
  * Creates the design matrix. The design matrix of a simple model is a n-by-2 matrix, where n is the number of data points.
  */
  def xMatrix(xData: Seq[Double]): SimpleMatrix = {
    val xMatrix = new SimpleMatrix(xData.length, 2)
    for (j <- 0 until xMatrix.numRows()) {
      xMatrix.setRow(j, 0, 1)
      xMatrix.setRow(j, 1, xData(j))
    }
    xMatrix
  }

 /**
  * Calculates the parameters of a simple model. The parameters of a simple model is a 2-by-1 vector.
  */    
  def plotModel(): SimpleMatrix = {
    val designMatrix = this.xMatrix(data.xValues)
    val depVarVector = this.yVector(data.yValues)
    // Produces the 2-by-1 parameter vector.
    designMatrix.pseudoInverse().mult(depVarVector)
  }
  
}

/**
 * The PolynomialModel class calculates the parameters of a polynomial regression model based on the give data set.
 * The equation is of the form a_0*x^n + a_1*x^(n-1) + ... + a_(n-1)*x + a_n, where n is the degree of the polynomial model.
 */
class PolynomialModel(data: Data, degree: Int) extends RegrModel(data) {

 /**
  * Creates the design matrix. The design matrix of a polynomial model is a n-by-m matrix,
  * where n is the number of data points and m depends on the degree of the polynomial model.
  */  
  def xMatrix(xData: Seq[Double]): SimpleMatrix = {
    val xMatrix = new SimpleMatrix(xData.length, degree + 1)
    for (j <- 0 until xMatrix.numRows()) {
      xMatrix.setColumn(0, j, 1)
      for (d <- 1 to degree) {
        xMatrix.setColumn(d, j, math.pow(xData(j), d))
      }    
    }
    xMatrix
  }

 /**
  * Calculates the parameters of a polynomial model. The parameters of a polynomial model is an m-by-1 vector,
  * where m depends on the degree of the polynomial model.
  */    
  def plotModel(): SimpleMatrix = {
    val designMatrix = this.xMatrix(data.xValues)
    val depVarVector = this.yVector(data.yValues)
    // Produces the m-by-1 parameter vector.
    designMatrix.pseudoInverse().mult(depVarVector)
  }
  
}
