package regressionModel

import collection.mutable.ListBuffer

/**
 * The main application of the program that user accesses through the GUI.
 */  
object RegressionModelApp {
  
 /**
  * A container of all graphs in a session. 
  * Each element of the buffer is a tuple which contains
  * 	1) the name of the graph,
  * 	2) the corresponding object of the graph,
  * 	3) the corresponding data points, and
  * 	4) the equation of the graph.
  */
  val models = ListBuffer[(String, (Graph, Data, String))]()

 /**
  * Helper object that gives GUI access to the parser and string manipulator methods.
  */
  val parser = Parser

 /**
  * Creates a new Graph object.
  */  
  def createGraph(regr: RegrModel) = new Graph(regr)

 /**
  * Creates a new SimpleModel object.
  */    
  def createSimpleModel(data: Data) = new SimpleModel(data)

 /**
  * Creates a new PolynomialModel object.
  */      
  def createPolynomialModel(data: Data, degree: Int) = new PolynomialModel(data, degree) 
  
 /**
  * Creates a new Data object.
  */      
  def createData(x: Array[Double], y: Array[Double]) = new Data(x, y)   

 /**
  * Ends a task for a given error or reason.
  */      
  def throwTaskEnded(s: String) = throw new TaskEnded(s)

}