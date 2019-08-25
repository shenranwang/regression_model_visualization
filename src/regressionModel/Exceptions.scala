package regressionModel

/**
 * A case class for exceptions while using GUI.
 */   
case class TaskEnded(description: String) extends java.lang.Exception(description)