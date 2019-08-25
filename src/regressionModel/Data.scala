package regressionModel

/**
 * The Data class is a data set. 
 */  
class Data(val xValues: Array[Double], val yValues: Array[Double]) {

 /** 
	* A ListBuffer of corresponding x and y values zipped together.  
  */     
  val dataPair = (xValues zip yValues).distinct

 /** 
	* A pair of arrays that contain the x and y values of the data as strings.
  */       
  val listed = ((str("x, ", xValues).trim.split(",").take(100)), (str("y, ", yValues).trim.split(",").take(100)))

 /** 
	* A string with prefix (x or y) and related values. Example output: "x, 1.2, 3.3, 5.4". 
  */         
  def str(prefix: String, list: Seq[Double]) = prefix + list.mkString(", ")

 /** 
	* Used for debugging.
  */       
  override def toString() = {
    str("x: ", xValues) + "\n" + str("y: ", yValues)
  }
  
}