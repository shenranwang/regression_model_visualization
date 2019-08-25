package regressionModel

import org.junit.Test
import org.junit.Assert._
import collection.mutable.ListBuffer

/**
 * Some simple unit tests.
 */
class Tests {
  
  val parser = Parser
  
  // Testing method loadData from Parser.  
  
  private def loadingData(data: Data, ans: Data) = {
    var boolean = true
    for (i <- 0 until data.xValues.length) {
      if (data.xValues(i) != ans.xValues(i) || data.yValues(i) != ans.yValues(i)) boolean = false
    }   
    boolean
  }
  
  @Test def testLoadDataCSV1() {
    val file = scala.io.Source.fromFile("./CSV_files/SimpleModel_1.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                       Array(4.8, 9.4, 14.6, 19.2, 24.3, 29.2, 34, 38.7, 43.8, 48.5))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }
  
  @Test def testLoadDataCSV2() {
    val file = scala.io.Source.fromFile("./CSV_files/SimpleModel_2.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(1.1, 2, 3.1, 3.9, 5, 5.9, 7, 8.1, 9, 10.1), 
                       Array(7.6, 14.9, 21.1, 28.5, 35.1, 42.9, 50.1, 58, 64.9, 72.5))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }

  @Test def testLoadDataCSV3() {
    val file = scala.io.Source.fromFile("./CSV_files/Parabola_test.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 10), 
                       Array(4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400, 1))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }
  
  @Test def testLoadDataCSV4() {
    val file = scala.io.Source.fromFile("./CSV_files/Linear.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                       Array(4.5, 9.2, 14.7, 20.3, 25.1, 31, 36.1, 43, 48.7, 53.343))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }

  @Test def testLoadDataXML1() {
    val file = scala.io.Source.fromFile("./XML_files/SimpleModel_1.xml")
    val data = parser.loadData(file)
    val ans = new Data(Array(1, 2, 3, 4, 5, 6), 
                       Array(4.5, 9.1, 14, 20.34, 23.434, 28.95))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }
  
  @Test def testLoadDataXML2() {
    val file = scala.io.Source.fromFile("./XML_files/Random_Model.xml")
    val data = parser.loadData(file)
    val ans = new Data(Array(2.6, -83.8, -4.9, 58.2, 34, -32.2), 
                       Array(8.4, 48.2, 123.9, 24.34, 122.4, -.2))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }
  
  @Test def testLoadDataNegatives() {
    val file = scala.io.Source.fromFile("./CSV_files/Negatives_test.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(-20, -17, -14, -11, -8, -5, -2, 1, 4, 7, 10, 13, 16, 19), 
                       Array(2, 4, 8, 15, 17, 23, 27, 30, 35, 36, 40, 42, 46, 49))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }  
  
  @Test def testLoadDataSpaces1() {
    val file = scala.io.Source.fromFile("./CSV_files/Spaces.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000), 
                       Array(-200, -164, -132, -95, -65, -32, 4, 35, 72, 104, 139))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  } 
  
  @Test def testLoadDataSpaces2() {
    val file = scala.io.Source.fromFile("./CSV_files/ForceZero.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(-200, -190, -180, -170, -160, -150), 
                       Array(350, 410, 410, 440, 480, 500))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }  

  @Test def testLoadDataSwitchXY() {
    val file = scala.io.Source.fromFile("./CSV_files/SwitchedXY.csv")
    val data = parser.loadData(file)
    val ans = new Data(Array(1, 2, 3, 4, 5, 6), 
                       Array(2, 3, 4, 5, 6, 7))    
    assertTrue("Should be\n" + ans.toString() + "\nLoaded data was\n" + data.toString() , loadingData(data, ans))
  }  

  // Testing methods parameters and plotModel from RegrModel. If parameters works, then plotModel also works.
  
  private def checkParams(model: Seq[Double], ans: Seq[Double], epsilon: Double) = {
    var boolean = true
    for (i <- 0 until ans.length) {
      if ((model(i) - ans(i)).abs > epsilon) boolean = false
    }
    boolean
  }
  
  @Test def testSimplePlotModel1() {
    val file = scala.io.Source.fromFile("./CSV_files/SimpleModel_1.csv")
    val data = parser.loadData(file)  
    val model = new SimpleModel(data)
    val params = model.parameters()
    val ans = ListBuffer(-0.14666666666, 
                          4.87212121212)
    assertTrue("Should be\n" + ans.toString() + "\nModel parameters were\n" + params.toString() , checkParams(params, ans, 1E-11))
  }  
  
  @Test def testSimplePlotModel2() {
    val file = scala.io.Source.fromFile("./CSV_files/SimpleModel_2.csv")
    val data = parser.loadData(file)  
    val model = new SimpleModel(data)
    val params = model.parameters()
    val ans = ListBuffer(-0.22567547972, 
                          7.20754990575)
    assertTrue("Should be\n" + ans.toString() + "\nModel parameters were\n" + params.toString() , checkParams(params, ans, 1E-11))
  }  
  
  @Test def testSimplePlotModel3() {
    val file = scala.io.Source.fromFile("./XML_files/onezero.xml")
    val data = parser.loadData(file)  
    val model = new SimpleModel(data)
    val params = model.parameters()
    val ans = ListBuffer(0.0, 
                         1.0)
    assertTrue("Should be\n" + ans.toString() + "\nModel parameters were\n" + params.toString() , checkParams(params, ans, 1E-11))
  }
  
  @Test def testPolyPlotModel1() {
    val file = scala.io.Source.fromFile("./XML_files/Random_Model.xml")
    val data = parser.loadData(file)  
    val (model1, model2) = (new PolynomialModel(data, 3), new PolynomialModel(data, 5))
    val (params1, params2) = (model1.parameters(), model2.parameters())
    val ans1 = ListBuffer(74.413536603215, 
                          2.088614437317,
                         -2.024798314674E-2,
                         -4.950912386714E-4)
    val ans2 = ListBuffer(4.7931896147E1,
                         -1.5562560916E1,
                          8.6832568402E-2,
                          1.9815610732E-2,
                         -8.9670337433E-5,
                         -3.4287418168E-6)
    assertTrue("Should be\n" + ans1.toString() + "\nModel parameters were\n" + params1.toString() , checkParams(params1, ans1, 1E-11))
    assertTrue("Should be\n" + ans2.toString() + "\nModel parameters were\n" + params2.toString() , checkParams(params2, ans2, 1E-6))
  }  

  @Test def testPolyPlotModel2() {
    val data = new Data(Array(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
                        Array(-5, -2, 1, 4, 3, 0, -3, -4, -1, 2, 5))
    val (model1, model2) = (new PolynomialModel(data, 1), new PolynomialModel(data, 8))
    val (params1, params2) = (model1.parameters(), model2.parameters())
    val ans1 = ListBuffer(0, 
                          1.727272727272E-1)
    val ans2 = ListBuffer(0,
                         -1.900112632466,
                          0,
                          7.308792106588E-2,
                          0,
                         -8.374183006540E-4,
                          0,
                          3.464927637724E-6,
                          0)
    assertTrue("Should be\n" + ans1.toString() + "\nModel parameters were\n" + params1.toString() , checkParams(params1, ans1, 1E-11))
    assertTrue("Should be\n" + ans2.toString() + "\nModel parameters were\n" + params2.toString() , checkParams(params2, ans2, 1E-8))
  }    
  
  @Test def testPolyPlotModel3() {
    val data = new Data(Array(4, 62, 97, 20, 63, 132, 59, 35, 122, 164, 13, 41, 199, 31), 
                        Array(185, 195, 91, 186, 133, 3, 51, 162, 43, 23, 51, 139, 99, 41))
    val (model1, model2) = (new PolynomialModel(data, 6), new PolynomialModel(data, 11))
    val (params1, params2) = (model1.parameters(), model2.parameters())
    val ans1 = ListBuffer(2.00654901986E2, 
                         -9.79257856026,
                          3.50631938614E-1,
                         -5.08694475821E-3,
                          3.30575389771E-5,
                         -9.71272680148E-8,
                          1.04977087921E-10)
    val ans2 = ListBuffer(6.8382244317E2,
                         -1.9917650545E2,
                          2.3036303257E1,
                         -1.2908038553,
                          4.1122289586E-2,
                         -8.0646827973E-4,
                          1.0119531622E-5,
                         -8.2106634996E-8,
                          4.2383346248E-10,
                         -1.3210153769E-12,
                          2.2029557114E-15,
                         -1.4258720825E-18)
    assertTrue("Should be\n" + ans1.toString() + "\nModel parameters were\n" + params1.toString() , checkParams(params1, ans1, 5E-2))
    assertTrue("Should be\n" + ans2.toString() + "\nModel parameters were\n" + params2.toString() , checkParams(params2, ans2, 1E4))
  }  
  
}