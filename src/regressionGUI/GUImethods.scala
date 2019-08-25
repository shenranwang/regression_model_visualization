package regressionGUI

import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Scene, Node}
import scalafx.scene.chart._
import scalafx.scene.chart.XYChart._
import scalafx.scene.control._
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.Includes._
import scalafx.event._
import scalafx.collections.ObservableBuffer
import scalafx.scene.layout.GridPane
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.geometry.Insets
import scala.io.Source
import scala.collection.mutable.ListBuffer
import regressionModel.RegressionModelApp

/**
 * A helper class that contains useful methods for the GUI.
 */  
class GUImethods(stage: PrimaryStage) {
  
/**
 * Main application.
 */
  val app = RegressionModelApp
   
 /**
  * A pop-up window that allows the user to choose the name for the new model.
  * The given name must not already exist in the active session, and the name cannot be empty.
  */  
  def chooseGraphName(strings: Seq[String]): String = {
    val dialog = textInput("A Graph", 
                           "Graph Name",
                           "Please choose a name for the graph.\nFor example: \"y as a function of x.\"", 
                           "Name:")
    val result = dialog.showAndWait()
    result match {
      case Some(name) => 
        if (strings.contains(name.trim()) || name.length == 0) {
          alert("Name Error", "Please choose a non-existing/non-empty name for the new graph.") // If the name already exists in the graph list or the name length is zero,
          chooseGraphName(strings)                                                              // alert the user and call the method again recursively.
        } else {
          name.trim()
        }
      case None =>
        app.throwTaskEnded("Task ended prematurely: No name given.")
    }
  }
  
 /**
  * A pop-up window that allows the user to choose degree of the polynomial regression model.
  * The degree must be between 1 and 100; the range from 1 to 10 produces better results. 
  * Typing in anything other than integers into the text field will produce an error.
  */  
  def chooseDegree(): String = {
    val dialog = textInput("2", 
                           "Degree of Polynomial Model", 
                           "Please choose the degree of the polynomial model.\nPlease choose an integer between 1-10. The highest possible value is 100." +
                           "\nHowever, higher degrees produce increasingly inaccurate regression models and are inefficient." +
                           "\nNote: It is strongly recommeneded to choose a degree between 1 and the number of data pairs in the set." +
                           "\nUsing degrees higher than the number of data pairs can yield interesting results.", 
                           "Degree:")
    val result = dialog.showAndWait()
    result match {
      case Some(degree) => 
        if (degree == "0") {
          alert("Value Error", "Please insert a non-zero integer.") // If the value is zero,
          chooseDegree()                                            // alert the user and call the method again recursively.
        } else if (!app.parser.checkPosInt(degree)) {
          alert("Value Error", "Please insert a positive integer.") // If the value is not a positive integer,
          chooseDegree()                                            // alert the user and call the method again recursively.
        } else if (degree == "100" || degree.length < 3) {
          degree
        } else {
          alert("Value Error", "Value out of bounds, please choose another integer (max 100).") // If the value is not in the range from 1 to 100,
          chooseDegree()                                                                        // alert the user and call the method again recursively.
        }
      case None => 
        app.throwTaskEnded("Task ended prematurely: No value given.")
    }
  }  

 /**
  * A pop-up window that allows the user to choose the number of data points to be added.
  * The number must be between 2 and 100. 
  * Typing in anything other than integers into the text field will produce an error.
  */  
  def chooseDataAdded(): String = {
    val number = textInput("2", 
                           "Data Points", 
                           "Choose the number of data points to be added (between 2-20).", 
                           "Number:")
    val result = number.showAndWait()
    result match {
      case Some(value) => 
        if (value == "0" || value == "1") {
          alert("Value Error", "Value out of bounds, please choose another integer (2-20).") // If the value is zero or one,
          chooseDataAdded()                                                                  // alert the user and call the method again recursively.
        } else if (!app.parser.checkPosInt(value)) {
          alert("Value Error", "Please insert a positive integer.") // If the value is not a positive integer,
          chooseDataAdded()                                         // alert the user and call the method again recursively.
        } else if (value.length < 3 && value.toInt <= 20) {
          value
        } else {
          alert("Value Error", "Value out of bounds, please choose another integer (2-20).") // If the value is not in the range from 1 to 100,
          chooseDataAdded()                                                                  // alert the user and call the method again recursively.
        }
      case None => 
        app.throwTaskEnded("Task ended prematurely: No value given.")
    }
  }    
  
 /**
  * A pop-up window that allows the user to choose the data to be fitted with a model.
  */
  private def fileChooser() = {
    val fileChooser = new FileChooser {
      this.title = "Open Resource File"
      this.extensionFilters += new ExtensionFilter("Text Files (*.csv, *.xml)", Seq("*.csv", "*.xml")) // Only allows files with extensions .csv and .xml to be loaded.
    }
    val selectedFile = fileChooser.showOpenDialog(stage)
    selectedFile
  }
   
 /**
  * A pop-up window that asks the user to select the model type that is to be fitted with the data.
  */
  def loadDataProcess() = {
    val file = fileChooser()                                                    // Choose the file.
    val data = app.parser.loadData(Source.fromFile(file))                       // Parse through the file and create a Data object.
    val name = chooseGraphName(app.models.unzip._1)                             // Name the graph.
    val model = chooseModel(data)
    (name, data, model)
  }
  
 /**
  * A pop-up window that asks the user to select the model type that is to be fitted with the data.
  */
  def chooseModel(data: regressionModel.Data) = {
    val choices = Seq("Simple Regression Model", "Polynomial Regression Model") // Choose the model.
    val dialog = new ChoiceDialog(defaultChoice = "Simple Regression Model", choices = choices) {
      this.title = "Regression Model"
      this.headerText = "Choose the regression model that you want to fit to your data."
      this.contentText = "Model:"
    }

    val result = dialog.showAndWait()

    result match {
      case Some("Simple Regression Model") =>     // Creates a simple regression model.
        app.createSimpleModel(data) 
      case Some("Polynomial Regression Model") => // Creates a polynomial regression model.
        val degree = chooseDegree() 
        app.createPolynomialModel(data, degree.toInt)
      case _ => 
        app.throwTaskEnded("Task ended prematurely: No model chosen.")
    } 
  }
  
 /**
  * Adds n number of data points to existing data.
  */  
  def newData() = {
    val number = chooseDataAdded().toInt
    val create = createData(number)
    val result = create.showAndWait()
    
    result match {
      case Some(DataPoints(array)) => 
        val filtered = array.filter(x => !x._1.isEmpty() && !x._2.isEmpty()).map(x => (x._1.replace(',', '.'), x._2.replace(',', '.')))
        if (!filtered.forall(x => app.parser.checkDouble(x._1) && app.parser.checkDouble(x._2))) {
          alert("Value Error", "All data points are not real numbers.")
          app.throwTaskEnded("All data points are not real numbers.")
        } else {
          app.createData(filtered.map(_._1.toDouble), filtered.map(_._2.toDouble))
        }
      case _ => 
        app.throwTaskEnded("Unknown Error.")
    }
  }
  
 /**
  * A pop-up that allows the user to input new bounds for the axes.
  * It is not possible to press OK unless all values in the text fields are real numbers.
  * An error will occur if minimums are greater than maximums.
  */
  def changeAxis(xMin: String, xMax: String, yMin: String, yMax: String): Array[Double] = {
    val dialog = axisChange(xMin, xMax, yMin, yMax)
    val result = dialog.showAndWait()

    result match {
      case Some(Axes(x1, x2, y1, y2)) => 
        val axes = Array(x1, x2, y1, y2)
        if (!axes.forall(app.parser.checkDouble(_))) {
          alert("Value Error", "Please insert valid inputs to the fields.") // If values are not real numbers,
          changeAxis(x1, x2, y1, y2)                                        // alert the user and call the method again recursively.
        } else if (x1.toDouble >= x2.toDouble || y1.toDouble >= y2.toDouble) {
          alert("Range Error", "Please insert values where minimums are smaller than maximum.") // If minimums are greater than maximums,
          changeAxis(x1, x2, y1, y2)                                                            // alert the user and call the method again recursively.
        } else if (x1.toDouble.isInfinity || x2.toDouble.isInfinity || y1.toDouble.isInfinity || y2.toDouble.isInfinity) {
          alert("Range Error", "Infinite value inputted.") // If one of the values is rounded to infinity,
          changeAxis(x1, x2, y1, y2)                                                            // alert the user and call the method again recursively.          
        } else {
          axes.map(_.toDouble)
        }
      case _ => 
        app.throwTaskEnded("Unknown Error.")
    }
  }
  
 /**
  * A helper method for creating text input dialogs.
  */     
  private def textInput(default: String, titleT: String, header: String, content: String): TextInputDialog = {
    new TextInputDialog(default) {
      this.title = titleT
      this.headerText = header
      this.contentText = content
    }
  }
  
 /**
  * A case class of new axes bounds for active graph.
  */   
  case class Axes(xMin: String, xMax: String, yMin: String, yMax: String)

 /**
  * Custom dialog for changing axis values.
  */  
  private def axisChange(currentXMin: String, currentXMax: String, currentYMin: String, currentYMax: String) = {
    val dialog = new Dialog[Axes]() {
      this.title = "Change Axes"
      this.headerText = "Please choose new values for the axes."
    }

    // Set the button types.
    val okButton = new ButtonType("OK", ButtonData.OKDone)
    dialog.dialogPane().buttonTypes = Seq(okButton, ButtonType.Cancel)

    // Create the axes labels and fields.
    val xMin = new TextField() { this.text = currentXMin }
    val yMin = new TextField() { this.text = currentYMin }
    val xMax = new TextField() { this.text = currentXMax }
    val yMax = new TextField() { this.text = currentYMax }

    val grid = new GridPane() {
      this.hgap = 10
      this.vgap = 10
      this.padding = Insets(20, 100, 10, 10)

      this.add(new Label("min:"), 1, 0)
      this.add(new Label("max:"), 2, 0)
      this.add(new Label("x-axis:"), 0, 1)
      this.add(xMin, 1, 1)
      this.add(xMax, 2, 1)
      this.add(new Label("y-axis:"), 0, 2)
      this.add(yMin, 1, 2)
      this.add(yMax, 2, 2)
    }

    // Enable/Disable OK button depending on whether all axes minimums and maximums have values.
    val enableButton = dialog.dialogPane().lookupButton(okButton)
    enableButton.disable = xMin.text().trim().isEmpty || xMax.text().trim().isEmpty() || yMin.text().trim().isEmpty() || yMax.text().trim().isEmpty()

    // Do some validation (disable when some field is empty).
    xMin.text.onChange { (_, _, newValue) => enableButton.disable = newValue.trim().isEmpty || xMax.text().trim().isEmpty() || yMin.text().trim().isEmpty() || yMax.text().trim().isEmpty() }
    xMax.text.onChange { (_, _, newValue) => enableButton.disable = newValue.trim().isEmpty || xMin.text().trim().isEmpty() || yMin.text().trim().isEmpty() || yMax.text().trim().isEmpty() }
    yMin.text.onChange { (_, _, newValue) => enableButton.disable = newValue.trim().isEmpty || xMin.text().trim().isEmpty() || xMax.text().trim().isEmpty() || yMax.text().trim().isEmpty() }
    yMax.text.onChange { (_, _, newValue) => enableButton.disable = newValue.trim().isEmpty || xMin.text().trim().isEmpty() || xMax.text().trim().isEmpty() || yMin.text().trim().isEmpty() }

    dialog.dialogPane().content = grid

    // When the ok button is clicked, convert the result to axis values.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == okButton) {
        Axes(xMin.text(), xMax.text(), yMin.text(), yMax.text())
      } else {
        null
      }
    dialog
  }  

 /**
  * A case class of new axes bounds for active graph.
  */   
  case class DataPoints(pairs: Array[(String, String)])  
  
 /**
  * Custom dialog for creating new data.
  */  
  private def createData(n: Int) = {
    val dialog = new Dialog[DataPoints]() {
      this.title = "Create Data"
      this.headerText = "Please enter the data points."
    }
    
    // Set the button types.
    val okButton = new ButtonType("OK", ButtonData.OKDone)
    dialog.dialogPane().buttonTypes = Seq(okButton, ButtonType.Cancel)

    // Create the data labels and fields.
    val fields = Array.fill(n, 2)(new TextField)
    val grid = new GridPane() {
      this.hgap = 10
      this.vgap = 10
      this.padding = Insets(20, 100, 10, 10)
      
      this.add(new Label("x:"), 0, 0)
      this.add(new Label("y:"), 1, 0)
      for (i <- 0 until n) {
        this.addRow(i + 1, fields(i)(0), fields(i)(1)) 
      }
    }

    // Disable OK button to begin with.
    val enableButton = dialog.dialogPane().lookupButton(okButton)
    enableButton.disable = true

    // Do some validation (disable when at most one data point is entirely filled or a data point is not entirely filled).
    fields.map(_.foreach(_.text.onChange { (_, _, _) => enableButton.disable = 
                                                        fields.map(_.forall(!_.text.get.isEmpty())).count(_ == true) < 2 ||
                                                        fields.map(_.map(!_.text.get.isEmpty())).exists(_.count(_ == true) == 1)}))

    dialog.dialogPane().content = grid

    // When the ok button is clicked, convert the result to DataPoints.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == okButton) {
        val result = fields.map(x => (x(0).text(), x(1).text()))
        DataPoints(result)
      } else {
        null
      }
    dialog
  }  

 /**
  * An alert pop-up in case of errors.
  */  
  def alert(header: String, content: String) = {
    new Alert(Alert.AlertType.Error) {
      this.initOwner(stage)
      this.title = "Error"
      this.headerText = header
      this.contentText = content
    }.showAndWait()
  }
  
 /**
  * A confirmation alert.
  */  
  def confirmAlert(header: String, content: String) = {
    new Alert(Alert.AlertType.Confirmation) {
      this.initOwner(stage)
      this.title = "Confirm remove graph"
      this.headerText = header
      this.contentText = content
    }.showAndWait()
  }
  
}