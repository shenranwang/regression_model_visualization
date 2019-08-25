package regressionGUI

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Node
import scalafx.scene.Scene
import scalafx.scene.chart._
import scalafx.scene.control._
import scalafx.scene.paint.Color._
import scalafx.Includes._
import scalafx.event._
import scalafx.collections.ObservableBuffer
import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.awt.{Dimension, Toolkit}
import regressionModel.RegressionModelApp

/**
 * The GUI object that gives access to the RegressionModelApp.
 */  
object RegressionModelGUI extends JFXApp {
  
  // Monitor dimensions.
  private val kit = Toolkit.getDefaultToolkit.getScreenSize
  private val dim = (kit.getWidth.toInt, kit.getHeight.toInt)
  
  // GUImethods object, contains main application and methods used in the GUI.
  val helper = new GUImethods(this.stage)
  
  // Main screen
  this.stage = new PrimaryStage {

    this.title = "Regression Model Visualization"
    
    this.scene = new Scene(dim._1, dim._2) {
      
      // Color of the GUI.
      this.fill = LightGray
      
      // Stylesheet for the chart.
      this.stylesheets.add("ChartStyleSheet.css")
      
      // BUTTONS
      
     /**
			* Load Button. When clicked, the user can:
			* 	1) choose the file with data (*.csv or *.xml), 
			* 	2) name the graph and
			* 	3) select the model type to be fitted on the data.
			* If all phases do not produce errors, then:
			* 	1) the graph will be updated with the chosen data and the fitted regression model,
			* 	2) the equation of the model will be updated,
			* 	3) the data points of the data (up to a certain number) will be listed and
			* 	4) the graph will be added to the graph list.
		  */  
      val loadD = new Button("Load Data") {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 13.5
        this.layoutX    = 0
        this.layoutY    = 0
        this.onAction   = (event:ActionEvent) => {
          val (name, data, model) = helper.loadDataProcess()
          val newGraph = helper.app.createGraph(model)        
          val graphPoints = newGraph.drawGraph(data.xValues.min, data.xValues.max, data.yValues.min, data.yValues.max)
          val eq = newGraph.equation()
          helper.app.models += (name -> (newGraph, data, eq))
          
          // Update elements of GUI
          graph.data = ObservableBuffer(graphPoints._1, graphPoints._2)
          graphList += name; graphList.selectionModel.get().select(name)
          equation.text = eq
          dataX.items = ObservableBuffer(data.listed._1).flatten
          dataY.items = ObservableBuffer(data.listed._2).flatten
          xAxis.autoRanging = true; yAxis.autoRanging = true
        }
      }

     /**
			* New Data Button. When clicked, the user can:
			* 	1) enter data points, 
			* 	2) name the graph and
			* 	3) select the model type to be fitted on the data.
			* If all phases do not produce errors, then:
			* 	1) the graph will be updated with the chosen data and the fitted regression model,
			* 	2) the equation of the model will be updated,
			* 	3) the data points of the data (up to a certain number) will be listed and
			* 	4) the graph will be added to the graph list.
		  */        
      val newD = new Button("New Data") {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 13.5
        this.layoutX    = dim._1 / 8.0
        this.layoutY    = 0
        this.onAction   = (event:ActionEvent) => {
          val data = helper.newData()                                // Enter data.
          
          if (data.dataPair.distinct.length < 2) {
            helper.app.parser.throwError("The file has more than two data points; however, they all have the same coordinates.")
          } else if (data.xValues.distinct.length < 2) {
            helper.app.parser.throwError("The file has more than two data points; however, all data points belong to the same x value.")
          }
          
          val name = helper.chooseGraphName(helper.app.models.unzip._1) // Name the graph.
          val model = helper.chooseModel(data)
          
          val newGraph = helper.app.createGraph(model)        
          val graphPoints = newGraph.drawGraph(data.xValues.min, data.xValues.max, data.yValues.min, data.yValues.max)
          val eq = newGraph.equation()
          helper.app.models += (name -> (newGraph, data, eq))
          
          // Update elements of GUI
          graph.data = ObservableBuffer(graphPoints._1, graphPoints._2)
          graphList += name; graphList.selectionModel.get().select(name)
          equation.text = eq
          dataX.items = ObservableBuffer(data.listed._1).flatten
          dataY.items = ObservableBuffer(data.listed._2).flatten
          xAxis.autoRanging = true; yAxis.autoRanging = true
        }
      }
      
     /**
			* Removes the active graph.
		  */            
      val removeGraph = new Button("Remove Graph") {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 13.5
        this.layoutX    = 0
        this.layoutY    = dim._2 / 27.0 * 23.0
        this.onAction   = (event:ActionEvent) => {
          if (helper.app.models.isEmpty) {
            helper.alert("Graph Error", "No active graph.")
            helper.app.throwTaskEnded("No active graph.")
          }
          val result = helper.confirmAlert("Remove currently active graph.", "Are you sure you want to remove the active graph?")
          
          result match {
            case Some(ButtonType.OK) => 
              val name = graphList.selectionModel.get().getSelectedItem
              val delete = helper.app.models.toMap.apply(name)
              helper.app.models -= ((name, delete))
              graphList -= name
            case _ => 
          }
        }
      } 

     /**
			* Resizes graph. When clicked, the user can adjust the minimum and maximum bounds of the current graph's axes.
			* If there are no errors, then both the x- and y-axis bounds will be updated, and the regression model will be re-plotted.
		  */      
      val resizeGraph = new Button("Resize Graph") {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 13.5
        this.layoutX    = dim._1 / 8.0
        this.layoutY    = dim._2 / 27.0 * 23.0
        this.onAction   = (event:ActionEvent) => {
          if (helper.app.models.isEmpty) {
            helper.alert("Graph Error", "No active graph.")
            helper.app.throwTaskEnded("No active graph.")
          }
          val axisValues = helper.changeAxis(xAxis.getLowerBound().toString(), xAxis.getUpperBound().toString(), 
                                             yAxis.getLowerBound().toString(), yAxis.getUpperBound().toString())
          xAxis.autoRanging = false; yAxis.autoRanging = false
          xAxis.setLowerBound(axisValues(0)); xAxis.setUpperBound(axisValues(1))
          yAxis.setLowerBound(axisValues(2)); yAxis.setUpperBound(axisValues(3))
          val update = graphList.selectionModel.get().getSelectedItem
          val info = helper.app.models.toMap.apply(update)
          val graphData = info._1.drawGraph(xAxis.lowerBound.get(), xAxis.upperBound.get(), yAxis.lowerBound.get(), yAxis.upperBound.get())
        
          // Update elements of GUI
          graph.data = ObservableBuffer(graphData._1, graphData._2)
        }
      }

     /**
			* List of the data points that the regression model is calculated with.
		  */            
      val (dataX, dataY) = (new ListView(Seq("x", "Data will come here...")) {        
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 27.0 * 20.0
        this.layoutX = 0
        this.layoutY = dim._2 / 18.0 * 2.0
        this.fixedCellSize = dim._2 / 27.0
      },
      new ListView(Seq("y", "Data will come here...")) {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 27.0 * 20.0
        this.layoutX = dim._1 / 8.0
        this.layoutY = dim._2 / 18.0 * 2.0
        this.fixedCellSize = dim._2 / 27.0
      }
      )

     /**
			* A text field that shows the the equation of the current graph.
		  */   
      val equation = new TextField {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 27.0        
        this.layoutX    = 0
        this.layoutY    = dim._2 / 27.0 * 2.0
        this.text = "The equation of the model..."
        this.editable = false
      }

     /** 
			* A list of graphs.
		  */    
      val graphList = new ComboBox(helper.app.models.unzip._1) {
        this.prefWidth  = dim._1 / 8.0
        this.prefHeight = dim._2 / 27.0      
        this.layoutX = dim._1 / 8.0
        this.layoutY = dim._2 / 27.0 * 2.0
        this.visibleRowCount = 25
        this.promptText = "A list of active graphs..."
        this.onAction = (event: ActionEvent) => {
          val update = this.selectionModel.get().getSelectedItem
          val info = helper.app.models.toMap.apply(update)
          val graphData = info._1.drawGraph(info._2.xValues.min, info._2.xValues.max, info._2.yValues.min, info._2.yValues.max)
        
          // Update elements of GUI
          graph.data = ObservableBuffer(graphData._1, graphData._2)
          equation.text = info._3
          dataX.items = ObservableBuffer(info._2.listed._1).flatten
          dataY.items = ObservableBuffer(info._2.listed._2).flatten
          xAxis.autoRanging = true; yAxis.autoRanging = true
        }
      }
      
     /** 
			* The chart axes.
		  */         
      val (xAxis, yAxis) = (NumberAxis(), NumberAxis())
      xAxis.forceZeroInRange = false
      yAxis.forceZeroInRange = false
      
     /** 
			* The chart where regression models are plotted.  
		  */   
      val graph = new ScatterChart(xAxis, yAxis) {
        this.prefWidth  = dim._1 / 48.0 * 35.0
        this.prefHeight = dim._2 / 108.0 * 101.0
        this.layoutX = dim._1 / 4.0
        this.layoutY = 0
      }

      // All elements on the GUI.
      content = List(loadD, newD, removeGraph, resizeGraph, equation, graphList, dataX, dataY, graph)
    }
  }

}