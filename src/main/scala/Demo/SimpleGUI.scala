package Demo

import Screen.Chip8Screen

import scala.swing.BorderPanel.Position.{Center, East, West}
import scala.swing.event.ButtonClicked
import scala.swing.{Action, BorderPanel, Button, Dimension, MainFrame, Menu, MenuBar, MenuItem, SimpleSwingApplication}

object SimpleGUI extends SimpleSwingApplication {

  def top = new MainFrame {

    title = "A Sample Scala Swing GUI"

    val button = new Button {
      text = "I am a button!"
    }
    val canvas = new Chip8Screen {
      preferredSize = new Dimension(100, 100)
    }

    contents = new BorderPanel {
      layout(button) = West
      layout(canvas) = Center
    }

    size = new Dimension(300, 200)

    menuBar = generateMenuBar()

    listenTo(button)
    reactions += {
      case ButtonClicked(component) if component == button =>
        button.text = "I got clicked!"
    }


  }

  def generateMenuBar(): MenuBar = {
    new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
  }

}
