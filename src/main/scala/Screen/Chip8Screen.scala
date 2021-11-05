package Screen

import java.awt.Color
import scala.swing.{Graphics2D, Panel}

class Chip8Screen extends Panel{

  override def paintComponent(g: Graphics2D): Unit = {
    // Clear the Canvas
    g.clearRect(0, 0, size.width, size.height)

    // Draw Background
    g.setColor(Color.black)
    g.fillRect(0, 0, size.width, size.height)

    // Draw things that change on top of the background
    g.setColor(Color.red)
    g.fillOval(20, 20, 60, 60)

  }

}
