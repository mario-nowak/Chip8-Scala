package Chip8

object Main extends App{

  val chip8: Chip8 = new Chip8
  chip8.loadGame("PONG")
  println(chip8.display)

  val myList = List(1,2,3,4,5)

  //chip8.loadGame("pong")
  /*
  while (true) {

    chip8.emulateCycle()
    if (chip8.drawFlag)
      drawGraphics()

    chip8.setKeys()

  }
  */

}
