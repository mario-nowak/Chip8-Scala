package Chip8

object Main extends App{

  val chip8: Chip8 = new Chip8

  val myList = List(1,2,3,4,5)
  for (i <- 0 until myList.length) println(myList(i))

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
