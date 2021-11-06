package Core

import java.io.File
import scala.collection.mutable
import scala.util.Random
import java.io.FileInputStream
import java.awt.Toolkit

class Chip8Core {

  private val chip8FontSet: Array[Int] = Array(
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
  )

  private var currentOpcode: Int = 0

  private val memory: Array[Int] = new Array(4069)

  /**
   * Chip-8 has 16 8-bit data registers named V0 to VF.
   */
  private val registerV: Array[Int] = new Array(16)

  /**
   * The address register I is 12 bits (here 16).
   */
  private var registerI: Int = 0

  private var programCounter: Int = 0

  private var delayTimer: Int = 0
  private var soundTimer: Int = 0

  private val stack: mutable.Stack[Int] = new mutable.Stack()
  private var stackPointer: Int = 0
  private val randomNumberGenerator = new Random()

  private val displayWidth: Int = 64
  private val displayHeight: Int = 32
  /**
   * The Chip-8 has a black and white screen with a screen size of 64*32.
   */
  private val keyRegister: Array[Int] = new Array(16)
  private var waitingForKeyPress: Boolean = false
  private var lastPressedKey: Int = 0
  val display: Array[Array[Int]] = Array.ofDim(displayWidth, displayHeight)


  private def loadFontset(): Unit = {
    for (i <- chip8FontSet.indices){
      memory(i) = chip8FontSet(i)
    }
  }

  def initialize(): Unit = {
    // initialize registers and memory once
    programCounter = 0x200 // Program counter starts at 0x200
    currentOpcode = 0
    registerI = 0
    stackPointer = 0
    delayTimer = 0
    soundTimer = 0
    waitingForKeyPress = false
    lastPressedKey = 0

    clearMemory()
    clearRegisterV()
    clearStack()
    clearKeyRegister()
    clearDisplay()

    loadFontset()
  }

  private def incrementProgramCounter(): Unit = {
    programCounter += 2
  }

  private def clearMemory(): Unit = {
    for (i <- memory.indices) {
      memory(i) = 0
    }
  }

  private def clearRegisterV(): Unit = {
    for (i <- registerV.indices) {
      registerV(i) = 0
    }
  }

  private def clearStack(): Unit = {
    stack.popAll()
  }

  private def clearKeyRegister(): Unit = {
    for (i <- keyRegister.indices) {
      keyRegister(i) = 0
    }
  }

  private def clearDisplay(): Unit = {
    for (x <- 0 until displayWidth)
      for (y <- 0 until displayHeight)
        display(x)(y) = 0
  }

  def emulateCycle(): Unit = {
    fetchNewOpcode()
    executeCurrentOpcode()
    updateTimers()
  }

  private def fetchNewOpcode(): Unit = {
    val firstHalf: Int = memory(programCounter) << 8
    val secondHalf: Int = memory(programCounter + 1)
    currentOpcode = firstHalf | secondHalf
  }

  private def executeCurrentOpcode(): Unit = {
    val X = (currentOpcode & 0x0F00) >> 8
    val Y = (currentOpcode & 0x00F0) >> 4
    val N = currentOpcode & 0x000F
    val NN = currentOpcode & 0x00FF
    val NNN = currentOpcode & 0x0FFF
    currentOpcode match {
      case 0x00E0 => // 00E0 - Clear the display by setting all pixels to off
        clearDisplay()
        incrementProgramCounter()
      case 0x00EE => // 00EE - Return from a subroutine
        programCounter = stack.pop()
        incrementProgramCounter()
      case _ =>
        currentOpcode & 0xF000 match {

          case 0x1000 => // 1NNN - Jump to address NNN
            programCounter = NNN

          case 0x2000 => // 2NNN - Call the subroutine at address NNN
            stack.push(programCounter)
            programCounter = NNN

          case 0x3000 => // 3XNN - Skip the next instruction if VX == NN
            if (registerV(X) == (currentOpcode & 0x00FF)) incrementProgramCounter()
            incrementProgramCounter()

          case 0x4000 => // 4XNN - Skip the next instruction if VX != NN
            if (registerV(X) != (currentOpcode & 0x00FF)) incrementProgramCounter()
            incrementProgramCounter()

          case 0x5000 => // 5XY0 - Skip next instruction if VX == VY
            if (registerV(X) == registerV(Y)) incrementProgramCounter()
            incrementProgramCounter()

          case 0x6000 => // 6XNN - Load value NN into register VX
            registerV(X) = currentOpcode & 0x00FF
            incrementProgramCounter()

          case 0x7000 => // 7XNN - Add value NN to the value of register VX
            registerV(X) = (registerV(X) + NN) % 256
            incrementProgramCounter()

          case 0x8000 =>
            currentOpcode & 0xF00F match {
              case 0x8000 => // 8XY0 - Put value of register VY into VX
                registerV(X) = registerV(Y)
                incrementProgramCounter()

              case 0x8001 => // 8XY1 - Store bitwise OR between values VX and VY in VX
                registerV(X) = registerV(X) | registerV(Y)
                incrementProgramCounter()

              case 0x8002 => // 8XY2 - Store bitwise AND between values VX and VY in VX
                registerV(X) = registerV(X) & registerV(Y)
                incrementProgramCounter()

              case 0x8003 => // 8XY3 - Perform bitwise XOE between values VX and VY in VX
                registerV(X) = registerV(X) ^ registerV(Y)
                incrementProgramCounter()

              case 0x8004 => // 8XY4 - Store sum of VY and VX in VX. Put the carry bit in VF
                registerV(0xF) = if (registerV(X) + registerV(Y) > 0xFF) 1 else 0
                registerV(X) = (registerV(X) + registerV(Y)) % 256
                incrementProgramCounter()

              case 0x8005 => // 8XY5 - Store subtraction of VX and VY in VX. Put the borrow in VF
                registerV(0xF) = if (registerV(X) > registerV(Y)) 1 else 0
                var subtraction = (registerV(X) - registerV(Y)) % 256
                subtraction = if (subtraction < 0) subtraction + 256 else subtraction
                registerV(X) = subtraction
                incrementProgramCounter()

              case 0x8006 => // 8XY6 - Store lest significant bit of VX in VF and shift VX right
                registerV(0xF) = registerV(X) & 0x01
                registerV(X) = registerV(X) / 2
                incrementProgramCounter()

              case 0x8007 => // 8XY7 - Store result of subtraction of VY and VX in VX. Set VF to 1 if there is no borrow, to 0 otherwise.
                registerV(0xF) = if (registerV(Y) > registerV(X)) 1 else 0
                var subtraction = (registerV(Y) - registerV(X)) % 256
                subtraction = if (subtraction < 0) subtraction + 256 else subtraction
                registerV(X) = subtraction
                incrementProgramCounter()

              case 0x800E => // 8XYE - Store most significant bit of VX in VF and shift VX left
                registerV(0xF) = registerV(X) & 0x80
                registerV(X) = (registerV(X) * 2) % 256
                incrementProgramCounter()

              case _ =>
                onUnsupportedOpcode()
            }
          case 0x9000 => // 9XY0 - Skip the next instruction if values of VX and VY are not equal
            if (registerV(X) != registerV(Y)) {
              incrementProgramCounter()
            }
            incrementProgramCounter()

          case 0xA000 => // ANNN - Set the value of I to the address NNN
            registerI = NNN
            incrementProgramCounter()

          case 0xB000 => // BNNN - Jump to address NNN + V0
            programCounter = NNN + registerV(0x0)

          case 0xC000 => // CXNN - Store the result of a bitwise AND of VX and a random byte
            val randomByte = randomNumberGenerator.nextInt(256)
            registerV(X) = randomByte & NN
            incrementProgramCounter()

          case 0xD000 => // DXYN - The draw instruction
            // save the starting position of the sprite we want to draw
            val startXCoordinate = registerV(X) % displayWidth
            val startYCoordinate = registerV(Y) % displayHeight

            registerV(0xF) = 0
            // iterate over all N rows of the sprite
            for (rowIndex <- 0 until N.min(displayHeight - startYCoordinate)) {
              // fetch the current row
              val spriteRow = memory(registerI + rowIndex)
              // calculate the y coordinate of the current pixel
              val currentYCoordinate = (startYCoordinate + rowIndex) % displayHeight

              // extract the bit at the current position from the a row of the sprite
              var binaryString = spriteRow.toBinaryString
              if (binaryString.length < 8) {
                val missingLength = 8 - binaryString.length
                binaryString = "0"*missingLength + binaryString
              }

              // iterate over all 8 columns of the sprite
              for (columnIndex <- 0 until 8.min(displayWidth - startXCoordinate)) {
                // calculate the x coordinate of the current pixel
                val currentXCoordinate = (startXCoordinate + columnIndex) % displayWidth
                val currentPixel = display(currentXCoordinate)(currentYCoordinate)

                val pixelInSprite = binaryString.slice(columnIndex, columnIndex+1).toInt
                // perform XOR between current color in the display and the bit in the sprite
                if (pixelInSprite == 1) {
                  if (currentPixel == 1) {
                    display(currentXCoordinate)(currentYCoordinate) = 0
                    registerV(0xF) = 1
                  } else {
                    display(currentXCoordinate)(currentYCoordinate) = 1
                  }
                }
              }
            }
            incrementProgramCounter()

          case 0xE000 =>
            currentOpcode & 0xF0FF match {
              case 0xE09E => // EX9E - Skip the next instruction if the key with index VX is currently pressed
                if (keyRegister(registerV(X)) == 1)
                  incrementProgramCounter()
                incrementProgramCounter()

              case 0xE0A1 => // EXA1 - Skip the next instruction if the key with index VX is currently NOT pressed
                if (keyRegister(registerV(X)) == 0)
                  incrementProgramCounter()
                incrementProgramCounter()

              case _ =>
                onUnsupportedOpcode()
            }

          case 0xF000 =>
            currentOpcode & 0xF0FF match {
              case 0xF007 => // FX07 - Store the delay timer value into VX
                registerV(X) = delayTimer
                incrementProgramCounter()

              case 0xF00A => // FX0A - Wait for a key press and then store the value of the key to VX
                if (waitingForKeyPress) {
                  registerV(X) = lastPressedKey
                  waitingForKeyPress = false
                  incrementProgramCounter()
                } else {
                  waitingForKeyPress = true
                }

              case 0xF015 => // FX15 - Load the value of VX into the delay timer
                delayTimer = registerV(X)
                incrementProgramCounter()

              case 0xF018 => // FX18 - Load the value of VX into the sound timer
                soundTimer = registerV(X)
                incrementProgramCounter()

              case 0xF01E => // FX1E - Store the sum of I and VX in I
                registerI = (registerI + registerV(X))
                incrementProgramCounter()

              case 0xF029 => // FX29 - Set the location of the sprite for the digit VX to I
                /* The font sprites start at address 0x000 and contain the hexadecimal digits from 1 to F.
                 * Each character has a height of 0x05 bytes. To obtain the starting address of a character, we need
                 * to multiply its value times the offset of 5 rows for the previous characters.
                 */
                registerI = registerV(X) * 0x05
                incrementProgramCounter()

              case 0xF033 => // FX33 - Store the binary decoded decimal VX into three consecutive memory slots I...I+2
                val hundreds: Int = registerV(X) / 100
                val tens: Int = (registerV(X) - hundreds * 100) / 10
                val ones: Int = registerV(X) - hundreds * 100 - tens * 10
                memory(registerI) = hundreds
                memory(registerI+1) = tens
                memory(registerI+2) = ones
                incrementProgramCounter()

              case 0xF055 => // FX55 - Store registers from V0 to VX in the main memory, starting at location I
                for (registerIndex <- 0 to X)
                  memory(registerI + registerIndex) = registerV(registerIndex)
                incrementProgramCounter()

              case 0xF065 => // FX65 - Load the memory data starting at address I into the registers V0 to VX
                for (registerIndex <- 0 to X)
                  registerV(registerIndex) = memory(registerI + registerIndex)
                incrementProgramCounter()

              case _ =>
                onUnsupportedOpcode()
            }
          case _ =>
            onUnsupportedOpcode()
        }
    }
  }

  private def onUnsupportedOpcode(): Unit = {
    throw new Exception(s"WARNING - encountered unsupported opcode ${currentOpcode.toHexString} !!!")
  }
  
  def registerKeyPress(keyIndex: Int): Unit = {
    if (keyRegister(keyIndex) == 0){
      keyRegister(keyIndex) = 1
      if (waitingForKeyPress) {
        waitingForKeyPress = false
        lastPressedKey = keyIndex
      }
    }
  }

  def registerKeyRelease(keyIndex: Int): Unit = {
    keyRegister(keyIndex) = 0
  }

  private def updateTimers(): Unit = {
    if (delayTimer > 0) delayTimer -= 1
    if (soundTimer > 0) {
      soundTimer -= 1
      if (soundTimer == 0) {
        Toolkit.getDefaultToolkit.beep()
      }
    }
  }

  def loadGame(name: String): Unit = {
    val gameFile = new File(getClass.getClassLoader.getResource(name).getPath)
    val binaryGameData: Array[Byte] = new FileInputStream(gameFile).readAllBytes()
    for ((byte: Byte, index: Int) <- binaryGameData.zipWithIndex) {
      val unsignedInt = byte.toInt & 0xFF
      memory(index + 512) = unsignedInt
    }
  }

  initialize()

}