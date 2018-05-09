package jp._5000164.text_emoji_generator.domain

object Text {
  def calculateFontSize(lines: List[String]): Int = {
    val side = 128
    val maxRow = lines.length
    side / maxRow
  }

  def calculatePosition(lines: List[String], align: Align): List[PrintChar] = {

    val toPrintChar = {
      val side = 128
      val maxLength = lines.map(_.length).max
      val maxRow = lines.length
      val maxWidth = side / Math.max(maxLength, maxRow)

      val heightUnit = side / maxRow
      val heightUnitCenter = heightUnit / 2
      val widthUnit = side / maxLength
      val widthUnitCenter = widthUnit / 2


      def toPointChar(margin: Int, char: Char, rowIndex: Int, columnIndex: Int) = {
        PrintChar(
          char.toString,
          margin + widthUnit * columnIndex + widthUnitCenter,
          heightUnit * rowIndex + heightUnitCenter,
          maxWidth
        )
      }

      val toPrintCharForAlignLeft = (_: Int, char: Char, rowIndex: Int, columnIndex: Int) => {
        toPointChar(0, char, rowIndex, columnIndex)
      }

      val toPrintCharForAlignCenter = (lineLength: Int, char: Char, rowIndex: Int, columnIndex: Int) => {
        val lineWidth = widthUnit * lineLength
        val margin = (side - lineWidth) / 2
        toPointChar(margin, char, rowIndex, columnIndex)
      }
      align match {
        case Left => toPrintCharForAlignLeft
        case Center => toPrintCharForAlignCenter
      }
    }

    for {
      (line, rowIndex) <- lines.zipWithIndex
      (char, columnIndex) <- line.zipWithIndex
    } yield toPrintChar(line.length, char, rowIndex, columnIndex)
  }
}

case class PrintChar(
                      content: String,
                      x: Double,
                      y: Double,
                      maxWidth: Double
                    )

