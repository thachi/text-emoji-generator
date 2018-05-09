package jp._5000164.text_emoji_generator.domain

object Text {
  def calculateFontSize(lines: List[String]): Int = {
    val side = 128
    val maxRow = lines.length
    side / maxRow
  }

  def calculatePosition(lines: List[String], align: Align): List[PrintChar] = {

    val toPrintChar = createToPrintChar(align, lines.map(_.length).max, lines.length)

    for {
      (line, rowIndex) <- lines.zipWithIndex
      (char, columnIndex) <- line.zipWithIndex
    } yield toPrintChar(line.length, char, rowIndex, columnIndex)
  }

  /**
    * PrintCharを作る関数を生成します。
    *
    * @param align Align
    * @param maxLength 最大の文字数
    * @param maxRow 最大の行数
    * @return 行の文字数、文字、行インデックス,列インデックスに応じてPrintCharを生成するための関数を返します。
    */
  private def createToPrintChar(align: Align, maxLength: Int, maxRow: Int) = {
    val side = 128
    val maxWidth = side / Math.max(maxLength, maxRow)

    val heightUnit = side / maxRow
    val heightUnitCenter = heightUnit / 2
    val widthUnit = side / maxLength
    val widthUnitCenter = widthUnit / 2

    def toPrintChar(margin: Int, char: Char, rowIndex: Int, columnIndex: Int) = PrintChar(
      char.toString,
      margin + widthUnit * columnIndex + widthUnitCenter,
      heightUnit * rowIndex + heightUnitCenter,
      maxWidth
    )

    align match {
      case Left => (_: Int, char: Char, rowIndex: Int, columnIndex: Int) => {
        toPrintChar(0, char, rowIndex, columnIndex)
      }
      case Center => (lineLength: Int, char: Char, rowIndex: Int, columnIndex: Int) => {
        val lineWidth = widthUnit * lineLength
        val margin = (side - lineWidth) / 2
        toPrintChar(margin, char, rowIndex, columnIndex)
      }
    }
  }
}

case class PrintChar(
                      content: String,
                      x: Double,
                      y: Double,
                      maxWidth: Double
                    )

