package jp._5000164.text_emoji_generator.domain

object Text {
  def calculateFontSize(lines: List[String]): Int = {
    val side = 128
    val maxRow = lines.length
    side / maxRow
  }

  def calculatePosition(lines: List[String], align: Align): List[PrintChar] = {
    val side = 128
    val maxLength = lines.reduceLeft((a, b) => if (a.length > b.length) a else b).length
    val maxRow = lines.length
    val maxWidth = side / (if (maxLength > maxRow) maxLength else maxRow)

    val heightUnit = side / maxRow
    val heightUnitCenter = heightUnit / 2
    val widthUnit = side / maxLength
    val widthUnitCenter = widthUnit / 2

    align match {
      case Left =>
        for {
          (line, rowIndex) <- lines.zipWithIndex
          (char, columnIndex) <- line.zipWithIndex
        } yield {
          PrintChar(
            char.toString,
            widthUnit * columnIndex + widthUnitCenter,
            heightUnit * rowIndex + heightUnitCenter,
            maxWidth
          )
        }

      case Center =>
        for {
          (line, rowIndex) <- lines.zipWithIndex
          (char, columnIndex) <- line.zipWithIndex
        } yield {
          val lineWidth = widthUnit * line.length
          val margin = (side - lineWidth) / 2
          PrintChar(
            char.toString,
            margin + widthUnit * columnIndex + widthUnitCenter,
            heightUnit * rowIndex + heightUnitCenter,
            maxWidth
          )
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

