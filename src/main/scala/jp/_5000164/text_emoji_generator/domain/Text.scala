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

    var result: List[PrintChar] = List()

    align match {
      case Left =>
        lines
          // ListのインデックスがほしいときはzipWithIndexを使います。但しindexは0から始まります。
          .zipWithIndex
          .foreach {
            case (line, rowIndex) =>
              val rowNumber = rowIndex + 1
              line.zipWithIndex.foreach {
                case (char, columnIndex) =>
                  val columnNumber = columnIndex + 1
                  result = PrintChar(
                    char.toString,
                    widthUnit * columnNumber - widthUnitCenter,
                    heightUnit * rowNumber - heightUnitCenter,
                    maxWidth
                  ) :: result
              }
          }
      case Center =>
        lines.zipWithIndex
          .foreach {
            case (line, rowIndex) =>
              val rowNumber = rowIndex + 1
              val lineWidth = widthUnit * line.length
              val margin = (side - lineWidth) / 2
              line.zipWithIndex.foreach {
                case (char, columnIndex) =>
                  val columnNumber = columnIndex + 1
                  result = PrintChar(
                    char.toString,
                    margin + widthUnit * columnNumber - widthUnitCenter,
                    heightUnit * rowNumber - heightUnitCenter,
                    maxWidth
                  ) :: result
              }
          }

    }

    result.reverse
  }
}

case class PrintChar(
                      content: String,
                      x: Double,
                      y: Double,
                      maxWidth: Double
                    )

