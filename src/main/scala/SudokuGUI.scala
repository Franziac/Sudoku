import javax.swing.border.MatteBorder
import scala.swing._
import scala.swing.event.{ButtonClicked, KeyTyped}

object SudokuGUI extends SimpleSwingApplication:
  val sudoku = Sudoku()
  val tiles: Array[Array[TileGUI]] = Array.ofDim[TileGUI](9, 9)
  var selected: Option[TileGUI] = None

  def restart() =
    sudoku.generate()
    updateGUI()

  def updateGUI() =
    val coords = (0 to 8).flatMap(x => (0 to 8).map( y => (x, y)))
    for (x, y) <- coords do
      tiles(y)(x).sync( sudoku.grid(y)(x) )

  def top: Frame = new MainFrame:
    title = "Sudoku"
    // Creating a 9x9 grid panel
    val gridPanel = new GridPanel(3, 3):
      // Add key listener
      listenTo(this.keys)
      reactions += {
        case KeyTyped(_, key, _, _) =>
          if key >= '1' && key <= '9' then
            selected match
              case Some(tile) =>
                val hasWon = sudoku.add(tile.gridPos, key.toString.toInt)
                if hasWon then restart()
                updateGUI()
              case None => println("no selection")
          if key == 'n' then
            sudoku.noteMode = !sudoku.noteMode

      }
      focusable = true
      requestFocus()
      for (sectorRow <- 0 until 3; sectorCol <- 0 until 3) do
        val subGrid = new GridPanel(3, 3):
          border = new MatteBorder(2, 2, 2, 2, BorderColor)
          for (subRow <- 0 until 3; subCol <- 0 until 3) do
            val row = sectorRow*3 + subRow
            val col = sectorCol*3 + subCol
            val tile = sudoku.grid(row)(col)
            val color = if (row % 2 == 0) == (col % 2 == 0) then GridColor1 else GridColor2
            val button = tile.value match
              case 0 => TileGUI("", GridPos(row, col), color)
              case other => TileGUI(other.toString, GridPos(row, col), color)
            tiles(row)(col) = button

            // Add an event listener for clicks
            listenTo(button)
            reactions += {
              case ButtonClicked(b: TileGUI) =>
                if !b.valueIsLocked then
                  selected = Some(b)
                else
                  selected = None
            }
            contents += button
        contents += subGrid


    contents = new BorderPanel {
      layout(gridPanel) = BorderPanel.Position.Center
    }

    size = new Dimension(600, 600)
    restart()


