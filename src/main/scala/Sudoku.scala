import scala.collection.mutable.Buffer
import scala.util.Random
class Sudoku:
  var grid: Array[Array[Tile]] = Array.ofDim[Tile](9, 9).map( row => row.map( tile => Tile(0)))
  val hiddenCount = 64
  var noteMode = false

  def getSector(pos: GridPos) =
    val sectorX = pos.x / 3
    val sectorY = pos.y / 3
    grid.slice(0 + sectorY*3, 3 + sectorY*3).map( a => a.slice(0 + sectorX*3, 3 + sectorX*3)).map( row => row.map(_.value) )

  def getRow(index: Int) = grid(index).map(_.value)
  def getColumn(index: Int) = grid.transpose.apply(index).map(_.value)

  def possibleValues(pos: GridPos) =
    val possibles = (1 to 9).toBuffer
    val column = getColumn(pos.x).toBuffer
    val row = getRow(pos.y).toBuffer
    val sector = getSector(pos).flatten.toBuffer

    column -= column(pos.y)
    row -= row(pos.x)
    sector -= sector(pos.y % 3 * 3 + pos.x % 3)

    possibles --= column
    possibles --= row
    possibles --= sector
    possibles.toVector

  def updatePossibleValues() =
    var x = 0
    var y = 0
    for (x <- 0 until 9; y <- 0 until 9) do
      grid(y)(x).possibleValues = possibleValues(GridPos(x, y))

  def hasWon = grid.flatten.forall( tile => tile.possibleValues.contains(tile.value) )

  def generate() =
    // Reset grid
    grid = Array.ofDim[Tile](9, 9).map( row => row.map( tile => Tile(0)))

    while grid.flatten.exists(_.value == 0) do
      updatePossibleValues()

      // Tile with the least possible values
      val tile = grid.flatten.filter(_.value == 0).minBy(_.possibleValues.length)

      // Find the value that can be assigned to the least amount of other tiles
      val values = Random.shuffle(grid.flatten.filter(_.value == 0).flatMap(_.possibleValues).groupBy( i=> i ).toBuffer)
      tile.value = values.filter((key, value) => tile.possibleValues.contains(key)).minBy((key, value) => value.length)._1

    val coords = Random.shuffle((0 to 8).flatMap(x => (0 to 8).map( y => (x, y))))
    for (x, y) <- coords.take(hiddenCount) do
      grid(y)(x).value = 0
      grid(y)(x).valueIsLocked = false
    updatePossibleValues()


  def add(pos: GridPos, num: Int) =
    if noteMode then
      grid(pos.x)(pos.y).addNoteValue(num)
    else
      grid(pos.x)(pos.y).setValue(num)
    updatePossibleValues()
    hasWon


  override def toString: String =
    var s = ""
    grid.map(row => row.map(tile => tile.value)).foreach(a => s += a.mkString("", ", ", "\n"))
    s
