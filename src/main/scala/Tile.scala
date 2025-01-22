import scala.collection.mutable.Set
class Tile(var value: Int):
  val noteValues = Set[Int]()
  var possibleValues = (0 to 9).toVector
  var valueIsLocked = true

  def setValue(num: Int) =
    if !this.valueIsLocked then
      this.value = num
      this.noteValues --= this.noteValues

  def addNoteValue(num: Int) =
    this.value = 0
    if this.noteValues.contains(num) then this.noteValues -= num
    else this.noteValues += num


