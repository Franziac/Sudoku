import scala.swing.{Button, Font}
class TileGUI(text: String, val gridPos: GridPos, color: java.awt.Color) extends Button(text):
  focusable = false
  background = color
  super.font = BoldFont
  var value = 0
  var valueIsLocked = true

  def setValue(num: Int) =
    if !this.valueIsLocked then
      super.text = num.toString

  def sync(tile: Tile) =
    this.valueIsLocked = tile.valueIsLocked
    if tile.value != 0 then
      super.text = tile.value.toString
      if this.valueIsLocked then
        super.font = BoldFont
        foreground = DefaultTextColor
      else
        super.font = NormalFont
        if tile.possibleValues.contains(tile.value) then
          foreground = DefaultTextColor
        else
          foreground = WrongColor
    else if tile.noteValues.nonEmpty then
      super.text ="<html>" + tile.noteValues.mkString("").grouped(3).mkString("<br>") +"</html>"
      super.font = NoteFont
      foreground = DefaultTextColor
    else super.text = ""
