package challenges


extension (c: (day: Int, year: Int)) def show: String =
  val d = f"${c.day}%02d"
  s"${c.year}_day$d"

inline def inputToday(suffix: String = "") =
  os.read(inputPathToday(suffix))

inline def inputPathToday(suffix: String = "") =
  val name = if suffix.nonEmpty then s"${today.show}_$suffix" else today.show
  currentDir / os.up / "inputs" / name

inline def currentDir = os.Path(sourcecode.File()) / os.up

inline def today: (day: Int, year: Int) =
  sourcecode.FileName() match
    case s"${year}_day$index.scala" =>
      (day = index.toInt, year = year.toInt)
    case _ =>
      (day = 0, year = 0)
