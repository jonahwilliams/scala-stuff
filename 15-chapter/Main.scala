
sealed trait JSON
case class JNumber(num: Double) extends JSON
case class JBoolean(bool: Boolean) extends JSON
case class JString(str: String) extends JSON
case class JObject(obj: JSON) extends JSON
case class JArray(ary: Array[JSON]) extends JSON
case object JNull extends JSON


object Main extends App {
  def toJSON(x: Any): JSON = x match {
    case s: String => JString(s)
    case b: Boolean => JBoolean(b)
    case n: Double => JNumber(n)
    case _ => JNull
  }

  println(toJSON(2))
  println(toJSON(true))
  println(toJSON("Hello"))
}
