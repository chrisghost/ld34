import play.api.libs.json._

object LevelMaker extends App {
  override def main(args: Array[String]) = {
    println(args.head)
    val FACTOR = 64
    val startx = -300 - FACTOR / 2
    val starty = -500
    val lines  = scala.io.Source.fromFile(args.head)
        .getLines
        .toSeq
        .zipWithIndex

    val goal = iterate(lines, { (c, xx, yy) =>  (c, xx, yy) match {
      case ('X', x, y) => Some(s"{x= ${startx+x*FACTOR}, y= ${starty+y*FACTOR}, size= 32}")
      case _ => None
    }}).mkString(",")

    val fires = iterate(lines, { (c, xx, yy) =>  (c, xx, yy) match {
      case ('F', x, y) => Some(s"{x= ${startx+x*FACTOR}, y= ${starty+y*FACTOR}, size= 32}")
      case _ => None
    }}).mkString(",")

    val walls = iterate(lines, { (c, x, y) =>
      List('C', '3', '=', '<', '>', '_', '^').contains(c) match {
        case true => {
          val orientation = c match {
            case 'C' => "ltb"
            case '=' => "tb"
            case '3' => "rtb"
            case '>' => "l"
            case '<' => "r"
            case '^' => "t"
            case '_' => "b"
            case _ => "lrtb"
          }
           Some(s"""
  { x= ${startx+x*FACTOR},
  y= ${starty+y*FACTOR},
  w = $FACTOR,
  h = $FACTOR,
  orientation = "$orientation"} """)
        }
      case _ => None
    }}).mkString(",")


    makeFile(fires, walls, goal)
  }

  def iterate(lst: Seq[(String, Int)], f: (Char, Int, Int) => Option[String]) ={
      lst.map { l =>
        val line = l._1
        val y = l._2
        line.zipWithIndex.map { a => f(a._1, a._2, y) }.flatten.toList
      }.toList.flatten
  }

  def makeFile(fires: String, walls: String, goal: String) = {
    val content = s"""module Levels
     ( level1_walls, level1_fires, level1_goal) where

--level1 : {a | fires : List {b | x: Float, y: Float, size: Float}, walls: List {c | x: Float, y: Float, size: Float} }
level1_walls = [$walls]
level1_fires = [$fires]
level1_goal = $goal
    """

    val writer = new java.io.PrintWriter(new java.io.File("../Levels.elm"))
    writer.write(content)
    writer.close()

  }
}
