import play.api.libs.json._

object LevelMaker extends App {
  override def main(args: Array[String]) = {
    makeFile(args.toList.map(f => parseFile(f)))
  }
  def parseFile(f: String) = {
    val FACTOR = 64
    val startx = -300 - FACTOR / 2
    val starty = -500
    val lines  = scala.io.Source.fromFile(f)
        .getLines
        .toSeq
        .zipWithIndex

    val goal = iterate(lines, { (c, xx, yy) =>  (c, xx, yy) match {
      case ('X', x, y) => Some(s"{x= ${startx+x*FACTOR}, y= ${starty+y*FACTOR}, size= 32}")
      case _ => None
    }}).mkString(",")

    val bonuses = iterate(lines, { (c, xx, yy) =>  (c, xx, yy) match {
      case ('E', x, y) => Some(s"{x= ${startx+x*FACTOR}, y= ${starty+y*FACTOR}, size= 32}")
      case _ => None
    }}).mkString(",")


    val boosts = iterate(lines, { (c, xx, yy) =>  (c, xx, yy) match {
      case ('B', x, y) => Some(s"{x= ${startx+x*FACTOR}, y= ${starty+y*FACTOR}, size= 32}")
      case _ => None
    }}).mkString(",")


    val fires = iterate(lines, { (c, xx, yy) =>  (c, xx, yy) match {
      case ('F', x, y) => Some(s"{x= ${startx+x*FACTOR}, y= ${starty+y*FACTOR}, size= 32, halo= 1 , sprite= ${(new scala.util.Random()).nextInt(3) + 1}}") 
      case _ => None
    }}).mkString(",")

    val walls = iterate(lines, { (c, x, y) =>
      List('O', 'C', '3', '=', '<', '>', '_', '^').contains(c) match {
        case true => {
          val orientation = c match {
            case 'C' => "ltb"
            case '=' => "tb"
            case '3' => "rtb"
            case '>' => "l"
            case '<' => "r"
            case '^' => "t"
            case '_' => "b"
            case 'O' => ""
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


    (fires, walls, goal, bonuses, boosts)
  }

  def iterate(lst: Seq[(String, Int)], f: (Char, Int, Int) => Option[String]) ={
      lst.map { l =>
        val line = l._1
        val y = l._2
        line.zipWithIndex.map { a => f(a._1, a._2, y) }.flatten.toList
      }.toList.flatten
  }

  def makeFile(l: List[(String, String, String, String, String)]) = {
    val content = s"""module Levels
     ( getWalls, getFires, getGoal, getBonuses, getBoosts) where

--level1 : {a | fires : List {b | x: Float, y: Float, size: Float}, walls: List {c | x: Float, y: Float, size: Float} }
getWalls l = case l of
  ${l.zipWithIndex.map {
      case ((fires, walls, goal, bonus, _), lvl) => (lvl+1)+" -> ["+walls+"]\n  "
    }.mkString
  }
  _ -> []
getFires l = case l of
  ${l.zipWithIndex.map {
      case ((fires, walls, goal, bonus, _), lvl) => (lvl+1)+" -> ["+fires+"]\n  "
    }.mkString
  }
  _ -> []
getGoal l = case l of
  ${l.zipWithIndex.map {
      case ((fires, walls, goal, bonus, _), lvl) => (lvl+1)+" -> "+goal+"\n  "
    }.mkString
  }
  _ -> getGoal 1
getBonuses l = case l of
  ${l.zipWithIndex.map {
      case ((fires, walls, goal, bonus, _), lvl) => (lvl+1)+" -> ["+bonus+"]\n  "
    }.mkString
  }
  _ -> []
getBoosts l = case l of
  ${l.zipWithIndex.map {
      case ((fires, walls, goal, bonus, boosts), lvl) => (lvl+1)+" -> ["+boosts+"]\n  "
    }.mkString
  }
  _ -> []
    """

    val writer = new java.io.PrintWriter(new java.io.File("../Levels.elm"))
    writer.write(content)
    writer.close()

  }
}
