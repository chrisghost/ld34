import play.api.libs.json._

object LevelMaker extends App {
  override def main(args: Array[String]) = {
    println(args.head)
    val FACTOR = 64
    val startx = -300 + FACTOR
    val r  = scala.io.Source.fromFile(args.head)
        .getLines
        .zipWithIndex
        .map { l =>
          val line = l._1
          val y = l._2
          line.zipWithIndex.map {
            case ('F', x) => Some( 
                 s"{x= ${startx+x*FACTOR}, y= ${y*FACTOR}, size= 32}"

            )
            case _ => None
          }.flatten.toList
        }.toList.flatten
  
    println(r.mkString(","))
  }
}
