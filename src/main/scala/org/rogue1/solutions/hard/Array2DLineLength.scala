package org.rogue1.solutions.hard


/**
  * Given n points on a 2D plane, find the maximum number of points that lie on the same straight line.
  *
  * Example 1:
  *
  * Input: [[1,1],[2,2],[3,3]]
  * Output: 3
  * Explanation:
  * ^
  * |
  * |        o
  * |     o
  * |  o
  * +------------->
  * 0  1  2  3  4
  * Example 2:
  *
  * Input: [[1,1],[3,2],[5,3],[4,1],[2,3],[1,4]]
  * Output: 4
  * Explanation:
  * ^
  * |
  * |  o
  * |     o        o
  * |        o
  * |  o        o
  * +------------------->
  * 0  1  2  3  4  5  6
  **/
object Array2DLineLength {

  case class Point(x: Int, y: Int)

  sealed trait Direction { def move(point: Point, pts: Array[Point]): Option[Point] }

  case object North extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(y=point.y + 1))
  }

  case object South extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(y=point.y - 1))
  }

  case object East extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(x=point.x + 1))
  }

  case object West extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(x=point.x - 1))
  }

  case object NorthEast extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(x=point.x + 1, y=point.y + 1))
  }

  case object NorthWest extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(x=point.x + 1, y=point.y - 1))
  }

  case object SouthEast extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(x=point.x - 1, y=point.y + 1))
  }

  case object SouthWest extends Direction {
    override def move(point: Point, pts: Array[Point]): Option[Point] =
      pts.find(x => x == point.copy(x=point.x - 1, y=point.y - 1))
  }

  private val directions = North :: South :: East :: West :: NorthEast :: NorthWest :: SouthEast :: SouthWest :: Nil


  def process(pt: Point, pts: Array[Point], direction: Direction, distance: Int): Int= {
    direction.move(pt, pts) match {
      case Some(x) => process(x, pts, direction, distance + 1)
      case None => distance
    }
  }

  def maxPoints(pts: Array[Point]): Int = {
    val results = for {
      point <- pts
      direction <- directions
    } yield process(point, pts, direction, 1)
    results match { case Array() => 0 case x => x.max }
  }

}
