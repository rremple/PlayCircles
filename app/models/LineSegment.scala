package models

import math.cos
import math.sin

class LineSegment (val p1 : Point, val p2 : Point) {

  private def square(n : Double) = n*n

  def slope = 
    if (p1.x==p2.x) {Double.PositiveInfinity} else (p1.y-p2.y)/(p1.x-p2.x)
  
  def contains (p : Point) = {
    ((Scalar.le(p.y,p1.y)&&Scalar.ge(p.y,p2.y)) 
     ||(Scalar.le(p.y,p2.y)&&Scalar.ge(p.y,p1.y))) &&
    {if (slope.isInfinity) Scalar.eq(p.x,p1.x)
     else Point(p2.x,p.y+(p2.x-p.x)*slope).closeTo(p2)
    }
  }

  def overlaps(ls : LineSegment) = 
    (ls.slope==slope) && (ls.contains(p1)||ls.contains(p2)||
                          contains(ls.p1)||contains(ls.p2))
  
  def length = math.sqrt(square(p1.x-p2.x)+square(p1.y-p2.y))

  def + (p : Point) = LineSegment(p1+p,p2+p)

  def mid = Point((p1.x+p2.x)/2,(p1.y+p2.y)/2)

  def rotate (angle : Double) = LineSegment(p1.rotate(angle), p2.rotate(angle))
  
  override def toString = "["+p1+"--"+p2+"]"
  
  def toSvg(id : String ) =
    <line id={id}
      x1={p1.x.toString} y1={p1.y.toString}
      x2={p2.x.toString} y2={p2.y.toString}/>

}

object LineSegment {
  def apply(p1 : Point, p2 : Point) = new LineSegment(p1,p2);
}
