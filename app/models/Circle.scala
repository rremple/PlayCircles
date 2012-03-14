package models

class Circle(val c : IdealCircle, val center : Point) {

  def radius = c.radius
  
  def area = c.area
  
  def contains (p : Point) = Scalar.le(center.distance(p), this.radius)

  def contains (another : Circle) = Scalar.le(center.distance(another.center)+another.radius, this.radius)
    
  def contains (s : LineSegment) : Boolean = contains(s.p1)&&contains(s.p2)

  def contains (i : CircleIntersection) : Boolean = contains(i.chord)&&contains(i.cross)

  override def toString = c.toString +", center at "+center

  def toSvgCircle = 
    <circle id={c.label+"-circle"} 
      cx={center.x.toString} cy={center.y.toString} r={radius.toString} />

  def toSvgCenter= center.toSvgPoint(c.label+"-center")

}

object Circle {
  def apply(c : IdealCircle, center : Point) = new Circle(c, center);
}