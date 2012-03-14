package models

import math.Pi
import math.sqrt

class IdealCircle (val label : String, val area : Double) {
  
  val radius = sqrt(area/Pi)

  override def toString = "Circle "+label+": radius="+radius
  
  def realizeWith (center : Point) = Circle(this, center)

}

object IdealCircle {
  def apply(label : String, area : Double) = new IdealCircle(label, area);
}