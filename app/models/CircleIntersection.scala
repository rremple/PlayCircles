package models

import math.Pi
import Scalar.format

class CircleIntersection(val i : IdealCircleIntersection, 
                         val c1Center : Point,
                         val rotation : Double ) {

  def chord = i.chord.rotate(rotation) + c1Center

  def cross = i.cross.rotate(rotation) + c1Center

  def area = i.area

  override def toString = i.toString + ", rotation = "+format(rotation)+" ("+180*rotation/Pi+" degrees)"

  def toSvgChord = chord.p1.toSvgPoint(i.c1.label+i.c2.label+"-chord")
//  def toSvgChord = 
//    <g>
//      {chord.toSvg(i.c1.label+i.c2.label+"-chord")}
//      {cross.toSvg(i.c1.label+i.c2.label+"-chord")}
//    </g>
  
  def toSvgP1String = chord.p1.toSvgCoordString
  
}

object CircleIntersection {
  def apply(i : IdealCircleIntersection, c1Center : Point, rotation : Double) = 
    new CircleIntersection(i, c1Center, rotation)
}