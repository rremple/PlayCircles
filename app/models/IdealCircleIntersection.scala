package models

import math.Pi
import math.pow
import math.min
import math.max
import math.acos
import math.cos
import math.sin
import math.abs
import math.sqrt
import scala.annotation.tailrec

class IdealCircleIntersection(val c1 : IdealCircle, val c2 : IdealCircle, val interesctionArea : Double) {

  private def square(x: Double) = pow(x,2)

  val r1 = c1.radius
  val r2 = c2.radius
  val epsilon = Scalar.epsilon/100
  val maxDepth : Int = (100/epsilon).toInt

  if ((interesctionArea>c1.area)||(interesctionArea>c2.area))
    throw new RuntimeException(c1.label+"\u2229"+c2.label+" is too large.")

  private def maxSep (r1 : Double, r2 : Double) = r1 + r2

  private def minSep (r1 : Double, r2 : Double) = max(r1,r2) - min(r1,r2)

  //returns the area and the chord at sep as a pair
  private def intersectionAttributes(guessSep : Double) = {
    val chordX = (square(r1) - square(r2) + square(guessSep)) / (2*guessSep)
    val chordY = sqrt(square(r1)  - square(chordX))
    val sector1Area = square(r1) * acos(chordX/r1)
    val sector2Area = square(r2) * acos((guessSep-chordX)/r2)
    val sectorIntersectionArea = chordY * guessSep
    (sector1Area + sector2Area - sectorIntersectionArea, 
     LineSegment(Point(chordX, chordY), Point(chordX, -chordY)))
  }

  val sep = {
    //tail-recursive bisection method - simple and converges quick enough
    @tailrec
    def findSepWithinEpsilon(minSep  : Double, maxSep  : Double, depth : Int) : Double = {
      val guessSep = (minSep+maxSep)/2
      val guessError = intersectionAttributes(guessSep)._1 - interesctionArea
      //println(" - Intermediate guessSep=" + guessSep+ ", error="+guessError)

      if (depth>maxDepth) throw new RuntimeException(
        "Did not converge at maxDepth="+maxDepth+";\n State"+
        ": minSep="+minSep+", maxSep="+maxSep+", guessSep="+guessSep+
        ", guessError="+guessError+", epsilon="+epsilon+
        ", r1="+r1+", r2="+r2+", interesctionTarget="+interesctionArea
      )

      if (abs(guessError)<epsilon) {
        guessSep
      } else if (guessError > 0 ) findSepWithinEpsilon(guessSep,maxSep,depth+1)
      else findSepWithinEpsilon(minSep,guessSep,depth+1)
    }
    if ((r1==0)||(r2==0)) 0 else
      findSepWithinEpsilon(minSep(r1,r2), maxSep(r1,r2), 0)
  }

  def area = intersectionAttributes(sep)._1
  
  def chord = intersectionAttributes(sep)._2
  
  def cross = LineSegment(Point(sep-c2.radius,0),Point(c1.radius,0))

  override def toString = c1.label+" \u2229 "+c2.label+ ", separation = "+sep 

  def realizeWith (c1Center : Point, rotation : Double) = CircleIntersection(this, c1Center, rotation)
}

object IdealCircleIntersection {
  def apply(c1 : IdealCircle, c2 : IdealCircle, intersectionArea : Double) = 
    new  IdealCircleIntersection(c1, c2, intersectionArea)
}