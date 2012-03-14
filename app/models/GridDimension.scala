package models

import math.min
import math.max

class GridDimension (val c1 : Circle, val c2 : Circle, val c3 : Circle) {

  val minGrid = Point (
    min(c1.center.x-c1.radius, min(c2.center.x-c2.radius,c3.center.x-c3.radius)) - 1,
    min(c1.center.y-c1.radius, min(c2.center.y-c2.radius,c3.center.y-c3.radius)) - 1)

  val maxGrid = Point (
    max(c1.center.x+c1.radius, max(c2.center.x+c2.radius,c3.center.x+c3.radius)) + 1,
    max(c1.center.y+c1.radius, max(c2.center.y+c2.radius,c3.center.y+c3.radius)) + 1)

  val rangeGridX = maxGrid.x - minGrid.x

  val rangeGridY = maxGrid.y - minGrid.y

  val middleGridX = (maxGrid.x + minGrid.x)/2

  val middleGridY = (maxGrid.y + minGrid.y)/2

  override def toString = "range=" + minGrid + " to " + maxGrid

  def toSvgAxes =
    <g>
      <line id="x-axis"
        x1={minGrid.x.toString} y1="0"
        x2={maxGrid.x.toString} y2="0"/>
      <line id="y-axis"
        x1="0" y1={minGrid.y.toString}
        x2="0" y2={maxGrid.y.toString}/>
      <rect id="box"
        x= {minGrid.x.toString} y= {minGrid.y.toString}
        width="100%" height="100%"/>
    </g>

  def toSvgGrid=
    <g>
      <line id="x-grid" stroke-width={rangeGridX.toString}
        x1={middleGridX.toString} y1={(math.ceil(minGrid.y)-0.05).toString}
        x2={middleGridX.toString} y2={maxGrid.y.toString}/>
      <line id="y-grid" stroke-width={rangeGridY.toString}
        x1={(math.ceil(minGrid.x)-0.05).toString} y1={middleGridY.toString}
        x2={maxGrid.x.toString} y2={middleGridY.toString}/>
    </g>

  def toSvgViewBoxAttribute =  
    minGrid.x.toString+" "+ minGrid.y.toString+" "+rangeGridX.toString +" "+rangeGridY.toString 
}

object GridDimension {
  def apply(c1 : Circle,  c2 : Circle,  c3 : Circle) = new GridDimension (c1,c2,c3)
}