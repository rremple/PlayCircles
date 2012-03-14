/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package models

object Scalar {

  val epsilon : Double = 0.0001
  
  def format (n1 : Double) = "%.4f".format(n1)
  
  def eq(n1 : Double, n2 : Double) = math.abs(n1-n2)<epsilon

  def le(n1 : Double, n2 : Double) = n1<=n2+epsilon

  def ge(n1 : Double, n2 : Double) = epsilon+n1>=n2
 
}
