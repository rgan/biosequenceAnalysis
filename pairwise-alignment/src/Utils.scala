package org.biosequenceanalysis

object Utils {
  def MIN_VALUE() : Int = { return -99999999 }
  
  def max(numberList : List[Int], maxValue : Int) : Int = numberList match {
     case Nil => maxValue
     case _ => if (numberList.head > maxValue) max(numberList.tail, numberList.head) else max(numberList.tail, maxValue)
  }
}