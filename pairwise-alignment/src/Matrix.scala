package org.biosequenceanalysis

class Matrix(rowSequence :List[String], columnSequence :List[String]) {
  private val this.rowSequence = rowSequence
  private val this.columnSequence = columnSequence
  private var values = new Array[Array[Int]](noRows,noCols)

  for(i <- 0 to noRows-1) {
      for(j <- 0 to noCols-1) {
          values(i)(j) = 0
      }
  }

  def valueFor(sRow : String, sCol : String) : Int = {
    return values(indexFor(sRow, true))(indexFor(sCol, false))
  }

  def setValueAt(row : Int, col : Int, value : Int) : Unit = {
     values(row)(col) = value
  }

  def getValueAt(row : Int, col : Int) : Int = {
     return values(row)(col)
  }

  def noRows() : Int = {
     return this.rowSequence.length
  }

  def noCols() : Int = {
    return this.columnSequence.length
  }

  private def indexFor(s : String, row : Boolean) : Int = {
     return if (row) rowSequence.indexOf(s) else columnSequence.indexOf(s)
  }
}