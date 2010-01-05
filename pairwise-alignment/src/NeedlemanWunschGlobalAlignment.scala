package org.biosequenceanalysis

class NeedlemanWunschGlobalAlignment(sequence1 : List[String], sequence2 : List[String], substitutionMatrix: Matrix, gapPenalty : Int) {
  val this.sequence1 = sequence1
  val this.sequence2 = sequence2
  val this.gapPenalty = gapPenalty
  var matrix = new Matrix("X" :: this.sequence1, "X" :: this.sequence2)
  // initialize top row F(0,j) to -i*gapPenalty
  for(j <- 0 to matrix.noCols-1) {
      matrix.setValueAt(0, j, -1*j*gapPenalty)
  }
  // initialize left column F(i, 0) to -j*gapPenalty
  for(i <- 0 to matrix.noRows-1) {
      matrix.setValueAt(i, 0, -1*i*gapPenalty)
  }

  def score(row : Int, col: Int) : Int = {
     if (row == 0 || col == 0) return matrix.getValueAt(row, col)
     Utils.max((matrix.getValueAt(row-1, col-1) + substitutionMatrix.valueFor(sequence1(row-1), sequence2(col-1))) ::
        (matrix.getValueAt(row, col-1) -gapPenalty) :: ((matrix.getValueAt(row-1, col) -gapPenalty) :: Nil),
       Utils.MIN_VALUE)
  }

  def computeMaximumScore() : Int = {
     for(i <- 1 to matrix.noRows-1) {
        for(j <- 1 to matrix.noCols-1) {
          matrix.setValueAt(i, j, score(i, j))
      }
     }
     return matrix.getValueAt(matrix.noRows-1, matrix.noCols-1)
  }
  
}