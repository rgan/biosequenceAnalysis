package org.biosequenceanalysis.tests

import junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

class NeedlemanWunschGlobalAlignmentTest extends AssertionsForJUnit {
    private var nwGlobalAlignment:NeedlemanWunschGlobalAlignment = _
  
    @Before def setup() {
      val rowSequence = List("P", "A", "W" ,"H", "E", "A", "E")
      val colSequence = List("H", "E", "A", "G", "A", "W", "G", "H", "E", "E")
      val blosum50Matrix = MatrixReader.fromFile("src/BLOSUM50.matrix");
      nwGlobalAlignment = new NeedlemanWunschGlobalAlignment(rowSequence, colSequence, blosum50Matrix, 8)
    }

    @Test def shouldReturnMultipleOfGapPenaltyForTopRowAndLeftColumn() {
            assertEquals(0, nwGlobalAlignment.score(0,0))
      assertEquals(-8, nwGlobalAlignment.score(0,1))
      assertEquals(-16, nwGlobalAlignment.score(0,2))
      assertEquals(-8, nwGlobalAlignment.score(1,0))
      assertEquals(-80, nwGlobalAlignment.score(0,10))
      assertEquals(-56, nwGlobalAlignment.score(7,0))
    }

    @Test def shouldReturnScoreForRow1Col1() {
      assertEquals(-2, nwGlobalAlignment.score(1,1))
    }

    @Test def shouldComputeMaxScore() {
      assertEquals(1, nwGlobalAlignment.computeMaximumScore)
    }
}

