package org.biosequenceanalysis.tests

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

class UtilsTest extends AssertionsForJUnit {

  @Test def shouldReturnMaxValue() {
    assertEquals(3, Utils.max(List(3,1,2), 0))
  }

  @Test def shouldReturnMaxValueForEmptyList() {
    assertEquals(0, Utils.max(List(), 0))
  }
}