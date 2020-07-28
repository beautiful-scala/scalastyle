// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatestplus.junit.AssertionsForJUnit

// scalastyle:off magic.number

class ThrowCheckerTest extends AssertionsForJUnit with CheckerTest {

  protected val classUnderTest = classOf[ThrowChecker]

  protected val key = "throw"

  @Test def testZeroErrors(): Unit = {
    val source = """
class C1 {
  /* This method does not throw an exception */
  def m1(n: Int) = n

  // This method also doesn't throw an exception */
  def m2(n: Int) = n

  // Use this function to throw away an integer.
  def throwAway(n: Int): Unit = ()
}
"""
    assertErrors(List(), source)
  }

  @Test def testOneError(): Unit = {
    val source = """
class C1 {
  def divide(a: Int, b: Int) = {
    if (b == 0) throw new IllegalArgumentException("please learn how to divide");
    a / b
  }
}
"""
    assertErrors(List(columnError(4, 16)), source)
  }

  @Test def testTwoErrors(): Unit = {
    val source = """
class C1 {
  def impatientOptionUser[A, B](o1: Option[A], o2: Option[B]): (A, B) = {
    val v1: A = o1.getOrElse(throw new Exception("I really wanted a value of type A"))
    val v2: B = o2.getOrElse(throw new Exception("I really wanted a value of type B"))
    (v1, v2)
  }
}
"""
    assertErrors(List(columnError(4, 29), columnError(5, 29)), source)
  }
}
