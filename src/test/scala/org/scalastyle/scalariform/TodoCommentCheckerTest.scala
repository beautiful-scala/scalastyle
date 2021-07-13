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

class TodoCommentCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "todo.comment"
  val classUnderTest = classOf[TodoCommentChecker]

  @Test def testSourceWithoutTodoComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |/**
        | * Todoy is a misspelling of today
        | */
        |class foobar {
        |  var myField = "one"
        |
        |  // This inline comment contains the word todo
        |  def doSomething(): Unit = {}
        |
        |  // Todoy is a misspelling of today
        |  def doSomethingElse() = ???
        |
        |  // To do this is...
        |  def doNothing() = ???
        |
        |  /*
        |   * Todoy, I'm very bad at spelling today
        |   */
        |   def stillDoNothing() = ???
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testSourceWithTodoComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  // TODO make this a val
        |  var myField = "one"
        |
        |  def doSomething(): Unit = {
        |    // Todo add some logic
        |  }
        |
        |  // ToDo implement this
        |  def doSomethingElse() = ???
        |
        |  def doNothing() = ??? // todo implement this too
        |
        |  def stillDoNothing() = ??? // TODO: implement this too
        |
        |  def nothing() = ??? /* TODO */
        |
        |  /**
        |   * TODO Add some more code
        |   * todo: refactor a little
        |   */
        |
        |   /** TODO reformat */
        |   /* TODO removed all these todos */
        |
        |   val x = "// TODO what about in a string?"
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 2, List("TODO")),
        columnError(9, 4, List("Todo")),
        columnError(12, 2, List("ToDo")),
        columnError(15, 24, List("todo")),
        columnError(17, 29, List("TODO")),
        columnError(19, 22, List("TODO")),
        columnError(21, 2, List("TODO")),
        columnError(26, 3, List("TODO")),
        columnError(27, 3, List("TODO"))
      ),
      source
    )
  }
  @Test def testSourceWithoutFixmeComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |/**
        | * Fix mechanism is a misspelling of fix mechanism
        | */
        |class foobar {
        |  var myField = "one"
        |
        |  // This inline comment contains the word fixme
        |  def doSomething(): Unit = {}
        |
        |  // Fixmechanism is a misspelling of fix mechanism
        |  def doSomethingElse() = ???
        |
        |  // Fix mechanism...
        |  def doNothing() = ???
        |
        |  /*
        |   * Fix mechanism, I'm very bad at spelling fix mechanism
        |   */
        |   def stillDoNothing() = ???
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testSourceWithFixmeComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  // FIXME make this a val
        |  var myField = "one"
        |
        |  def doSomething(): Unit = {
        |    // Fixme add some logic
        |  }
        |
        |  // FixMe implement this
        |  def doSomethingElse() = ???
        |
        |  def doNothing() = ??? // fixme this too
        |
        |  def stillDoNothing() = ??? // FIXME: this too
        |
        |  def nothing() = ??? /* FIXME */
        |
        |  /**
        |   * FIXME Add some more code
        |   * fixme: refactor a little
        |   */
        |
        |   /** FIXME reformat */
        |   /* FIXME removed all these fixmes */
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 2, List("FIXME")),
        columnError(9, 4, List("Fixme")),
        columnError(12, 2, List("FixMe")),
        columnError(15, 24, List("fixme")),
        columnError(17, 29, List("FIXME")),
        columnError(19, 22, List("FIXME")),
        columnError(21, 2, List("FIXME")),
        columnError(26, 3, List("FIXME")),
        columnError(27, 3, List("FIXME"))
      ),
      source
    )
  }

  @Test def testSourceWithAlternativeComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  // AFAIRE make this a val
        |  var myField = "one"
        |
        |  def doSomething(): Unit = {
        |    // afaire add some logic
        |  }
        |
        |  // AFaire implement this
        |  def doSomethingElse() = ???
        |
        |  def doNothing() = ??? // TODO this should be ignore
        |
        |   /** FIXME reformat */
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 2, List("AFAIRE")),
        columnError(9, 4, List("afaire")),
        columnError(12, 2, List("AFaire")),
        columnError(17, 3, List("FIXME"))
      ),
      source,
      params = Map("words" -> "fixme|afaire")
    )
  }
}
