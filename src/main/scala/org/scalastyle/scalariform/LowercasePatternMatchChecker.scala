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

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._ // scalastyle:ignore underscore.import
import _root_.scalariform.parser.CasePattern
import _root_.scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.visit

class LowercasePatternMatchChecker extends ScalariformChecker {
  val errorKey = "lowercase.pattern.match"

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      f <- visit(map)(ast.immediateChildren.head)
      t <- matches(f.pattern.tokens, Nil)
    } yield PositionError(t.offset)

    it
  }

  @scala.annotation.tailrec
  private def matches(tokens: List[Token], res: List[Token]): List[Token] =
    tokens.headOption match {
      case None => res
      case Some(t @ Token(VARID, text, _, _)) if text.headOption.exists(_.isLower) =>
        val tail = tokens.tail
        tail.headOption match {
          case Some(Token(COMMA | RPAREN | AT, _, _, _)) =>
            matches(tail.tail, t :: res)
          case None => t :: res
          case _    => matches(tail.tail, res)
        }
      case _ => matches(tokens.tail, res)
    }

  private def map(t: CasePattern): List[CasePattern] =
    List(t) ::: visit(map)(t.pattern) ::: visit(map)(t.guardOption)
}
