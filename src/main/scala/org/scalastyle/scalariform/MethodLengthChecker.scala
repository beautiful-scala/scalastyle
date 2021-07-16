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

// scalastyle:off underscore.import
import scala.annotation.tailrec

import _root_.scalariform.lexer._
import _root_.scalariform.parser._

import org.scalastyle._
import org.scalastyle.scalariform.VisitorHelper.Clazz
import org.scalastyle.scalariform.VisitorHelper.visit

class MethodLengthChecker extends CombinedChecker {
  val errorKey = "method.length"
  val DefaultMaximumLength = 50
  val DefaultIgnoreComments = false
  val DefaultIgnoreEmpty = false

  private val SinglelineComment = "//"
  private val MultilineCommentsOpener = "/*"
  private val MultilineCommentsCloser = "*/"

  case class FunDefOrDclClazz(t: FunDefOrDcl, position: Option[Int], subs: List[FunDefOrDclClazz])
      extends Clazz[FunDefOrDcl]()

  @tailrec
  private def getInnerExpr(expr: Expr): ExprElement = expr.contents match {
    case List(subexpr) =>
      subexpr match {
        case t: Expr => getInnerExpr(t)
        case t: Any  => t
      }
    case _ => expr
  }

  private def getNextToken(tokens: List[Token], ignoreComments: Boolean): Option[Token] =
    tokens.tail.headOption match {
      case x @ Some(tok) if !ignoreComments =>
        tok.associatedWhitespaceAndComments.tokens.collectFirst { case t: Comment => t.token }.orElse(x)
      case x: Any => x
    }

  private def getNextLastToken(tokens: List[Token], ignoreComments: Boolean): Option[Token] = {
    val lastTwoTokens = tokens.drop(tokens.length - 2)
    val lastComment = lastTwoTokens.lastOption match {
      case Some(tok) if !ignoreComments =>
        tok.associatedWhitespaceAndComments.tokens.reverse.find(_.isInstanceOf[Comment]).map(_.token)
      case _ => None
    }
    lastComment.orElse(lastTwoTokens.headOption)
  }

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val maxLength = getInt("maxLength", DefaultMaximumLength)
    val ignoreComments = getBoolean("ignoreComments", DefaultIgnoreComments)
    val ignoreEmpty = getBoolean("ignoreEmpty", DefaultIgnoreEmpty)

    val lines = ast.lines
    val it = for {
      t <- localvisit(ast.compilationUnit.immediateChildren.head)
      f <- traverse(t)
      body <- f.t.funBodyOpt match {
        case Some(t: ProcFunBody) => Some(t.bodyBlock)
        case Some(t: ExprFunBody) => Some(getInnerExpr(t.body))
        case _                    => None
      }
      isBlock = body.isInstanceOf[BlockExpr]
      tokens = body.tokens
      headToken     <- if (isBlock) getNextToken(tokens, ignoreComments) else tokens.headOption
      lastToken     <- if (isBlock) getNextLastToken(tokens, ignoreComments) else tokens.lastOption
      begLineColumn <- lines.toLineColumn(headToken.offset)
      endLineColumn <- lines.toLineColumn(lastToken.lastCharacterOffset)
      numLines = methodLength(lines.lines, begLineColumn, endLineColumn, ignoreComments, ignoreEmpty)
      if numLines > maxLength
    } yield PositionError(t.position.get, List(s"$numLines > $maxLength"))

    it
  }

  private def traverse(t: FunDefOrDclClazz): List[FunDefOrDclClazz] = t :: t.subs.flatMap(traverse)

  // scalastyle:off method.length cyclomatic.complexity
  private def methodLength(
    lines: Array[Line],
    begLineColumn: LineColumn, // included, 1-based
    endLineColumn: LineColumn, // included, 1-based
    ignoreComments: Boolean,
    ignoreEmpty: Boolean
  ): Int = {
    val begLine = begLineColumn.line - 1 // 0-based, included
    if (ignoreComments) {
      var count = 0
      var multilineComment = false
      val endLine = endLineColumn.line - 1 // 0-based, included
      for {
        i <- begLine to endLine
        lineText = lines(i).text
        slcIndex = lineText.indexOf(SinglelineComment)
        if !(slcIndex == 0 || ignoreEmpty && lineText.isEmpty)
      } {
        val begIndex = if (i == begLine) begLineColumn.column else 0
        val endIndex = {
          val idx = if (slcIndex < 0) lineText.length else slcIndex
          if (i == endLine) math.min(endLineColumn.column + 1, idx) else idx
        }
        @tailrec
        def iter(index: Int, res: Boolean = false): Boolean = {
          val delim = if (multilineComment) MultilineCommentsCloser else MultilineCommentsOpener
          val nextIndex = {
            val idx = lineText.indexOf(delim, index)
            if (idx < 0 || idx > endIndex) endIndex else idx
          }
          val nextRes = res || !multilineComment && {
            val idx = lineText.indexWhere(!Character.isSpaceChar(_), index)
            idx >= 0 && idx < nextIndex
          }
          if (nextIndex == endIndex) { nextRes }
          else {
            multilineComment = !multilineComment
            iter(nextIndex + delim.length, nextRes)
          }
        }
        if (iter(begIndex)) count += 1
      }
      count
    } else {
      val endLine = endLineColumn.line // 0-based, excluded
      if (ignoreEmpty) {
        lines.view.slice(begLine, endLine).count(_.text.nonEmpty)
      } else {
        endLine - begLine
      }
    }
  }
  // scalastyle:on method.length

  private def localvisit(ast: Any): List[FunDefOrDclClazz] =
    ast match {
      case t: FunDefOrDcl => List(FunDefOrDclClazz(t, Some(t.nameToken.offset), localvisit(t.localDef)))
      case t: Any         => visit(t, localvisit)
    }
}
