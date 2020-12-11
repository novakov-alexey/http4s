/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.http4s
package headers

import cats.syntax.all._
import cats.data.NonEmptyList
import cats.parse.Parser1
import org.http4s.internal.parsing.Rfc7235

object `WWW-Authenticate` extends HeaderKey.Internal[`WWW-Authenticate`] with HeaderKey.Recurring {
  val parser: Parser1[`WWW-Authenticate`] = {
    Rfc7235.challenges.map(`WWW-Authenticate`.apply)
  }

  override def parse(s: String): ParseResult[`WWW-Authenticate`] =
    parser.parseAll(s).leftMap(err => ParseFailure("Invalid WWW-Authenticate", err.toString))
}

final case class `WWW-Authenticate`(values: NonEmptyList[Challenge])
    extends Header.RecurringRenderable {
  override def key: `WWW-Authenticate`.type = `WWW-Authenticate`
  type Value = Challenge
}
