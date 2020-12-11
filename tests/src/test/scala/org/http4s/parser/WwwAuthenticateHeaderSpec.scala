/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.http4s
package parser

import cats.data.NonEmptyList
import org.http4s.headers.`WWW-Authenticate`
import org.specs2.mutable.Specification

class WwwAuthenticateHeaderSpec extends Specification with HeaderParserHelper[`WWW-Authenticate`] {
  def hparse(value: String): ParseResult[`WWW-Authenticate`] =
    `WWW-Authenticate`.parse(value)

  override def parse(value: String) =
    hparse(value).fold(err => sys.error(s"Couldn't parse: `$value`, ${err.details}"), identity)

  val params = Map("a" -> "b", "c" -> "d")
  val c = Challenge("Basic", "foo")

  val str = "Basic realm=\"foo\""

  val wparams = c.copy(params = params)

  "WWW-Authenticate Header parser" should {
    "Render challenge correctly" in {
      c.renderString must be_==(str)
    }

    "Parse a basic authentication" in {
      parse(str) must be_==(`WWW-Authenticate`(c))
    }

    "Parse a basic authentication with params" in {
      parse(wparams.renderString) must be_==(`WWW-Authenticate`(wparams))
    }

    "Parse multiple concatenated authentications" in {
      val twotypes = "Newauth realm=\"apps\", Basic realm=\"simple\""
      val twoparsed = Challenge("Newauth", "apps") :: Challenge("Basic", "simple") :: Nil

      parse(twotypes).values.toList must be_==(twoparsed)
    }

    "parse multiple concatenated authentications with params" in {
      val twowparams =
        "Newauth realm=\"apps\", type=1, title=\"Login to apps\", Basic realm=\"simple\""
      val twp = Challenge("Newauth", "apps", Map("type" -> "1", "title" -> "Login to apps")) ::
        Challenge("Basic", "simple") :: Nil

      parse(twowparams).values.toList must be_==(twp)
    }
  }
}
