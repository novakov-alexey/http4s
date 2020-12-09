/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.http4s
package headers

import cats.syntax.all._
import org.http4s.util.{CaseInsensitiveString, Writer}
import cats.parse._

object Authorization extends HeaderKey.Internal[Authorization] with HeaderKey.Singleton {
  //https://tools.ietf.org/html/rfc7235#section-4.2
  private val parser: Parser1[Authorization] = {
    import Parser.char
    import cats.parse.Rfc5234.sp

    import org.http4s.internal.parsing.Rfc7230.{headerRep1, token, quotedString}
    import org.http4s.internal.parsing.Rfc7235.token68

    //auth-scheme = token
    val scheme = token.map(CaseInsensitiveString(_))
    //val authParamValue = token.orElse(quotedString)
    // auth-param = token BWS "=" BWS ( token / quoted-string )
    val authParam: Parser1[(String, String)] = (token ~ char('=').void ~ quotedString).map{
      case ((k, _), str) => k -> str
    }

    val tokenCred: Parser1[Credentials] = ((scheme <* sp) ~ token68).map { case (scheme, token) =>
      Credentials.Token(scheme, token)
    }

    val params: Parser1[Credentials] = ((scheme <* sp) ~ headerRep1(authParam)).map {
      case (scheme, params) => Credentials.AuthParams(scheme, params)
    }

    tokenCred.orElse1(params).map(Authorization(_))
  }

  override def parse(s: String): ParseResult[Authorization] =
    parser.parseAll(s).leftMap(err => ParseFailure("Invalid Authorization", err.toString))

  def apply(basic: BasicCredentials): Authorization =
    Authorization(Credentials.Token(AuthScheme.Basic, basic.token))
}

final case class Authorization(credentials: Credentials) extends Header.Parsed {
  override def key: `Authorization`.type = `Authorization`
  override def renderValue(writer: Writer): writer.type = credentials.render(writer)
}
