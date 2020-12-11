/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.http4s
package headers

import cats.syntax.all._
import cats.parse.Parser1
import org.http4s.util.Writer

/** {{{
  *   The "Proxy-Authorization" header field allows the client to identify
  *   itself (or its user) to a proxy that requires authentication.
  * }}}
  *
  *  From [[https://tools.ietf.org/html/rfc7235#section-4.4 RFC-7235]]
  */
object `Proxy-Authorization`
    extends HeaderKey.Internal[`Proxy-Authorization`]
    with HeaderKey.Singleton {
  //https://tools.ietf.org/html/rfc7235#section-4.2
  private val parser: Parser1[`Proxy-Authorization`] = {
    import org.http4s.internal.parsing.Rfc7235.credentials
    credentials.map(`Proxy-Authorization`(_))
  }

  override def parse(s: String): ParseResult[`Proxy-Authorization`] =
    parser.parseAll(s).leftMap(err => ParseFailure("Invalid Authorization", err.toString))

  def apply(basic: BasicCredentials): Authorization =
    Authorization(Credentials.Token(AuthScheme.Basic, basic.token))
}

final case class `Proxy-Authorization`(credentials: Credentials) extends Header.Parsed {
  override def key: `Proxy-Authorization`.type = `Proxy-Authorization`
  override def renderValue(writer: Writer): writer.type = credentials.render(writer)
}
