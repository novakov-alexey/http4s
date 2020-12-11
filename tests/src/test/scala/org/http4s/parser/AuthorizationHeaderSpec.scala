/*
 * Copyright 2013-2020 http4s.org
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.http4s
package parser

import cats.data.NonEmptyList
import org.http4s.headers.Authorization

class AuthorizationHeaderSpec extends Http4sSpec {
  def hparse(value: String) = Authorization.parse(value)

  "Authorization header" should {
    "Parse a valid OAuth2 header" in {
      val token = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-._~+/".toSeq).mkString
      val h = Authorization(Credentials.Token(AuthScheme.Bearer, token + "="))
      hparse(h.value) must beRight(h)
    }

    "Reject an invalid OAuth2 header" in {
      val invalidTokens = Seq("f!@", "=abc", "abc d")
      forall(invalidTokens) { token =>
        val h = Authorization(Credentials.Token(AuthScheme.Bearer, token))
        hparse(h.value) must beLeft
      }
    }

    "Parse a KeyValueCredentials header" in {
      val scheme = "foo"
      val params = NonEmptyList.of("abc" -> "123")
      val h = Authorization(Credentials.AuthParams(scheme.ci, params))
      hparse(h.value) must beRight(h)
    }

    "Parse a KeyValueCredentials header unquoted" in {
      val scheme = "foo"
      val params = NonEmptyList.of("abc" -> "123")
      val h = Authorization(Credentials.AuthParams(scheme.ci, params))
      hparse("foo abc=123") must beRight(h)
    }

    "Parse a KeyValueCredentials with weird spaces" in {
      val scheme = "foo"
      hparse("foo abc = \"123 yeah\tyeah yeah\"") must beRight(
        Authorization(
          Credentials.AuthParams(scheme.ci, NonEmptyList.of("abc" -> "123 yeah\tyeah yeah"))))

      //quoted-pair
      //TODO: Not sure how to interpret this
      hparse("foo abc = \"\\123\"") must beRight(
        Authorization(Credentials.AuthParams(scheme.ci, NonEmptyList.of("abc" -> "\\123"))))
    }
  }
}
