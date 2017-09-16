package org.http4s
package headers

class AcceptCharsetSpec extends HeaderLaws {
  checkAll("Accept-Charset", headerLaws(`Accept-Charset`))

  "is satisfied by a charset if the q value is > 0" in {
    prop { (h: `Accept-Charset`, cs: Charset) =>
      h.qValue(cs) > QValue.Zero ==> { h isSatisfiedBy cs }
    }
  }

  "is not satisfied by a charset if the q value is 0" in {
    prop { (h: `Accept-Charset`, cs: Charset) => !(h.map(_.withQValue(QValue.Zero)) isSatisfiedBy cs) }
  }

  "matches atom before splatted" in {
    val acceptCharset = `Accept-Charset`(CharsetRange.*, CharsetRange.Atom(Charset.`UTF-8`, QValue.q(0.5)))
    acceptCharset.qValue(Charset.`UTF-8`) must_== QValue.q(0.5)
  }

  "matches splatted if atom not present" in {
    val acceptCharset = `Accept-Charset`(CharsetRange.*, CharsetRange.Atom(Charset.`ISO-8859-1`, QValue.q(0.5)))
    acceptCharset.qValue(Charset.`UTF-8`) must_== QValue.One
  }

  "rejects non-matching charsets with no splat" in {
    val acceptCharset = `Accept-Charset`(CharsetRange.Atom(Charset.`ISO-8859-1`, QValue.q(0.5)))
    acceptCharset.qValue(Charset.`UTF-8`) must_== QValue.Zero
  }
}
