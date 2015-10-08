/*
 * Copyright 2015 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.play.filters.frontend

import play.api.Logger
import play.api.http.HeaderNames
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait CookieCryptoFilter extends Filter {

  protected val cookieName: String = Session.COOKIE_NAME
  protected val encrypter: (String) => String
  protected val decrypter: (String) => String

  override def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader) =
    encryptCookie(next(decryptCookie(rh)))

  private def encryptCookieString(cookieValue: String): String = encrypter(cookieValue)

  private def decryptCookieString(encryptedCookieValue: String): String = {

    def decryptValue(value: String) = Try(decrypter(value)) match {
      case Success(result) => result
      case Failure(ex) =>
        Logger.debug("Decryption of session cookie failed!")
        throw ex
    }

    decryptValue(encryptedCookieValue)
  }


  private def decryptCookie(rh: RequestHeader) = rh.copy(headers = new Headers {
    override protected val data: Seq[(String, Seq[String])] = {
      val updatedCookies: Seq[Cookie] = rh.headers.getAll(HeaderNames.COOKIE).flatMap(Cookies.decode).flatMap {
        case cookie if shouldBeEncrypted(cookie) =>
          decryptValue(cookie.value)
            .map(dv => cookie.copy(value = dv))
            .orElse {
            Logger.debug(s"Could not decrypt cookie $cookieName"); None
          }
        case other => Some(other)
      }

      if (updatedCookies.isEmpty)
        rh.headers.toMap - HeaderNames.COOKIE
      else
        rh.headers.toMap + (HeaderNames.COOKIE -> Seq(Cookies.encode(updatedCookies)))
    }.toSeq

    def decryptValue(value: String): Option[String] = Try(decryptCookieString(value)) match {
      case Success(v) => Option(v)
      case _ => None
    }
  })

  private def encryptCookie(f: Future[Result]): Future[Result] = f.map {
    result =>
      val updatedHeader: Option[String] = result.header.headers.get(HeaderNames.SET_COOKIE).map {
        cookieHeader =>
          Cookies.encode(Cookies.decode(cookieHeader).map {
            case cookie if shouldBeEncrypted(cookie) => cookie.copy(value = encryptCookieString(cookie.value))
            case other => other
          })
      }

      updatedHeader.map(header => result.withHeaders(HeaderNames.SET_COOKIE -> header)).getOrElse(result)
  }

  private def shouldBeEncrypted(cookie: Cookie) = cookie.name == cookieName && !cookie.value.isEmpty
}
