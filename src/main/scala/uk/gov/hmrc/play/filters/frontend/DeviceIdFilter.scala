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

import java.security.MessageDigest

import play.api.http.HeaderNames
import play.api.mvc._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


trait DeviceIdFilter extends Filter with DeviceIdFromCookie {

  case class CookeResult(cookies:Seq[Cookie], newDeviceIdCookie:Option[Cookie])

  private def findDeviceIdCookie : PartialFunction[Cookie, Cookie] = { case cookie if cookie.name == MDTPDeviceId && !cookie.value.isEmpty => cookie }

  override def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader) = {

    val requestCookies: Seq[Cookie] = rh.headers.getAll(HeaderNames.COOKIE).flatMap(Cookies.decode)

    def allCookiesApartFromDeviceId = requestCookies.filterNot(_.name == MDTPDeviceId)

    val cookieResult = requestCookies.collectFirst(findDeviceIdCookie).fold {
        // No deviceId cookie found. Create new deviceId cookie, add to request and response.
        val newDeviceIdCookie = buildNewDeviceIdCookie()
        CookeResult(requestCookies ++ Seq(newDeviceIdCookie), Some(newDeviceIdCookie))
      } { deviceCookeValueId =>
          from(deviceCookeValueId.value) match {

            case Some(DeviceId(uuid, None, hash)) =>
              // Replace legacy cookie with new format. Replace existing cookie from request and add new cookie to response.
              val deviceIdCookie = makeCookie(generateDeviceId(uuid))
              CookeResult(allCookiesApartFromDeviceId ++ Seq(deviceIdCookie), Some(deviceIdCookie))

            case Some(DeviceId(uuid, timestamp, hash)) =>
              // Valid new format cookie. No change to request or response.
              CookeResult(requestCookies, None)

            case None =>
              // Invalid deviceId cookie. Replace invalid cookie from request with new deviceId cookiem and return in response.
              val deviceIdCookie = buildNewDeviceIdCookie()
              CookeResult(allCookiesApartFromDeviceId ++ Seq(deviceIdCookie), Some(deviceIdCookie))
          }
      }

    val updatedInputHeaders = rh.copy(headers = new Headers {
      override protected val data: Seq[(String, Seq[String])] = {
        (rh.headers.toMap + (HeaderNames.COOKIE -> Seq(Cookies.encode(cookieResult.cookies)))).toSeq
      }
    })

    next((updatedInputHeaders)).map(theHttpResponse => {
      cookieResult.newDeviceIdCookie.fold(theHttpResponse)(newDeviceIdCookie => theHttpResponse.withCookies(newDeviceIdCookie))
    })

  }

}
