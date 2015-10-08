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

import play.api.http.HeaderNames
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait DeviceIdFilter extends Filter with DeviceIdFromCookie {

  override def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader) = {

    def findDeviceIdCookie : PartialFunction[Cookie, Cookie] = { case cookie if cookie.name == MDTPDeviceId && !cookie.value.isEmpty => cookie }

    val updatedCookies: Seq[Cookie] = rh.headers.getAll(HeaderNames.COOKIE).flatMap(Cookies.decode)


    val (cookies, newCookie:Option[play.api.mvc.Cookie]) = if (updatedCookies.isEmpty) {
      // No cookies in request. Add a new deviceId cookie to the request.
      val newCookie=buildNewCookie()
      (Seq(newCookie), Some(newCookie))
    } else {
      // Cookies exist. Check for legacy and new format mtdpdi.
      updatedCookies.collectFirst(findDeviceIdCookie) match {

        case Some(deviceId) =>
          if (deviceId.value.split("_").length<=2) {
            // Replace legacy cookie with new format.
            val deviceIdCookie = deviceIdAndCookie(deviceId).fold(buildNewCookie())((a:DeviceFromCookie) => makeCookie(generateDeviceId(a._1.uuid)))

// TODO...FUNCTION!!! CALLED MORE THAN ONCE!!!
            (updatedCookies.filterNot(a => a.name==MDTPDeviceId) ++ Seq(deviceIdCookie), Some(deviceIdCookie))
          } else {

            deviceIdAndCookie(deviceId) match {
              case Some(deviceId) => (updatedCookies, None)

              case None =>
                val deviceIdCookie = buildNewCookie()
// TODO...DUPLICATED...common function.
                (updatedCookies.filterNot(a => a.name==MDTPDeviceId) ++ Seq(deviceIdCookie), Some(deviceIdCookie))
            }
          }

        case None =>
          val newDeviceIdCookie = buildNewCookie()
          (updatedCookies ++ Seq(newDeviceIdCookie), newDeviceIdCookie)
      }
    }

    val updatedHeaders = new Headers {
      override protected val data: Seq[(String, Seq[String])] = {
        (rh.headers.toMap + (HeaderNames.COOKIE -> Seq(Cookies.encode(cookies)))).toSeq
      }
    }

    val updatedInputHeaders = rh.copy(headers = updatedHeaders)


    (next((updatedInputHeaders)).map(a => {
      newCookie.fold(a)(newDeviceIdCookie => a.withCookies(newDeviceIdCookie))
    }
    ))


  }

}
