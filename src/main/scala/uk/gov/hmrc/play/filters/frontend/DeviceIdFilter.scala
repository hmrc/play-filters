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
import uk.gov.hmrc.play.audit.model.{EventTypes, DataEvent}
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.Future
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.AuditExtensions._

import scala.concurrent.ExecutionContext.Implicits.global


trait DeviceIdFilter extends Filter with DeviceIdCookie {

  def auditConnector: AuditConnector
  def appName: String

  case class CookeResult(cookies:Seq[Cookie], newDeviceIdCookie:Option[Cookie])

  private def findDeviceIdCookie : PartialFunction[Cookie, Cookie] = { case cookie if cookie.name == DeviceId.MdtpDeviceId && !cookie.value.isEmpty => cookie }

  override def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader) = {
    val requestCookies = rh.headers.getAll(HeaderNames.COOKIE).flatMap(Cookies.decode)

    def allCookiesApartFromDeviceId = requestCookies.filterNot(_.name == DeviceId.MdtpDeviceId)

    val cookieResult = requestCookies.collectFirst(findDeviceIdCookie).fold {
        // No deviceId cookie found or empty cookie value. Create new deviceId cookie, add to request and response.
        val newDeviceIdCookie = buildNewDeviceIdCookie()
        CookeResult(allCookiesApartFromDeviceId ++ Seq(newDeviceIdCookie), Some(newDeviceIdCookie))
      } { deviceCookeValueId =>

          DeviceId.from(deviceCookeValueId.value, secret, previousSecrets) match {

            case Some(DeviceId(uuid, None, hash)) =>
              // Replace legacy cookie with new format and add new cookie to response.
              val deviceIdCookie = makeCookie(generateDeviceId(uuid))
              CookeResult(allCookiesApartFromDeviceId ++ Seq(deviceIdCookie), Some(deviceIdCookie))

            case Some(DeviceId(uuid, timestamp, hash)) =>
              // Valid new format cookie. No change to request or response.
              CookeResult(requestCookies, None)

            case None =>
              // Invalid deviceId cookie. Replace invalid cookie from request with new deviceId cookie and return in response.
              val deviceIdCookie = buildNewDeviceIdCookie()
              sendDataEvent(rh, deviceCookeValueId.value, deviceIdCookie.value)
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

  private def sendDataEvent(rh: RequestHeader, badDeviceId:String, goodDeviceId:String) : Unit = {
    val hc = HeaderCarrier.fromHeadersAndSession(rh.headers)
    auditConnector.sendEvent(DataEvent(appName, EventTypes.Failed,
      tags = hc.toAuditTags("deviceIdFilter", rh.path) ++ hc.toAuditTags("tamperedDeviceId", "Hash check failure"),
      detail = getTamperDetails(badDeviceId,goodDeviceId)))
  }

  private def getTamperDetails(tamperDeviceId :String, newDeviceId:String) =
    Map("tamperedDeviceId"  -> tamperDeviceId,
        "newDeviceId"       -> newDeviceId)

}
