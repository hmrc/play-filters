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

import java.util.UUID

import play.api.libs.iteratee.Iteratee
import play.api.mvc._

object HeadersFilter extends HeadersFilter

trait HeadersFilter extends EssentialFilter {

  val xRequestId = "X-Request-ID"
  val xRequestTimestamp = "X-Request-Timestamp"

  def apply(nextAction: EssentialAction): EssentialAction = new EssentialAction {
    def apply(request: RequestHeader): Iteratee[Array[Byte], Result] = {
      request.session.get(xRequestId) match {
        case Some(s) => nextAction(request)
        case _ => nextAction(addHeaders(request))
      }
    }

    def addHeaders(request: RequestHeader): RequestHeader = {
      val rid = s"govuk-tax-${UUID.randomUUID().toString}"
      val requestIdHeader: (String, Seq[String]) = xRequestId -> Seq(rid)
      val requestTimestampHeader = xRequestTimestamp -> Seq(System.nanoTime().toString)

      val newHeaders = new Headers {
        val data: Seq[(String, Seq[String])] = (request.headers.toMap + requestIdHeader + requestTimestampHeader).toSeq
      }

      request.copy(headers = newHeaders)
    }
  }
}
