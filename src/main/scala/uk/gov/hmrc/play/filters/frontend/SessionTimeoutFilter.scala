/*
 * Copyright 2017 HM Revenue & Customs
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


import org.joda.time.{DateTime, DateTimeZone, Duration}
import play.api.http.HeaderNames.COOKIE
import play.api.mvc._
import uk.gov.hmrc.play.filters.MicroserviceFilterSupport
import uk.gov.hmrc.play.http.SessionKeys.{lastRequestTimestamp, loginOrigin, redirect}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Filter that clears session data if 'ts' session field has missing, or older than configured timeout.
  *
  * It clears data on the incoming request, so that the controller does not receive any session information.
  * It also changes the SET-COOKIE header for the outgoing request, so that the browser knows the session has expired.
  *
  * A white-list of session values are omitted from this process.
  *
  * @param clock function that supplies the current [[DateTime]]
  * @param timeoutDuration how long an untouched session should be considered valid for
  */
class SessionTimeoutFilter(clock: () => DateTime = () => DateTime.now(DateTimeZone.UTC),
                           timeoutDuration: Duration) extends Filter with MicroserviceFilterSupport {

  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {

    val result =
      if (sessionHasExpired(rh)) {
        f(wipeRequest(rh)).map(result => result.withSession(preservedSessionData(result.session(rh)): _*))
      } else {
        f(rh)
      }

    result.map(_.addingToSession(lastRequestTimestamp -> clock().getMillis.toString)(rh))
  }

  private def sessionHasExpired(requestHeader: RequestHeader): Boolean = {
    extractTimestamp(requestHeader.session).fold(true)(hasExpired(clock))
  }

  private def extractTimestamp(session: Session): Option[DateTime] = {
    try {
      session.get(lastRequestTimestamp) map (t => new DateTime(t.toLong, DateTimeZone.UTC))
    } catch {
      case e: NumberFormatException => None
    }
  }

  private def hasExpired(now: () => DateTime)(timestamp: DateTime): Boolean = {
    val timeOfExpiry = timestamp plus timeoutDuration
    now() isAfter timeOfExpiry
  }

  private def wipeRequest(requestHeader: RequestHeader): RequestHeader = {
    val wipedCookie = Session.encodeAsCookie(Session.deserialize(preservedSessionData(requestHeader.session).toMap))
    val wipedHeaders = requestHeader.headers.replace(COOKIE -> Cookies.encodeCookieHeader(Seq(wipedCookie)))
    requestHeader.copy(headers = wipedHeaders)
  }

  private def preservedSessionData(session: Session): Seq[(String, String)] = for {
    key <- SessionTimeoutFilter.whitelistedSessionKeys.toSeq
    value <- session.get(key)
  } yield key -> value

}

object SessionTimeoutFilter {
  val whitelistedSessionKeys: Set[String] = Set(lastRequestTimestamp, redirect, loginOrigin, "Csrf-Token")
}
