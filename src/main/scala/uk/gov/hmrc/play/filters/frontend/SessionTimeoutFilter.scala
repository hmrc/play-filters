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
import uk.gov.hmrc.play.http.SessionKeys._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Filter that manipulates session data if 'ts' session field is older than configured timeout.
  *
  * If the 'ts' has expired, we wipe the session, and update the 'ts'.
  * If the 'ts' doesn't exist, or is invalid, we just wipe the authToken.
  *
  * This filter clears data on the incoming request, so that the controller does not receive any session information.
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

    val updateTimestamp: (Result) => Result =
      result => result.addingToSession(lastRequestTimestamp -> clock().getMillis.toString)(rh)

    val wipeAllFromSessionCookie: (Result) => Result =
      result => result.withSession(preservedSessionData(result.session(rh)): _*)

    val wipeAuthTokenFromSessionCookie: (Result) => Result =
      result => result.withSession(result.session(rh) - authToken)

    extractTimestamp(rh.session) match {
      case Some(ts) if hasExpired(ts) =>
        f(wipeSession(rh))
          .map(wipeAllFromSessionCookie)
          .map(updateTimestamp)
      case Some(ts) =>
        f(rh)
          .map(updateTimestamp)
      case _ =>
        f(wipeAuthToken(rh))
          .map(wipeAuthTokenFromSessionCookie)
    }
  }

  private def extractTimestamp(session: Session): Option[DateTime] = {
    try {
      session.get(lastRequestTimestamp) map (t => new DateTime(t.toLong, DateTimeZone.UTC))
    } catch {
      case e: NumberFormatException => None
    }
  }

  private def hasExpired(timestamp: DateTime): Boolean = {
    val timeOfExpiry = timestamp plus timeoutDuration
    clock() isAfter timeOfExpiry
  }

  private def wipeSession(requestHeader: RequestHeader): RequestHeader = {
    val sessionMap: Map[String, String] = preservedSessionData(requestHeader.session).toMap
    mkRequest(requestHeader, Session.deserialize(sessionMap))
  }

  private def wipeAuthToken(requestHeader: RequestHeader): RequestHeader = {
    mkRequest(requestHeader, requestHeader.session - authToken)
  }

  private def mkRequest(requestHeader: RequestHeader, session: Session): RequestHeader = {
    val wipedCookie = Session.encodeAsCookie(session)
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
