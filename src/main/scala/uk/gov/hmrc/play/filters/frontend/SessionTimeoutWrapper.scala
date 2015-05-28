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

import org.joda.time.{DateTime, DateTimeZone, Duration}
import play.api.mvc._

/**
 * This is a duplicate of the same class in play-authorised-frontend; if making changes here, do the same there - sorry
 */
object SessionTimeoutWrapper {
  val timeoutSeconds = 900

  val lastRequestTimestamp = "ts"

  def userNeedsNewSession(session: Session, now: () => DateTime): Boolean = {
    extractTimestamp(session) match {
      case Some(time) => !isTimestampValid(time, now)
      case _ => false
    }
  }

  private def isTimestampValid(timestamp: DateTime, now: () => DateTime): Boolean = {
    val timeOfExpiry = timestamp plus Duration.standardSeconds(timeoutSeconds)
    now() isBefore timeOfExpiry
  }

  private def extractTimestamp(session: Session): Option[DateTime] = {
    try {
      session.get(lastRequestTimestamp) map (timestamp => new DateTime(timestamp.toLong, DateTimeZone.UTC))
    } catch {
      case e: NumberFormatException => None
    }
  }
}
