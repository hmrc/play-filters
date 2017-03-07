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

import org.joda.time.{DateTime, Duration}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Matchers, WordSpecLike}
import org.scalatestplus.play.OneAppPerSuite
import play.api.mvc.{AnyContentAsEmpty, RequestHeader, Results, Session}
import play.api.test.{FakeHeaders, FakeRequest}
import uk.gov.hmrc.play.filters.frontend.SessionTimeoutFilter.whitelistedSessionKeys
import uk.gov.hmrc.play.http.SessionKeys.{authToken, lastRequestTimestamp, loginOrigin}

import scala.concurrent.Future

class SessionTimeoutFilterSpec extends WordSpecLike with Matchers with ScalaFutures with OneAppPerSuite {

  "SessionTimeoutFilter" should {

    val now = new DateTime(2017, 1, 12, 14, 56)
    val timeoutDuration = Duration.standardMinutes(1)
    val clock = () => now
    val filter = new SessionTimeoutFilter(clock, timeoutDuration)

    "strip non-whitelist session variables from request if timestamp is old" in {
      val timestamp = now.minusMinutes(5).getMillis.toString
      implicit val rh = exampleRequest.withSession(lastRequestTimestamp -> timestamp, authToken -> "a-token")

      filter.apply { req =>
        req.session should onlyContainWhitelistedKeys
        req.session.get(lastRequestTimestamp) shouldBe Some(timestamp)
        Future.successful(Results.Ok)
      }(rh)
    }

    "strip non-whitelist session variables from result if timestamp is old" in {
      val timestamp = now.minusMinutes(5).getMillis.toString
      implicit val rh = exampleRequest.withSession(lastRequestTimestamp -> timestamp, loginOrigin -> "gg", authToken -> "a-token")

      val result = filter(successfulResult)(rh)

      result.futureValue.session should onlyContainWhitelistedKeys
      result.futureValue.session.get(loginOrigin) shouldBe Some("gg")
    }

    "pass through all session values if timestamp is recent" in {
      val timestamp = now.minusSeconds(5).getMillis.toString
      implicit val rh = exampleRequest.withSession(lastRequestTimestamp -> timestamp, authToken -> "a-token", "custom" -> "custom")

      val result = filter.apply { req =>
        req.session.get("custom") shouldBe Some("custom")
        Future.successful(Results.Ok)
      }(rh)

      result.futureValue.session.get("custom") shouldBe Some("custom")
    }

    "strip only auth token from request if timestamp is missing" in {
      implicit val rh = exampleRequest.withSession(authToken -> "a-token", "custom" -> "custom")

      filter.apply { req =>
        req.session.get(authToken) shouldBe None
        req.session.get("custom") shouldBe Some("custom")
        Future.successful(Results.Ok)
      }(rh)
    }

    "strip auth token from result if timestamp is missing" in {
      implicit val rh = exampleRequest.withSession(authToken -> "a-token", loginOrigin -> "gg", "custom" -> "custom")

      val result = filter(successfulResult)(rh)

      result.futureValue.session.get(authToken) shouldBe None
      result.futureValue.session.get(loginOrigin) shouldBe Some("gg")
      result.futureValue.session.get("custom") shouldBe Some("custom")
    }

    "update old timestamp with current time" in {
      implicit val rh = exampleRequest.withSession(lastRequestTimestamp -> now.minusDays(1).getMillis.toString)
      val result = filter.apply(successfulResult)(rh)
      result.futureValue.session.get(lastRequestTimestamp) shouldBe Some(now.getMillis.toString)
    }

    "update recent timestamp with current time" in {
      implicit val rh = exampleRequest.withSession(lastRequestTimestamp -> now.minusSeconds(1).getMillis.toString)
      val result = filter.apply(successfulResult)(rh)
      result.futureValue.session.get(lastRequestTimestamp) shouldBe Some(now.getMillis.toString)
    }

    "ignore an invalid timestamp" in {
      implicit val rh = exampleRequest.withSession(lastRequestTimestamp -> "invalid-format")
      val result = filter.apply(successfulResult)(rh)
      result.futureValue.session.get(lastRequestTimestamp) shouldBe Some("invalid-format")
    }

    "do not add timestamp if it is missing" in {
      implicit val rh = exampleRequest.withSession()
      val result = filter.apply(successfulResult)(rh)
      result.futureValue.session.get(lastRequestTimestamp) shouldBe None
    }

  }

  private def exampleRequest = FakeRequest("POST", "/something", FakeHeaders(), AnyContentAsEmpty)
  private val successfulResult = (rh: RequestHeader) => Future.successful(Results.Ok)

  private def onlyContainWhitelistedKeys = new Matcher[Session] {
    override def apply(session: Session): MatchResult = {
      MatchResult(
        (session.data.keySet -- whitelistedSessionKeys).isEmpty,
        s"""Session keys ${session.data.keySet} did not contain only whitelisted keys: $whitelistedSessionKeys""",
        s"""Session keys ${session.data.keySet} contained only whitelisted keys: $whitelistedSessionKeys"""
      )
    }
  }
}
