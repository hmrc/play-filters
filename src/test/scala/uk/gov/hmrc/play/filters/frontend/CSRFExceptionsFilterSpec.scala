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

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.mock.MockitoSugar
import org.scalatest.{Matchers, WordSpecLike}
import play.api.mvc.AnyContentAsEmpty
import play.api.test.{WithApplication, FakeHeaders, FakeRequest}

class CSRFExceptionsFilterSpec extends WordSpecLike with Matchers with MockitoSugar {

  private val now = () =>  DateTime.now().withZone(DateTimeZone.UTC)

  "CSRF exceptions filter" should {

    "do nothing if POST request and not ida/login" in new WithApplication {
      val validTime = now().minusSeconds(SessionTimeoutWrapper.timeoutSeconds/2).getMillis.toString
      val rh = FakeRequest("POST", "/something", FakeHeaders(), AnyContentAsEmpty).withSession("ts" -> validTime)

      val requestHeader = CSRFExceptionsFilter.filteredHeaders(rh, now)

      requestHeader.headers.get("Csrf-Token") shouldBe None
    }

    "do nothing for GET requests" in new WithApplication {
      val rh = FakeRequest("GET", "/ida/login", FakeHeaders(), AnyContentAsEmpty)

      val requestHeader = CSRFExceptionsFilter.filteredHeaders(rh)

      requestHeader.headers.get("Csrf-Token") shouldBe None
    }

    "add Csrf-Token header with value nocheck to bypass validation for ida/login POST request" in new WithApplication {
      val rh = FakeRequest("POST", "/ida/login", FakeHeaders(), AnyContentAsEmpty)

      val requestHeader = CSRFExceptionsFilter.filteredHeaders(rh)

      requestHeader.headers("Csrf-Token") shouldBe "nocheck"
    }

    "add Csrf-Token header with value nocheck to bypass validation for SSO POST request" in new WithApplication {
      val rh = FakeRequest("POST", "/ssoin", FakeHeaders(), AnyContentAsEmpty)

      val requestHeader = CSRFExceptionsFilter.filteredHeaders(rh)

      requestHeader.headers("Csrf-Token") shouldBe "nocheck"
    }

    "add Csrf-Token header with value nocheck to bypass validation of posting using cached HTML partial for error reporting" in new WithApplication {
      val rh = FakeRequest("POST", "/contact/problem_reports", FakeHeaders(), AnyContentAsEmpty)

      val requestHeader = CSRFExceptionsFilter.filteredHeaders(rh)

      requestHeader.headers("Csrf-Token") shouldBe "nocheck"
    }

    "add Csrf-Token header with value nocheck to bypass validation if the session has expired" in new WithApplication {
      val invalidTime = new DateTime(2012, 7, 7, 4, 6, 20, DateTimeZone.UTC).minusDays(1).getMillis.toString
      val rh = FakeRequest("POST", "/some/post", FakeHeaders(), AnyContentAsEmpty).withSession("ts" -> invalidTime)

      val requestHeader = CSRFExceptionsFilter.filteredHeaders(rh, now)

      requestHeader.headers("Csrf-Token") shouldBe "nocheck"
    }

  }

}
