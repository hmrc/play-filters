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

import org.joda.time.DateTime
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpecLike}
import play.api.mvc.{AnyContentAsEmpty, Results}
import play.api.test.{FakeHeaders, FakeRequest, WithApplication}

import scala.concurrent.Future

class UpdateSessionTimestampFilterSpec extends WordSpecLike with Matchers with ScalaFutures {

  "UpdateSessionTimestampFilter" should {
    val now = new DateTime(2017, 1, 12, 14, 56)
    val clock: () => Long = () => now.getMillis
    val filter = new UpdateSessionTimestampFilter(clock)

    "update the timestamp if it exists" in new WithApplication {
      val timestamp = now.minusMinutes(1).getMillis.toString
      implicit val rh = FakeRequest("POST", "/something", FakeHeaders(), AnyContentAsEmpty).withSession("ts" -> timestamp)

      val result = filter.apply(requestHeader => Future.successful(Results.Ok))(rh)

      result.futureValue.session.get("ts") shouldBe Some(now.getMillis.toString)
    }

    "do nothing if the timestamp does not exist" in new WithApplication {
      implicit val rh = FakeRequest("POST", "/something", FakeHeaders(), AnyContentAsEmpty)

      val result = filter.apply(requestHeader => Future.successful(Results.Ok))(rh)

      result.futureValue.session.get("ts") shouldBe None
    }
  }

}
