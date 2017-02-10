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

import play.api.mvc._
import uk.gov.hmrc.play.filters.MicroserviceFilterSupport
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.SessionKeys.lastRequestTimestamp
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

class UpdateSessionTimestampFilter(clock: () => Long) extends Filter with MicroserviceFilterSupport {

  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {

    implicit val requestHeader = rh
    implicit val loggingDetails = HeaderCarrier.fromHeadersAndSession(rh.headers, Some(rh.session))

    f(rh).map { result =>
      result.session.get(lastRequestTimestamp) match {
        case Some(_) => result.addingToSession(lastRequestTimestamp -> clock().toString)
        case None => result
      }
    }
  }

}
