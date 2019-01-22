/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.http.HttpVerbs.POST
import play.api.mvc.{Result, _}
import uk.gov.hmrc.play.filters.MicroserviceFilterSupport

import scala.concurrent.Future

class CSRFExceptionsFilter(whitelist: Set[String]) extends Filter with MicroserviceFilterSupport {

  def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    f(filteredHeaders(rh))
  }

  private[filters] def filteredHeaders(rh: RequestHeader, now: () => DateTime = () => DateTime.now.withZone(DateTimeZone.UTC)) =
    if (rh.method == POST && whitelist.contains(rh.path))
      rh.copy(headers = rh.headers.add("Csrf-Token" -> "nocheck"))
    else rh

}

object CSRFExceptionsFilter extends CSRFExceptionsFilter(Set.empty)
