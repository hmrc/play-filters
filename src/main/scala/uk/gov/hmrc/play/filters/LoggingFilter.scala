/*
 * Copyright 2015 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.play.filters

import java.util.Date
import org.joda.time.DateTimeUtils
import scala.concurrent.Future
import play.api.mvc.{Result, RequestHeader, Filter}
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import org.apache.commons.lang3.time.FastDateFormat

trait LoggingFilter extends Filter {
  private val dateFormat = FastDateFormat.getInstance("yyyy-MM-dd HH:mm:ss.SSSZZ")

  def controllerNeedsLogging(controllerName: String): Boolean

  def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    val startTime = DateTimeUtils.currentTimeMillis()

    val result = next(rh)

    if (needsLogging(rh)) logString(rh, result, startTime).map(Logger.info(_))

    result
  }

  def needsLogging(request: RequestHeader): Boolean = {
    import play.api.Routes
    (for {
      name <- request.tags.get(Routes.ROUTE_CONTROLLER)
    } yield controllerNeedsLogging(name)).getOrElse(true)
  }

  def logString(rh: RequestHeader, result: Future[Result], startTime: Long): Future[String] = {
    val start = dateFormat.format(new Date(startTime))
    def elapsedTime = DateTimeUtils.currentTimeMillis() - startTime

    result.map {
      result => s"$start ${rh.method} ${rh.uri} ${result.header.status} ${elapsedTime}ms"
    }.recover {
      case t => s"$start ${rh.method} ${rh.uri} $t ${elapsedTime}ms"
    }
  }
}
