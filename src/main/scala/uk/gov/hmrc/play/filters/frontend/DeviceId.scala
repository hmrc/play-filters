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

import org.apache.commons.codec.binary.Base64

import scala.util.Try

case class DeviceId(uuid: String, timestamp:Option[Long], hash: String) {

  def value = timestamp.fold(uuid + "_" + hash)(time => DeviceId.prefix_value + "_" + uuid + "_" + time + "_" + hash)
}

object DeviceId {

  final val prefix_value = "mtdpdi"

  def from(value: String): Option[DeviceId] = {

    value.split("_") match {
      case Array(prefix, uuid, timestamp, hash) if validUuid(uuid) && validHash(hash) && timestamp.toLong > 0 => Some(DeviceId(uuid, Some(timestamp.toLong), hash))
      case Array(uuid, hash) if validUuid(uuid) && validHash(hash) => Some(DeviceId(uuid, None, hash))
      case _ => None
    }
  }

  private def validUuid(uuid: String) = Try{UUID.fromString(uuid)}.isSuccess

  private def validHash(hash: String) = hash.length > 0 && Base64.isBase64(hash)

}
