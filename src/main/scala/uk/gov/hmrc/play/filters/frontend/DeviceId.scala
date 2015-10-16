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

import java.security.MessageDigest
import java.util.UUID
import org.apache.commons.codec.binary.Base64
import scala.util.Try


object DeviceIdData {
  final val token1 = "#"
  final val token2 = "_"
  final val TenYears = 315360000
  final val MDTPDeviceId = "mdtpdi"
}

case class DeviceId(uuid: String, timestamp:Option[Long], hash: String) {

  def value = timestamp.fold(s"$uuid${DeviceIdData.token2}$hash")(time => s"${DeviceIdData.MDTPDeviceId}${DeviceIdData.token1}$uuid${DeviceIdData.token1}$time${DeviceIdData.token2}$hash")
}

trait DeviceIds {
  val secret : String
  val md : MessageDigest


  def generateHash(uuid:String, timestamp:Option[Long]) = {
    val oneWayHash = timestamp.fold(uuid)(time => s"${DeviceIdData.MDTPDeviceId}${DeviceIdData.token1}$uuid${DeviceIdData.token1}$time")
    val digest = md.digest((oneWayHash + secret).getBytes)
    new String(Base64.encodeBase64(digest))
  }

  def deviceIdHashIsValid(hash:String, uuid:String, timestamp:Option[Long]) = hash == generateHash(uuid, timestamp)

  def from(value: String) : Option[DeviceId] = {

    def isValid(uuid:String, timestamp:String, hash:String) = validUuid(uuid) && validLongTime(timestamp) && deviceIdHashIsValid(hash, uuid, Some(timestamp.toLong))

    def isValidLegacy(uuid:String, hash:String) = validUuid(uuid) && deviceIdHashIsValid(hash, uuid, None)

    value.split("(#)|(_)") match {

      case Array(prefix, uuid, timestamp, hash) if isValid(uuid, timestamp, hash) =>
        Some(DeviceId(uuid, Some(timestamp.toLong), hash))

      case Array(uuid, hash) if isValidLegacy(uuid, hash) =>
        Some(DeviceId(uuid, None, hash))

      case _ => None
    }
  }

  private def validUuid(uuid: String) = Try{UUID.fromString(uuid)}.isSuccess
  private def validLongTime(timestamp: String) = Try{timestamp.toLong}.isSuccess

}
