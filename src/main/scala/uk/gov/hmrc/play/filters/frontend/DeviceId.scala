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


trait DeviceIdData {
  final val token1 = "#"
  final val token2 = "_"
  final val prefix_value = "mtdpdi"
  final val TenYears = 315360000
  final val MDTPDeviceId = "mdtpdi"
}

case class DeviceId(uuid: String, timestamp:Option[Long], hash: String) extends DeviceIdData {

  def value = timestamp.fold(s"$uuid$token2$hash")(time => s"${prefix_value}$token1$uuid$token1$time$token2$hash")
}

// TODO...NAMING!!! NOT PLURAL!

// DEVICEID CONSTRUCTION/DECONSTRUCTION!!! TODO...

trait DeviceIds extends DeviceIdData {
  val secret : String
  val md : MessageDigest

  def generateHash(uuid:String, timestamp:Option[Long]) = {
    val oneWayHash = timestamp.fold(uuid)(time => s"${prefix_value}$token1$uuid$token1$time")
    val digest = md.digest((oneWayHash + secret).getBytes)
    new String(Base64.encodeBase64(digest))
  }

  def deviceIdHashIsValid(hash:String, uuid:String, timestamp:Option[Long]) = hash == generateHash(uuid, timestamp)

  def from(value: String): Option[DeviceId] = {

    def isValid(uuid:String, timestamp:String, hash:String) = validUuid(uuid) && validLongTime(timestamp) && deviceIdHashIsValid(hash, uuid, Some(timestamp.toLong))

    value.split("(#)|(_)") match {

      case Array(prefix, uuid, timestamp, hash) if isValid(uuid, timestamp, hash) =>
        Some(DeviceId(uuid, Some(timestamp.toLong), hash))

      case Array(uuid, hash) if validUuid(uuid) && deviceIdHashIsValid(hash, uuid, None) =>
        Some(DeviceId(uuid, None, hash))

      case _ => None
    }
  }

  private def validUuid(uuid: String) = Try{UUID.fromString(uuid)}.isSuccess
  private def validLongTime(timestamp: String) = Try{timestamp.toLong}.isSuccess

}
