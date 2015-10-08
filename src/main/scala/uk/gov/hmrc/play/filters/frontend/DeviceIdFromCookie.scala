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
import play.api.mvc.{Cookie, Request}

trait DeviceIdFromCookie {

  type DeviceFromCookie = (DeviceId, Boolean, Cookie)
  
  val TenYears = 315360000
  val MDTPDeviceId = "mdtpdi"

// TODO...SECRET!!!
  private val md = MessageDigest.getInstance("MD5")

  // TODO...OVERRIDE IN TEST!
  private val secret = " SOME SECRET" // Play.configuration.getString("DeviceMD5").getOrElse(throw new IllegalArgumentException("configuration requires value for DeviceMD5"))

  def deviceIdAndCookie(deviceIdCookie:Cookie): Option[DeviceFromCookie] =
    for {
      deviceId <- DeviceId.from(deviceIdCookie.value)
      if deviceIdHashIsValid(deviceId)
    } yield (deviceId, true, deviceIdCookie)  // TODO...REMOVE!!!

  def deviceIdCookie(request: Request[_]): Option[Cookie] = {
    request.cookies.get(MDTPDeviceId)
  }

  def getTimeStamp = Some(System.currentTimeMillis())

  def generateDeviceId(uuid: String = generateUUID): DeviceId = {

// TODO...REPEATED IN DEVICE ID!!!
    val timestamp = getTimeStamp
    DeviceId(uuid, timestamp, generateHash(uuid, timestamp))
  }

  def generateUUID = UUID.randomUUID().toString

  def generateHash(uuid:String, timestamp:Option[Long]) = {
    val toHash = timestamp.fold(uuid)(time => DeviceId.prefix_value + "_" + uuid + "_" + time)
    val digest = md.digest((toHash + secret).getBytes)
    new String(Base64.encodeBase64(digest))
  }

  def deviceIdHashIsValid(deviceId: DeviceId) = deviceId.hash == generateHash(deviceId.uuid, deviceId.timestamp)

  def buildNewCookie(): Cookie = {
    val deviceId = generateDeviceId()
    makeCookie(deviceId)
  }

  def makeCookie(deviceId: DeviceId): Cookie = Cookie(MDTPDeviceId, deviceId.value, Some(TenYears))
}
