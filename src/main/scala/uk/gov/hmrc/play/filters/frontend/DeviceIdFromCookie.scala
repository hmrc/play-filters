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

import play.api.mvc.Cookie

trait DeviceIdFromCookie extends DeviceIds {

  def getTimeStamp = Some(System.currentTimeMillis())

  def generateUUID = UUID.randomUUID().toString

  def generateDeviceId(uuid: String = generateUUID): DeviceId = {
    val timestamp = getTimeStamp
    DeviceId(uuid, timestamp, generateHash(uuid, timestamp))
  }

  def buildNewDeviceIdCookie(): Cookie = {
    val deviceId = generateDeviceId()
    makeCookie(deviceId)
  }

  def makeCookie(deviceId: DeviceId): Cookie = Cookie(MDTPDeviceId, deviceId.value, Some(TenYears))
}
