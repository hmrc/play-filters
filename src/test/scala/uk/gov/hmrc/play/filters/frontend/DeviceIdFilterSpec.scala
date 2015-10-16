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

import org.mockito.{Matchers, ArgumentCaptor}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import play.api.mvc._
import play.api.test.{FakeApplication, FakeRequest}
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{EventTypes, DataEvent}
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.Future

trait FakePlayApplication extends WithFakeApplication {
  this: Suite =>
  override lazy val fakeApplication = FakeApplication()
}

class DeviceIdFilterSpec extends UnitSpec with WithFakeApplication with ScalaFutures with MockitoSugar with BeforeAndAfterEach with FakePlayApplication with TypeCheckedTripleEquals with Inspectors {

  lazy val timestamp = Some(System.currentTimeMillis())

  private trait Setup extends Results {

    val normalCookie1 = Cookie("AnotherCookie1", "normalValue1")

    val resultFromAction: Result = Ok

    lazy val action = {
      val mockAction = mock[(RequestHeader) => Future[Result]]
      val outgoingResponse = Future.successful(resultFromAction)
      when(mockAction.apply(any())).thenReturn(outgoingResponse)
      mockAction
    }

    lazy val filter = new DeviceIdFilter {
      lazy val cookie = super.buildNewDeviceIdCookie()

      lazy val auditConnector = mock[AuditConnector]

      val appName = "SomeAppName"

      override val md = MessageDigest.getInstance("MD5")

      override val secret = "SOMESECRET"

      override def getTimeStamp = timestamp

      override def buildNewDeviceIdCookie(): Cookie = cookie
    }

    lazy val newFormatGoodCookieDeviceId = filter.cookie


    def requestPassedToAction: RequestHeader = {
      val updatedRequest = ArgumentCaptor.forClass(classOf[RequestHeader])
      verify(action).apply(updatedRequest.capture())
      updatedRequest.getValue
    }

    def getCookieStringValue(input:String): Array[String] = input.substring((DeviceIdData.MDTPDeviceId+"_").length,input.length).split(";")

    def generateDeviceIdLegacy(uuid: String = filter.generateUUID): DeviceId = DeviceId(uuid, None, filter.generateHash(uuid, None))

    def expectAuditIdEvent(badCookie:String, validCookie:String) = {
      val captor = ArgumentCaptor.forClass(classOf[DataEvent])
      verify(filter.auditConnector).sendEvent(captor.capture())(Matchers.any(), Matchers.any())
      val event = captor.getValue

      event.auditType shouldBe EventTypes.Failed
      event.auditSource shouldBe "SomeAppName"

      event.detail should contain ("tamperedDeviceId" -> badCookie)
      event.detail should contain ("newDeviceId" -> validCookie)
    }
  }

  "During request pre-processing, the filter" should {

    "create new deviceId cookie and response returns the new deviceId cookie when no cookies exists" in new Setup {

      val incomingRequest = FakeRequest()
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatGoodCookieDeviceId.value

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value
    }

    "not change the request or the response when a valid new format mtdpdi cookie + other cookie is sent in a request" in new Setup {
      val incomingRequest = FakeRequest().withCookies(newFormatGoodCookieDeviceId, normalCookie1)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatGoodCookieDeviceId.value

      val expectedCookie1 = requestPassedToAction.cookies.get("AnotherCookie1").get
      expectedCookie1.value shouldBe "normalValue1"

      result.header.headers.get("Set-Cookie") shouldBe None
    }

    "auto convert legacy DeviceId cookie to new format (carrying over the UUID from the legacy cookie) and response contains a new formatted device Id cookie with the legacy UUID" in new Setup {

      val (legacyDeviceIdCookie, newFormatDeviceIdCookie) = {
        val testUUID = filter.generateUUID

        val legacyDeviceId = generateDeviceIdLegacy(testUUID)
        val currentDeviceId = filter.generateDeviceId(testUUID)

        val legacyCookieValue = Cookie(DeviceIdData.MDTPDeviceId, legacyDeviceId.value, Some(DeviceIdData.TenYears))
        val newFormatCookieValue = Cookie(DeviceIdData.MDTPDeviceId, currentDeviceId.value, Some(DeviceIdData.TenYears))

        (legacyCookieValue, newFormatCookieValue)
      }

      val incomingRequest = FakeRequest().withCookies(legacyDeviceIdCookie)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie: Cookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatDeviceIdCookie.value

      val responseCookieString = result.header.headers.get("Set-Cookie").get

      getCookieStringValue(responseCookieString)(0) shouldBe newFormatDeviceIdCookie.value
    }

    "identify legacy deviceId cookie is invalid and create new deviceId cookie, response contains the new deviceId cookie" in new Setup {

      val legacyFormatBadCookieDeviceId = {
        val legacyDeviceId = generateDeviceIdLegacy().copy(hash="wrongvalue")
        Cookie(DeviceIdData.MDTPDeviceId, legacyDeviceId.value, Some(DeviceIdData.TenYears))
      }

      val incomingRequest = FakeRequest().withCookies(legacyFormatBadCookieDeviceId)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatGoodCookieDeviceId.value

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value

      expectAuditIdEvent(legacyFormatBadCookieDeviceId.value,newFormatGoodCookieDeviceId.value)
    }

    "identify new format deviceId cookie has invalid hash and create new deviceId cookie, response contains the new deviceId cookie" in new Setup {

      val newFormatBadCookieDeviceId = {
        val deviceId = filter.generateDeviceId().copy(hash="wrongvalue")
        Cookie(DeviceIdData.MDTPDeviceId, deviceId.value, Some(DeviceIdData.TenYears))
      }

      val incomingRequest = FakeRequest().withCookies(newFormatBadCookieDeviceId)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatGoodCookieDeviceId.value

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value

      expectAuditIdEvent(newFormatBadCookieDeviceId.value,expectedCookie.value)
    }

    "identify new format deviceId cookie has invalid timestamp and create new deviceId cookie, response contains the new deviceId cookie" in new Setup {

      val newFormatBadCookieDeviceId = {
        val deviceId = filter.generateDeviceId()
        val fields = deviceId.value.split(DeviceIdData.token1)
        val time: Array[String] = fields(2).split(DeviceIdData.token2)

        Cookie(DeviceIdData.MDTPDeviceId, deviceId.value.replace(time(0),"BAD_TIME"), Some(DeviceIdData.TenYears))
      }

      val incomingRequest = FakeRequest().withCookies(newFormatBadCookieDeviceId)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatGoodCookieDeviceId.value

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value
    }

    "identify new format deviceId cookie has invalid prefix and create new deviceId cookie, response contains the new deviceId cookie" in new Setup {

      val newFormatBadCookieDeviceId = {
        val deviceId = filter.generateDeviceId()
        Cookie(DeviceIdData.MDTPDeviceId, deviceId.value.replace(DeviceIdData.MDTPDeviceId,"BAD_PREFIX"), Some(DeviceIdData.TenYears))
      }

      val incomingRequest = FakeRequest().withCookies(newFormatBadCookieDeviceId)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceIdData.MDTPDeviceId).get
      expectedCookie.value shouldBe newFormatGoodCookieDeviceId.value

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value
    }

  }



}
