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

import org.mockito.ArgumentCaptor
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

  lazy val timestamp = System.currentTimeMillis()

  private trait Setup extends Results {

    val normalCookie = Cookie("AnotherCookie1", "normalValue1")

    val resultFromAction: Result = Ok

    lazy val action = {
      val mockAction = mock[(RequestHeader) => Future[Result]]
      val outgoingResponse = Future.successful(resultFromAction)
      when(mockAction.apply(any())).thenReturn(outgoingResponse)
      mockAction
    }

    lazy val filter = new DeviceIdFilter {

      lazy val mdtpCookie = super.buildNewDeviceIdCookie()

      override def getTimeStamp = timestamp

      override def buildNewDeviceIdCookie() = mdtpCookie

      override val secret = "SOME_SECRET"

      override val appName = "SomeAppName"

      lazy val auditConnector = mock[AuditConnector]
    }

    lazy val newFormatGoodCookieDeviceId = filter.mdtpCookie


    def requestPassedToAction: RequestHeader = {
      val updatedRequest = ArgumentCaptor.forClass(classOf[RequestHeader])
      verify(action).apply(updatedRequest.capture())
      updatedRequest.getValue
    }

    def getCookieStringValue(input: String): Array[String] = input.substring((DeviceId.MdtpDeviceId + "_").length, input.length).split(";")

    def generateDeviceIdLegacy(uuid: String = filter.generateUUID): DeviceId = DeviceId(uuid, None, DeviceId.generateHash(uuid, None, filter.secret))

    def expectAuditIdEvent(badCookie: String, validCookie: String) = {
      val captor = ArgumentCaptor.forClass(classOf[DataEvent])
      verify(filter.auditConnector).sendEvent(captor.capture())(any(), any())
      val event = captor.getValue

      event.auditType shouldBe EventTypes.Failed
      event.auditSource shouldBe "SomeAppName"

      event.detail should contain("tamperedDeviceId" -> badCookie)
      event.detail should contain("newDeviceId" -> validCookie)
    }


    def invokeFilter(cookies: Seq[Cookie], expectedResultCookie: Cookie) = {
      val incomingRequest = if (cookies.isEmpty) FakeRequest() else FakeRequest().withCookies(cookies: _*)
      val result = filter(action)(incomingRequest).futureValue

      val expectedCookie = requestPassedToAction.cookies.get(DeviceId.MdtpDeviceId).get
      expectedCookie.value shouldBe expectedResultCookie.value

      result
    }
  }

  "During request pre-processing, the filter" should {

    "create new deviceId cookie when no cookies exists" in new Setup {
      val result = invokeFilter(Seq.empty, newFormatGoodCookieDeviceId)

      val responseCookie = result.header.headers.get("Set-Cookie").get

      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value
    }

    "not change the request or the response when a valid new format mtdpdi cookie exists" in new Setup {
      val result = invokeFilter(Seq(newFormatGoodCookieDeviceId, normalCookie), newFormatGoodCookieDeviceId)

      val expectedCookie1 = requestPassedToAction.cookies.get("AnotherCookie1").get
      val expectedCookie2 = requestPassedToAction.cookies.get(DeviceId.MdtpDeviceId).get

      expectedCookie1.value shouldBe "normalValue1"
      expectedCookie2.value shouldBe newFormatGoodCookieDeviceId.value

      result.header.headers.get("Set-Cookie") shouldBe None
    }

    "auto convert legacy DeviceId cookie to new format" in new Setup {

      val (legacyDeviceIdCookie, newFormatDeviceIdCookie) = {
        val testUUID = filter.generateUUID

        val legacyDeviceId = generateDeviceIdLegacy(testUUID)
        val currentDeviceId = filter.generateDeviceId(testUUID)

        val legacyCookieValue = Cookie(DeviceId.MdtpDeviceId, legacyDeviceId.value, Some(DeviceId.TenYears))
        val newFormatCookieValue = Cookie(DeviceId.MdtpDeviceId, currentDeviceId.value, Some(DeviceId.TenYears))

        (legacyCookieValue, newFormatCookieValue)
      }

      val result = invokeFilter(Seq(legacyDeviceIdCookie), newFormatDeviceIdCookie)

      val responseCookieString = result.header.headers.get("Set-Cookie").get

      getCookieStringValue(responseCookieString)(0) shouldBe newFormatDeviceIdCookie.value
    }

    "identify legacy deviceId cookie is invalid and create new deviceId cookie" in new Setup {

      val legacyFormatBadCookieDeviceId = {
        val legacyDeviceId = generateDeviceIdLegacy().copy(hash="wrongvalue")
        Cookie(DeviceId.MdtpDeviceId, legacyDeviceId.value, Some(DeviceId.TenYears))
      }

      val result = invokeFilter(Seq(legacyFormatBadCookieDeviceId), newFormatGoodCookieDeviceId)

      val responseCookie = result.header.headers.get("Set-Cookie").get

      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value

      expectAuditIdEvent(legacyFormatBadCookieDeviceId.value,newFormatGoodCookieDeviceId.value)
    }

    "identify new format deviceId cookie has invalid hash and create new deviceId cookie" in new Setup {

      val newFormatBadCookieDeviceId = {
        val deviceId = filter.generateDeviceId().copy(hash="wrongvalue")
        Cookie(DeviceId.MdtpDeviceId, deviceId.value, Some(DeviceId.TenYears))
      }

      val result = invokeFilter(Seq(newFormatBadCookieDeviceId), newFormatGoodCookieDeviceId)


      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value

      expectAuditIdEvent(newFormatBadCookieDeviceId.value,newFormatGoodCookieDeviceId.value)
    }

    "identify new format deviceId cookie has invalid timestamp and create new deviceId cookie" in new Setup {

      val newFormatBadCookieDeviceId = {
        val deviceId = filter.generateDeviceId()
        val fields = deviceId.value.split(DeviceId.Token1)
        val time: Array[String] = fields(2).split(DeviceId.Token2)

        Cookie(DeviceId.MdtpDeviceId, deviceId.value.replace(time(0),"BAD TIME"), Some(DeviceId.TenYears))
      }

      val result = invokeFilter(Seq(newFormatBadCookieDeviceId), newFormatGoodCookieDeviceId)

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value
    }

    "identify new format deviceId cookie has invalid prefix and create new deviceId cookie" in new Setup {

      val newFormatBadCookieDeviceId = {
        val deviceId = filter.generateDeviceId()
        Cookie(DeviceId.MdtpDeviceId, deviceId.value.replace(DeviceId.MdtpDeviceId,"BAD_PREFIX"), Some(DeviceId.TenYears))
      }

      val result = invokeFilter(Seq(newFormatBadCookieDeviceId), newFormatGoodCookieDeviceId)

      val responseCookie = result.header.headers.get("Set-Cookie").get
      getCookieStringValue(responseCookie)(0) shouldBe newFormatGoodCookieDeviceId.value
    }

  }

}
