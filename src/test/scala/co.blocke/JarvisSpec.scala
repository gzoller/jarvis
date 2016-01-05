package co.blocke
package jarvis
package test

import org.scalatest._
import scala.collection.JavaConversions._
import org.joda.time._
import org.joda.time.format._

class JarvisSpec extends FunSpec with Matchers {

	implicit val j = new Jarvis(
		"/Users/wmy965/Downloads/lk/habitats.json",
		"/Users/wmy965/Downloads/lk/habitats-last4h.json",
		"/Users/wmy965/Downloads/lk/players.json",
		"/Users/wmy965/Downloads/lk/mine.dat",
		"Akka"
		)

	def time(wait:Long) = {
		val hr = wait/(60*60)
		val min = (wait - (hr*60*60))/60
		val sec = wait - (hr*60*60) - (min*60)
		s"$hr:$min:$sec"
	}

	describe("======================\n|  Connection Tests  |\n======================") {
		it("Must parse data files") {
			println("Players: "+j.players.size())
			println("Castles: "+j.castles.size())
		}
	}
	/*
	it("Calcs") {
		import scala.math.abs
		val fmt = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")
		// val targetTime = (new DateTime()).plus(new Duration(30*60000))
		// val launchAround = (new DateTime()).plus(new Duration(10*60000))
		val targetTime   = fmt.withOffsetParsed().parseDateTime("2016-01-04T20:20:00-06:00")
		val launchAround = fmt.withOffsetParsed().parseDateTime("2016-01-04T16:45:00-06:00")
		val oneBlockSpeed = new Duration(12*60000+38000)
		val distanceInBlocks = 14

		//	delayDuration = targetTime - (distance*speed) - launchAround
		val z = targetTime.minus( new Duration( oneBlockSpeed.getMillis*distanceInBlocks ) )
		val wait = abs(Seconds.secondsBetween( launchAround, z ).getSeconds)
		println("Target     : "+targetTime)
		println("Launch Time: "+launchAround)
		println(s"Wait: ${time(wait)}")
		println("Travel time: "+time((oneBlockSpeed.getStandardSeconds()*distanceInBlocks)))
		println("------------------------")
	}
	*/
	it("Battle") {
		val fmt = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")
		val landing = fmt.withOffsetParsed().parseDateTime("2016-01-04T19:00:00-06:00")
		val launchAbout = fmt.withOffsetParsed().parseDateTime("2016-01-04T15:20:00-06:00")

		val st = SelfTrickle(
			j.myCastles.get.findByName("Umfahrt").get,
			landing,
			launchAbout
			)

		println( st.sheet() )
	}
	/*
	it("Battle") {
		val fmt = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")
		val landing = fmt.withOffsetParsed().parseDateTime("2016-01-04T20:00:00-06:00")
		val launchAbout = fmt.withOffsetParsed().parseDateTime("2016-01-04T16:45:00-06:00")

		val t = Trickle(
			j.myCastles.get.findByName("Umfahrt").get,
			landing,
			launchAbout
		)

		println( t.sheet() )
	}
	*/
}