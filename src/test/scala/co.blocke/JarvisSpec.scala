package co.blocke
package jarvis
package test

import org.scalatest._
import scala.collection.JavaConversions._
import org.joda.time._
import org.joda.time.format._

class JarvisSpec extends FunSpec with Matchers {

	implicit val j = new Jarvis(){}
	val me = j.players.find(_.nick == "Akka")

	def time(wait:Long) = {
		val hr = wait/(60*60)
		val min = (wait - (hr*60*60))/60
		val sec = wait - (hr*60*60) - (min*60)
		s"$hr:$min:$sec"
	}

	describe("======================\n|  Connection Tests  |\n======================") {
		it("Must parse data files") {
			println(j.players.size())
			println(j.castles.size())
		}
		it("Find Akka") {
			println(me)
			val c = j.myCastles(me.get.id).get
			println(c.size)

			// import scala.math._
			// val c1 = c.findByName("Umfahrt") 
			// val c2 = j.findCastleByName("Beast Mode #10") 
			// println(j.distance(c1.get,c2.get)) // 13

			// val n = j.neighbors( c.findByName("Umfahrt").get, 10 )
			// println(n.size)

//			val m = j.chickenWire(c.findByName("Umfahrt").get, 20, 10)
//			println(m.map({case(k,v) => s"$k -> ${v.name}/${v.id}"}))

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
		val landing = fmt.withOffsetParsed().parseDateTime("2016-01-04T20:00:00-06:00")
		val launchAbout = fmt.withOffsetParsed().parseDateTime("2016-01-04T16:45:00-06:00")

		val b = Battle(
			j.findCastleByName("Umfahrt").get,
			landing,
			launchAbout,
			Compliment(List(0,1000,1000,0,0,1000)),
			Slowest.Sword,
			new Duration(12*60000+38000)
			)

		println( b.selfTrickleSheet() )
	}
}