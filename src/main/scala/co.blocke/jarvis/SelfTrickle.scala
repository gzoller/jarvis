package co.blocke
package jarvis

import org.joda.time._
import org.joda.time.format._
import scala.math.abs
import scala.collection.mutable.ListBuffer

case class SelfTrickle(
	base : Castle,
	landing : DateTime,
	launchAround : DateTime
)(implicit j:Jarvis) extends TrickleLike {
	def sheet() = {
		var comp = base.compliment.getOrElse(Compliment(List(0,0,0,0,0,0)))

		// 1. Figure out how many cycles we can afford
		var dugIn = {if(base.isFortress) 520 else 120 }
		val numCycles = scala.math.floor((comp.total()-120)/batchSize(base)).toInt

		// 2. Find a starting reference point close to current time
		val rtrip = base.speed.getMillis * 2
		var baseTime = landing.plus(tenMin).plus(tenMin).minus(dur(1)) // skip first round then arrive 1 min before next round
		val start = (new Period(launchAround,baseTime).toStandardDuration().getMillis / rtrip).toInt
		val fence = j.chickenWire(base, start+numCycles+1)

		// 3. Plot the sheet!
		var bounceTo = 0
		val commands = scala.collection.mutable.ListBuffer.empty[(DateTime,Castle,Compliment)]
		val slots = ListBuffer.empty[DateTime]
		(0 to numCycles).foreach{ i =>
			val batch = comp.getBatch(batchSize(base),base.slowest,dugIn)
			batch.map({ case (deploy,leftOver) =>
				if( deploy.total() < {if(base.isFortress) SURVIVE_FORTRESS else SURVIVE_CASTLE} ) {
					dugIn += deploy.total()
				}
				else {
					comp = leftOver
					slots += baseTime

					val bestTimeAndDist = fence.map( {case(dist,castle) => (timeToWaitToHitTargetByTime(dist*2, base.speed, baseTime, launchAround),dist) } )
						.filter(_._1 != 0)
						.toList
						.sortWith( (a,b) => a._1 < b._1 )
						.head
					bounceTo = bestTimeAndDist._2

					// (when, castle, troops)
					val cmd = ( roundDate(launchAround.plus( new Duration(bestTimeAndDist._1*1000) )), fence(bounceTo), deploy)
					commands += cmd

					baseTime = baseTime.plus(tenMin)
				}
			})
		}
		val cmdsGrouped = commands.groupBy(_._1).map{ case (k,v) => 
			(k, v.map({case(when,castle,troops) => Supply(troops,castle,base)}).toList)
		}.toList.sortBy(_._1.getMillis)

		SelfTrickleSheet(base,comp,cmdsGrouped,slots.toList)
			// launchTime + Dur(start*rtrip) == baseTime
			// launchTime = baseTime - Dur(start*trip)
	}
}