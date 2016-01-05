package co.blocke
package jarvis

import org.joda.time._
import org.joda.time.format._
import Slowest._
import scala.math.abs
import scala.collection.mutable.ListBuffer

case class Battle(
	target : Castle,
	landing : DateTime,
	launchAround : DateTime,
	startingComp : Compliment,
	slowest : Slowest,
	oneWayOneBlock : Duration
)(implicit j:Jarvis) {
    def dur(min:Int) = new Duration(min*60000)
    val tenMin = dur(10)

    val SURVIVE_CASTLE = 52
    val SURVIVE_FORTRESS = 502

	def selfTrickleSheet() = {
		var comp = startingComp
		val blockSize = 52  // larger for fortresses later!
		implicit val src = target

		// 1. Figure out how many cycles we can afford
		var dugIn = {if(target.isFortress) 520 else 120 }
		val numCycles = scala.math.floor((comp.total()-120)/blockSize).toInt

		// 2. Find a starting reference point close to current time
		val rtrip = oneWayOneBlock.getMillis * 2
		var targetTime = landing.plus(tenMin).plus(tenMin).minus(dur(1)) // skip first round then arrive 1 min before next round
		val start = (new Period(launchAround,targetTime).toStandardDuration().getMillis / rtrip).toInt
		val fence = j.chickenWire(target, start+numCycles+1)

		// 3. Plot the sheet!
		var bounceTo = 0
		val commands = scala.collection.mutable.ListBuffer.empty[(DateTime,Castle,Compliment)]
		val slots = ListBuffer.empty[DateTime]
		(0 to numCycles).foreach{ i =>
			val batch = comp.getBatch(blockSize,slowest,dugIn)
			batch.map({ case (deploy,leftOver) =>
				if( deploy.total() < {if(target.isFortress) SURVIVE_FORTRESS else SURVIVE_CASTLE} ) {
					dugIn += deploy.total()
				}
				else {
					comp = leftOver
					slots += targetTime

					val bestTimeAndDist = fence.map( {case(dist,castle) => (timeToWaitToHitTargetByTime(dist*2, oneWayOneBlock, targetTime),dist) } )
						.filter(_._1 != 0)
						.toList
						.sortWith( (a,b) => a._1 < b._1 )
						.head
					bounceTo = bestTimeAndDist._2

					// (when, castle, troops)
					val cmd = ( roundDate(launchAround.plus( new Duration(bestTimeAndDist._1*1000) )), fence(bounceTo), deploy)
					commands += cmd

					targetTime = targetTime.plus(tenMin)
				}
			})
		}
		val cmdsGrouped = commands.groupBy(_._1).map{ case (k,v) => 
			(k, v.map({case(when,castle,troops) => Supply(troops,castle)}).toList)
		}.toList.sortBy(_._1.getMillis)

		SelfTrickleSheet(target,comp,cmdsGrouped,slots.toList)
			// launchTime + Dur(start*rtrip) == targetTime
			// launchTime = targetTime - Dur(start*trip)
	}

	def roundDate(dateTime:DateTime) = dateTime.minuteOfDay().roundFloorCopy()

	def time(wait:Long) = {
		val hr = wait/(60*60)
		val min = (wait - (hr*60*60))/60
		val sec = wait - (hr*60*60) - (min*60)
		s"$hr:$min:$sec"
	}

	def timeToWaitToHitTargetByTime( distanceInBlocks:Int, oneBlockSpeed:Duration, targetTime:DateTime ) = {
		// speed is in seconds-per-block
		if( distanceInBlocks == 0 ) 0
		else {
			//	delayDuration = targetTime - (distance*speed) - launchAround
			val z = targetTime.minus( new Duration( oneBlockSpeed.getMillis*distanceInBlocks ) )
			val delay = Seconds.secondsBetween( launchAround, z ).getSeconds
			if( delay > 0 ) delay else 0
		}
	}
}