package co.blocke
package jarvis

import org.joda.time._

trait TrickleLike {

	val SURVIVE_CASTLE = 52
	val SURVIVE_FORTRESS = 502

	def dur(min:Int) = new Duration(min*60000)
	val tenMin = dur(10)

	def timeToWaitToHitTargetByTime( distanceInBlocks:Int, oneBlockSpeed:Duration, targetTime:DateTime, launchAround:DateTime ) = {
		// speed is in seconds-per-block
		if( distanceInBlocks == 0 ) 0
		else {
			//	delayDuration = baseTime - (distance*speed) - launchAround
			val z = targetTime.minus( new Duration( oneBlockSpeed.getMillis*distanceInBlocks ) )
			val delay = Seconds.secondsBetween( launchAround, z ).getSeconds
			if( delay > 0 ) delay else 0
		}
	}

	def roundDate(dateTime:DateTime) = dateTime.minuteOfDay().roundFloorCopy()

	def batchSize(c:Castle) = if( c.isFortress ) SURVIVE_FORTRESS else SURVIVE_CASTLE

	def time(wait:Long) = {
		val hr = wait/(60*60)
		val min = (wait - (hr*60*60))/60
		val sec = wait - (hr*60*60) - (min*60)
		s"$hr:$min:$sec"
	}

	def sheet() : TrickleSheet
}