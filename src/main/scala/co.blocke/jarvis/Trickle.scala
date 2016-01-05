package co.blocke
package jarvis

import org.joda.time._
import scala.collection.mutable.ListBuffer

case class Trickle(
	toC : Castle,
	landing : DateTime,
	launchAround : DateTime,
	blocked : List[Castle] = List.empty[Castle]
)(implicit j:Jarvis) extends TrickleLike {

	def sheet() = {
		val possible:List[Castle] = j.myCastles.get.castles.diff( toC +: blocked )
		val (bestTime,bestFrom) = possible.map(p => (timeToWaitToHitTargetByTime( j.distance(p,toC), p.speed, landing, launchAround),p))
			.filter(_._1 != 0)
			.sortBy(_._1)
			.head

		var baseTime = landing
		var comp = bestFrom.compliment.getOrElse(Compliment(List(0,0,0,0,0,0)))
		val commands = scala.collection.mutable.ListBuffer.empty[(DateTime,Castle,Compliment)]
		val travelDistance = j.distance(bestFrom,toC)
		val slots = ListBuffer.empty[DateTime]

		var dugIn = {if(bestFrom.isFortress) 520 else 120 }
		var done = false
		while(comp.total() > batchSize(bestFrom) && !done) {
			val batch = comp.getBatch(batchSize(bestFrom),bestFrom.slowest,dugIn)
			batch.map({ case (deploy,leftOver) =>
				if( deploy.total() < batchSize(bestFrom) ) {
					dugIn += deploy.total()
					done = true
				}
				else {
					comp = leftOver
					slots += baseTime

					val delay = timeToWaitToHitTargetByTime(travelDistance, bestFrom.speed, baseTime, launchAround)

					// (when, castle, troops)
					val cmd = ( roundDate(launchAround.plus( new Duration(delay*1000) )), toC, deploy)
					commands += cmd

					baseTime = baseTime.plus(tenMin)
				}
			})
		}
		implicit val src = bestFrom
		val cmdsGrouped = commands.groupBy(_._1).map{ case (k,v) => 
			(k, v.map({case(when,castle,troops) => Support(troops,castle,bestFrom)}).toList)
		}.toList.sortBy(_._1.getMillis)

		SupportTrickleSheet(toC,cmdsGrouped,slots.toList)
	}
}