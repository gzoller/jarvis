package co.blocke
package jarvis

import co.blocke.scalajack._
import scala.io.Source
import scala.reflect.runtime.universe._
import org.joda.time.DateTime

trait Jarvis{
	val vc = VisitorContext().copy(estFieldsInObj=5000000)
	val sj = ScalaJack()

	def parseFile[T](fname:String)(implicit tt:TypeTag[T]):List[T] = 
		sj.read[List[T]](Source.fromFile(fname).getLines.mkString,vc)

	val castles = parseFile[Castle]("/Users/wmy965/Downloads/lk/habitats.json")
		.map(c => c.copy(name = c.name.map(_.replaceAll("[^\\x00-\\x7F]", "").trim)))
	val players = parseFile[Player]("/Users/wmy965/Downloads/lk/players.json")
		.map( p => p.copy(nick = p.nick.replaceAll("[^\\x00-\\x7F]", "").trim ))

	private def findPlayer(id:Long) = players.find(_.id == id )
	private def findCastle(id:Long) = castles.find(_.id == id )

	def findCastleByName(name:String) = castles.find(_.name == Some(name))

	def myCastles(playerId:Long) : Option[CastleHolder] = findPlayer(playerId) match {
		case None    => None
		case Some(p) => 
			p.habitatIDs.map(a => {
				val b= a.collect{
					case CastleFind(hid) => hid
				}
				CastleHolder(b)
			})
	}

	import scala.math._
	private def xform( u:Double, v:Double ) : (Double,Double) = (u+floor(v/2),v)

	def distance(c1:Castle, c2:Castle) = {
		val (x1,y1) = xform(c1.mapX.toDouble,c1.mapY.toDouble)
		val (x2,y2) = xform(c2.mapX.toDouble,c2.mapY.toDouble)
		val dx = x2-x1
		val dy = y2-y1
		val raw = 
			if( (dx>=0 && dy>=0) || (dx < 0 && dy < 0)) 
				max(abs(dx),abs(dy))
			else 
				abs(dx)+abs(dy)
		round(raw).toInt
	}

	def neighbors(base:Castle, inRange:Int) = castles.collect{
		case c if( distance(base,c) <= inRange ) => c
	}

	def showTroops( c:Compliment ) = {
		val names = List("SP","SW","LB","XB","AH","LN")
		c.troops.zip(names).collect{ case(t,n) if(t>0) => n+":"+t }.mkString("(",",",")")
	}

	// 1 castle per distance-range
	def chickenWire(base:Castle, inRange:Int, startRange:Int=1) = {
		val m = scala.collection.mutable.Map.empty[Int,Castle]
		castles.collect{
			case c if( c.name.isDefined && c.name.get != "" && { val d = distance(base,c); d <= inRange && d >= startRange && !m.contains(d) }) =>
				m.put(distance(base,c),c)
		}
		m.toMap
	}

	object CastleFind {
		def unapply(id:Long): Option[Castle] = findCastle(id)
	}
}

object Jarvis {
	val j = new Jarvis(){}
}
