package co.blocke
package jarvis

import co.blocke.scalajack._
import scala.io.Source
import scala.reflect.runtime.universe._
import org.joda.time.DateTime
import scala.collection.mutable.ListBuffer
import scala.util.{Try=>ScalaTry,Success,Failure}

case class Jarvis(
	castleFile:  String,
	castleLast4: String,
	playerFile:  String,
	valuesFile:  String,
	myName:      String
){
	val vc = VisitorContext().copy(estFieldsInObj=5000000)
	val sj = ScalaJack()

	private def loadCastles = {
		val castles = parseFile[Castle](castleFile)
			.map(c => c.copy(name = c.name.map(_.replaceAll("[^\\x00-\\x7F]", "").trim)))
		val overlay = parseFile[Castle](castleLast4)
			.map(c => c.copy(name = c.name.map(_.replaceAll("[^\\x00-\\x7F]", "").trim)))
		val byCoords = castles.groupBy(_.mapX).map({case(k,v) => (k,v.groupBy(_.mapY)) })
		overlay.foreach{ o => 
			ScalaTry(byCoords(o.mapX)(o.mapY)) match {
				case Success(c) => 
					val list = byCoords(o.mapX)(o.mapY)
					list.clear()
					list += o
				case Failure(x) => 
			}
		}
		byCoords.values.flatten.toList.map(_._2.toList).flatten
	}

	def parseFile[T](fname:String)(implicit tt:TypeTag[T]):ListBuffer[T] = 
		sj.read[ListBuffer[T]](Source.fromFile(fname).getLines.mkString,vc)

	val castles = loadCastles
	// val castles = parseFile[Castle](castleFile)
	// 	.map(c => c.copy(name = c.name.map(_.replaceAll("[^\\x00-\\x7F]", "").trim)))
	val players = parseFile[Player](playerFile)
		.map( p => p.copy(nick = p.nick.replaceAll("[^\\x00-\\x7F]", "").trim ))
	val me = playerByName(myName).get
	val myCastles = castlesForPlayer( me.id, true )

	private def findPlayer(id:Long) = players.find(_.id == id )
	private def findCastle(id:Long) = castles.find(_.id == id )
	private def playerByName(n:String) = players.find(_.nick == n)

	def findCastleByName(name:String) = castles.find(_.name == Some(name))

	def castlesForPlayer(playerId:Long, matchValues:Boolean=false) : Option[CastleHolder] = {
		val tt = "\t+".r
		val values = {
			if( matchValues ) {
				Source.fromFile(valuesFile).getLines.filterNot( ln => ln.trim == "" || ln(0) == '#' )
					.map{ ln => 
						val tokens = tt.replaceAllIn(ln,"\t").split("\t")
						(tokens.head,tokens.tail.toList)
					}
					.toMap
			} else Map.empty[String,List[String]]
		}
		findPlayer(playerId) match {
			case None    => None
			case Some(p) => 
				p.habitatIDs.map(a => {
					val b = a.collect{
						case CastleFind(hid) => hid
					}
					CastleHolder(b.map{ aCastle => 
						values.get(aCastle.name.getOrElse("**none**")).map{ rawValues =>
							println("Matched castle "+aCastle.name.get)
							aCastle.copy(
								withMap = { if( rawValues(6) == "Y" ) true else false },
								withCompass = { if( rawValues(7) == "Y" ) true else false },
								compliment = Some(Compliment( rawValues.take(6).map( _.toInt ) ))
								)
							}
							.getOrElse(aCastle) // not found in values
					}
					)
				})
		}
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

	def showTroops( c:Compliment ) = {
		val names = List("SP","SW","LB","XB","AH","LN")
		c.troops.zip(names).collect{ case(t,n) if(t>0) => n+":"+t }.mkString("(",",",")")
	}

	def showDirection( fromC:Castle, toC:Castle ) = {
		val ns = (fromC.mapY - toC.mapY) match {
			case 0 => ""
			case x if(x < 0) => "S"+abs(x)
			case x if(x > 0) => "N"+abs(x)
		}
		val we = (fromC.mapX - toC.mapX) match {
			case 0 => ""
			case x if(x < 0) => "E"+abs(x)
			case x if(x > 0) => "W"+abs(x)
		}
		"["+ns+" "+we+"]"
	}

	def showAlly(c:Castle) = c.allianceID.map(a => if( a == me.allianceID.getOrElse(0) ) "<ally>").getOrElse("")

	// Find 1 castle per distance-range
	def chickenWire(base:Castle, inRange:Int, startRange:Int=1) = {
		val m = scala.collection.mutable.Map.empty[Int,Castle]
		castles.collect{
			case c if( c.name.isDefined && c.name.get.length > 1 && { val d = distance(base,c); d <= inRange && d >= startRange && !m.contains(d) }) =>
				m.put(distance(base,c),c)
		}
		m.toMap
	}

	object CastleFind {
		def unapply(id:Long): Option[Castle] = findCastle(id)
	}
}

object Jarvis {
	val j = new Jarvis(
		"/Users/wmy965/Downloads/lk/habitats.json",
		"/Users/wmy965/Downloads/lk/habitats-last4h.json",
		"/Users/wmy965/Downloads/lk/players.json",
		"/Users/wmy965/Downloads/lk/mine.dat",
		"Akka"
		)
}
