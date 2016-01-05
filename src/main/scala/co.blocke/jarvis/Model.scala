package co.blocke
package jarvis

import org.joda.time.DateTime
import org.joda.time.format._
import scala.math._
import scala.collection.mutable.ListBuffer

class LKDate( val d:String ) extends AnyVal {
	def toDate = {
		val dtp:Array[DateTimeParser] = Array(
			DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ").getParser(),
			DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").getParser()
			)
		val infmt = new DateTimeFormatterBuilder().append(null,dtp).toFormatter()
		infmt.withOffsetParsed().parseDateTime(d)
	}
	override def toString() = toDate.toString
}

case class Castle(
	id: Long,
	name: Option[String],
	mapX: Long,
	mapY: Long,
	points: Int,
	creationDate: LKDate,
	playerID: Option[Int],
	publicType: Int,
	allianceID: Option[Int],
	compliment: Option[Compliment],
	withMap:Boolean = false,
	withCompass:Boolean = false
){
	def isFortress = (points > 290)
	def isOffensive = compliment.fold(false)(c => c.troops(1) > c.troops(0))
	val slowest = {	if(isOffensive) Slowest.Sword else Slowest.Spear	}
	val speed = slowest match {
		case Slowest.Spear =>
			if(withCompass) Constants.SpearSpeedCompass
			else if(withMap) Constants.SpearSpeedMap
			else Constants.SpearSpeed
		case Slowest.Sword =>
			if(withCompass) Constants.SwordSpeedCompass
			else if(withMap) Constants.SwordSpeedMap
			else Constants.SwordSpeed
	}
}

case class Player(
	id: Long,
	nick: String,
	habitatIDs: Option[List[Long]],
	allianceID: Option[Int],
	alliancePermission: Option[Int],
	points: Int,
	rank: Long,
	underAttackProtection: Boolean,
	onVacation: Boolean
)

case class CastleHolder(
	castles:List[Castle]
){
	def findByName(name:String) = castles.find(_.name == Some(name))
	def size = castles.size
	def names = castles.map(_.name.get)
}

object Slowest extends Enumeration {
  type Slowest = Value
  val Sword, Spear = Value
}
import Slowest._

// Troop List: (in-order)
// (spear,sword,archer,xbow,horse,lancer)
case class Compliment( troops:List[Int] ) {
	def total(all:List[Int] = troops) = all.foldLeft(0)( _+_ )

	private def burnDown( num:Int, block:Int, starting:ListBuffer[Int], digIn:Int ) = {
		var working = starting
		val running = ListBuffer.empty[Int]++troops
		if( starting(0) == 1 ) running(0) -= 1
		else running(1) -= 1
		var i = 5
		var targetCount = num
		var last:Option[(Compliment,Compliment)] = None
		while( total(working.toList) < num && i >= 0 && total(running.toList) > digIn ) {
			val allocate = min(targetCount, running(i)) 
			working(i) += allocate
			running(i) -= allocate
			i -= 1
			targetCount -= allocate
			if( i == block ) i -= i
			if( total(running.toList) > digIn )
				last = Some((Compliment(working.toList), Compliment(running.toList)))
		}
		last
	}

	def getBatch( size:Int, slowest:Slowest, digIn:Int ) = 
		slowest match {
			case Spear => burnDown(size-1, 1, ListBuffer(1,0,0,0,0,0), digIn)
			case Sword => burnDown(size-1, 0, ListBuffer(0,1,0,0,0,0), digIn)
		}
}

trait Action
case class Supply(troops:Compliment, target:Castle, src:Castle)(implicit j:Jarvis) extends Action {
	override def toString() = s"Send 1 supply and ${j.showTroops(troops)} to "+target.name.getOrElse("(symbols)")+
		j.showAlly(target)+s" (${j.distance(src,target)}) -- " + j.showDirection(src,target)
}
case class Support(troops:Compliment, target:Castle, src:Castle)(implicit j:Jarvis) extends Action {
	override def toString() = s"Send ${j.showTroops(troops)} from "+src.name.getOrElse("(symbols)")+s"(${j.distance(src,target)})"
}

trait TrickleSheet {
	val forCastle : Castle
	val commandGroup : List[(DateTime,List[Action])]
	val slotsCovered : List[DateTime]
}
case class SupportTrickleSheet(
	forCastle    : Castle,
	commandGroup : List[(DateTime,List[Action])],
	slotsCovered : List[DateTime]
) extends TrickleSheet {
	override def toString() = {
		val sb = new StringBuffer()
		val offensive = if(forCastle.isOffensive) "offensive" else "defensive"
		sb.append("-------<  Trickle Sheet From Castle: "+forCastle.name.getOrElse("(symbols)")+s" ($offensive)"+"\n")
		sb.append("Actions: "+"\n")
		commandGroup.foreach{ a =>
			val (whenTime, cmd) = a
			sb.append("  @ "+ DateTimeFormat.forStyle("SM").print(whenTime) +" do:\n")
			cmd.foreach( s => sb.append( "        "+s+"\n") )
			}
		sb.append("\nSlots Covered:\n")
		sb.append("   "+slotsCovered.map(t => DateTimeFormat.forStyle("-S").print(t)).mkString(",")+"\n")
		sb.append("-------------------------------------------------------\n")
		sb.toString
	}
}

case class SelfTrickleSheet(
	forCastle    : Castle,
	garison      : Compliment,
	commandGroup : List[(DateTime,List[Action])],
	slotsCovered : List[DateTime]
)(implicit j:Jarvis) extends TrickleSheet {
	override def toString() = {
		val sb = new StringBuffer()
		val offensive = if(forCastle.isOffensive) "offensive" else "defensive"
		sb.append("-------<  Self-Trickle Sheet From Castle: "+forCastle.name.getOrElse("(symbols)")+s" ($offensive)"+"\n")
		sb.append("Initial Garrison: "+j.showTroops(garison)+"\n")
		sb.append("Actions: "+"\n")
		commandGroup.foreach{ a =>
			val (whenTime, cmd) = a
			sb.append("  @ "+ DateTimeFormat.forStyle("SM").print(whenTime) +" do:\n")
			cmd.foreach( s => sb.append( "        "+s+"\n") )
			}
		sb.append("\nSlots Covered:\n")
		sb.append("   "+slotsCovered.map(t => DateTimeFormat.forStyle("-S").print(t)).mkString(",")+"\n")
		sb.append("-------------------------------------------------------\n")
		sb.toString
	}
}