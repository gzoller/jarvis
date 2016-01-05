package co.blocke
package jarvis

import org.joda.time.Duration

object Constants {
	val MIN               = 1000*60
	val SEC               = 1000

	val SpearSpeed        = new Duration(11*MIN+38*SEC) // 00:11:38
	val SpearSpeedMap     = new Duration(11*MIN+3*SEC)  // 00:11:03
	val SpearSpeedCompass = new Duration(10*MIN+29*SEC) // 00:10:29

	val SwordSpeed        = new Duration(11*MIN+38*SEC) // 00:11:  TBD!!!
	val SwordSpeedMap     = new Duration(12*MIN+38*SEC) // 00:12:38
	val SwordSpeedCompass = new Duration(12)            // 00:12:00
}