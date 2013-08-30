package keyel.state

import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import StateTransition._

@RunWith(classOf[JUnitRunner]) 
class StateTransitionTestSuite extends FlatSpec with ShouldMatchers {

	case class IntState(n: Int)
	val zerost = IntState(0)
	val onest = IntState(1)
	val twost = IntState(2)
	
	val intstate_incrementer : StateTransition[IntState, Int] = StateTransition { intState =>
	  (IntState(intState.n + 1), intState.n + 1)
	}
	
	val intstate_incrementAndDouble : StateTransition[IntState, Int] = intstate_incrementer map { _ * 2 }
  
	"state transition twice" should "work" in {
	  val (onest, onezero) = intstate_incrementer.trans(zerost) 
	  val (twost, two) = intstate_incrementer.trans(onest)
	  (two) should equal(2)
	}
	
	"state transition repeat" should "work" in {
	  val incrementThreeTimes = repeat(intstate_incrementer, 3)
	  
	  val (_, three) = incrementThreeTimes.trans(zerost)
	  three should equal(3)
	}
	
	"state transition map" should "work" in {
	  val (_, two) = intstate_incrementAndDouble.trans(zerost) 
	  two should equal(2)
	}
	
	"mapbin" should "work" in {
	  
	}
}