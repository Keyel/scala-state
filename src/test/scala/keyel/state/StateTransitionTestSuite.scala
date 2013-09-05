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
	
	val intstate_incrementReturnDouble : StateTransition[IntState, Int] = intstate_incrementer map { _ * 2 }

	val intstate_doubler : StateTransition[IntState, Int] = StateTransition { intState => 
		(IntState(intState.n * 2), intState.n * 2)
	}
  
	"state transition twice" should "work" in {
	  val (onest, onezero) = intstate_incrementer.evolve(zerost) 
	  val (twost, two) = intstate_incrementer.evolve(onest)
	  (two) should equal(2)
	}
	
	"state transition repeat" should "work" in {
	  val incrementThreeTimes = repeat(intstate_incrementer, 3)
	  
	  val (IntState(n), three) = incrementThreeTimes.evolve(zerost)
	  three should equal(3)
	  n should equal(3)
	}
	
	"state transition map" should "gives the modified answer, without changing the state" in {
	  val (IntState(n), two) = (intstate_incrementer map { _ * 2 }).evolve(zerost) 
	  two should equal(2)
	  n should equal(1)
	}
	
	"mapbin" should "evolve the state twice, and use the two results" in {
		val sttr = intstate_incrementer.mapbin(intstate_doubler) { (_,_) }
		val (IntState(n), r) = sttr.evolve(zerost)
		r should equal( (1,2) )
		n should equal( 2 )
	}

	"compose" should "return a StateTransition, which is equivalent of applying all the StateTransitions from right to left" in {
		val sttr = compose(List(intstate_doubler, intstate_incrementer, intstate_incrementer))
		val (IntState(n), r) = sttr.evolve(zerost)
		r should equal( 4 )
		n should equal( 4 )
	}

	"sequence" should "return the intermediate results also" in {
		def unitis = unit[IntState, Int] _
		val sttr = sequence(List(unitis(1), unitis(2), unitis(3)))
		val (IntState(n), r) = sttr.evolve(zerost)
		n should equal(0) //unit does not change state
		r should equal(List(1,2,3))
	}

	"repeatGetAll" should "return the list of the intermediate results of repeating the StateTransition" in {
		val sttr = repeatGetAll(intstate_incrementer, 3)
		val (IntState(n), r) = sttr.evolve(zerost)
		n should equal(3)
		r should equal(List(1,2,3))
	}

	"modify" should "work like an imperative assignment" in {
		def increment(s: IntState) : IntState = { 
			val IntState(n) = s
			IntState(n + 1) 
		}
		
		val (IntState(n), r) = modify(increment).evolve(zerost)
		n should equal(1)
		r should equal(())
	}
}