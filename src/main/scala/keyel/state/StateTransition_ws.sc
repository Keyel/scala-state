package fpinscala.state.keyel

import StateTransition._

object StateTransition_ws {
  
  type IntState = Int

  val incrementer = StateTransition[IntState, Int]( n => (n+1, n+1) )
                                                  //> incrementer  : fpinscala.state.keyel.StateTransition[fpinscala.state.keyel.S
                                                  //| tateTransition_ws.IntState,Int] = StateTransition(<function1>)
  
  val (onestate, one) = incrementer.trans(0)      //> onestate  : fpinscala.state.keyel.StateTransition_ws.IntState = 1
                                                  //| one  : Int = 1
  val addThree = repeat(incrementer, 3)           //> addThree  : fpinscala.state.keyel.StateTransition[fpinscala.state.keyel.Stat
                                                  //| eTransition_ws.IntState,Int] = StateTransition(<function1>)
  
  val (threeState, three) = addThree.trans(0)     //> threeState  : fpinscala.state.keyel.StateTransition_ws.IntState = 3
                                                  //| three  : Int = 3
  val double = StateTransition[IntState, Int] (n => (n * 2, n * 2))
                                                  //> double  : fpinscala.state.keyel.StateTransition[fpinscala.state.keyel.StateT
                                                  //| ransition_ws.IntState,Int] = StateTransition(<function1>)
  val addThreeThenMultiply = compose(List(double, addThree))
                                                  //> addThreeThenMultiply  : fpinscala.state.keyel.StateTransition[Int,Int] = Sta
                                                  //| teTransition(<function1>)
  val (sixState, six) = addThreeThenMultiply.trans(0)
                                                  //> sixState  : Int = 6
                                                  //| six  : Int = 6
  val (s3, x) = compose(List(addThree, double)).trans(0)
                                                  //> s3  : Int = 3
                                                  //| x  : Int = 3
  
  val doubleStr = double map { n => n.toString }  //> doubleStr  : fpinscala.state.keyel.StateTransition[fpinscala.state.keyel.Sta
                                                  //| teTransition_ws.IntState,String] = StateTransition(<function1>)
  doubleStr.trans(1)                              //> res0: (fpinscala.state.keyel.StateTransition_ws.IntState, String) = (2,2)
  
  val quad = double map { n => n * 2 }            //> quad  : fpinscala.state.keyel.StateTransition[fpinscala.state.keyel.StateTra
                                                  //| nsition_ws.IntState,Int] = StateTransition(<function1>)
  val y = incrementer.mapbin(incrementer) { _ + _ }
                                                  //> y  : fpinscala.state.keyel.StateTransition[fpinscala.state.keyel.StateTransi
                                                  //| tion_ws.IntState,Int] = StateTransition(<function1>)
  
  quad.trans(1)                                   //> res1: (fpinscala.state.keyel.StateTransition_ws.IntState, Int) = (2,4)
  
}