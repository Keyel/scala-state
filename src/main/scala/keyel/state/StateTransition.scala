package keyel.state

import StateTransition._

case class StateTransition[S, +A](trans: S => (S, A)) {
  def flatMap[B](f: A => StateTransition[S, B]): StateTransition[S, B] = StateTransition { s: S =>
    val (s2, a) = trans(s)
    f(a).trans(s2)
  }

  def map[B](f: A => B): StateTransition[S, B] = 
    flatMap { a => unit(f(a)) }
  
  def mapbin[B, C] ( st : StateTransition[S, B] )(f: (A,B) => C): StateTransition[S, C] =
    flatMap { a => st.map { b => f(a,b) } }

  def mapbin_viaFor[B, C] ( st : StateTransition[S, B] )(f: (A,B) => C): StateTransition[S, C] =
    for{
      a <- this  //a bejovo s-et transzformalja (s2, a) -ra , es kiveszi belole az a-t
      b <- st
    } yield f(a,b)
  
}

object StateTransition {
  def unit[S, A](a: A): StateTransition[S, A] = StateTransition { s => (s, a) }

  def repeat[S, A](transition: StateTransition[S, A], count: Int): StateTransition[S, A] = StateTransition { s =>
    val (newState, a) = transition.trans(s)
    if (count == 1)
      (newState, a)
    else
      repeat(transition, count - 1).trans(newState)
  }

  //eloszor az utolso transitiont hajtjuk vegre
  def compose[S, A](transitions: List[StateTransition[S, A]]): StateTransition[S, A] = transitions.reduceRight { (t1, t2) =>
    StateTransition { s =>
      val (s2, _) = t2.trans(s)
      t1.trans(s2)
    }
  }

}

