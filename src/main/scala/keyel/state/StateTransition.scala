package keyel.state

import StateTransition._

case class StateTransition[S, +A](evolve: S => (S, A)) {
  def flatMap[B](f: A => StateTransition[S, B]): StateTransition[S, B] = StateTransition { s: S =>
    val (s2, a) = evolve(s)
    f(a).evolve(s2)
  }

  //a mdified statetransition, changing the result without further state evolution
  def map[B](f: A => B): StateTransition[S, B] = 
    flatMap { a => unit(f(a)) }
  
  //evolves a state with two kind of stateTransition, getting the intermediate and final result, and applying a binary function to them
  def mapbin[B, C] ( st : StateTransition[S, B] )(f: (A,B) => C): StateTransition[S, C] =
    flatMap { a => st.map { b => f(a,b) } }

  def mapbin_viaFor[B, C] ( st : StateTransition[S, B] )(f: (A,B) => C): StateTransition[S, C] =
    for{
      a <- this  //evolve by this, get the result from it to a
      b <- st    //evolve by st, get the result to b
    } yield f(a,b)  //the combined evolutions result will be mapped to f(a,b)
  
}

object StateTransition {
  //returns a unit state transition, with result a
  def unit[S, A](a: A): StateTransition[S, A] = StateTransition { s => (s, a) }

  //returns a state transition which is equivalent with $transition applied $count times
  def repeat[S, A](transition: StateTransition[S, A], count: Int): StateTransition[S, A] = StateTransition { s =>
    val (newState, a) = transition.evolve(s)
    if (count == 1)
      (newState, a)
    else
      repeat(transition, count - 1).evolve(newState)
  }

  //applying transitions from right to left, forgetting intermediate results
  def compose[S, A](transitions: List[StateTransition[S, A]]): StateTransition[S, A] = transitions.reduceRight { (t1, t2) =>
    t2.mapbin(t1) { (a2, a1) => a1 }
  }

  //applying transitions from right to left, collecting the intermediate results
  def sequence[S, A](transitions: List[StateTransition[S, A]]) : StateTransition[S, List[A]] = transitions.foldRight(unit[S,List[A]](List[A]())) { (t, acc) =>
    t.mapbin(acc) { (a, as) => a :: as }
  }

  def repeatGetAll[S,A](transition: StateTransition[S,A], count: Int) : StateTransition[S, List[A]] =
    sequence(List.fill(count)(transition))


  def modify[S](f: S => S): StateTransition[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  //get returns a stateTransition, which result is the state itself, without changing the state
  def get[S] : StateTransition[S,S] = StateTransition { s: S =>
    (s, s)
  }

  def set[S](s: S) : StateTransition[S,Unit] = StateTransition{ _ => 
    (s, ())
  }
}

