package example

class Pouring(capacity: Vector[Int]) {

  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, capacity(glass))

  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amt: Int = (capacity(to) - state(to)) min state(from)
      state updated (to, state(to) + amt) updated (from, state(from) - amt)
    }

  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (f <- glasses; t <- glasses if f != t) yield Pour(f, t));

  class Path(history: List[Move]) {
    def endState: State = (history foldRight initialState)(_ change _)
    def extend(move: Move) = new Path(move :: history)
    override def toString = (history.reverse mkString " ") + "=>" + endState
  }

  val initialPath = new Path(Nil)

  def from(paths: Set[Path], vis: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !vis.contains(next.endState)
      } yield next
//      for {
//        path <- paths
//        next <- moves map path.extend
//      } println(next, next.endState, vis.contains(next.endState))
//      println("insidefrom",more)
      paths #:: from(more, vis ++ more.map(_.endState))
    }
  }

  val pathsets = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] = {
    for {
      lp <- pathsets
      path <- lp
      g <- path.endState
      if g == target
    } yield path
  }
}


