// Kamil Herbetko

// Zadanie 3
sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val tt = Node(1,
              Node(2,
                    Node(4,
                          Empty,
                          Empty
                        ),
                    Empty
                  ),
              Node(3,
                    Node(5,
                          Empty,
                          Node(6,
                                Empty,
                                Empty
                              )
                        ),
                    Empty
                  )
              )
val sBT = Node("a",
                  Node("b",
                          Node("d",
                                  Node("g",
                                      Empty,
                                      Empty
                                      ),
                              Empty
                              ),
                          Node("e",
                              Empty,
                              Empty
                              )
                      ),
                  Node("c",
                          Node("f",
                              Empty,
                              Empty
                              ),
                          Empty
                      )
              )
def breadthBT[A](bt: BT[A]) =
  def breadthBT_iter(xs: List[BT[A]]): List[A] =
    xs match
      case Nil => Nil
      case Empty :: t => breadthBT_iter (t)
      case Node(v, t1, t2) :: t => v :: breadthBT_iter (t ::: List(t1, t2))
  breadthBT_iter (List(bt))

breadthBT(tt) == List(1, 2, 3, 4, 5, 6)
breadthBT(sBT) == List("a", "b", "c", "d", "e", "f", "g")
breadthBT(Empty) == Nil

// Zadanie 4
// a
def intBT[A](bt: BT[A]) =
  def intBT_iter(bt: BT[A], depth: Int): Int =
    bt match
      case Empty => 0
      case Node(v, t1, t2) => depth + intBT_iter (t1, depth + 1) + intBT_iter (t2, depth + 1)
  intBT_iter (bt, 0)

intBT(tt) == 9
intBT(sBT) == 11
intBT(Empty) == 0

// b
def extBT[A](bt: BT[A]) =
  def extBT_iter(bt: BT[A], depth: Int): Int =
    bt match
      case Empty => depth
      case Node(v, t1, t2) => extBT_iter (t1, depth + 1) + extBT_iter (t2, depth + 1)
  extBT_iter (bt, 0)

extBT(tt) == 21
extBT(sBT) == 25
extBT(Empty) == 0

// Zadanie 5
sealed trait Graphs[A]
  case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0, 2)
    case n => throw new Exception(s"Graph g: node $n doesn't exist")
)

val sg = Graph((s: String) =>
  s match
    case "a" => List("a", "b", "d")
    case "b" => List("g")
    case "c" => Nil
    case "d" => List("c", "e")
    case "e" => Nil
    case "f" => List("e")
    case "g" => List("d", "f")
    case str => throw new Exception(s"Graph g: node $str doesn't exist")
)

def depthSearch[A](graph: Graph[A])(startNode: A) =
  def search(visited: List[A], queue: List[A]): List[A] =
    queue match
      case Nil => Nil
      case h :: t => if visited.contains(h) then search (visited, t)
                     else h :: search (h :: visited, graph.succ(h) ::: t)
  search(Nil, List(startNode))

depthSearch(g)(4) == List(4, 0, 3, 2, 1)
depthSearch(sg)("a") == List("a", "b", "g", "d", "c", "e", "f")
depthSearch(g)(2) == List(2, 1, 0, 3, 4)


















