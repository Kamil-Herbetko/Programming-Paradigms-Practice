import scala.annotation.tailrec
// Kamil Herbetko

// zadanie 2
def fib (n: Int): Int =
  if n <= 0 then 0
  else if n == 1 then 1
  else fib (n - 1) + fib (n - 2)

def fibTail (n: Int) =
  @tailrec
  def fibTail_iter (n: Int, accum1: Int, accum2: Int): Int=
    if n < 0 then accum1
    else if n == 0 then accum2
    else fibTail_iter (n - 1, accum2, accum1 + accum2)
  fibTail_iter(n - 1, 0, 1)

fib (5) == 5
fib (0) == 0
fib (-3) == 0
fibTail (5) == 5
fibTail (0) == 0
fibTail (-3) == 0
fib (42) == 267914296
fibTail (42) == 267914296

// zadanie 3
def root3 (a: Double) =
  @tailrec
  def root3_iter (a: Double, accum: Double): Double =
    if math.abs (math.pow (accum, 3.0) - a) <= math.abs (a) * math.pow (10.0, -15.0) then accum
    else root3_iter (a, accum + (a / (accum * accum) -accum) / 3.0)

  if a > 1.0 then root3_iter (a, a / 3) else root3_iter (a, a)

root3 (0.0) - 0.0 <= math.pow (10.0, -15.0)
root3 (10.0) - 2.154434690031883 <= math.pow (10.0, -15.0)
root3 (-10.0) + 2.1544346900318837 <= math.pow (10.0, -15.0)


val root3_2_iter: (Double, Double) => Double = (a, accum) =>
  if math.abs (math.pow (accum, 3.0) - a) <= math.abs (a) * math.pow (10.0, -15.0) then accum
  else root3_2_iter(a, accum + (a / (accum * accum) - accum) / 3.0)

val root3_2: Double => Double = a =>
  if a > 1.0 then root3_2_iter(a, a / 3) else root3_2_iter (a, a)

root3_2 (0.0) - 0.0 <= math.pow (10.0, -15.0)
root3_2 (10.0) - 2.154434690031883 <= math.pow (10.0, -15.0)
root3_2 (-10.0) + 2.1544346900318837 <= math.pow (10.0, -15.0)

// zadanie 4
// a)
val List(_, _, xa, _, _) = List(-2, -1, 0, 1, 2)

// b)
val List((_, _), (xb, _)) = List((1, 2), (0, 1))

// zadanie 5
def initSegment[A](xs: List[A], ys: List[A]): Boolean =
  (xs, ys) match
  case (Nil, ys) => true
  case (h1 :: t1, h2 :: t2) if h1 == h2 => initSegment (t1, t2)
  case _ => false

initSegment(List(1, 2, 3, 4), List(1, 2, 3, 4, 5, 6)) == true
initSegment(Nil, List(1, 2, 3)) == true
initSegment(List(1, 2, 3, 4, 5), List(1, 2, 6, 7, 8, 9)) == false
initSegment(List('a', 'b', 'c'), List('a', 'b', 'c', 'd')) == true

// zadanie 6
// a)
def replaceNth[A](xs: List[A], n: Int, x: A): List[A] =
  (xs, n) match
    case (Nil, n) => Nil
    case (h :: t, n) if n <= 0 => x::t
    case (h :: Nil, n) => List(x)
    case (h :: t, n) => h::replaceNth(t, n - 1, x)

replaceNth (List('o','l', 'a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's') == List('o','s','a', 'm', 'a', 'k', 'o', 't', 'a')
replaceNth (List(1, 2, 3, 4, 5), 3, 7) == List(1, 2, 3, 7, 5)
replaceNth (Nil, 3, 7) == Nil
replaceNth (List(1, 2, 3, 4, 5), 0, 6) == List(6, 2, 3, 4, 5)
replaceNth (List(1, 2, 3), -5, 6) == List(6, 2, 3)
replaceNth (List(1, 2, 3, 4, 5), 10, 8) == List(1, 2, 3, 4, 8)

