// Kamil Herbetko

// Zadanie 2
def curry3[A, B, C, D](f: (A, B, C) => D)(a: A)(b: B)(c: C) = f (a, b, c)
def curry3[A, B, C, D] = (f: (A, B, C) => D) => (a: A) => (b: B) => (c: C) => f (a, b, c)
// def curry3[A, B, C, D](f: (A, B, C) => D)(a: A)(b: B)(c: C): D
// def curry3[A, B, C, D] => ((A, B, C) => D) => A => B => C => D

val plus: (x1: Int, x2: Int, x3: Int) => Int = (x1, x2, x3) => x1 + x2 + x3
val concat3: (x1: String, x2: String, x3: String) => String = (x1, x2, x3) => x1 + x2 + x3

val curriedPlus = curry3(plus)
val curriedConcat3 = curry3(concat3)

curriedPlus(1)(2)(3) == 6
curriedConcat3("a")("b")("c") == "abc"

def uncurry3[A, B, C, D](f: A => B => C => D)(a: A, b: B, c: C) = f (a)(b)(c)
def uncurry3[A, B, C, D] = (f: A => B => C => D) => (a: A, b: B, c: C) => f (a)(b)(c)
// def uncurry3[A, B, C, D](f: A => B => C => D)(a: A, b: B, c: C): D
// def uncurry3[A, B, C, D] => (A => B => C => D) => (A, B, C) => D

val cPlus: (x1: Int) => (x2: Int) => (x3: Int) => Int = x1 => x2 => x3 => x1 + x2 + x3
val cConcat3: (x1: String) => (x2: String) => (x3: String) => String = x1 => x2 => x3 => x1 + x2 + x3

val uncurriedCPlus= uncurry3(cPlus)
val uncurriedCConcat3 = uncurry3(cConcat3)

uncurriedCPlus(1, 2, 3) == 6
uncurriedCConcat3("a", "b", "c") == "abc"


// Zadanie 3
val sumProd: (xs: List[Int]) => (Int, Int) = xs => (xs.foldLeft (0, 1))((sumAndProd, h) => (sumAndProd._1 + h, sumAndProd._2 * h))

sumProd (List(4, 3, 2, 1)) == (10, 24)
sumProd (List(1, 2, 3, 4)) == (10, 24)
sumProd (Nil) == (0, 1)

// Zadanie 5
// a
def insertionsort[A](pred: A => A => Boolean)(xs: List[A]) =
  def insert(pred: A => A => Boolean)(x: A)(xs: List[A]): List[A] =
    xs match
      case Nil => List(x)
      case h :: t => if pred (x) (h) then x :: xs
                     else h :: (insert (pred) (x) (t))
  def insertionsort_iter(xs: List[A], ys: List[A]): List[A] =
    xs match
      case Nil => ys
      case h :: t => insertionsort_iter (t, (insert(pred)(h)(ys)))
  insertionsort_iter(xs, Nil)

insertionsort ((x1: Int) => (x2: Int) => x1 < x2) (List(5, 4, 4, 2, 5, 3)) == List(2, 3, 4, 4, 5, 5)
insertionsort ((x1: String) => (x2: String) => x1 < x2) (List("zs", "ab1", "dc", "cd")) ==  List("ab1", "cd", "dc", "zs")
insertionsort ((k1: (Int, Int)) => (k2: (Int, Int)) => k1._1 < k2._1) (List((6, 2), (6, 3), (4, 2), (1, 3), (1, 2))) == List((1, 3), (1, 2), (4, 2), (6, 2), (6, 3))
insertionsort ((k1: (Int, Int)) => (k2: (Int, Int)) => k1._1 < k2._1) (List((6, 3), (6, 2), (1, 2), (4, 2), (1, 3))) == List((1, 2), (1, 3), (4, 2), (6, 3), (6, 2))

// b
def mergesort[A](pred: A => A => Boolean)(xs: List[A]): List[A] =
  def halfsplit(n: Int, xs: List[A]): (List[A], List[A]) =
    (n, xs) match
      case (0, xs) => (Nil, xs)
      case (n, h :: t) =>
        val (l1, l2) = halfsplit (n - 1, t)
        (h :: l1, l2)

  def merge(pred: A => A => Boolean, xs: List[A], ys: List[A]): List[A] =
    (xs, ys) match
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (h1 :: t1, h2 :: t2) => if pred(h1)(h2) then h1 :: merge(pred, t1, ys)
                                   else h2 :: merge(pred, xs, t2)

  xs match
    case Nil => Nil
    case h :: Nil => List(h)
    case h :: t =>
      val (h1, h2) = halfsplit(xs.length / 2, xs)
      merge(pred, mergesort(pred)(h1), mergesort(pred)(h2))

mergesort ((x1: Int) => (x2: Int) => x1 <= x2) (List(5, 4, 4, 2, 5, 3)) == List(2, 3, 4, 4, 5, 5)
mergesort ((x1: String) => (x2: String) => x1 <= x2) (List("zs", "ab1", "dc", "cd")) ==  List("ab1", "cd", "dc", "zs")
mergesort ((k1: (Int, Int)) => (k2: (Int, Int)) => k1._1 <= k2._1) (List((6, 2), (6, 3), (4, 2), (1, 3), (1, 2))) == List((1, 3), (1, 2), (4, 2), (6, 2), (6, 3))
mergesort ((k1: (Int, Int)) => (k2: (Int, Int)) => k1._1 <= k2._1) (List((6, 3), (6, 2), (1, 2), (4, 2), (1, 3))) == List((1, 2), (1, 3), (4, 2), (6, 3), (6, 2))

