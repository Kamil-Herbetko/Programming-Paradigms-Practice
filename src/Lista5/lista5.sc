// Kamil Herbetko

// Zadanie 1
def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] =
  def lrepeat_iter(n: Int)(lys: LazyList[A]): LazyList[A] =
    (n, lys) match
    case (_, LazyList()) => LazyList()
    case (1, x #:: tail) => x #:: (lrepeat_iter(k)(tail))
    case (_, x #:: _ ) => x #:: (lrepeat_iter(n - 1)(lys))
  lrepeat_iter(k)(lxs)

lrepeat(3)(LazyList('a','b','c','d')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
lrepeat(3)(LazyList()).take(15).toList == List()

// Zadanie 2
val lfib: LazyList[Int] =
  def lfib_iter(l0: Int)(l1: Int): LazyList[Int] = l0 #:: lfib_iter(l1)(l0 + l1)
  lfib_iter(0)(1)

lfib.take(12).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
lfib.take(2).toList == List(0, 1)
lfib.take(1).toList == List(0)
lfib.take(0).toList == List()

// Zadanie 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

// a
def lBreadth[A](ltree: lBT[A]) =
  def lBreadth_iter(xs: List[lBT[A]]): LazyList[A] =
    xs match
      case LEmpty :: t => LazyList()
      case LNode(x, lLT, lRT) :: t => x #:: (lBreadth_iter(t ::: List(lLT(), lRT())))
  lBreadth_iter(List(ltree))

// b
def lTree(n: Int): lBT[Int] = LNode(n, ( () => lTree(2 * n)), (() => lTree(2 * n + 1)))

lBreadth(lTree(1)).take(20).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
lBreadth(LEmpty).take(20).toList == List()




