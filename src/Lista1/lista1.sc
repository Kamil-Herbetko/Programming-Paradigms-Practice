// Kamil Herbetko

// zadanie 1
def flatten1[A](xss: List[List[A]]): List[A] =
  if (xss.length == 0) then Nil else xss.head ::: flatten1(xss.tail)

flatten1(List(List(5,6), List(1,2,3))) == List(5,6,1,2,3)
flatten1(List(List())) == Nil
flatten1(List(List("Ma", "ły "), List("Ko", "tek"))) == List("Ma", "ły ", "Ko", "tek")

// zadanie 2
def count[A](x: A, xs: List[A]): Int =
  if (xs.length == 0) then 0
  else if (xs.head == x) then 1 + count(x, xs.tail)
  else count(x, xs.tail)

count('a', List('a', 'l', 'a')) == 2
count(5, List()) == 0
count(1, List(1, 2, 3, 4, 5, 1, 1, 3)) == 3
count(Nil, List()) == 0
count(Nil, List(Nil)) == 1

// zadanie 3
def replicate[A](x: A, n: Int): List[A] =
  if (n <= 0) then Nil
  else x :: replicate(x, n - 1)

replicate("la", 3) == List("la", "la", "la")
replicate("komar", 0) == Nil
replicate(2, 5) == List(2, 2, 2, 2, 2)
replicate(Nil, 2) == List(Nil, Nil)
replicate("la", -3)

// zadanie 4
def sqrList(xs: List[Int]): List[Int] =
  if (xs.length == 0) then Nil
  else xs.head * xs.head :: sqrList(xs.tail)

sqrList(List(1, 2, 3, -4)) == List(1, 4, 9, 16)
sqrList(List(0, -9, -3, 3, 9, 0)) == List(0, 81, 9, 9, 81, 0)
sqrList(List()) == Nil

val sqrList2: List[Int] => List[Int] = xs =>
  if xs == Nil then Nil
  else xs.head * xs.head :: sqrList2(xs.tail)

// zadanie 5
def palindrome[A](xs: List[A])=
  xs == xs.reverse

palindrome(List('a', 'l', 'a')) == true
palindrome(List(1, 2, 3, 2, 1)) == true
palindrome(List('n', 'i', 'e')) == false
palindrome(Nil) == true

// zadanie 6
def listLength[A](xs: List[A]): Int =
  if xs == Nil then 0
  else 1 + listLength(xs.tail)

listLength(List('a', 'b', 'c')) == 3
listLength(List()) == 0
listLength(List(1, 2, 3, 4)) == 4
listLength(List(Nil, Nil)) == 2





