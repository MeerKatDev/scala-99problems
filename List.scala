import scala.annotation.tailrec

object List {

  def main(args: Array[String]): Unit = {}
  // Problems done in a creative way, more than think at performance :)
  
  // P01: Find the last element of a list.
  // last(List(1, 1, 2, 3, 5, 8)) => 8
  def last(numbers: List[Int]) = numbers.reverse(0)
  
  // P02: Find the last but one element of a list
  // penultimate(List(1, 1, 2, 3, 5, 8)) => 5
  def penultimate(numbers: List[Int]) = numbers( numbers.size-2 )
  
  // P03: FInd the kth element of a list
  // nth(2, List(1, 1, 2, 3, 5, 8)) => 2
  def nth(idx: Int, numbers: List[Int]) = last(numbers.slice(0,idx+1))
  
  // P04: Find the number of elements of a list
  def length(numbers: List[Int]) = numbers.map(s=>1).foldLeft(0)(_+_)
  
  // P05: Reverse a list 
  // reverse(List(1, 1, 2, 3, 5, 8))
  // Note: I don't know how to use MapConserve here, to correct
  // numbers.map( s=> sub( numbers.length-1-sub.indexOf(s) ) )
  def reverse(numbers: List[Int]): List[Int] = numbers.reverse
  
  // P06: Find out whether a list is a palindrome.
  // isPalindrome(List(1, 2, 3, 2, 1)) => true
  def isPalindrome(numbers: List[Int]) = 
    numbers.dropRight(length(numbers)/2) == reverse(numbers.drop(length(numbers)/2))
  
  // P07: Flatten a nested list structure.
  // flatten(List(List(1, 1), 2, List(3, List(5, 8)))) => List(1, 1, 2, 3, 5, 8)
  def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
    case head :: tail => head :: flatten(tail)
  }
  
  // P08: Eliminate consecutive duplicates of list elements, e.g. If a list contains repeated elements,
  // they should be replaced with a single copy of the element. The order of the elements should not be changed.
  // compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) => List('a, 'b, 'c, 'a, 'd, 'e)
  def compress(list: List[Char]): List[Char] = {
    @tailrec def checkNext(acc: List[Char], l: List[Char]): List[Char] = {
      if (!l.nonEmpty) acc
      else checkNext( if (acc.contains(l.head)) acc else acc:::List(l.head), l.tail)
    }
    checkNext(List(list.head),list.tail)
  }
  
  // P09: Pack consecutive duplicates of list elements into sublists, e.g. If a list contains repeated elements,
  // they should be placed in separate sublists.
  // pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) => 
  //   List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack(list: List[Char]): List[List[Char]] = {
	@tailrec def checkNext(a: List[List[Char]], prev: Char, l: List[Char]): List[List[Char]] = {
	  if (!l.nonEmpty) a
	  else {
	    val res = if (prev == l.head) ((l.head::a.head)::a.tail) else List(l.head)::a
	    checkNext(res, l.head, l.tail)
	  }
	}
	checkNext(List(List[Char](list.last)), list.last, list.init.reverse) 
  }
  
  // P10: Run-length encoding of a list.
  // Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
  // Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
  // encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) =>
  //   List[(Int, Char)] = List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))
  def encode(list: List[Char]): List[(Int,Char)] = {
    def recurse(list: List[Char], prev: Char, acc: List[(Int,Char)]): List[(Int,Char)] = list match {
      case Nil => acc
      case h::tail if h == prev => recurse(tail,h,(acc.head._1+1,h)::acc.tail)
      case h::tail => recurse(tail, h, (1,h)::acc)
    }
    recurse( list.init.reverse, list.last, List[(Int,Char)]((1,list.last))) 
  }
  
  // P11: Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.   
  // Only elements with duplicates are transferred as (N, E) terms. 
  // encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) =>
  //   List[Any] = List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))
  def encodeModified(list: List[Char]): List[Any] = {
    def recurse(list: List[Char], prev: Char, acc: List[Any]): List[Any] = list match {
      case Nil => acc
      case h::tail if h == prev =>  println(h); recurse(tail,h,((acc.head.asInstanceOf[(Int,Char)])._1+1,h)::acc.tail)
      case h1::h2::tail if h1 != h2 =>  println(h1,h2::tail); recurse(h2::tail,h1,h1::acc)
      case h::tail => println(3,h); recurse(tail, h, (1,h)::acc)
    }
    recurse( list.init.reverse, list.last, List[(Int,Char)]((1,list.last))) 
  }
  
  // P12: Decode a run-length encoded list.
  // Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  // List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))
  //   => List[Char] = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  def decode(list: List[(Int,Char)]): List[Char] = {
    def recurse(list: List[(Int,Char)], acc: List[Char]): List[Char] = list match {
      case Nil => acc
      case (a,b)::tail => recurse((if(a>1) (a-1,b)::tail else tail), b::acc)
    }
    recurse(list.reverse,List[Char]())
  }
  
} 