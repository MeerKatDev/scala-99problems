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
  //   => List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse(numbers: List[_]): List[_] = {	  
    def recurse(list: List[_], acc: List[_], n: Int): List[_] = list match {
      case Nil => acc     
      case head::tail if(n==0) => acc 
      case head::tail => recurse(tail,head::acc, n-1)      
    }
    recurse(numbers, List(), numbers.size)
  }
    
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
  
  // P13: Run-length encoding of a list (direct solution). DUPLICATED
  // Implement the so-called run-length encoding data compression method directly. 
  // I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  // encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
  //   => List[(Int, Char)] = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
  def encodeDirect(list: List[Char]): List[(Int,Char)] = {
    def recurse(list: List[Char], prev: Char, acc: List[(Int,Char)]): List[(Int,Char)] = list match {
      case Nil => acc
      case h::tail if h == prev => recurse(tail,h,(acc.head._1+1,h)::acc.tail)
      case h::tail => recurse(tail, h, (1,h)::acc)
    }
    recurse( list.init.reverse, list.last, List[(Int,Char)]((1,list.last))) 
  } 
  
  // P14 (*) Duplicate the elements of a list.
  // duplicate(List('a', 'b', 'c', 'c', 'd'))
  //   => List[Symbol] = List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd')
  def duplicate(list: List[Char]): List[Char] = {
    def recurse(init: List[Char], acc: List[Char]): List[Char] = init match {
      case Nil => acc
      case a::tail => recurse(tail,a::a::acc)
    }
    recurse(list.init.reverse,List(list.last,list.last) )
  }  
  // P15: Duplicate the elements of a list a given number of times.
  // duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  //    => List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  def duplicateN(Num: Int, list: List[Symbol]):List[Symbol] = {
    def recurse(init: List[Symbol], acc: List[Symbol], n: Int): List[Symbol] = init match {
      case Nil => acc
      case a::tail if n>1 => recurse(a::tail,a::acc, n-1)
      case a::tail if n==1 => recurse(tail,a::acc, Num)
    }
    recurse(list.reverse,List[Symbol](), Num)    
  } 
  
  // P16: Drop every Nth element from a list.
  // drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //   => List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  def drop(Num: Int, list: List[Symbol]): List[Symbol] = {
    def recurse(init: List[Symbol], acc: List[Symbol], n: Int): List[Symbol] = init match {
      case Nil => acc
      case a::tail if n>1 => recurse(tail,a::acc, n-1)
      case a::tail if n==1 => recurse(tail,acc, Num)
    }
    recurse(list.reverse,List[Symbol](), Num)    
  }
  
  // P17: Split a list into two parts.
  // The length of the first part is given. Use a Tuple for your result.
  // split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //   => (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split(Num: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    def recurse(init: List[Symbol], acc: (List[Symbol], List[Symbol]), n: Int): (List[Symbol], List[Symbol]) = init match {
      case Nil => acc.swap
      case a::tail if n>0 => recurse(tail,(a::acc._1,acc._2), n-1)
      case a::tail if n==0 => recurse(tail,(acc._1,a::acc._2), 0)
    }
    recurse(list.reverse,(List[Symbol](), List[Symbol]()), list.length-Num)    
  }
 
  // P18: Extract a slice from a list.
  // Given two indices, I and K, the slice is the list containing the elements
  // from and including the Ith element up to but not including the Kth element of the original list.
  // Start counting the elements with 0.
  // slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //   => List[Symbol] = List('d, 'e, 'f, 'g)
  def slice(start: Int, end: Int, list: List[Symbol]): List[Symbol] = {
    def recurse(init: List[Symbol], acc: List[Symbol], n: Int): List[Symbol] = init match {
      case Nil => acc
      case a::tail if n>0 || acc.length == end-start => recurse(tail,acc, n-1)
      case a::tail if n==0 => recurse(tail,a::acc, 0)
    }
    recurse(list.reverse,List[Symbol](), list.length-end)    
  }
  
  // P19: Rotate a list N places to the left.
  // rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //   => List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  // rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //   => List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  def rotate(num: Int, list: List[Symbol]): List[Symbol] = {
    var l = (List[Symbol](), List[Symbol]())
    if(num<0){       
      l = split(-num,list.reverse).swap 
      l = (l._1.reverse,l._2.reverse)
    }
    else{ l = split(num,list) }
    l._2:::l._1
  }

  // P20: Remove the Kth element from a list.
  // Return the list and the removed element in a Tuple. 
  // Elements are numbered from 0.
  // removeAt(1, List('a, 'b, 'c, 'd))
  //   => (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def removeAt(num: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
    def recurse(init: List[Symbol], acc: List[Symbol], n: Int): (List[Symbol], Symbol) = init match {
      case a::tail if n>1 => recurse(tail,a::acc, n-1)
      case a::tail if n==1 => (tail:::acc,a)
    }
    recurse(list.reverse,List[Symbol](), list.length-num)    
  }
  
  // P21: Insert an element at a given position into a list.
  // insertAt('new, 1, List('a, 'b, 'c, 'd))
  //   => List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt(new_symbol: Symbol, num: Int, list: List[Symbol]): List[Symbol] = {
    def recurse(init: List[Symbol], acc: List[Symbol], n: Int): List[Symbol] = init match {
      case a::tail if n>1 => recurse(tail,a::acc, n-1)
      case a::tail if n==1 => (tail:::(new_symbol::a::acc))
    }
    recurse(list.reverse,List[Symbol](), list.length-num)    
  }
  // P22: Create a list containing all integers within a given range.
  // range(4, 9)
  //   => List[Int] = List(4, 5, 6, 7, 8, 9)
  
  def range(a: Int, b: Int): List[Int] = {
    def recurse(acc: List[Int], n: Int): List[Int] = acc match {
      case acc if n==b => n::acc
      case acc => recurse(n::acc,n+1)
    }
    recurse(List[Int](), a).reverse    
  }
  
  // P23: Extract a given number of randomly selected elements from a list.
  // randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  //   => List[Symbol] = List('e, 'd, 'a)
  // Hint: Use the solution to problem P20
  def randomSelect(num: Int, list: List[Symbol]): List[Symbol] = {
    val r = new Random
    @tailrec def recurse(acc: List[Symbol],n: Int): List[Symbol] = {
      if(n==0) acc
      else {
        val removed = removeAt(r.nextInt(list.size),list)._2
        if(acc contains removed)
          recurse(acc, n)
        else
          recurse(removed::acc, n-1)
      }
    }
    recurse(List[Symbol](),num)         
  }
  
  // P24: Lotto: Draw N different random numbers from the set 1..M.
  // lotto(6, 49)
  //   => List[Int] = List(23, 1, 17, 33, 21, 37)
  // Hint: Use the solution of problem P23.
  def lotto(howMany: Int, _interval: Int): List[Int] = {
    val interval = range(1,_interval)
    val r = new Random
    def remove(num: Int, list: List[Int]): (List[Int], Int) = {
      def recurse(init: List[Int], acc: List[Int], n: Int): (List[Int], Int) = init match {
        case a::tail if n>1 => recurse(tail,a::acc, n-1)
        case a::tail if n==1 => (tail:::acc,a)
      }
      recurse(list.reverse,List[Int](), list.length-num)    
    }
    @tailrec def recurse(acc: List[Int],n: Int): List[Int] = {
      if(n==0) acc
      else {
        val removed = remove(r.nextInt(interval.size),interval)._2
        if(acc contains removed)
          recurse(acc, n)
        else
          recurse(removed::acc, n-1)
      }
    }
    recurse(List[Int](),howMany)
  }
  
  // P25: Generate a random permutation of the elements of a list.
  // randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  //   => List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)  
  def randomPermute(list: List[Symbol]): List[Symbol] = {
    randomSelect(list.size,list).reverse
  }
  println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  
  // P26: Generate the combinations of K distinct objects chosen from the N elements of a list.
  // In how many ways can a committee of 3 be chosen from a group of 12 people? 
  // We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). 
  // For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
  // combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  //   => List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  def combinations(num: Int, list: List[Symbol]): List[List[Symbol]] = { 
    def binomialCoefficient(n:Int, k:Int)=fact(n) / (fact(k) * fact(n-k))
    def fact(n:Int):Int=if (n==0) 1 else n*fact(n-1)   
    
    @tailrec def recurse(acc : List[List[Symbol]], n: Int): List[List[Symbol]] = {
      if(n==0) acc
      else {
        val gen = randomSelect(num,list)
        if(acc contains gen)
          recurse(acc, n)
        else
          recurse(gen::acc, n-1)
      }
    }

    recurse( List[List[Symbol]](),binomialCoefficient(list.size,num)) 
  }
  
} 