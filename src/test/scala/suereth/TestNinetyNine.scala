package suereth

import org.junit._

import Assert._
class TestNinetyNine {
  
  //TODO - Use Specs or some property based testing for awesomeness.
  
  @Test def x_01_last_in_list() {
    def last[A](list : List[A]) : A = list match {      
      case head :: Nil => head
      case _ :: tail => last(tail)
      case Nil => error("O NOES!")
    }
    val list = List(0,1,2,3,4)    
    assertEquals(list.last, last(list))
  }
  
  @Test def x_02_pnenultimate() {
    def penultimate[A](list : List[A]) : A = list match {
      case x :: _ :: Nil => x
      case head :: tail => penultimate(tail)
      case _ => error("O NOES")
    }
    val list = List(1, 1, 2, 3, 5, 8)
    assertEquals(5, penultimate(list))
  }
  
  @Test def x_03_nth() {
    def nth[A](n : Int, list : List[A]) : A = list match {
      case head :: _  if n == 0 => head
      case head :: tail => nth(n-1, tail)
      case _ => error("O NOES")
    }
    val list = List(1, 1, 2, 3, 5, 8)
    assertEquals(2, nth(2, list))
  }
  
  @Test def x_04_length() {
    def length(list : List[_]) = list.foldLeft(0) { (count, ignore) => count + 1 }
    
    assertEquals(6, length(List(1, 1, 2, 3, 5, 8)))
  }
  
  @Test def x_05_reverse() {
    
    def reverse[A](list : List[A]) : List[A] = list.foldLeft(List[A]()) { (newList, value) => value :: newList }
    val list = List(1, 1, 2, 3, 5, 8)
    val rlist = reverse(list)
    
    assertTrue(list.reverse == rlist)
  }
  
  @Test def x_06_isPalindrome() {
    def isPalindrome[A](list : List[A]) : Boolean = {      
       list == list.reverse
    }
    assertEquals(true, isPalindrome(List(4,3,2,1,2,3,4)))
    assertEquals(false, isPalindrome(List(2,3,2,1,2,3,4)))
  }
  
  @Test def x_07_flatten() {
    def flatten[A](list : List[_]) : List[Any] = list flatMap {
      _ match {
        case x : List[_] => flatten(x)
        case x => List(x)
      }
    }
    val listOfLists : List[_] = List(List(1, 1), 2, List(3, List(5, 8)))
    assertTrue(  flatten(listOfLists) == List(1, 1, 2, 3, 5, 8)  )
  }
  
  @Test def x_08_eliminate_duplicates = {
    def compress[A](list : List[A]) : List[A] = list match {
      case head :: Nil => List(head)
      case head :: tail => head :: compress(tail.dropWhile(_ == head))
      case Nil => Nil
    }
    val expected = List('a, 'b, 'c, 'a, 'd, 'e)
    val result = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assertEquals(expected, result)
  }

  
  def pack[A](list : List[A]) : List[List[A]] = list match {
     case head :: Nil => List(List(head))
     case head :: tail => 
     def extractLike(head : A, list : List[A], oldList : List[A] ) : (List[A], List[A]) = oldList  match {
        case x :: tail if x == head => extractLike(head, x :: list, tail)
        case x => (list, x)
     }
     val (like, unlike) = extractLike(head, head :: Nil, tail)
     like :: pack(unlike)
     case Nil => Nil
  }
  @Test def x_09_pack_duplicates() {
    val expected =  List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    val result =pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assertEquals(expected, result)

  }
  
  
  def encode[A](list : List[A]) = {
       pack(list) map {
         x =>
           (x.length, x.head)
       }
  }
  @Test def x_10_runlength_encoding() {
     
     val expected = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
     val actual  = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     assertEquals(expected, actual)
  }
  
  @Test def x_11_encodeModified() {
    def encodeModified[A](list : List[A]) = {
      pack(list).map(_ match {
        case x if x.length == 1 => x.head
        case x => (x.length, x.head)
      })
    }
    val expected = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    val actual = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assertEquals(expected, actual)
  }
  
  @Test def x_12_decodeModified() {
    def decodeModified(items : List[_]) : List[Any] = {
      items flatMap {
        case (n : Int, item) => (0 until n).map( x => item).toList
        case item => List(item)
      }
    }
    val expected =  List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val actual = decodeModified(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    assertEquals(expected, actual)
  }
  
  @Test def x_13_encode_direct() {
    def encode_direct(list : List[Any]) : List[_] = {
      Nil
    }
    
    val expected = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    val actual = encode_direct(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assertEquals(expected, actual)
  }
  
  @Test def x_14_duplicate() {
    def duplicate(list : List[Any]) : List[_] = list flatMap { x => List(x,x) }
    
    val expected =  List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    val actual = duplicate(List('a, 'b, 'c, 'c, 'd))
    assertEquals(expected, actual)
  }
  
  @Test def x_15_duplicateN() {
    def duplicateN(n : Int, list : List[Any]) : List[_] = list flatMap { x => for(i <- 0 until n) yield x }
    
    val expected =  List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    val actual = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    assertEquals(expected, actual)
  }
  
  @Test def x_16_drop() {
    def drop(n : Int, list : List[Any]) : List[Any] = list.zipWithIndex.filter(_._2 % n != 0).map(_._1)
    
    val expected =  List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    val actual = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assertEquals(expected, actual)
  }
  
  @Test def x_17_split() {
    def split(n : Int, list : List[Any]) : (List[Any], List[Any]) = list match {
      case head :: tail if n == 0 => (List(head), tail)
      case head :: tail => 
        val (front, back) = split(n-1, tail)
        (head :: front, back)
      case _ => (Nil, Nil)
    }
    val expected =  (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val actual = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assertEquals(expected, actual)
  }
  
  
  @Test def x_18_slice() {
    def slice(start : Int, end : Int, list : List[Any]) : List[Any] = list match { 
      case head :: tail if start > 0 => slice(start -1, end -1, tail)
      case head :: tail if end > 0 => head :: slice(start-1,end-1, tail)
      case _ => Nil
    }
    val expected =  List('d, 'e, 'f, 'g)
    val actual = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assertEquals(expected, actual)
  }
  
  @Test def x_19_rotate() {
    def rotate(n : Int, list : List[Any]) : List[Any] = list match {
      case head :: tail if n >= 0 => rotate(n-1, tail) ::: List(head)
      case x => x
      
    }
    val actual = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val expected = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    assertEquals(expected, actual)
    
    val actual2 =  rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    val expected2 = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    assertEquals(expected2, actual2)
  }
  
  @Test def x_20_remove_at() {
    def remove_at(n : int, list : List[Any]) : (List[Any], Any) = {
      (Nil, null)
    }
    val actual =  remove_at(1, List('a, 'b, 'c, 'd))
    val expected = (List('a, 'c, 'd),'b)
    assertEquals(expected, actual)
  }
  
  @Test def x_96_syntax() {
    import util.parsing._
    import combinator._
    object parser extends RegexParsers {
      
      def syntax : Parser[String] = {
        letter ~ rep(optionalDashOrContent) ^^ {
          case x ~ Nil => x
          case x ~ y => x + y.mkString("")
        }
      }
      
      def optionalDashOrContent : Parser[String] = {
        opt(dash) ~ letterOrNumber ^^ {
          case Some(x) ~ y => x + y
          case None ~ y => y
        }
      }
      def dash : Parser[String] = "\\-".r
      def letterOrNumber = letter | number
      def number : Parser[String] = "[0-9]".r
      def letter : Parser[String] = "[a-zA-Z]".r
      
      def isIdentifier(literal : String) : Boolean = parse(syntax, literal) match {
        case Success(x,_) => 
          
          x == literal //This means we matched the whole string!
        case Failure(msg,pos) => 
          Console.println("Failed to parse: " + msg)
          Console.println(pos.pos.longString)
          false
        case Error(msg, pos) => 
          Console.println("Failed to parse: " + msg)
          Console.println(pos.pos.longString)
          false
      }
    }
    assertTrue(parser.isIdentifier("x-abc12-21"))
  }
}
