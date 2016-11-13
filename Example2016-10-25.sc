def exercise1(aList: List[Int]): List[Int] =
  aList.map(element => (element * 3) + 1)
exercise1(List(1,3,5))

def triplePlus1(n: Int) : Int = (3*n) + 1

triplePlus1(5)

def exercise2(aList: List[Int]): List[Int] =
  aList.map(triplePlus1)

exercise2(List(4,3,5))

def exercise3(aList: List[Int]): List[Int] =
  aList
    .filter(_ > 4)
  .map(triplePlus1)

/*def exercise4(aList: List[Int]): List[Int] =
  aList
    .filter(_ > 4)
    .map(triplePlus1)
    .foldLeft(0) (_+_) */

def myMember(element: Int, aList: List[Int]): Boolean =
  aList match {
    case Nil => false
    case listHead::listTail => if (element == listHead) true else myMember(element, listTail)
  }

myMember(23, List(4,356,2,1))

def myUnion(list1: List[Int], list2: List[Int]): List[Int] =
  list2 match {
    case Nil => Nil
    case anotherListHead::anotherListTail =>
      if (myMember(anotherListHead, list1)) myUnion(list1, anotherListTail)
      else anotherListHead::myUnion(list1,anotherListTail)
  }
myUnion(List(1,2,3,4),List(2,3,4,5))

