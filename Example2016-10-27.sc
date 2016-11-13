
def il2fl(aList: List[Int]): List[Float] = aList.map(_.toFloat);

def squarelist(aList: List[Int]): List[Int] = aList.map(e => e * e).filter(_>4)
squarelist(List(1,2,3,4))

def bor (aList: List[Boolean]): Boolean = aList.foldLeft(false)(_||_)
def band(aList: List[Boolean]): Boolean = aList.foldLeft(true)(_&&_)

def evens(aList: List[Int]): List[Int] = aList.filter(a => a%2 == 0)
evens(List(1,2,3,4))

def convert(aList: List[Boolean]): List[Int] =
  aList.map{
    case false => 0
    case true => 1
  }
convert(List(true, true, false, true))

def convertA(aList: List[Int]): List[Boolean] =
  aList.map{
    case 0 => false
    case 1 => true
  }
convertA(List(1,1,0,1))

def howManyPassed(aList: List[String]): List[String] =
  aList.{"A","B","C","D","F"}

}