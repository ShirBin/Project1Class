List (1,2,3)
List("Shiraz", "Binyamin")
val someList = (1,2,3)

val turtles = "I like turtles!" toList

//::
"hello" :: turtles
println(turtles)

List() == Nil
List[Int]()

"abc" :: Nil
"abc" :: "def" :: Nil

val wisc = "Wisconsin" toList;

wisc head;
println(wisc)

wisc tail;

wisc isEmpty;

Nil isEmpty;

wisc take 4;
wisc drop 4;
wisc;
wisc splitAt(4);

List(1,2,3).toString()

List(1,2,3).mkString(" is less than ")
List(1,2,3).mkString("*")
List(1,2,3).mkString("<:", "--", ":>")

val words = List("one", "two", "three")
val numbers = List(1,2,3)

//zip & unzip
val z = words zip numbers
val z2 = numbers zip(words)
z unzip;

//High-Order Functions
// I like turtles!

turtles count((ch: Char) => ch == 't')
turtles count((josh: Char) => (josh isLetter))
turtles count(_ == 't')

val numbersList = List(23,6,78,3,3,689,5)
numbersList sortWith(_ < _)
numbersList sortWith(_ > _)

numbersList forall(_ < 1000)

numbersList foreach(n => if (n < 100) println())

numbersList map(i => i*2)

//Write a function that takes list

//returns a new list with each element
//incremented by 1
val moon = aList = List(1,5,2,7)
def myFunction(someList: List) : List = someList map(e => e + 1)
myFunction(aList)


