def fizzer(i: Int): Unit = {
  if (i % 15 == 0) println("FizzBuzz")
  else if (i % 5 == 0) println("Fizz")
  else if (i % 3 == 0) println("Buzz")
  else println(i)
}

(1 to 100) foreach fizzer
