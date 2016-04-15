trait RNG {
    def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt 
        (n, nextRNG)
    }   
}

case class State[S, +A](run: S => (A, S))

object SimpleRNG {
    type State[S, +A] = S => (A, S)
    type Rand[A] = State[RNG, A]
    
    val int: Rand[Int] = _.nextInt
    
    def unit[A](a: A): Rand[A] =
        rng => (a, rng)
        
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => unit(f(a)))
        
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => map(rb)(b => f(a, b)))
      
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, rng1) = f(rng)
            g(a)(rng1)
        }
        
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
        fs.foldRight(unit( List[A]() ))((f, acc) => map2(f, acc)(_ :: _))
    }
    
    def ints(count: Int): Rand[List[Int]] =
        sequence(List.fill(count)(int))
        
    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb)((_, _))
        
        
    val randIntDouble: Rand[(Int, Double)] =
        both(int, double)
    
    val randDoubleInt: Rand[(Double, Int)] =
        both(double, int)
    
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
        val (i1, rng2) = int(rng)
        val (i2, rng3) = int(rng2)
        ((i1, i2), rng3)
    }
    
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, rng2) = int(rng)
        (if (i == Int.MinValue) Int.MaxValue else Math.abs(i) , rng2)
    }
    
    def nonNegativeEven: Rand[Int] =
        map(nonNegativeInt)(i => i - i % 2)
    
    def double: Rand[Double] =
        map(int)(i => (i.toDouble - Int.MinValue) / (Int.MaxValue - Int.MinValue))
        
    def nonNegativeLessThan(n: Int): Rand[Int] =
        flatMap(nonNegativeInt) { i =>
            val mod = i % n 
            if (i + (n - 1) - mod >= 0)
                unit(mod) 
            else 
                nonNegativeLessThan(n)
         }
            
  
}



object Main extends App {
    import SimpleRNG._ 
    
    val a = SimpleRNG(42)
    val (n, b) = a.nextInt 
    
    println(randomPair(b)._1)
    println(nonNegativeInt(b)._1)
}