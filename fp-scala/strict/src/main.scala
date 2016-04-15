trait Stream[+A] {
    
    def toList: List[A] = this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil:List[A]
    }
    
    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) =>  {
            if (n > 0) Stream.cons(h(), t().take(n - 1))
            else Empty
        }
        case Empty => Empty
    }
    
    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) => {
            if (n > 0) t().drop(n - 1)
            else this
        }
        case Empty => Empty
    }
    
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _          => z
        }
        
    
    def exists(p: A => Boolean): Boolean =
        foldRight(false)((a, b) => p(a) || b)
    
    def forAll(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) && b)
        
    def takeWhile(p: A => Boolean): Stream[A] =
        foldRight(Empty:Stream[A])((a, b) => 
            if ( p(a) ) Stream.cons(a, b)
            else Empty)
            
    def headOption: Option[A] =
        foldRight(None:Option[A])((a, b) => Some(a))
        
    def map[B](f: A => B): Stream[B] =
        foldRight(Empty:Stream[B])((a, b) => Stream.cons(f(a), b))
        
    def filter(f: A => Boolean): Stream[A] =
        foldRight(Empty:Stream[A])((a, b) => {
            if ( f(a) ) Stream.cons(a, b)
            else b 
        })
    
    def append[B >: A](s: => Stream[B]): Stream[B] =
        foldRight(s)((a, b) => Stream.cons(a, b))
        
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Empty:Stream[B])((h, t) => f(h) append(t) )
        
    def find(p: A => Boolean): Option[A] =
        filter(p).headOption
        
    def startsWith[B >: A](s2: Stream[B]): Boolean =
        zipWith(s2)(_ == _).forAll(_ == true)
        
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
        unfold((this, s2)) {
            case (Cons(h1, t1), Cons(h2, t2)) =>
                Some(f(h1(), h2()), (t1(), t2()))
            case _  => None
        }
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
        unfold((this, s2)) {
            case (Cons(h1, t1), Cons(h2, t2)) =>
                Some(
                    (Some(h1()), Some(h2())),
                    (t1(), t2())
                 )
            case (Cons(h1, t1), _) =>
                Some(
                    (Some(h1()), None),
                    (t1(), Empty)
                )
            case (_, Cons(h2, t2)) =>
                Some(
                    (None, Some(h2())),
                    (Empty, t2())
                 )
            case (_, _) => None
        }
        
    def mapViaUnfold[B](f: A => B): Stream[B] =
        unfold(this) {
            case Cons(h, t)  => Some(f(h()), t())
            case Empty  => None
        }
    
    def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
        unfold(this) {
            case Cons(h, t) if f(h()) => Some(h(), t())
            case Empty      => None
        }
    
    def takeViaUnfold(n: Int): Stream[A] =
        unfold((this, n)) {
            case (Cons(h, t), 1) => Some(h(), (Empty, 0))
            case (Cons(h, t), n) => Some(h(), (t(), n - 1))
            case _               => None 
        }
        
    def tails: Stream[Stream[A]] =
        unfold(this) {
            case Empty      => None
            case s => Some(s, s drop 1)
        } append Stream(Empty)
        
    def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
        foldRight((z, Stream(z))((a, p0) => {
            lazy val p1 = p0
            val b2 = f(a, p1._1)
            (b2, Stream.cons(b2, p1_2))
        })._2
    }
            
    def hasSubsequence(s: Stream[A]): Boolean =
        tails exists (_ startsWith s)
        
    def unfold[B, S](z: S)(f: S => Option[(B, S)]): Stream[B] =
        f(z) match {
            case Some((a, s)) => Stream.cons(a, unfold(s)(f))
            case None       => Empty:Stream[B]
        }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd 
        lazy val tail = tl 
        Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty 
    
    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty 
        else cons(as.head, apply(as.tail: _*))
        
        
    def constant[A](a: A): Stream[A] =
        unfold(a)(s => Some(s, s))
        
    def fibs: Stream[BigInt] =
        unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
        
    def from(n: Int): Stream[Int] =
        unfold(n)(s => Some(s, s + 1))
        
    def ones: Stream[Int] =
        unfold(1)(s => Some(1, s))
    
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match {
            case Some((a, s)) => Stream.cons(a, unfold(s)(f))
            case None       => Empty:Stream[A]
        }
     
}


object Main extends App {
    val b = Stream(1, 2, 3, 4, 5)
    println(b.tails.toList.map(_.toList))
    
}