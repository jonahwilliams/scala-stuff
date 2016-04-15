// java.util.concurrent api
class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}
//

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = false
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = a(es)
      UnitFuture(f(af.get, bf.get))
    }
  
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  
  // Exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => unit(f(a))
    
  // Exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]]
    
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))
    
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)
  
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
}

object Main extends App {
    
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}