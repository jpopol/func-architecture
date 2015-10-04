package kraken.free

import scala.language.reflectiveCalls

import cats.free.Free
import cats.Functor

object ops {

  sealed trait OpA[Next]
  case class Ask[T, Next](a: () => T, next: T => Next) extends OpA[Next]
  case class Async[T, Next](a: () => T, next: T => Next) extends OpA[Next]
  case class Tell[T, Next](a: () => T, next: Next) extends OpA[Next]



  type Op[A] = Free[OpA, A]
  
  implicit val functor: Functor[OpA] =
    new Functor[OpA] {
      def map[A, B](op: OpA[A])(f: A => B): OpA[B] = 
        op match {
          case Ask(a, next) => Ask(a, next andThen f) 
          case Async(a, next) => Async(a, next andThen f)
          case Tell(a, next) => Tell(a, f(next))
        }
    }


  def ask[A](a: => A): Op[A] = Free.liftF(Ask[A, A](() => a, identity))

  def async[A](a: => A): Op[A] = Free.liftF(Async[A, A](() => a, identity))

  def tell(a: => Unit): Op[Unit] = Free.liftF(Tell(() => a, ()))

}
