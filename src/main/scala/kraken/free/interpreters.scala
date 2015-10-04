package kraken.free

import kraken.free.ops._

import scala.concurrent.{ExecutionContext, Future}

import cats.{Id, ~>}


object interpreters {

  /*class UnsafeFutureInterpreter(implicit ec : ExecutionContext) extends (OpA ~> Future) {
    def apply[A](op: OpA[A]) = {
      println(s"Running ProdInterpreter interpreter")
      op match {
        case Ask(a, next) => 
          Future.successful(a()) map next
        case Async(a, next) => 
          Future(a())(ec) map next
        case Tell(a, next) =>
          Future.successful(a()); next
      }
    }
  }*/

  object TestInterpreter extends (OpA ~> Id) {
    def apply[A](op: OpA[A]) = {
      println(s"Running Test interpreter")
      op match {
        case Ask(a, next) => 
          next(a())
        case Async(a, next) => 
          next(a())
        case Tell(a, next) =>
          a()
          next
      }
    }
  }

}
