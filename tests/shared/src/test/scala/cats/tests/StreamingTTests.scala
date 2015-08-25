package cats
package tests

import algebra.laws.{GroupLaws, OrderLaws}

import cats.data.StreamingT
import cats.laws.discipline.{CoflatMapTests, EqK, MonadCombineTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class StreamingTTests extends CatsSuite {

  implicit val e0: Eq[StreamingT[Eval, Int]] = EqK[StreamingT[Eval, ?]].synthesize[Int]
  checkAll("StreamingT[Eval, ?]", MonadCombineTests[StreamingT[Eval, ?]].monad[Int, Int, Int])
  checkAll("StreamingT[Eval, ?]", CoflatMapTests[StreamingT[Eval, ?]].coflatMap[Int, Int, Int])
  checkAll("StreamingT[Eval, Int]", OrderLaws[StreamingT[Eval, Int]].order)
  checkAll("Monad[StreamingT[Eval, ?]]", SerializableTests.serializable(Monad[StreamingT[Eval, ?]]))

  implicit val e1: Eq[StreamingT[Option, Int]] = EqK[StreamingT[Option, ?]].synthesize[Int]
  checkAll("StreamingT[Option, ?]", MonadCombineTests[StreamingT[Option, ?]].monad[Int, Int, Int])
  checkAll("StreamingT[Option, ?]", CoflatMapTests[StreamingT[Option, ?]].coflatMap[Int, Int, Int])
  checkAll("StreamingT[Option, Int]", OrderLaws[StreamingT[Option, Int]].order)
  checkAll("Monad[StreamingT[Option, ?]]", SerializableTests.serializable(Monad[StreamingT[Option, ?]]))

  implicit val e2: Eq[StreamingT[List, Int]] = EqK[StreamingT[List, ?]].synthesize[Int]
  checkAll("StreamingT[List, ?]", MonadCombineTests[StreamingT[List, ?]].monad[Int, Int, Int])
  checkAll("StreamingT[List, ?]", CoflatMapTests[StreamingT[List, ?]].coflatMap[Int, Int, Int])
  checkAll("StreamingT[List, Int]", OrderLaws[StreamingT[List, Int]].order)
  checkAll("Monad[StreamingT[List, ?]]", SerializableTests.serializable(Monad[StreamingT[List, ?]]))

  test("counter example"){
    type StreamingTList[A] = StreamingT[List, A]

    val fa: StreamingTList[Boolean] =
      StreamingT.cons(
        true,
        List(
          StreamingT.cons(true,List.empty[StreamingTList[Boolean]]),
          StreamingT.empty[List, Boolean]
        )
      )

    val f: Boolean => StreamingTList[Boolean] = {
      case true =>
        StreamingT.cons(false,List(StreamingT.cons(true,List.empty[StreamingTList[Boolean]])))
      case false =>
        StreamingT.empty[List, Boolean]
    }

    val g: Boolean => StreamingTList[Boolean] = {
      case true =>
        StreamingT.empty
      case false =>
        StreamingT.cons(
          true,
          List(
            StreamingT.cons(false,List(StreamingT.empty[List, Boolean])),
            StreamingT.cons(true, List(StreamingT.empty[List, Boolean]))
          )
        )
    }

    // Next(List(Next(List(This(true,List(This(false,List(Next(List()))), This(true,List(Next(List())))))))))
    val x = fa.flatMap(f).flatMap(g)
    println(x)

    // Next(List(This(true,List(This(false,List()), This(true,List())))))
    val y = fa.flatMap(a => f(a).flatMap(g))
    println(y)

    assert(Eq[StreamingTList[Boolean]].eqv(x, y))
  }
}
