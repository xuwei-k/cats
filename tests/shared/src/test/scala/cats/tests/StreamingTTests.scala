package cats
package tests

import cats.data.StreamingT
import cats.laws.discipline.{EqK, MonadTests, SerializableTests}
import org.scalacheck.Prop

class StreamingTTests extends CatsSuite {

  test("counter example associativity law"){
    import StreamingT._

    type StreamingTList[A] = StreamingT[List, A]

    val fa: StreamingTList[Boolean] = This[List, Boolean](true, Nil)

    val f1: Boolean => StreamingTList[Boolean] = {
      case false =>
        Empty()
      case true =>
        This[List, Boolean](true,List(This[List, Boolean](false,List(This[List, Boolean](true,List(Empty()))))))
    }

    val f2: Boolean => StreamingTList[Boolean] = {
      case false => This[List, Boolean](true,List(Empty()))
      case true => Empty()
    }

    val x = fa.flatMap(f1).flatMap(f2)
    val y = fa.flatMap(a => f1(a).flatMap(f2))

    assert(x == y)
  }

  implicit val e0: Eq[StreamingT[Eval, Int]] = EqK[StreamingT[Eval, ?]].synthesize[Int]
  checkAll("StreamingT[Eval, ?]", MonadTests[StreamingT[Eval, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[Eval, ?]]", SerializableTests.serializable(Monad[StreamingT[Eval, ?]]))

  implicit val e1: Eq[StreamingT[Option, Int]] = EqK[StreamingT[Option, ?]].synthesize[Int]
  checkAll("StreamingT[Option, ?]", MonadTests[StreamingT[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[Option, ?]]", SerializableTests.serializable(Monad[StreamingT[Option, ?]]))

  implicit val e2: Eq[StreamingT[List, Int]] = EqK[StreamingT[List, ?]].synthesize[Int]
  checkAll("StreamingT[List, ?]", MonadTests[StreamingT[List, ?]].monad[Int, Int, Int])
  checkAll("Monad[StreamingT[List, ?]]", SerializableTests.serializable(Monad[StreamingT[List, ?]]))
}
