package cats.tests

import cats.{Monad, Traverse}
import cats.data.IdT
import cats.laws.discipline.{MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._

class IdTTests extends CatsSuite {

  checkAll("IdT[List, Int]", MonadTests[IdT[List, ?]].monad[Int, Int, Int])
  checkAll("Monad[IdT[List, ?]]", SerializableTests.serializable(Monad[IdT[List, ?]]))

  checkAll("IdT[Option, Int]", TraverseTests[IdT[Option, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[IdT[Option, ?]]", SerializableTests.serializable(Traverse[IdT[Option, ?]]))

}
