package cats
package tests

import cats.data.StreamingT
import cats.std.all._
import scalaprops._

object StreamingTScalapropsTests extends Scalaprops {

  implicit def streamingTGen[F[_]: cats.Applicative, A: Gen]: Gen[StreamingT[F, A]] =
    Gen.frequency(
      1 -> Gen.value(StreamingT.empty[F, A]),
      2 -> Gen[A].map(StreamingT(_)),
      5 -> Gen[List[A]].map(StreamingT.fromList(_))
    )

  def filterTest[F[_]: Applicative](implicit
    M: MonadCombine[StreamingT[F, ?]],
    E: Eq[StreamingT[F, Byte]]
  ) = Property.forAll{ (s: StreamingT[F, Byte], f: Byte => Boolean) =>
    val x = s.filter(f)
    val y = M.flatMap(s)(a => if (f(a)) M.pure(a) else M.empty[Byte])
    E.eqv(x, y)
  }

  val filterList = filterTest[List]
  val filterOption = filterTest[Option]

}
