package cats
package tests

import cats.data.StreamingT
import cats.std.list._
import scalaprops._
import scalaz.std.list.listOrder
import scalaz.std.anyVal._

object StreamingTScalapropsTest extends Scalaprops {

  implicit def streamingTScalazMonadPlus[F[_]: Monad] =
    new scalaz.MonadPlus[StreamingT[F, ?]] with scalaz.Cobind[StreamingT[F, ?]] {
      def point[A](a: => A) =
        StreamingT(a)
      def empty[A] =
        StreamingT.empty
      def plus[A](xs: StreamingT[F, A], ys: => StreamingT[F, A]): StreamingT[F, A] =
        xs %::: ys
      override def map[A, B](fa: StreamingT[F, A])(f: A => B) =
        fa map f
      def bind[A, B](fa: StreamingT[F, A])(f: A => StreamingT[F, B]) =
        fa flatMap f
      def cobind[A, B](fa: StreamingT[F, A])(f: StreamingT[F, A] => B) =
        fa coflatMap f
    }

  implicit def orderInstance[F[_]: Monad, A](
    implicit E: scalaz.Order[F[List[A]]]
  ): scalaz.Order[StreamingT[F, A]] = scalaz.Order.order{
    (a, b) =>
      E.order(a.toList, b.toList)
  }

  implicit def gen[F[_]: Applicative, A](implicit
    A: Gen[A], F: shapeless.Lazy[Gen[F[StreamingT[F, A]]]]
  ): Gen[StreamingT[F, A]] =
    Gen.oneOf(
      Gen.value(StreamingT.empty[F, A]),
      A.map(StreamingT(_)),
      scalaz.Apply[Gen].apply2(A, F.value)(StreamingT.This(_, _))
    )

  implicit def stramingTCogen[F[_]: Monad, A](implicit F: Cogen[F[List[A]]]): Cogen[StreamingT[F, A]] =
    F.contramap(_.toList)

  type StreamTList[A] = StreamingT[List, A]

  override def param = super.param.copy(minSuccessful = 1000)

  val orderLaws = scalazlaws.order.all[StreamTList[Byte]].andThenParam(
    Param.maxSize(3)
  )

  val laws = Properties.list(
    scalazlaws.cobind.all[StreamTList],
    scalazlaws.monadPlus.all[StreamTList]
  ).andThenParam(
    Param.maxSize(3)
  )

}
