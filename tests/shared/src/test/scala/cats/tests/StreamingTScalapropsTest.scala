package cats
package tests

import cats.data.StreamingT
import scalaprops._
import scalaz.std.list.listEqual
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.tuple._

object StreamingTScalapropsTest extends Scalaprops {

  implicit def streamingTScalazMonad[F[_]: Monad]: scalaz.Monad[StreamingT[F, ?]] =
    new scalaz.Monad[StreamingT[F, ?]] {
      def point[A](a: => A) =
        StreamingT(a)
      def bind[A, B](fa: StreamingT[F, A])(f: A => StreamingT[F, B]) =
        fa flatMap f
    }

  implicit def equal[F[_]: Monad, A](
    implicit E: shapeless.Lazy[scalaz.Equal[F[Option[(A, F[StreamingT[F, A]])]]]]
  ): scalaz.Equal[StreamingT[F, A]] = scalaz.Equal.equal{
    (a, b) =>
      E.value.equal(a.uncons, b.uncons)
  }

  implicit def gen[F[_]: Applicative, A](implicit
    A: Gen[A], F: shapeless.Lazy[Gen[F[StreamingT[F, A]]]]
  ): Gen[StreamingT[F, A]] =
    Gen.oneOf(
      Gen.value(StreamingT.empty[F, A]),
      A.map(StreamingT(_)),
      scalaz.Apply[Gen].apply2(A, F.value)(StreamingT.This(_, _))
    )


  type StreamTList[A] = StreamingT[List, A]

  implicit val listCatMonad: cats.Monad[List] =
    new cats.Monad[List] {
      def pure[A](x: A) =
        x :: Nil
      def flatMap[A, B](fa: List[A])(f: A => List[B]) =
        fa flatMap f
      override def map[A, B](fa: List[A])(f: A => B) =
        fa map f
    }

  override def param = super.param.copy(maxSize = 2, rand = Rand.standard(1))

  val laws = scalazlaws.monad.all[StreamTList]
}
