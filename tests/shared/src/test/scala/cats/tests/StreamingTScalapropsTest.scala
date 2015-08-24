package cats
package tests

import cats.data.StreamingT
import scalaprops._
import scalaz.std.list.listEqual
import scalaz.std.anyVal._

object StreamingTScalapropsTest extends Scalaprops {

  implicit def streamingTScalazMonad[F[_]: Monad]: scalaz.Monad[StreamingT[F, ?]] =
    new scalaz.Monad[StreamingT[F, ?]] {
      def point[A](a: => A) =
        StreamingT(a)
      def bind[A, B](fa: StreamingT[F, A])(f: A => StreamingT[F, B]) =
        fa flatMap f
    }

  implicit def streamingTEqual[F[_], A](implicit ev: cats.Monad[F], eva: scalaz.Equal[A], evfb: scalaz.Equal[F[Boolean]]): scalaz.Equal[StreamingT[F, A]] =
    scalaz.Equal.equal[StreamingT[F, A]]{ (xs, ys) =>
      import scalaz.syntax.equal._
      (xs izipMap ys)(_ === _, _ => false, _ => false)
        .forall(_ == true) === ev.pure(true)
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

  override def param = super.param.copy(maxSize = 2)

  val laws = scalazlaws.monad.all[StreamTList]
}
