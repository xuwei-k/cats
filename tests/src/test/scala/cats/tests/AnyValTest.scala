package cats.tests

class AnyValTest extends CatsSuite {

  checkAll("Monoid[Double]", algebra.laws.GroupLaws[Double].monoid)
  checkAll("Monoid[Float]", algebra.laws.GroupLaws[Float].monoid)

}
