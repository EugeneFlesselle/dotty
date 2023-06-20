package lib

//case class Foo(x: Int)
case class Foo(x: Int = 0)

// case object Foo is its own mirror, so the mirror for Foo will be anonymous.
//case object Foo


object OldMirrors {

  val mFoo = summon[deriving.Mirror.ProductOf[Foo]]

}
