package lib

import deriving.Mirror

case class Foo(x: Int = 0)

object OldMirrors { // AR s
  val mFoo = summon[Mirror.ProductOf[Foo]]
}



/*

case class Foo private[lib] (x: Int)

// case object Foo is its own mirror, so the mirror for Foo will be anonymous.
case object Foo


case class Bar protected[lib] (x: Int)

// case object Bar is its own mirror, so the mirror for Bar will be anonymous.
case object Bar

object OldMirrors {
  val mFoo = summon[deriving.Mirror.ProductOf[Foo]]
  val mFooObj = summon[deriving.Mirror.ProductOf[Foo.type]]

  object SubBar extends Bar(1) {
    val mBar = summon[deriving.Mirror.ProductOf[Bar]]
    val mBarObj = summon[deriving.Mirror.ProductOf[Bar.type]]
  }
}
*/
