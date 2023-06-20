import scala.deriving.Mirror

package lib {
  object NewMirrors {

//    val mFoo = summon[Mirror.ProductOf[Foo]]
    val mFoo = summon[Mirror.ProductOf[Foo]]

  }
}

package app {
  object Main:
    def main(args: Array[String]): Unit = {

      case class Bar(d: Double = 3d)
      val m = summon[Mirror.Of[Bar]]
      summon[m.MirroredElemHasDefaults =:= Tuple1[true]]
      assert(m.defaultArgument(0) == 3d)


//      @implicitAmbiguous(msg = "Can prove that ${B} <:< ${A}")
      trait </<[A, B]

      given [A,B]: </<[A, B] with {}

      given ambiguous1 [A, B >: A]: </<[A, B] with {}
      given ambiguous2 [A, B >: A]: </<[A, B] with {}


      trait =/=[A, B]
      given [A,B](using A </< B, B </< A): =/=[A, B] with {}



      val oldMirrorFoo: Mirror.Of[lib.Foo] = lib.OldMirrors.mFoo
      assert(oldMirrorFoo eq lib.Foo)

      summon[Mirror.Of[lib.Foo] </< Mirror.ProductOf[lib.Foo]]
      summon[oldMirrorFoo.type </< Mirror.ProductOf[lib.Foo]]


      val newMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.NewMirrors.mFoo

      summon[newMirrorFoo.type <:< Mirror.ProductOf[lib.Foo]]

      summon[oldMirrorFoo.type </< newMirrorFoo.type]
      summon[oldMirrorFoo.type =/= newMirrorFoo.type]

      assert(newMirrorFoo ne oldMirrorFoo) // FAILS


      summon[newMirrorFoo.MirroredElemHasDefaults <:< Tuple]
//      summon[newMirrorFoo.MirroredElemHasDefaults <:< NonEmptyTuple]
//      summon[newMirrorFoo.MirroredElemHasDefaults =:= Tuple1[true]]

//      assert(newMirrorFoo ne lib.Foo)
//      assert(newMirrorFoo ne oldMirrorFoo)

//      assert(newMirrorFoo.defaultArgument(0) == 0)


//      assert(oldMirrorFoo ne newMirrorFoo)

    }
}

