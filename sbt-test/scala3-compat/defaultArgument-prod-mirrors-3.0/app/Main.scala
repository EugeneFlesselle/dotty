import scala.deriving.Mirror

package lib {
  object NewMirrors {

    val mFoo = summon[Mirror.ProductOf[Foo]]

  }
}

package app {
  object Main:
    def main(args: Array[String]): Unit = {

      val oldMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.OldMirrors.mFoo
      assert(oldMirrorFoo eq lib.Foo)
//      assert(oldMirrorFoo.defaultArgument(0) == 0)

      val newMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.NewMirrors.mFoo
      assert(newMirrorFoo ne lib.Foo)
      assert(newMirrorFoo.defaultArgument(0) == 0)

      assert(oldMirrorFoo ne newMirrorFoo)

    }
}
