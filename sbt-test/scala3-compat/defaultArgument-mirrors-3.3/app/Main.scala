import scala.deriving.Mirror

package lib {
  object NewMirrors {
    val mFoo = summon[Mirror.Of[Foo]]
  }
}

package app {
  object Main {

    /*def isConsistent(mFoo: Mirror.Of[Foo]): Boolean = {

      val hasDefault = inline erasedValue[mFoo.MirroredElemHasDefaults] match
        case _: (true *: EmptyTuple) => true
        case _: (false *: EmptyTuple) => false
        case _ => ???

      val foundDefault =
        try { mFoo.defaultArgument(0); true }
        catch case NoSuchElementException(0) => false

      hasDefault == foundDefault
    }*/

    def main(args: Array[String]): Unit = {

      val newMirror = lib.NewMirrors.mFoo

      assert(newMirror eq lib.Foo)

      try {
        newMirror.defaultArgument(0)
        assert(false)
      } catch case _: NoSuchElementException => () // Ok:

      summon[newMirror.MirroredElemHasDefaults =:= Tuple1[false]]
//      summon[newMirror.MirroredElemHasDefaults =:= Tuple1[true]]


/*
      val oldMirror = lib.OldMirrors.mFoo

      summon[oldMirror.MirroredElemHasDefaults <:< Any]
      summon[oldMirror.MirroredElemHasDefaults <:< Tuple]

//      summon[oldMirror.MirroredElemHasDefaults <:< Tuple1[Any]]
//      summon[oldMirror.MirroredElemHasDefaults <:< Tuple1[Boolean]]

//      summon[oldMirror.MirroredElemHasDefaults =:= Tuple1[false]]
//      summon[oldMirror.MirroredElemHasDefaults =:= Tuple1[true]]
*/

    }
  }
}

/*
package lib {
  object NewMirrors {
    val mFoo = summon[Mirror.Of[Foo]] // we can access the constructor of Foo here.
    val mFooObj = summon[Mirror.Of[Foo.type]]

    object SubBar extends Bar(1) {
      val mBar = summon[deriving.Mirror.ProductOf[Bar]]
      val mBarObj = summon[deriving.Mirror.ProductOf[Bar.type]]
    }
  }
}

package app {
  object Main:

    def testFoo(): Unit = {
      val oldMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.OldMirrors.mFoo
      val oldMirrorFooObj: Mirror.ProductOf[lib.Foo.type] = lib.OldMirrors.mFooObj

      assert(oldMirrorFoo eq oldMirrorFooObj) // - not good as oldMirrorFoo is really the mirror for `Foo.type`
      assert(oldMirrorFooObj eq lib.Foo) // - object Foo is its own mirror

      // 3.1 bug: mirror for Foo behaves as mirror for Foo.type
      assert(oldMirrorFooObj.fromProduct(EmptyTuple) == lib.Foo)

      val newMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.NewMirrors.mFoo
      val newMirrorFooObj: Mirror.ProductOf[lib.Foo.type] = lib.NewMirrors.mFooObj

      assert(oldMirrorFooObj eq newMirrorFooObj) // mirror for Foo.type has not changed.

      assert(newMirrorFoo ne lib.Foo) // anonymous mirror for Foo
      assert(newMirrorFoo.fromProduct(Tuple(23)).x == 23) // mirror for Foo behaves as expected
    }

    def testBar(): Unit = {
      val oldMirrorBar: Mirror.ProductOf[lib.Bar] = lib.OldMirrors.SubBar.mBar
      val oldMirrorBarObj: Mirror.ProductOf[lib.Bar.type] = lib.OldMirrors.SubBar.mBarObj

      assert(oldMirrorBar eq oldMirrorBarObj) // - not good as oldMirrorBar is really the mirror for `Bar.type`
      assert(oldMirrorBarObj eq lib.Bar) // - object Bar is its own mirror

      // 3.1 bug: mirror for Bar behaves as mirror for Bar.type
      assert(oldMirrorBarObj.fromProduct(EmptyTuple) == lib.Bar)

      val newMirrorBar: Mirror.ProductOf[lib.Bar] = lib.NewMirrors.SubBar.mBar
      val newMirrorBarObj: Mirror.ProductOf[lib.Bar.type] = lib.NewMirrors.SubBar.mBarObj

      assert(oldMirrorBarObj eq newMirrorBarObj) // mirror for Bar.type has not changed.

      assert(newMirrorBar ne lib.Bar) // anonymous mirror for Bar
      assert(newMirrorBar.fromProduct(Tuple(23)).x == 23) // mirror for Bar behaves as expected
    }

    def main(args: Array[String]): Unit =
      testFoo()
      testBar()
}
*/
