import scala.deriving.Mirror

package lib {

  case class NewFoo(x: Int = 1, y: Int)

  object NewMirrors {
    val mNewFoo = summon[Mirror.Of[NewFoo]]

    val mOldFoo = summon[Mirror.Of[OldFoo]]
    val mOldBar = summon[Mirror.Of[OldBar]]
  }
}

package app {
  import lib.*

  object Main {

    def foundDefaultArgument(m: Mirror.Product): Boolean = try {
      m.defaultArgument(0)
      true
    } catch {
      case _: NoSuchElementException => false
    }

    def main(args: Array[String]): Unit = {

      // NewFoo: normal case

      assert(NewMirrors.mNewFoo.defaultArgument(0) == 1)
      summon[NewMirrors.mNewFoo.MirroredElemHasDefaults =:= (true, false)]

      // OldFoo: does not have defaultArgument implementation

      assert(!foundDefaultArgument(NewMirrors.mOldFoo)) // Ok: since mirror of old case class
      summon[NewMirrors.mOldFoo.MirroredElemHasDefaults =:= (false, false)] // Ok: should be consistent with above

      assert(!foundDefaultArgument(OldMirrors.mOldFoo)) // Ok: since mirror of old case class
      summon[OldMirrors.mOldFoo.MirroredElemHasDefaults <:< Tuple] // old mirrors do not have refinment

      // OldBar: anon mirror => could implement defaultArgument
      // AR but should stay consistent with other mirrors of other old case classes

      assert(NewMirrors.mOldBar ne lib.OldBar)
      assert(!foundDefaultArgument(NewMirrors.mOldBar))
      summon[NewMirrors.mOldBar.MirroredElemHasDefaults =:= (false, false)] // Ok: should be consistent with above

      assert(OldMirrors.mOldBar ne lib.OldBar)
      assert(!foundDefaultArgument(OldMirrors.mOldBar))
      summon[OldMirrors.mOldBar.MirroredElemHasDefaults <:< Tuple]

    }
  }
}
