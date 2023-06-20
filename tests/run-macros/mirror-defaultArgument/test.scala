import scala.deriving._
import scala.annotation.experimental

import MirrorOps.*

object Test extends App:

  case class WithoutDefault(x: Int)
  assert(!overridesDefaultArgument[WithoutDefault])

  case class WithDefault(x: Int, y: Int = 1)
  assert(overridesDefaultArgument[WithDefault])
  val m = summon[Mirror.Of[WithDefault]]
  assert(m.defaultArgument(1) == 1)

