package scala

import scala.language.experimental.{modularity, clauseInterleaving}
import scala.annotation.experimental
import scala.TmpPredef.TypeClass


/** TODO
 *   - documentation
 *   - move into Predef (we get errors if it is not in src-bootstrapped for now)
 *   - rename file
 * */
object TmpPredef:

  extension [T](x: T) @experimental def as [U](using Conversion[T, U]): U = x.convert

  @experimental trait TypeClass:
    type Self

  type Convertible[To] = [From] =>> Conversion[From, To]

end TmpPredef


/** A value together with an evidence of its type conforming to some type class.
 *
 *  The `witness` is instance of the `Concept` for `Value`.
 *  It is included in the implicit scope of expressions with type `this.Value`,
 *  the only source of which is `this.value`.
 *
 *  Any selection of the form
 *     `qual.name`
 *  where `qual` derives from `Containing` and `name` is not a member of `qual` will attempt
 *     `qual.value.name`
 *
 *  @tparam Concept The type class representing the interface required for the wrapped value
 *
 */
sealed trait Container:
  /** The type of the contained value. */
  type Value
  /** The contained value. */
  val value: Value


class ContainerNil[V](val value: V) extends Container:
  type Value >: V <: V

class ContainerCons[+C <: Container, T <: TypeClass](val c: C)(using w: c.Value is T) extends Container:
  export c.{Value, value}
  given witness: (Value is T) = w

infix type :&[+C <: Container, T <: TypeClass] = ContainerCons[C, T]



object Container:

  /** A `Container` of a value known to have type `V`.
   *
   *  Keeping the member `type Value` abstract by using equal bounds instead of `type Value = V`,
   *  preserves it as an non-dealisable achor to the `witness`.
   * */
  type Of[V] = Container { type Value >: V <: V }
  type OfCo[+V] = Container { type Value <: V }

  def apply[V](v: V): ContainerNil[V] = ContainerNil(v)

  extension [V, C <: Container.Of[V]](self: C)
    implicit def apply[T <: TypeClass](using V is T): C :& T = ContainerCons(self)

end Container
