
import scala.language.experimental.modularity
import scala.language.experimental.clauseInterleaving

import scala.TmpPredef.*
import scala.Container

object TypeClassExamples:

  trait Showable extends TypeClass:
    extension (self: Self) def show: String

  object Showable:

    given String is Showable:
      extension (self: String) def show: String = self

    given Int is Showable:
      extension (self: Int) def show: String = s"My int is $self"

    given EmptyTuple is Showable:
      extension (self: EmptyTuple) def show: String = "EmptyTuple"
    given [H : Showable, T <: Tuple : Showable] => (H *: T) is Showable:
      extension (self: H *: T) def show: String = s"${self.head.show} andThen ${self.tail.show}"

  end Showable


  trait Numeric extends TypeClass:
    extension (self: Self) def toInt: Int

  object Numeric:
    given [A : math.Numeric] => A is Numeric:
      extension (self: A) def toInt: Int = A.toInt(self)


  trait Serializable extends TypeClass:
    type Output
    extension (self: Self) def serialized: Output

  object Serializable:
    type To[O] = Serializable { type Output = O }

    given String is Serializable:
      type Output = String
      extension (self: String) def serialized = self

    given Int is Serializable as intIsSerializableToByte:
      type Output = Byte
      extension (self: Int) def serialized: Byte = self.toByte

    given Int is Serializable as intIsSerializableToString:
      type Output = String
      extension (self: Int) def serialized: String = self.toString
  end Serializable

end TypeClassExamples
import TypeClassExamples.*


/** Basic example */
object Example1:

  // Constructing
  val x1 = Container[String]("Hello")[Showable](using Showable.given_is_String_Showable)
  val x2 = Container("Hello")[Showable]
  val x3 : Container :& Showable = Container("Hello")

  // Destructing
  val v3: x3.Value = x3.value
  val w3: x3.Value is Showable = x3.witness
  // with explicit selections of the witness, value, and extension method
  val r1: String = w3.show(v3)
  // with implicit selection of the witness and extension method
  val r2: String = v3.show
  // with implicit selection of the value in addition
  val r3: String = x3.show

end Example1


/** Using collections of heterogenous conformances */
object Example2:

  def showTwice(x: Container :& Showable) = x.show + x.show

  def showAll(xs: Seq[Container :& Showable]): Seq[String] = xs.map(_.show)

  showTwice(Container("Hello"))

  showAll(Seq(
    Container("Hello again"),
    Container(12),
    Container(("Hello again", 34)),
  ))

end Example2

