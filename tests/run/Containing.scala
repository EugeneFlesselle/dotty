
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

    given String is Numeric:
      extension (self: String) def toInt: Int = self.length
  end Numeric

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
  val x3: Container :& Showable = Container("Hello")

  // Destructing
  // with explicit selections of the witness, value, and extension method
  val r1: String =
    val v3: x3.Value = x3.value
    val w3: x3.Value is Showable = x3.witness
    w3.show(v3)
  // with implicit selection of the witness and extension method
  val r2: String = x3.value.show
  // with implicit selection of the value too
  val r3: String = x3.show

  // Calling
  def showTwice(x: Container :& Showable) = x.show + x.show
  showTwice(x3)

end Example1


/** Using collections of heterogenous conformances */
object Example2:

  def showAll(xs: Seq[Container :& Showable]): Seq[String] = xs.map(_.show)

  showAll(Seq(
    Container("Hello again"),
    Container(12),
    Container(("Hello again", 34)),
  ))

end Example2


/** Using containers with multiple conformances */
object Example3:

  def showPos(xs: Container :& Showable :& Numeric *) =
    for x <- xs if x.toInt > 0 yield x.show

  showPos(
    Container(12)[Showable][Numeric],
    Container("hi")[Showable][Numeric],
  )

end Example3


/** Using containers with bounded `Value`s */
object Example4:

  def showPairs(xs: Container.OfCo[Tuple] :& Showable *) = xs.collect:
    case x if x.size == 2 => x.show

  showPairs(
    Container((12, "34", 56)),
    Container(("12", 89)),
  )

end Example4
