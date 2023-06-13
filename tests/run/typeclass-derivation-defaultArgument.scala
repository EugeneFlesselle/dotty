import scala.deriving.*
import scala.Tuple.*
import scala.compiletime.*
import scala.compiletime.ops.int.S

trait Migration[From, To]:
  def apply(x: From): To

object Migration:

  extension [From](x: From)
    def migrateTo[To](using m: Migration[From, To]): To = m(x)

  given[T]: Migration[T, T] with
    override def apply(x: T): T = x

  private type IndexOf[Elems <: Tuple, X] <: Int = Elems match {
    case (elem *: elems) =>
      elem match {
        case X => 0
        case _ => S[IndexOf[elems, X]]
      }
    case EmptyTuple => Nothing
  }

  private inline def migrate[F,T](x: F): T = summonFrom {
    case migration: Migration[F,T] => migration(x)
  }

  private inline def migrateElem[F,T, ToIdx <: Int]
                                (from: Mirror.ProductOf[F], to: Mirror.ProductOf[T])
                                (x: Product): Any =
    type Label = Elem[to.MirroredElemLabels, ToIdx]
    type FromIdx = IndexOf[from.MirroredElemLabels, Label]
    inline constValueOpt[FromIdx] match
      case Some(fromIdx) =>
        type FromType = Elem[from.MirroredElemTypes, FromIdx]
        type ToType = Elem[to.MirroredElemTypes, ToIdx]
        val elem = x.productElement(fromIdx).asInstanceOf[FromType]
        migrate[FromType, ToType](elem)
      case None =>
        type HasDefault = Elem[to.MirroredElemHasDefaults, ToIdx] // NOTE when are the annotations checked ?? bug expr is not checked for match types
        inline erasedValue[HasDefault] match
          case _: true => to.defaultArgument(constValue[ToIdx])
          case _: false => compiletime.error("An element has no equivalent in source or default")
  end migrateElem

  private inline def migrateElems[F,T, ToIdx <: Int]
                                 (from: Mirror.ProductOf[F], to: Mirror.ProductOf[T])
                                 (x: Product): Seq[Any] =
    inline erasedValue[ToIdx] match
      case _: Tuple.Size[to.MirroredElemLabels] => Seq()
      case _ => migrateElem[F,T,ToIdx](from, to)(x) +: migrateElems[F,T,S[ToIdx]](from, to)(x)

  private inline def migrateProduct[F,T](from: Mirror.ProductOf[F], to: Mirror.ProductOf[T])
                                        (x: Product): T =
    val elems = migrateElems[F,T,0](from, to)(x)
    to.fromProduct(new Product:
      override def canEqual(that: Any): Boolean = false
      override def productArity: Int = x.productArity
      override def productElement(n: Int): Any = elems(n)
    )

  implicit inline def migration[F,T](using from: Mirror.Of[F], to: Mirror.Of[T]):  Migration[F,T] =
    new Migration[F,T]:
      override def apply(x: F): T = inline from match
        case fromP: Mirror.ProductOf[F] => inline to match
          case _: Mirror.SumOf[T] => compiletime.error("Can not migrate a product to a sum")
          case toP: Mirror.ProductOf[T] => migrateProduct[F, T](fromP, toP)(x.asInstanceOf[Product])

end Migration

import Migration.*
object Test extends App:

  case class A1(x: Int)
  case class A2(x: Int)
  given Migration[A1, A2] = migration
  assert(A1(2).migrateTo[A2] == A2(2))

  case class B1(x: Int, y: String)
  case class B2(y: String, x: Int)
  given Migration[B1, B2] = migration
  assert(B1(5, "hi").migrateTo[B2] == B2("hi", 5))

  case class C1(x: A1)
  case class C2(x: A2)
  given Migration[C1, C2] = migration
  assert(C1(A1(0)).migrateTo[C2] == C2(A2(0)))

  case class D1(x: Double)
  case class D2(b: Boolean = true, x: Double)
  given Migration[D1, D2] = migration
  assert(D1(9).migrateTo[D2] == D2(true, 9))

  case class E1(x: D1, y: D1)
  case class E2(y: D2, s: String = "hi", x: D2)
  given Migration[E1, E2] = migration
  assert(E1(D1(1), D1(2)).migrateTo[E2] == E2(D2(true, 2), "hi", D2(true, 1)))

  case class F1(x: Int)
  case class F2(x: Int = 3)
  given Migration[F1, F2] = migration
  assert(F1(7).migrateTo[F2] == F2(7))

