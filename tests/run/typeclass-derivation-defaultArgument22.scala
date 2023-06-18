import scala.deriving.Mirror as M
import scala.deriving.*
import scala.Tuple.*
import scala.compiletime.*
import scala.compiletime.ops.int.S

trait Migration[-From, +To]:
  def apply(x: From): To

object Migration:

  extension [From](x: From)
    def migrateTo[To](using m: Migration[From, To]): To = m(x)

  given[T]: Migration[T, T] with
    override def apply(x: T): T = x

  type IndexOf[Elems <: Tuple, X] <: Int = Elems match {
    case (X *: elems) => 0
    case (_ *: elems) => S[IndexOf[elems, X]]
    case EmptyTuple => Nothing
  }

  inline def migrateElem[F,T, ToIdx <: Int](from: M.ProductOf[F], to: M.ProductOf[T])(x: Product): Any =
    type Label = Elem[to.MirroredElemLabels, ToIdx]
    type FromIdx = IndexOf[from.MirroredElemLabels, Label]
    inline constValueOpt[FromIdx] match

      case Some(fromIdx) =>
        type FromType = Elem[from.MirroredElemTypes, FromIdx]
        type ToType = Elem[to.MirroredElemTypes, ToIdx]
        summonFrom { case _: Migration[FromType, ToType] =>
          x.productElement(fromIdx).asInstanceOf[FromType].migrateTo[ToType]
        }

      case None =>
        type HasDefault = Elem[to.MirroredElemHasDefaults, ToIdx]
        inline erasedValue[HasDefault] match
          case _: true => to.defaultArgument(constValue[ToIdx])
          case _: false => compiletime.error("An element has no equivalent or default")


  inline def migrateElems[F,T, ToIdx <: Int](from: M.ProductOf[F], to: M.ProductOf[T])(x: Product): Seq[Any] =
    inline erasedValue[ToIdx] match
      case _: Tuple.Size[to.MirroredElemLabels] => Seq()
      case _ => migrateElem[F,T, ToIdx](from, to)(x) +: migrateElems[F,T, S[ToIdx]](from, to)(x)

  inline def migrateProduct[F,T](from: M.ProductOf[F], to: M.ProductOf[T])(x: Product): T =
    val elems = migrateElems[F, T, 0](from, to)(x)
    to.fromProduct(new Product:
      def canEqual(that: Any): Boolean = false
      def productArity: Int = elems.length
      def productElement(n: Int): Any = elems(n)
    )

  // TODO also check is subtype and not the same type (infinite rec)
  private inline def deriveOrSummon[F,T, FromElem, ToElem]: Migration[FromElem, ToElem] =
    inline erasedValue[FromElem] match
      case _: F =>
        inline erasedValue[ToElem] match
          case _: T =>
            migration[FromElem, ToElem](using
              summonInline[M.Of[FromElem]],
              summonInline[M.Of[ToElem]]
            )

          case _ => summonInline[Migration[FromElem, ToElem]]
      case _ => summonInline[Migration[FromElem, ToElem]]

  private inline def migrateCase[F, T, FromIdx <: Int](from: M.SumOf[F], to: M.SumOf[T]): Migration[?,?] =
    type Label = Elem[from.MirroredElemLabels, FromIdx]
    type ToIdx = IndexOf[to.MirroredElemLabels, Label]

    inline constValueOpt[ToIdx] match
      case None => compiletime.error("A case has no equivalent")
      case Some(_) =>
        type FromType = Elem[from.MirroredElemTypes, FromIdx]
        type ToType = Elem[to.MirroredElemTypes, ToIdx]

        deriveOrSummon[F,T, Elem[from.MirroredElemTypes, FromIdx], Elem[to.MirroredElemTypes, ToIdx]]
//        deriveOrSummon[F,T, FromType, ToType] FAILS !!!!!!!!!!!

  private inline def migrateCases[F, T, FromIdx <: Int](from: M.SumOf[F], to: M.SumOf[T]): List[Migration[?,?]] =
    inline erasedValue[FromIdx] match
      case _: Tuple.Size[from.MirroredElemLabels] => List()
      case _ => migrateCase[F, T, FromIdx](from, to) :: migrateCases[F, T, S[FromIdx]](from, to)

  private inline def migrateSum[F, T](from: M.SumOf[F], to: M.SumOf[T])(x: F): T =
    val migrations = migrateCases[F, T, 0](from, to)
    val n = from.ordinal(x)
    val m = migrations(n).asInstanceOf[Migration[Any, Nothing]]
    m(x)

  inline def migration[F, T](using from: M.Of[F], to: M.Of[T]): Migration[F, T] = (x: F) =>
    inline from match

      case fromP: M.ProductOf[F] => inline to match
        case toP: M.ProductOf[T] => migrateProduct[F, T](fromP, toP)(x.asInstanceOf[Product])
        case _: M.SumOf[T] => compiletime.error("Can not migrate a product to a sum")

      case fromS: M.SumOf[F] => inline to match
        case toS: M.SumOf[T] =>

          deriveOrSummon[F,T, Elem[fromS.MirroredElemTypes, 0], Elem[toS.MirroredElemTypes, 0]]

          migrateSum[F, T](fromS, toS)(x)
        case _: M.ProductOf[T] => compiletime.error("Can not migrate a sum to a product")


end Migration


import Migration.*
object Test extends App:

  trait T1 // TODO RM
  trait T2 // TODO RM

  case class A1(x: Int) extends T1
  case class A2(x: Int) extends T2
  given Migration[A1, A2] = migration
  assert(A1(2).migrateTo[A2] == A2(2))

  case class B1(x: Int, y: String)
  case class B2(y: String, x: Int)
  given Migration[B1, B2] = migration
  assert(B1(5, "hi").migrateTo[B2] == B2("hi", 5))

  case class C1(x: A1) extends T1
  case class C2(x: A2) extends T2
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

  // should only use default when needed
  case class F1(x: Int)
  case class F2(x: Int = 3)
  given Migration[F1, F2] = migration
  assert(F1(7).migrateTo[F2] == F2(7))


  enum L1:
    case Cn(t: Int, ts: L1)
    case Nl

  enum L2:
    case Cn(t: Int, ts: L2)
    case Nl

  given Migration[L1,L2] = migration[L1,L2]

  val l1 = L1.Cn(1, L1.Cn(2, L1.Nl))
  val l2 = L2.Cn(1, L2.Cn(2, L2.Nl))
  assert(l1.migrateTo[L2] == l2)
  assert(L1.Cn(3, L1.Nl).migrateTo[L2] == L2.Cn(3, L2.Nl))



  enum En1:
    case X(i: Int)
    case Y(d: Double)

  enum En2:
    case X(i: Int)
    case Y(d: Double)

//  given Migration[En1,En2] = migration[En1,En2]
//  assert(En1.X(3).migrateTo[En2] == En2.X(3))

