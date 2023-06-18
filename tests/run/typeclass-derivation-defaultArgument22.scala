import scala.deriving.Mirror as M
import scala.deriving.*
import scala.Tuple.*
import scala.compiletime.*
import scala.compiletime.ops.int.S

trait Migration[-From, +To]:
  def apply(x: From): To

object Migration:

  def from[F,T](f: F => T): Migration[F,T] = new Migration[F,T]:
    def apply(x: F): T = f(x)

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
          case _: false => error("An element has no equivalent or default")


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

  inline def deriveOrSummon[F,T, FromElem, ToElem]: Migration[FromElem, ToElem] =
    inline erasedValue[(FromElem, ToElem)] match
      case _: (F, T) => inline erasedValue[(F, T)] match // allow recusrive derivation for elems when they are subtypes
        case _: (FromElem, ToElem) => error("infinite recursive derivation") // but not the same types
        case _ => migration[FromElem, ToElem](using summonInline[M.Of[FromElem]], summonInline[M.Of[ToElem]])
      case _ => summonInline[Migration[FromElem, ToElem]]

  inline def migrateCase[F, T, FromIdx <: Int](from: M.SumOf[F], to: M.SumOf[T]): Migration[?,?] =
    type Label = Elem[from.MirroredElemLabels, FromIdx]
    type ToIdx = IndexOf[to.MirroredElemLabels, Label]

    inline constValueOpt[ToIdx] match
      case None => error("A case has no equivalent")
      case Some(_) =>
//        type FromType = Elem[from.MirroredElemTypes, FromIdx]
//        type ToType = Elem[to.MirroredElemTypes, ToIdx]
//        deriveOrSummon[F,T, FromType, ToType] FAILS !!!!!!!!!!!

        deriveOrSummon[F,T, Elem[from.MirroredElemTypes, FromIdx], Elem[to.MirroredElemTypes, ToIdx]]

  inline def migrateCases[F, T, FromIdx <: Int](from: M.SumOf[F], to: M.SumOf[T]): List[Migration[?,?]] =
    inline erasedValue[FromIdx] match
      case _: Tuple.Size[from.MirroredElemLabels] => List()
      case _ => migrateCase[F, T, FromIdx](from, to) :: migrateCases[F, T, S[FromIdx]](from, to)

  inline def migrateSum[F, T](from: M.SumOf[F], to: M.SumOf[T])(x: F): T =
    val migrations = migrateCases[F, T, 0](from, to)
    migrations(from.ordinal(x)).asInstanceOf[Migration[Any, Nothing]](x)

  inline def migration[F, T](using from: M.Of[F], to: M.Of[T]): Migration[F, T] = (x: F) =>
    inline from match
      case fromP: M.ProductOf[F] => inline to match
        case toP: M.ProductOf[T] => migrateProduct[F, T](fromP, toP)(x.asInstanceOf[Product])
        case _: M.SumOf[T] => error("Can not migrate a product to a sum")
      case fromS: M.SumOf[F] => inline to match
        case toS: M.SumOf[T] => migrateSum[F, T](fromS, toS)(x)
        case _: M.ProductOf[T] => error("Can not migrate a sum to a product")


  trait Migratable[F](using M.Of[F]):

    trait To[T] extends Migration[F,T] {}
    object To:
      inline def derived[T](using M.Of[T]): To[T] = // abstract method syntax
        migration[F,T](using summonInline[M.Of[F]], summonInline[M.Of[T]])(_: F) // summonInline to keep refinments

  object Migratable:
    inline def derived[F](using M.Of[F]) = new Migratable[F] {}

end Migration


object Test extends App:
  import Migration.*

  case class Foo(x: Int) derives Migratable
  case class Bar(x: Int) derives Foo.derived$Migratable.To
  summon[Migratable[Foo]#To[Bar] <:< Migration[Foo, Bar]]
  assert(Foo(10).migrateTo[Bar] == Bar(10))


  given Migration[Int, Double] = Migration.from(_.toDouble)


  enum F derives Migratable:
    case A(x: Int)
    case B(x: Int, y: String)
    case C(x: F)
    case D(x: Double)
    case E(x: F, y: F)
    case G(s: String)

  enum T derives F.derived$Migratable.To:
    case A(x: Int)
    case B(y: String, x: Double)
    case C(x: T)
    case D(b: Boolean = true, x: Double)
    case E(y: T, z: T = A(0), x: T)
    case G(s: String = "not needed to migrate from F")


  def test[F, T](f: F, t: T)(using Migration[F,T]) = assert(f.migrateTo[T] == t)

  test(F.A(2), T.A(2))
  test(F.B(1, "hi"), T.B("hi", 1d))
  test(F.C(F.A(0)), T.C(T.A(0)))
  test(F.D(3d), T.D(true, 3d))
  test(F.E(F.B(8, "hi again"), F.D(7d)), T.E(T.D(true, 7d), T.A(0), T.B("hi again", 8d)))
  test(F.G("from F"), T.G("from F"))



  inline def lift[C[_], F,T](using Migration[F,T]): Migration[C[F], C[T]] =
    migration[C[F], C[T]](using summonInline[M.Of[C[F]]], summonInline[M.Of[C[T]]])

  given [F,T](using Migration[F,T]): Migration[Option[F], Option[T]] = lift[Option, F,T]
  assert(Some(2).migrateTo[Option[Double]] == Some(2d))

//  given [F,T](using Migration[F,T]): Migration[List[F], List[T]] = lift[F,T, List]
//  assert(List(1, 2).migrateTo[List[Double]] == List(1d, 2d))



  type MigrationOf[U <: Tuple] = U match { case (f *: t *: EmptyTuple) => Migration[f,t] }

  transparent inline def lift2[C[_], U <: Tuple : MigrationOf] =
    inline erasedValue[U] match
      case _: (f *: t *: EmptyTuple) =>
        migration[C[f], C[t]](using summonInline[M.Of[C[f]]], summonInline[M.Of[C[t]]])

  given (using Migration[Int, Double]): Migration[List[Int], List[Double]] = lift2[List, (Int, Double)]
  assert(List(1, 2).migrateTo[List[Double]] == List(1d, 2d))


//  transparent inline def lift4[C[_]] = [U <: Tuple] => () => // TODO using MigrationOf[U]
//    inline erasedValue[U] match
//      case _: (f *: t *: EmptyTuple) =>
//        migration[C[f], C[t]](using summonInline[M.Of[C[f]]], summonInline[M.Of[C[t]]])
//
//  given (using Migration[Int, Double]): Migration[List[Int], List[Double]] = lift4[List][(Int, Double)]()
//  assert(List(1, 2).migrateTo[List[Double]] == List(1d, 2d))



//  type MigrationOf2[U <: Tuple2[_,_]] = U match { case (f, t) => Migration[f,t] }
//
//  type Test0 = MigrationOf2[(Int, Int)]
//  summon[Test0 =:= Migration[Int, Int]]
//
//  type Map2[U <: Tuple2[_,_], C[_]] = U match { case (f, t) => (C[f], C[t]) }
//
//  inline def lift3[C[_], U <: Tuple2 : MigrationOf2]: MigrationOf[Map2[U, C]] =
//    inline erasedValue[U] match
//      case _: (f *: t *: EmptyTuple) =>
//        migration[C[f], C[t]](using summonInline[M.Of[C[f]]], summonInline[M.Of[C[t]]])
//
////  given [U: MigrationOf]: MigrationOf[Map[U, List]] = lift2[U, List]
//
////  given MigrationOf[Map[(Int, Double), List]] = lift2[U, List]
////  given (using Migration[Int, Double]): Migration[List[Int], List[Double]] = lift2[List, (Int, Double)]
//  given (using Migration[Int, Double]): Migration[List[Int], List[Double]] = lift2[List, (Int, Double)]
//
//  assert(List(1, 2).migrateTo[List[Double]] == List(1d, 2d))



//  given [F,T, C[_]](using Migration[F, T]): Migration[C[F], C[T]] =
//    migration[C[F], C[T]](using summonInline[M.Of[C[F]]], summon[M.Of[C[T]]])


//  given [U: MigrationOf]: MigrationOf[Tuple.Map[U, Option]] = ???
//  given [U <: Tuple2 : MigrationOf]: MigrationOf[Option[U]] =




//  enum L1:
//    case Cn(t: Int, ts: L1)
//    case Nl
//
//  enum L2:
//    case Cn(t: Int, ts: L2)
//    case Nl
//
//  given Migration[L1,L2] = migration[L1,L2]
//
//  val l1 = L1.Cn(1, L1.Cn(2, L1.Nl))
//  val l2 = L2.Cn(1, L2.Cn(2, L2.Nl))
//  assert(l1.migrateTo[L2] == l2)
//  assert(L1.Cn(3, L1.Nl).migrateTo[L2] == L2.Cn(3, L2.Nl))




