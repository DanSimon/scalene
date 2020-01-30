package scalene.corerouting

import scalene.Deferred

trait CellContainer[In <: Clonable[In], Out]{self: RoutingSuite[In, Out] =>

/**
 * A VSet stores extracted values while a route is being executed.
 */
class VSet(size: Int) {
  private val values = new Array[Any](math.max(size, 1))
  def set[T](cell: Cell[T], value: T) {
    values(cell.index) = value
  }
  def get[T](cell: Cell[T]): T = {
    values(cell.index).asInstanceOf[T]
  }
}

object VSet {
  def apply(size: Int): VSet = new VSet(size)

  //very weird, but calling `VSet(0)` instead of `new VSet(0)` causes a
  //stackoverflow exception ???
  val empty = new VSet(0)
}

/**
 * A Cell represents a typed location into a VSet
 **/
case class Cell[T](val index: Int)


trait WrappedParser[I] {
  def parse(input: I, values: VSet): Result[Unit]

}

class WrappedParserImpl[I,O](parser: Parser[I,O], cell: Cell[O]) extends WrappedParser[I] {
  val ok: Result[Unit] = Right(())
  def parse(input: I, values: VSet): Result[Unit] = parser.parse(input) match {
    case Left(err) => Left(err)
    case Right(value) => {
      values.set(cell, value)
      ok
    }
  }
}

trait WrappedFilter[I] {
  def parse(input: I, values: VSet): Deferred[Unit]
}

class WrappedFilterImpl[I,O](filter: Filter[I,O], cell: Cell[O]) extends WrappedFilter[I] {
  val ok: Result[Unit] = Right(())
  def parse(input: I, values: VSet): Deferred[Unit] = filter(input) map {value =>
    values.set(cell, value)
    ()
  }
}

sealed trait CellComponent[I, O] {
  def document(builder: DocType): DocType
}

case class CellParser[I, O](parser: Parser[I, O]) extends CellComponent[I,O] {
  def wrapped(index: Int) : (WrappedParser[I], Cell[O]) = {
    val cell = new Cell[O](index)
    (new WrappedParserImpl(parser, cell), cell)
  }
  def document(builder: DocType): DocType = parser.document(builder)
}


case class CellFilter[I,O](filter: Filter[I,O]) extends CellComponent[I,O] {
  def wrapped(index: Int) : (WrappedFilter[I], Cell[O]) = {
    val cell = new Cell[O](index) 
    (new WrappedFilterImpl(filter, cell), cell)
  }
  def document(builder: DocType): DocType = ???//parser.document(builder)
}

//used when making phantom clones of QLists for dealing with subroute trees
case class CellPhantom[I,O](cell: Cell[O]) extends CellComponent[I,O] {

  def document(builder: DocType): DocType = builder
}

trait AsCellComponent[I, O, P] {
  def apply(p: P): CellComponent[I,O]
}

object AsCellComponent {

  
  implicit def liftParser[I,A] = new AsCellComponent[I, A, Parser[I,A]] {
    def apply(parser: Parser[I,A]): CellComponent[I,A] = CellParser(parser)
  }
  

  implicit def liftFilter[I,A] = new AsCellComponent[I, A, Filter[I,A]] {
    def apply(filter: Filter[I,A]): CellComponent[I,A] = CellFilter(filter)
  }

}

}
