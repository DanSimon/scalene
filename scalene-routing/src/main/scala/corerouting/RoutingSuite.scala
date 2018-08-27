package scalene.corerouting

trait Types {

  type Result[T] = Either[ParseError, T]

}

trait RoutingSuite[I <: Clonable[I],O]
extends RouteBuilderOpsContainer[I, O] 
with RouteBuilding[I, O] 
with ParserContainer[I,O]
with ExtractionContainer[I,O]
with CellContainer[I,O]
with DocContainer
with Types 
