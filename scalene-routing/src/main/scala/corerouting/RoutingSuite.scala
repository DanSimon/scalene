package scalene.corerouting


trait Types {

  type Result[T] = Either[ParseError, T]

}

trait RoutingSuite[I <: Clonable[I],O]
extends RouteBuilderOpsContainer[I, O] 
with RouteBuilding[I, O] 
with Types {

}
