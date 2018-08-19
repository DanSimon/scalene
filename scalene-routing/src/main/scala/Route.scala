package scalene.routing


trait Route[I,O] {
  
  def vsetSize: Int

  def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[O]

  final def apply(input: I): RouteResult[O] = execute(input, Nil, if (vsetSize == 0) VSet.empty else VSet(vsetSize))

}

