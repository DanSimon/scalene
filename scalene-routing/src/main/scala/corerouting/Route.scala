package scalene.corerouting

import scalene.Deferred

trait Route[I,O] {
  
  def vsetSize: Int

  def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : Result[Deferred[O]]

  final def apply(input: I): Result[Deferred[O]] = execute(input, Nil, if (vsetSize == 0) VSet.empty else VSet(vsetSize))

}

