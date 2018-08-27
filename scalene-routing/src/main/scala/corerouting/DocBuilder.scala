package scalene.corerouting

trait DocContainer {

  //this type should be whatever is need to generate docs for the a single route.  
  //probably should be a monoid
  type DocType

  def EmptyDoc: DocType

  case class DocTreeNode(value: DocType, children: List[DocTreeNode])

}
