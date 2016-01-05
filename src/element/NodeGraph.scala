package element

case class NodeGraph(var value: Double,
                     val     i: Int,
                     val     j: Int){
  
  var left:  NodeGraph = null
  var right: NodeGraph = null
  var up:    NodeGraph = null
  var down:  NodeGraph = null
  
  def setRight(node: NodeGraph): Unit = {
    if (this.right != null){
      this.right.left = node
      node.right      = this.right  
    }
    this.right = node
    node.left  = this
  }
  
  def delete: Unit = {
    if (right != null) right.left = left
    if (left  != null) left.right = right
    if (up    != null) up.down    = down
    if (down  != null) down.up    = up
    value = 0.0
  }
  
 /* def setUDLR(node: NodeGraph): Unit = {
    if (node.down  != null) {down  = node.down;  down.up    = this}
    if (node.up    != null) {up    = node.up;    up.down    = this}
    if (node.left  != null) {left  = node.left;  left.right = this}
    if (node.right != null) {right = node.right; right.left = this}
  }*/
  
  def setDown(node: NodeGraph): Unit = {
    if (this.down != null){
      this.down.up = node
      node.down    = this.down  
    }
    this.down = node
    node.up   = this
  }
  def setUp(node: NodeGraph): Unit = {
    if (this.up != null){
      this.up.down = node
      node.up      = this.up  
    }
    this.up    = node
    node.down  = this
  }
  def setLeft(node: NodeGraph): Unit = {
    if (this.left != null){
      this.left.right = node
      node.left      = this.left  
    }
    this.left   = node
    node.right  = this
  }
  
  def * (node2: NodeGraph) = this.value*node2.value
  def * (a: Double) = this.value*a
  def + (node2: NodeGraph) = this.value+node2.value
  def + (a: Double) = this.value+a
  
  override def toString = "("+i+", "+j+": "+value+")"
}