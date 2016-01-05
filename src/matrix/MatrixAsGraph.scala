package matrix

import element.NodeGraph
  
/** For matrix (n \times n), with O(n) non-zero elements
 * 
 * (we get a speed's order as 
 * 						mean of |{both column's and string's elements}|, 
 * where |{}| -- amount of elements, i.g., |{1,2,5,2}|=4).
 */
class MatrixAsGraph(size: Int) {
     
  ///
  /// vals and vars, access methods
  /// 
  val n = size
  
  val firstElementsColumns = new Array[NodeGraph](n)
  val firstElementsStrings = new Array[NodeGraph](n)
  
  val isExistColumn = new Array[Boolean](n)
  val isExistString = new Array[Boolean](n)
  
  import vector.Function.AccessMethods._
  def get(i: Int, j: Int): Double = getElementString(firstElementsStrings(i), j)
  def getNode(i: Int, j: Int)     = getNodeOfString(firstElementsStrings(i), j)

  def getDiag: Array[Double] = {
    val n = this.n
    val res = new Array[Double](n)
    
    for (i <- 0 until n){
      var node = this.firstElementsStrings(i)
      if (node!=null){
        if (node.i > node.j)
          while (node.i != node.j)
            node = node.right
        else 
          while (node.i != node.j)
            node = node.left
        res(i) = node.value
      }
    }
    
    res
  }
  
  def getNodesDiag: Array[NodeGraph] = {
    val n = this.n
    val res = new Array[NodeGraph](n)
    
    for (i <- 0 until n){
      var node = this.firstElementsStrings(i)
      if (node!=null){
        if (node.i > node.j)
          while (node.i != node.j)
            node = node.right
        else 
          while (node.i != node.j)
            node = node.left
        res(i) = node
      }
    }
    
    res
  }
  
  def trans: MatrixAsGraph = {
    val res = matrix.Function.Create.Zero(n)
    for (i <- 0 until n){
      var node = firstElementsStrings(i)
      while(node != null){
        res.Modify.add(node.value, node.j, node.i)
        node = node.right
      }          
    }
    res
  }
  
  object Create{
    def copy(withoutString1: Int, withoutString2: Int): MatrixAsGraph = {
      val res = new MatrixAsGraph(n)
      for (i <- 0 until n){
        if (i!=withoutString1 && i!=withoutString2){
          var node = firstElementsStrings(i)
          while(node!=null)
            res.Modify.add(node.value, i, node.j)
        }
      }
      res
    }
    def copy: MatrixAsGraph = {
      val res = new MatrixAsGraph(n)
      for (i <- 0 until n){
        var node = firstElementsStrings(i)
        while(node!=null){
          res.Modify.add(node.value, i, node.j)
          node = node.right
        }
      }
      res
    }
  }
  
  object Modify{
    
    def delete(i: Int): Unit = {
      var node = firstElementsStrings(i)
      while(node.right != null) {        
        isExistColumn(node.j) = !(node.down == null && node.up == null)
        if (isExistColumn(node.j) && firstElementsColumns(node.j) == node)
          firstElementsColumns(node.j) = node.down
        node = node.right
        node.left.delete
      }
      isExistColumn(node.j) = !(node.down == null && node.up == null)
      node.delete
      isExistString(i) = false
    }
    
    def add(x: Double, i: Int, j: Int): Unit = {
      if (number.Function.isZero(x)) return
      
      val addNode = new NodeGraph(x, i, j)
      
      def addToString: Unit = {
        if (isExistString(i)){
          var node = firstElementsStrings(i)
          if (node.left!=null) throw new Exception("node.left!=null")
          while(node.right != null && node.right.j<j){
            node = node.right
            if (node.right == node) 
              throw new Exception(
                  "node.right == node:\n"
                  +"i="+node.i
                  +", j="+node.j)
          }
            
          if (node.j<j) node.setRight(addNode)
          else{
            /*if (node.j==j) {  
              addNode.setUDLR(node)
            }
            else{*/
              node.setLeft(addNode)
              firstElementsStrings(i) = addNode
            //}
          }
        } else{
          firstElementsStrings(i) = addNode
          isExistString(i) = true
        }
      }
      
      if (isExistColumn(j)) {
        var node = firstElementsColumns(j)
        
        while(node.down != null && node.down.i<i)
          node = node.down
        if (node.i<i) node.setDown(addNode)
        else {
          /*if (node.i==i) {
            addNode.setUDLR(node)
          }
          else {*/
            node.setUp( addNode)
            firstElementsColumns(j) = addNode
          //}
        }
        addToString      
      } else {
        firstElementsColumns(j) = addNode
        isExistColumn(j) = true 
        
        addToString
      }
    }
  }
  
  object Info{ 
    
    override def toString: String = toString(toFull)
    
    def toString(A: Array[Array[Double]]): String = {
      val out = new java.lang.StringBuilder()
      for (i <- 0 until A.length) out.append(toString(A(i))).append('\n')
      out.toString()
    }
    
    /** 
    *  @count the number of digits after the decimal point. 
    *  format = '%'+@count.toString+'f'.  
    *  @return sum(format.format(a(:))+' ')
    *  */
    def toString(a: Array[Double], count: Int): String = {
      val format = "%."+count+'f'
      val out = new java.lang.StringBuilder()
      for (i <- 0 until a.length) out.append(format.format(a(i))).append(' ')
      out.toString()
    }
    
    def toString(a: Array[Double]): String = {
      val out = new java.lang.StringBuilder()
      for (i <- 0 until a.length-1) out.append("%.1f".format(a(i))).append(' ')
      out.append("%.1f".format(a(a.length-1)))
      out.toString()
    }
    
    def toFull: Array[Array[Double]] = {
      val res = Array.ofDim[Double](n,n)
      for (i <- 0 until n){
        var strNode = firstElementsStrings(i)
        var j = 0
        while (strNode!=null){
          while(j<strNode.j) {
            res(i)(j) = 0.0
            j+=1
          }
          if (j<n){
            res(i)(j) = strNode.value
            j+=1
          }
          strNode = strNode.right
        }
      }
      res
    }
    
    def asFigure: String = { 
      val m = math.min(n,20)                    // size
      val scala = math.max(n/m+1,1)
      val scalaD2 = scala>>1
      val mat = Array.ofDim[Boolean](m+1,m+1)
      
      for (i <- 0 until n){
        var node = firstElementsStrings(i)
        val iNode = i/scala
        while(node!=null){
          mat(iNode)(node.j/scala) = true
          node = node.right
        }
      }
      val res = new java.lang.StringBuffer();
      for(i <- 0 until m){
        for (j <- 0 until m)
          res.append(if (mat(i)(j)) "X " else "  ") 
        res.append('\n')
      }
      res.toString()
    }
  }
  override def toString = Info.toString
}