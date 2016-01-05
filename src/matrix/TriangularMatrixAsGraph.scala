package matrix

import matrix.Function.BasicMultiply._
import matrix.Function.Info._
import number.Function._
import vector.Function._

class TriangularMatrixAsGraph(size: Int) extends MatrixAsGraph(size) {
                     
  lazy val detL: Double = {
    val diags = getDiag
    var res = 1.0
    for (i <- 0 until n) res*=diags(i)
    res
  }
  
  lazy val detLLT: Double = sqr(detL)
  
  lazy val cond1LLT = norm1(multiTrianglesLLT)*norm1(inverseLowerTriangleMatrix.multiTrianglesLTL) 
  
  lazy val multiTrianglesLLT: TriangularMatrixAsGraph = {
    val res = new TriangularMatrixAsGraph(n)
    for (i <- 0 until n; j <- 0 until n){
      val value = Multi.multiLLT(i, j)
      res.Modify.add(value, i, j)
    }
    res
  }
  
  lazy val multiTrianglesLTL: TriangularMatrixAsGraph = {
    val res = new TriangularMatrixAsGraph(n)
    for (i <- 0 until n; j <- 0 until n){
      val value = Multi.multiLTL(i, j)
      res.Modify.add(value, i, j)
    }
    res
  }
  
   /** 
  \section{Matrix Methods for LL^T} 
   */
  //  Каплан И.А. Практические занятия по высшей математике. Часть 5. Шестое практическое занятие.  
  lazy val inverseLowerTriangleMatrix: TriangularMatrixAsGraph = {
    val n = this.n
    val thisDiags = this.getDiag
    val resDiags  = new Array[Double](n)
    for (i <- 0 until n) resDiags(i) = 1.0/thisDiags(i)
    
    val res = new TriangularMatrixAsGraph(n)
    for (i <- 0 until n) res.Modify.add(resDiags(i), i, i)
    for (i <- 0 until n; j <- 0 until i) {
      val c = -resDiags(i)*vector.Function.Scalar.scalar(
                                      this.firstElementsStrings(i),
                                      res.firstElementsColumns(j),
                                      0, i)
      res.Modify.add(c, i, j)     
    }
    res
  }
  
  object Multi{
    def multiLLT(i: Int, j: Int): Double = {
      var sum = 0.0
      
      var nodeA = firstElementsStrings(i)
      var nodeB = firstElementsStrings(j)
      
      while (nodeA!=null && nodeB!=null){
        while (nodeA!=null && nodeA.j<nodeB.j)
          nodeA = nodeA.right
          
        if (nodeA!=null)
        while (nodeB!=null && nodeB.j<nodeA.j)
          nodeB = nodeB.right
          
        if (nodeA!=null && nodeB!=null){
          sum += nodeA.value*nodeB.value
          nodeA = nodeA.right; nodeB = nodeB.right;
        }
      }
      sum
    }
    
    def multiLTL(i: Int, j: Int): Double = {
      var sum = 0.0
      
      var nodeA = firstElementsColumns(i)
      var nodeB = firstElementsColumns(j)
      
      while (nodeA!=null && nodeB!=null){
        while (nodeA!=null && nodeA.i<nodeB.i)
          nodeA = nodeA.down
          
        if (nodeA!=null)
        while (nodeB!=null && nodeB.i<nodeA.i)
          nodeB = nodeB.down
          
        if (nodeA!=null && nodeB!=null){
          sum += nodeA.value*nodeB.value
          nodeA = nodeA.down; nodeB = nodeB.down;
        }
      }
      sum
    }
    
    /**
     * res(j:n-1) = L(j:n-1,0:j-1)*L(j,0:j-1)'
     * for matrix with d diagonals both up and lower
     */
    def multiL(j: Int, d: Int): Array[Double] = {
      // !!! --- Андреев, В.Б.: Численные методы, часть I
      val size = math.min(n-j,d+1)
      val res = new Array[Double](size)
      if (!isExistString(j)) return res
      
      // first of L(j,:)
      var nodeStrJ = firstElementsStrings(j)
      /** first's L(j+1:n,:) */
      var listNode = {
        val list = new Array[element.NodeGraph](size-1)
        for (i <- 0 until size-1) list(i) = firstElementsStrings(i+j+1)
        list
      }
      
      while (nodeStrJ!=null && nodeStrJ.j<j){
        // do listNode(:).j >= nodeStrJ.j, where listNode(:)!=null
        for (i <- 0 until size-1)
          if (listNode(i)!=null)
          while (listNode(i).j < nodeStrJ.j && listNode(i).right!=null)
            listNode(i) = listNode(i).right
       
        for (i <- 1 until size) 
          if (listNode(i-1)!=null)
          if (listNode(i-1).j == nodeStrJ.j) 
            res(i) += listNode(i-1).value*nodeStrJ.value
        res(0)  += number.Function.sqr(nodeStrJ.value)
        nodeStrJ = nodeStrJ.right
      }
      res
    }
  }
  
  object Solution{
    def solveLLT(b: Array[Double]): Array[Double] = {
      val diags = getDiag
      val oneDivL_ii_s = new Array[Double](n)
      for (i <- 0 until n) oneDivL_ii_s(i) = 1.0/diags(i)
      // $ A = L \cdot L^T $    
      // $ L \cdot (L^T x) = b $ 
      // $ L \cdot     y   = b $ 
      val ys = new Array[Double](n)
      for (i <- 0 until n)  
      // $ y(i) = /frac{b(i) - L(i, 0:i-1)*y(0:i-1)}          {1.0/L_{ii}} $
          ys(i) =      (b(i) - vector.Function.Scalar.multiString(firstElementsStrings(i), ys, 0, i))*oneDivL_ii_s(i)
        
      // $ L^T x   = y $
      /**
       * ( 1 0 0 )^T 	 ( 1 2 4 )
       * ( 2 3 0 )   = ( 0 3 5 ).
       * ( 4 5 6 ) 		 ( 0 0 6 )
       */
      val xs = new Array[Double](n)
      var i = n-1
      while (i >= 0) {
        xs(i) = (ys(i) - vector.Function.Scalar.multiColumn(firstElementsStrings(i),xs,i+1,n))*oneDivL_ii_s(i)  
        i -= 1
      }
      
      xs
    }
  }
}