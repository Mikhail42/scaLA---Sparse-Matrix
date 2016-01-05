package matrix

import math._
import element.NodeGraph
import vector.Function._
import number.Function._

  /**
   * 1. Basic multiply
   * 2. QR
   * 3. methods to get information about matrix
   * 4. solution's methods
   * 5. specific methods
   * 6. norms, condition
   */
package Function {
  import number.Function._
  
  object BasicMultiply{
    /**
     * $ O(n^2) $ operations 
     */
    def multi(A: MatrixAsGraph, B: MatrixAsGraph): MatrixAsGraph = {
      val n = A.n
      val res = new MatrixAsGraph(n)
      for (i <- 0 until n; j <- 0 until n) {
        val c = multi(A,i,B,j)
        res.Modify.add(c, i, j)
      }
      res
    }
    /**
     * O(n) operation
     */
    def multi(mat: MatrixAsGraph, a: Array[Double]): Array[Double] = {
      val res = new Array[Double](a.length)
      for (i <- 0 until a.length) res(i) = 
        vector.Function.Scalar.multiString(mat.firstElementsStrings(i), a)
      res
    }
    /**
     $ A(i,:) \times B(:,j) $
     O(1) operations
     */
    def multi(A: MatrixAsGraph, i: Int, B: MatrixAsGraph, j: Int): Double = {
      var sum = 0.0
      var nodeA = A.firstElementsStrings(i)
      var nodeB = B.firstElementsColumns(j)
      while (nodeA!=null && nodeB!=null){
        while (nodeA!=null && (nodeB==null || nodeA.j<nodeB.i))
          nodeA = nodeA.right
        while (nodeB!=null && (nodeA==null || nodeB.i<nodeA.j))
          nodeB = nodeB.down
        if (nodeA!=null && nodeB!=null) { 
          sum += nodeA.value*nodeB.value
          nodeA = nodeA.right; nodeB = nodeB.down
        }
      }
      sum
    }
  }
  
  object Create{
    def I(n: Int) = {
      val res = new MatrixAsGraph(n)
      for (i <- 0 until n) res.Modify.add(1.0, i, i)
      res
    }
    def Zero(n: Int) = new MatrixAsGraph(n)
  }
      
  /**Если нужна высокая точность, то, видимо, лучше использовать 
   * другой тип данных (например, BigDecimal), нежели QR метод
   * 
   * A2 = QR => ошибка в последнем столбце 
   * время подсчета пропорционально некоторой степени величины заполнения
   * считает дольше, чем число обусловленности
   */
  object QR{
    
    def getRT(A: MatrixAsGraph): (MatrixAsGraph, MatrixAsGraph) = { 
      val n = A.n
      val R = A.Create.copy
      
      val T = Create.I(n)
      
      // result is upper triangle matrix
      for (j <- 0 until n) {
        var node = R.firstElementsColumns(j)
        while(node != null   &&   node.i <= j) node = node.down
        while(node != null   &&   node.i > j) {
          val (c,s) = getCS_GivensRotation(R.firstElementsColumns(j), node.i, j)
          
          val twoListR = vector.Function.Scalar.multiCS(
                          R.firstElementsStrings(node.i), 
                          R.firstElementsStrings(node.j), 
                          c, s, 
                          node.i, node.j)
          val twoListT = vector.Function.Scalar.multiCS(
                          T.firstElementsStrings(node.i), 
                          T.firstElementsStrings(node.j), 
                          c, s, 
                          node.i, node.j)                
          
          R.Modify.delete(node.j)
          for (x <- twoListR._2) R.Modify.add(x.value, node.j, x.j)
          
          R.Modify.delete(node.i)
          for (x <- twoListR._1) R.Modify.add(x.value, node.i, x.j)
          
          T.Modify.delete(node.j)
          for (x <- twoListT._2) T.Modify.add(x.value, node.j, x.j)
           
          T.Modify.delete(node.i)
          for (x <- twoListT._1) T.Modify.add(x.value, node.i, x.j)

          node = node.down
        }
      }
      (R, T)
    }
    
    def getQR(A: MatrixAsGraph) = {
      val RT = getRT(A)
      val R = RT._1
      val T = RT._2
      val Q = T.trans
      (Q, R)
    }
    
    /** Compute a pair (c,s) for Given's rotation
    
    O(1) time

    МГТУ им. Буамана, Москва 2014
    Е.С. Тверская. Семинары по вычислительной математике
    стр. 12
    @param firstElementOfColumn 
    @param i --- index of string 
    @param j --- index of column
    */
    protected def getCS_GivensRotation(
                      firstElementOfColumn: NodeGraph,
                      i: Int, j: Int
                      ): (Double, Double) = {
      var node = firstElementOfColumn
      
      var vI = 0.0; var vJ = 0.0; val maxInd = max(i,j)
      while(node != null  &&  node.i <= maxInd){
        if (node.i==i) vI = node.value
        if (node.i==j) vJ = node.value
        node = node.down
      }
      val oneDivTay = 1.0/sqrt(sqr(vI)+sqr(vJ))
      (vJ*oneDivTay, -vI*oneDivTay)
    }
  } 
  object Info{
    def normInf(A: MatrixAsGraph): Double = {
      var res = 0.0
      for (i <- 0 until A.n) 
        res = max(res, vector.Function.Norms.sumAbsString(A.firstElementsStrings(i)))
      res
    }
    def norm1(A: MatrixAsGraph): Double = {
      var res = 0.0
      for (j <- 0 until A.n) 
        res = max(res, vector.Function.Norms.sumAbsColumn(A.firstElementsColumns(j)))
      res
    }
    def toString(A: Array[Array[Double]]): String = {
      val out = new java.lang.StringBuilder()
      for (i <- 0 until A.length) out.append(vector.Function.Info.toString(A(i))).append('\n')
      out.toString()
    }
  }
  
  object Solution{
    /** Solution of $ Ax=b $ via Gauss-Seidel Method
    */
    def seidel(A: MatrixAsGraph, b: Array[Double], eps: Double): Array[Double] = {
      
      val n = A.n
      val diagA = A.getDiag
      val m1DiagA = new Array[Double](n)
      for (i <- 0 until n) m1DiagA(i) = 1.0/diagA(i)
      val res = new Array[Double](n)
      
      val B = new MatrixAsGraph(A.n)
      for (i <- 0 until n) {
        var node = A.firstElementsStrings(i)
        while (node!=null) {
          if (node.j != i)
            B.Modify.add(-node.value*m1DiagA(i), i, node.j)
          node = node.right
        }
      }
      val normB = Info.normInf(B)
      var iter = 0
      if (normB >= 1.0) {
        Console.println()
        Console.println("""Решение по методу Гаусса-Зейделя может не сходится --- 
                           норма матрицы -D^{-1} (L+R) равна """+normB)
        iter = 10
      }
      else {
        val res0 = new Array[Double](n)
        val res1 = new Array[Double](n)
        val resDif = new Array[Double](n)
        for (i <- 0 until n) res0(i) = b(i)*m1DiagA(i)
        for (i <- 0 until n) res1(i) = 
            ((b(i) - (vector.Function.Scalar.scalar(A.firstElementsStrings(i),res) 
                      - diagA(i)*res(i)))
             *m1DiagA(i))
        for (i <- 0 until n) resDif(i) = res1(i)-res0(i)
        val normResDif = vector.Function.Norms.normE(resDif)
        val number = (1-normB)*eps/normResDif
        var iter = max(log(number)/log(normB),1)
      }
      while (iter > 0) {
        for (i <- 0 until n) res(i) = 
          ((b(i) - (vector.Function.Scalar.scalar(A.firstElementsStrings(i),res) 
                    - diagA(i)*res(i)))
           *m1DiagA(i))
        
        iter -= 1
      }
      res
    }
  } 
}

  /**
    \subsection{Givens rotations}
    */
    /*def getQR_GR(A: MatrixAsGraph): (MatrixAsGraph, MatrixAsGraph) = { 
      val n = A.n
      var curR = A.copy
      for (i <- 0 until n){
        var node = curR.firstElementsStrings(i)
        while(node != null   &&   node.j < i){
          val (c,s) = getGivensMatrixCS(curR.firstElementsColumns(node.j), i, node.j)
          curR = multiGR(curR, c, s, i, node.j)          
          node = node.right
        }
      }
      
      def inverseUpperTriangleMatrix(U: MatrixAsGraph): TriangularMatrixAsGraph = {
        val n = U.n
        val thisDiags = U.getDiag
        val resDiags  = new Array[Double](n)
        for (i <- 0 until n) resDiags(i) = 1.0/thisDiags(i)
        
        val res = new TriangularMatrixAsGraph(n)
        for (i <- 0 until n) res.add(resDiags(i), i, i)
        for (i <- 0 until n; j <- i+1 until n) {
          val c = -resDiags(i)*vector.Function.scalar(
                                          res.firstElementsStrings(i),
                                          U.firstElementsColumns(j),
                                          0, j)
          if (c != 0.0) res.add(c, i, j)     
        }
        res
      }
      
      val Q = Function.BasicMultiply.multi(A, inverseUpperTriangleMatrix(curR))
      (Q, curR)
    }*/
    
    /** 
    \subsection{Multi for Given's rotation}
		
		O(1) operations
		for example:
    * 1 0  0 0    11 12 13 14   11      12      13      14 
    * 0 c  s 0  * 21 22 23 24 = c21+s31 c22+s32 c23+s33 c24+s34 
    * 0 -s c 0    31 32 33 34  -s21+c31-s22+c32
    * 0 0  0 1    41 42 43 44   41      42      43      44
    **/
    /*protected def multiGR(B: MatrixAsGraph, c: Double, s: Double,  i00: Int, j00: Int): MatrixAsGraph = {
      val n = B.n
      val res = B
      var i0 = min(i00,j00); var j0 = max(i00,j00)
      val listI0 = vector.Function.copyStr(B, i0)
      val listJ0 = vector.Function.copyStr(B, j0)
        
      var nodeI = B.firstElementsStrings(i0)
      var nodeJ = B.firstElementsStrings(j0)
          
      while(nodeI!=null && nodeJ!=null){  
        val curJ0 = min(nodeI.j, nodeJ.j)
        val val1 = if (nodeI.j == curJ0) nodeI.value else 0.0
        val val2 = if (nodeJ.j == curJ0) nodeJ.value else 0.0
           
        val val3 =  c*val1 + s*val2
        val val4 = -s*val1 + c*val2
           
        if (val3!=0.0) res.add(val3, i0, curJ0)
        if (val4!=0.0) res.add(val4, j0, curJ0)
          
        if (nodeI.j==nodeJ.j) {nodeI = nodeI.right; nodeJ = nodeJ.right}
        if (nodeI.j<nodeJ.j) nodeI = nodeI.right
        if (nodeI.j>nodeJ.j) nodeJ = nodeJ.right
      }
      while(nodeI != null){
        val val3 =  c*nodeI.value
        val val4 = -s*nodeI.value
        if (val3!=0.0) res.add(val3, i0, nodeI.j)
        if (val4!=0.0) res.add(val4, j0, nodeI.j)
        nodeI = nodeI.right
      }
      while(nodeJ != null){
        val val3 = s*nodeJ.value
        val val4 = c*nodeJ.value
        if (val3!=0.0) res.add(val3, i0, nodeJ.j)
        if (val4!=0.0) res.add(val4, j0, nodeJ.j)
        nodeJ = nodeJ.right
      }
      res
    }
    */