package matrix
  import math._
  import element.NodeGraph
  
/**
 * you can find other type matrix, on: 
 * 	https://commons.apache.org/proper/commons-math/jacoco/org.apache.commons.math3.linear/OpenMapRealMatrix.java.html 
 *  https://github.com/scalanlp/breeze/wiki/Data-Structures	
 * 
 * Matrix(size,size):
 * a(i,i)   = four
 * a(i,i+1) = a(i,i-1) = one
 * a(i,i+d) = a(i,i-d) = one
 * 
 ***************************
     * d = 2, size = 5
     * 
     * 4 & 1 & 1 & 0 & 0   
     * 1 & 4 & 1 & 1 & 0  
     * 1 & 1 & 4 & 1 & 1 
     * 0 & 1 & 1 & 4 & 1  
     * 0 & 0 & 1 & 1 & 4  
		 ************************
     * d = 3, size = 5
     * 
     * 4 & 1 & 0 & 1 & 0  
     * 1 & 4 & 1 & 0 & 1 
     * 0 & 1 & 4 & 1 & 0 
     * 1 & 0 & 1 & 4 & 1  
     * 0 & 1 & 0 & 1 & 4  
     * ************************
     * SLAE: 
     * 			$	Ax = b, $
     * where $ x={(1,2,3,4,5)}^T $
 ****************************
 * #det(A) != 0
 * @param size < 10^5
 * @param 1 < d < size
 * @author misha
 */
case class MyMatrix(size: Int, d: Int, four: Double, one: Double, x: Array[Double]){
  
  ///
  /// matrix's properties
  /// 
  lazy val isDiagonal:      Boolean = abs(four) >= number.Function.two(abs(one))
  lazy val isSymmetrically: Boolean = true
  
  ///
  /// solutions
  ///  
  /** see first function G = CholGax(A) on p. 23 (cornell.edu) */ 
  lazy val cholesky: (TriangularMatrixAsGraph, Int) = {
    var amount = 0
    val L = new TriangularMatrixAsGraph(n)
    
    for (j <- 0 until n){
      val res = L.Multi.multiL(j, d)   // |{res}| = min(n-j,d+1) 
      val s = new Array[Double](min(n-j,d+1))
      for (k <- 0 until min(n-j,d+1))
        s(k) = getA(k+j,j) - res(k)
      
      // see http://introcs.cs.princeton.edu/java/95linear/Cholesky.java.html
      if (s(0)<=0.0)  throw new RuntimeException("Matrix not positive definite")
      if (s(0)<=1e-8) throw new RuntimeException("L_ii <= 10^(-8)")
      oneDivL_ii_s(j)     = 1.0/s(0)
      sqrtOneDivL_ii_s(j) = sqrt(oneDivL_ii_s(j))
      
      import number.Function._
      for (k <- 0 until min(n-j,d+1))
        if (!isZero(s(k))){
          val value = sqrtOneDivL_ii_s(j)*s(k)
          L.Modify.add(value, k+j, j)
          if (k>1 && k<d) amount += 1
        }
    }
    (L, amount)
  }
  
  // 
  def multiA(x: Array[Double]): Array[Double] = {
    val res = new Array[Double](n)
    for (i <- 0 until n) res(i) = 
      four*x(i) + one*(get(x,i-1)+get(x,i+1)+get(x,i-d)+get(x,i+d))
    res
  }
  
  lazy val toMatrixAsGraph: MatrixAsGraph = {
    val res = new MatrixAsGraph(n)
    for (i <- 0 until n) res.Modify.add(diag(i), i, i)
    for (i <- 0 until n-1) {
      res.Modify.add(diagUp(i), i, i+1)
      res.Modify.add(diagUp(i), i+1, i)
    }
    for (i <- 0 until n-d) {
      res.Modify.add(diagUp(i), i, i+d)
      res.Modify.add(diagUp(i), i+d, i)
    }
    res
  }
  
  ///
  /// vals, vars and access methods, first init
  ///
  val n = size
  
  val diag    = new Array[Double](n)
  val diagUpD = new Array[Double](n-d)
  val diagUp  = new Array[Double](n-1)
  
  for (i <- 0 until n)   diag(i)    = four
  for (i <- 0 until n-1) diagUp(i)  = one
  for (i <- 0 until n-d) diagUpD(i) = one
  
  val oneDivL_ii_s     = new Array[Double](n)
  val sqrtOneDivL_ii_s = new Array[Double](n)
  
  lazy val b = multiA(x)
  
  def getA(i: Int, j: Int) = {
    if (i==j)               diag(i)
    else if (abs(i-j) == 1) diagUp(max(i,j)-1)
    else if (abs(i-j) == d) diagUpD(max(i,j)-d)
    else                    0.0
  }
  def isA(i: Int, j: Int) = {
    if (i==j)               true
    else if (abs(i-j) == 1) true
    else if (abs(i-j) == d) true
    else                    false
  }
  
  def get(x: Array[Double], i: Int) = 
    if (i < 0 || i > n-1) 0.0
    else                  x(i) 
}