package vector

import element._
import math._
import number.Function._
import matrix._
import scala.collection.mutable._

object Function {
  object Scalar{
    def scalar(strp: NodeGraph, colp: NodeGraph): Double = {
      var nodeStr = strp
      var nodeCol = colp
      var sum = 0.0
      while(nodeStr!=null && nodeCol!=null){
        while(nodeStr!=null && nodeStr.j<nodeCol.i)
          nodeStr = nodeStr.right
        while(nodeCol!=null && nodeCol.i<nodeStr.j)
          nodeCol = nodeCol.down
        if (nodeStr!=null && nodeCol!=null){
          sum += nodeStr.*(nodeCol)
          nodeStr = nodeStr.right
          nodeCol = nodeCol.down        
        }
      }
      sum
    }
    import scala.collection.mutable.ListBuffer
    /**
     result two list: res a multiply matrix with cs-elements on exist matrix with two string: 
     																				 I0String and J0String
     @param i0 > j0
     */
    def multiCS(
              firstElementI0String: NodeGraph, firstElementJ0String: NodeGraph, 
              c: Double, s: Double,
              i0: Int, j0: Int 
              ): 
              (ListBuffer[NodeGraph], ListBuffer[NodeGraph]) = {
      
      var nodeI = firstElementI0String
      var nodeJ = firstElementJ0String
      
      val listI = new ListBuffer[NodeGraph]()
      val listJ = new ListBuffer[NodeGraph]()
      
      while (nodeI != null || nodeJ != null){
        while (nodeJ != null  &&  (nodeI==null || nodeJ.j < nodeI.j)){
          listJ.+=(new NodeGraph(c*nodeJ.value, j0, nodeJ.j))
          listI.+=(new NodeGraph(s*nodeJ.value, i0, nodeJ.j))
          nodeJ = nodeJ.right
        }
        while (nodeI != null  &&  (nodeJ==null || nodeI.j < nodeJ.j)){
          listJ.+=(new NodeGraph(-s*nodeI.value, j0, nodeI.j))
          listI.+=(new NodeGraph( c*nodeI.value, i0, nodeI.j))
          nodeI = nodeI.right
        }
        if (nodeI!=null  &&  nodeJ!=null && nodeI.j==nodeJ.j){
          val vJ = c*nodeJ.value - s*nodeI.value
          val vI = s*nodeJ.value + c*nodeI.value
          import number.Function._
          if (!isZero(vJ)) listJ.+=(new NodeGraph(vJ, j0, nodeI.j))
          if (!isZero(vI)) listI.+=(new NodeGraph(vI, i0, nodeI.j))
          
          nodeI = nodeI.right
          nodeJ = nodeJ.right
        }
      }
      (listI, listJ)
    }
    
    def scalar(strp: NodeGraph, colp: NodeGraph, k1: Int, k2: Int): Double = {
      var nodeStr = strp
      while (nodeStr.j < k1 && nodeStr.right != null) 
        nodeStr = nodeStr.right
      
      var nodeCol = colp
      while (nodeCol.i < k1 && nodeCol.down != null) 
        nodeCol = nodeCol.down
        
      var sum = 0.0
      while(nodeStr!=null && nodeStr.j<k2 && nodeCol!=null && nodeCol.i<k2){
        while(nodeStr!=null && nodeStr.j<nodeCol.i)
          nodeStr = nodeStr.right
          
        if (nodeStr!=null)
        while(nodeCol!=null && nodeCol.i<nodeStr.j)
          nodeCol = nodeCol.down
        
        if (nodeStr!=null && nodeCol!=null){
          sum += nodeStr.*(nodeCol)
          nodeStr = nodeStr.right
          nodeCol = nodeCol.down        
        }
      }
      sum
    }
    
    /** mat(i, j1:j2-1)*a(j1:j2-1)  */
    def multiString(firstElementString: NodeGraph, a: Array[Double], j1: Int, j2: Int): Double = {
      var node = firstElementString
      while (node.j < j1 && node.right != null) 
        node = node.right
     
      var sum = 0.0
      if (node.j >= j1){
        // compute mat(i, j1:j2-1)*a(j1:j2-1)
        while (node.j < j2 && node.right != null){
          sum += node.value*a(node.j)
          node = node.right
        }
        if (node.j < j2) sum += node.value*a(node.j)
      }
      
      sum
    }
    
    /** mat(i, j1:j2-1)*a(j1:j2-1)  */
    def multiString(firstElementString: NodeGraph, a: Array[Double]): Double = {
      var sum = 0.0
      var node = firstElementString
      while (node.j < a.length && node.right != null){
          sum += node.value*a(node.j)
          node = node.right
      }
      sum
    }
    
    def multiColumn(firstElementColumn: NodeGraph, a: Array[Double], i1: Int, i2: Int): Double = {
      var sum = 0.0
      
      var node = firstElementColumn
      while (node.i < i1 && node.down != null) 
        node = node.down
      
      if (node.i >= i1){
        while (node.i < i2 && node.down != null){
          sum += node.value*a(node.i)
          node = node.down
        }
        if (node.i < i2) sum += node.value*a(node.i)
      }
      
      sum
    }
   
    def scalar(nodeStr: NodeGraph, b: Array[Double]): Double = {
      var sum = 0.0
      var node = nodeStr
      while(node!=null){
        sum += node.value*b(node.j)
        node = node.right
      }
      sum
    }
  }
  object Property{
    def isIntersectSortedLists(a: ListBuffer[Int], b: ListBuffer[Int] ): Boolean = {
      val iterA = a.iterator
      val iterB = b.iterator
      
      var curA = a.head
      var curB = b.head
      
      var flag = false
      while (((curA<curB && iterA.hasNext) || (curB<curA && iterB.hasNext)) && !flag){
        while(curA<curB && iterA.hasNext)
          curA = iterA.next()
        while(curB<curA && iterB.hasNext)
          curB = iterB.next()
        flag = (curA==curB)
      }
      
      flag
    }
  }
  object Error{
    def sumAbsError(a: Array[Double]): Double = {
      val n = a.length
      var sum = 0.0
      for (i <- 0 until n) sum += abs(a(i)-(i+1))
      sum
    }
    
    def error(a: Array[Double]): Double = {
      var sum1 = 0.0; var sum2 = 0.0
      val n = a.length
      for (i <- 0 until n) {
        sum1 += sqr(abs(a(i)-(i+1)))
        sum2 += sqr(i+1)
      }
      math.sqrt(sum1/sum2)
    }
  }
  object Norms{
    def normE(a: Array[Double]) = {var res = 0.0; for (x <- a) res+=sqr(x); sqrt(res)}
  
    def sumAbsString(nodeStr: NodeGraph): Double = {
      var sum = 0.0
      var node = nodeStr
      while (node!=null) {
        sum += abs(node.value)
        node = node.right
      }
      sum
    }
    
    def sumAbsColumn(nodeCol: NodeGraph): Double = {
      var sum = 0.0
      var node = nodeCol
      while (node!=null) {
        sum += abs(node.value)
        node = node.down
      }
      sum
    }
  }
  
  object AccessMethods{
    def getNodeOfString(nodeString: NodeGraph, j: Int): NodeGraph = {
      var node = nodeString
      if (node.j < j)
        while (node.right!=null && node.j<j)
          node = node.right
      else 
        while (node.left!=null && node.j>j)
          node = node.left
      if (node.j == j) node else null
    }
    def getElementString(nodeString: NodeGraph, j: Int): Double = {
      val res = getNodeOfString(nodeString,j)
      if (res!=null) res.value else 0.0
    }
    
    def getElementColumn(nodeColumn: NodeGraph, i: Int): Double = {
      var node = nodeColumn
      if (node.i < i)
        while (node.down!=null && node.i<i)
          node = node.down
      else 
        while (node.up!=null && node.i>i)
          node = node.up
      if (node.i == i) node.value else 0.0
    }  
  }
  object Info{
    def toFullString(a: NodeGraph, n: Int): Array[Double] = {
      val res = new Array[Double](n)
      var node = a
      while(node.left!=null) node = node.left
      while(node.right!=null) {
          res(node.j) = node.value
          node = node.right
      }
      res
    }
    
    def copyStr(A: MatrixAsGraph, i: Int): ListBuffer[Node] = {
      val list = ListBuffer[Node]()
      var node = A.firstElementsStrings(i)
      while(node!=null){
        list.+=(new Node(node.value, node.i, node.j))
        node = node.right
      }
      list
    }
    
    def toString(a: Array[Double], count: Int): String = {
      val format = "%."+count+'f'
      val out = new java.lang.StringBuilder()
      for (i <- 0 until a.length) out.append(format.format(a(i))).append(' ')
      out.toString()
    }
    
    /** toString(a,4) */
    def toString(a: Array[Double]): String = {
      val out = new java.lang.StringBuilder()
      for (i <- 0 until a.length-1) out.append("%.4f".format(a(i))).append(' ')
      out.append("%.4f".format(a(a.length-1)))
      out.toString()
    }
  }
}