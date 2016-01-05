package tests

/**
 * @author misha
 */
object Test {
  // n * d < O(10^6)
  // Using 1GB RAM: 12 seconds (figure, cholecky, solution, error, seidel methods) 
  //           for n = 1e6.toInt and d = 4
  def test(n: Int, d: Int, eps: Double, four: Double, one: Double): Unit = {
    Console.println("Вариант №2")
    Console.println()
    
    val x = new Array[Double](n)
    for (i <- 0 until n) x(i) = i+1
    val mat = new matrix.MyMatrix(n, d, four, one, x)
    
    Console.println("Портрет исходной матрицы (O(n) операций):")
    
    val graph = mat.toMatrixAsGraph
    Console.println(graph.Info.asFigure)
    
    Console.println("n="+n)
    Console.println("d="+d)
    Console.println("eps="+eps)
    
    val res = mat.cholesky
    val L = res._1
    val fill = res._2
    val solution = L.Solution.solveLLT(mat.b)
    
    Console.println
    Console.println("Относительная погрешность решения, полученного методом Холецкого: "+
        vector.Function.Error.error(solution))
    Console.println("Величина заполнения: " + fill)
    
    // A = QR => ошибка в последнем столбце
    // время подсчета пропорционально некоторой степени величины заполнения
    // считает дольше, чем число обусловленности
    // При больших n считает лишь половину столбцов
    
    // val QR = matrix.Function.QR.getQR(graph)
    // Console.println("QR")
    
    try{
      val solveSeidel = matrix.Function.Solution.seidel(graph, mat.b, eps)
      
      Console.println
      Console.print("Относительная погрешность решения, полученного методом Гаусса-Зейделя: ")
      Console.println(vector.Function.Error.error(solveSeidel))
    } catch{
      case e: Exception => Console.println(e)
    }
    
    Console.println
    Console.print("Число обусловленности (O(n^3) операций): ")
    Console.println(L.cond1LLT)
  }    
}