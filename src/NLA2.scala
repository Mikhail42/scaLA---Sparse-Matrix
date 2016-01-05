/**
 * @author misha
 * for solution, see
 *  Андреев, В.Б.: Численные методы, часть I
 *  http://www.cs.cornell.edu/courses/CS4210/2014fa/CVLBook/chap7.pdf
 *  Каплан И.А. Практические занятия по высшей математике. Часть 5. Шестое практическое занятие.
 **/
object NLA2 {
  def main(args: Array[String]): Unit = { 
    val in = new java.util.Scanner(System.in);
    
    Console.println("Введите количество строк в квадратной матрице")
    val n   = in.nextInt
    Console.println("Введите полуширину матрицы") 
    val d   = in.nextInt
    Console.println("Введите погрешность результата итерационного метода")
    val eps = in.nextDouble
    
    val four = 4.0
    val one  = 1.0
    tests.Test.test(n, d, eps, four, one)
  }
}
    // val mat = Array.ofDim[Double](100,100)
    // for (i <- 0 until 100) mat(i)(i)=4.0
    // val sparse = new SparseMatrix(mat)
    // val x = new Array[Double](100)
    // val b = new Array[Double](100)
    // for (i <- 0 until 100) {x(i)=i+1000; b(i)=i+1}
    // sparse.asolve(x, b)
    // val plot = new SparseMatrixPlot(sparse)
    // SparseMatrixPlot.plot(sparse)
    // val myFrame = new MyFrameForm()
    // MyFrameForm.main(args)
    // val plot = new SparseMatrixPlot(sparse)
    // val myFrameForm = new MyFrameForm(sparse, java.awt.Color.BLUE)
    // myFrameForm.paint(new graph.MyFrame())
    // plot.paint()
    // val plotCanvas = smile.plot.SparseMatrixPlot.plot(sparse)