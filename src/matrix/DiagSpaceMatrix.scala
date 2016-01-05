package matrix

  import vector.Function._
  import element.NodeGraph
  import scala.collection.mutable.ListBuffer
  import math._
  import matrix.Function._
  
class DiagSpaceMatrix(size: Int) extends MatrixAsGraph(size) {

  val isExistLowerDiagonals = new Array[Boolean](n)    //  \,  i>=j
  val isExistUpperDiagonals = new Array[Boolean](n)    //  \,  j>=i
  
  def add(x: Double, i: Int, j: Int): Unit = {
    this.Modify.add(x, i, j)
    if (i>=j) isExistLowerDiagonals(i-j) = true
    if (j>=i) isExistUpperDiagonals(j-i) = true  
  }
  private def getSetNumbersNotZeroDiags(isExistArray: Array[Boolean]) = {
    val list = ListBuffer[Int]()
    for (i <- 0 until n) 
      if (isExistArray(i)==true) list.+=(i)
    list
  }
  def getSetNumbersNotZeroLowerDiags = getSetNumbersNotZeroDiags(isExistLowerDiagonals)
  def getSetNumbersNotZeroUpperDiags = getSetNumbersNotZeroDiags(isExistUpperDiagonals)
  
  val mapNonZeroElements = new Array[ListBuffer[Int]](n)
  def setNonZeroElementsInStrings = {
    for (i <- 0 until n) {
      var node = this.firstElementsStrings(i)
      while (node!=null){
        mapNonZeroElements(i).+=(node.j)
        node = node.right
      }
    }
  }
  def setNonZeroElementsInColumns = {
    for (j <- 0 until n) {
      var node = this.firstElementsColumns(j)
      while (node!=null){
        mapNonZeroElements(j).+=(node.i)
        node = node.down
      }
    }
  }
  def lastNonZeroElementIndLowerDiag: Int = {
    var ind = n-1
    while (ind>0 && isExistLowerDiagonals(ind)==false) ind-=1
    ind
  }
  def lastNonZeroElementIndUpperDiag: Int = {
    var ind = n-1
    while (ind>0 && isExistUpperDiagonals(ind)==false) ind-=1
    ind
  }
  
  // edit!!!
  def multi(B: DiagSpaceMatrix): DiagSpaceMatrix = {
    val C = new DiagSpaceMatrix(n)
    
    val listUpperDiagsA = this.getSetNumbersNotZeroUpperDiags
    val listLowerDiagsA = this.getSetNumbersNotZeroLowerDiags
    val listUpperDiagsB = B.getSetNumbersNotZeroUpperDiags;     
    val listLowerDiagsB = B.getSetNumbersNotZeroLowerDiags;     
    
    val iterUpperDiagsB        = listUpperDiagsB.iterator
    val inverseIterLowerDiagsB = listLowerDiagsB.tail.iterator
    
    this.setNonZeroElementsInStrings
    B.setNonZeroElementsInColumns
    
    val indLastNonZeroUpperDiagB = B.lastNonZeroElementIndUpperDiag
    val indLastNonZeroLowerDiagB = B.lastNonZeroElementIndLowerDiag
    val indLastNonZeroUpperDiagA = this.lastNonZeroElementIndUpperDiag
    val indLastNonZeroLowerDiagA = this.lastNonZeroElementIndLowerDiag
    
    /** Если в матрицах есть достаточно большие блоки полностью свободного пространства (все элементы --- нули), 
     *  сравнимого по размерам с самой матрицей, то, воспользовавшись (грубым) знанием о форме матриц,
     *  можно выполнять умножение за линейное время относительно числа ненулевых элементов. 
     *  
     *  Для определения блоков предлагается использовать знание о нулевых диагоналях. Предполагается, что они идут
     *  с левого верхнего угла в правый нижний. В этом случае, важно определить полуширину каждой матрицы с каждой
     *  из сторон относительно главной диагонали. Далее, важно определить пустые и значительные по площади места
     *  между главной диагональю и крайними диагонялями с каждой из сторон. 
     */
    
    /** Фиксируем очередную строку матрицы A. */
    for (i <- 0 until n) {
      /** Проходим по столбцам B с возможностью выхода в основной цикл.  */
      var j = 0
      while (j < n){
        /** Находим элементы, пересекающиеся по индексам (A.ik==B.kj). */ 
        val per = vector.Function.Property.isIntersectSortedLists(
                        this.mapNonZeroElements(i), 
                        B.mapNonZeroElements(j))
        /** Если они есть, выполняем обычное умножение. */
        if (per == true) {
          val value = matrix.Function.BasicMultiply.multi(this, i, B, j)
          if (value!=0.0) C.add(value, i, j)
        } 
        /*   Иначе, мы строим предположение о том, будут ли следующищие элементы результирующей матрицы нулевыми, 
         *   и, если да, то сколько? (грубо) 
         *  
         *   Мы знаем, что, как минимум, 
         *   							B(0:j-1-indLastNonZeroUpperDiagB,   j) = 0,     j-1 > indLastNonZeroUpperDiagB;
         *   							B(indLastNonZeroLowerDiagB+j+1:n-1, j) = 0,     indLastNonZeroLowerDiagB+j+1 <= n-1.
         *   Также, 
         *   							A(i, 0:i-1-indLastNonZeroLowerDiagA)   = 0,    	i-1 > indLastNonZeroLowerDiagA;
         *   							A(i, i+1+indLastNonZeroUpperDiagA:n-1) = 0,			indLastNonZeroUpperDiagA+i+1 <= n-1.
         *   
         *   Следовательно, при ограничениях вышеприведенных 2-х систем:
         *      	A(i, :)*B(:,j) =
         *   				= A(i, i-indLastNonZeroLowerDiagA:i+indLastNonZeroUpperDiagA)*
         *   				 *B(j-indLastNonZeroUpperDiagB:indLastNonZeroLowerDiagB+j, j) = 
         *   				= A(i, is)*B(js, j).
         *   Если пересечение множеств is и js пусто, то произведение точно будет нулевым. Это условие выполняется, 
         *   если 
         *   				    n > {i-indLastNonZeroLowerDiagA} > {indLastNonZeroLowerDiagB+j} >= 0,
         *   	 =>	          i-indLastNonZeroLowerDiagA-indLastNonZeroLowerDiagB > j >= 0, 
         *   или 
         *  				    n > {j-indLastNonZeroUpperDiagB} > {i+indLastNonZeroUpperDiagA} >= 0,
         *   	 =>	 n+indLastNonZeroLowerDiagB > j > {i+indLastNonZeroUpperDiagA+indLastNonZeroLowerDiagB}.
         **/  
        else{    
          val if1 = i-indLastNonZeroLowerDiagA > indLastNonZeroLowerDiagB+j
          val if2 = j-indLastNonZeroUpperDiagB > i+indLastNonZeroUpperDiagA
          if (if2) j = n
          if (if1) j = max(j,i-indLastNonZeroLowerDiagA-indLastNonZeroLowerDiagB)
          
         /*  Используя информацию {1)} о структуре ненулевых элементов между крайними диагоналями каждой из матриц 
         *   (A и B), можно получить более сильные условия того, что некоторые элементы матрицы C юудут априори нулевыми. 
         *   
         *   Допустим, что из анализа нулевых диагоналей мы получили, что:
         *   		а) в текущей строке есть следующие ненулевые места (точнее, что во всех остальных местах -- нули): 
         *   						setNonZeroA_i = [i_1,i_2] \{union} [i_3,i_4] \{union} \dots \{union} [i_{m-1},i_m],
         *   		б) в текущем столбце есть следующие ненулевые места (точнее, что во всех остальных местах -- нули): 
         *   						setNonZeroB_j = [j_1,j_2] \{union} [j_3,j_4] \{union} \dots \{union} [j_{m-1},j_m].
         *      в) в столце {j+k}, k>0 (не обращаем пока внимание на граничные случаи) есть следующие ненулевые 
         *      	 места (точнее, что во всех остальных местах -- нули):
         *      				  setNonZeroB_{j+k} = [j_1+k,j_2+k] \{union} \dots \{union} [j_{m-1+k},j_{m+k}].
         *   
         *   Допустим, что на основе анализа диагоналей мы знаем, что пересечение setNonZeroA_i с setNonZeroB_j пусто:
         *   					setNonZeroA_i \{intersect} setNonZeroB_j = 
         *   						= \{union}_{p=1}^{I_m} {[i_{p-1},p]} \{intersect} \{union}_{p=1}^{J_m} {[j_{p-1},p]} = 
         *   						= \emptyset .
         *   Требуется найти такое k_{max}>=0, что при всех 0<=k<=k_{max} пересечения оставались бы пустыми: 
         *   					setNonZeroA_i \{intersect} setNonZeroB_{j+k}  = 
         *   						= \{union}_{p=1}^{I_m} {[i_{p-1},p]} \{intersect} \{union}_{p=1}^{J_m} {[j_{p-1}+k,p+k]} = 
         *   						= \emptyset .
         *   Проходим по каждому элементу setA_i_l массива объединений setNonZeroA_i, и определяем локальный 
         *   максимум k_{max}_loc, такой что пересечение setA_i_l с \{union}_{p=1}^{J_m} {[j_{p-1}+k,p+k]} давало бы \emptyset.
         *   Если число таких элементов асимптотически мало относительно n, то число операций будет константым.     
         *   
         *   ************************************************************************************************	 
         *   1) Необходимо учитывать то, что исходные условия должны получаться относительно быстро.  
         **/
         if (!if1 && !if2){
            
         }
        }
      }
    }
    
    C
  }
}