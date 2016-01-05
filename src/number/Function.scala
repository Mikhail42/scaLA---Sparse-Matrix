package number

object Function {
  def sqr( x: Double) = x*x
  def two( x: Double) = x+x
  def four(x: Double) = two(x+x)
  def mod2(x: Int)    = x&1
  def div2(x: Int)    = x>>1
  val ZERO = 1e-15
  def isZero(x: Double) = math.abs(x)<ZERO
}