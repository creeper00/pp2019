package pp201902.hw1
import scala.annotation.tailrec

object Main {
  /*
   Implement given functions, which is currently blank. (???)
   */

  /*
   Exercise 1: Perfect, Deficient, Abundant number
   A) A pefect number is a natural number which is equal to the sum of its
   divisors excluding itself.
   Similarly, a number is a deficient number (abundant number), when the number
   is smaller (bigger) than the sum of its divisors excluding itself.
   For example, 6, 28, and 496 are perfect numbers; 1, 4, and 10 are deficient
   numbers; 12, 18 and 20 are abudant number.
   Implement a function that returns 0, -1, and 1, when a given natural number n is
   a perfect number, a deficiet number, and a abundant number respectively.
   For any input n s.t. 1 <= n <= 10^3, your program should terminate within 1 second.
   */
  def PDAcheckA(n: Int): Int = {
    def cal(m : Int) : Int = {
      if(m==0) 0
      else if (n%m==0) m+cal(m-1)
      else cal(m-1)
    }
    if(cal(n-1)==n) 0
    else if( cal(n-1)<n ) -1
    else 1
  }

  /*
   B) Implement A) using tail recursion.
   The waste of the stack space can be avoided by using tail recursion.
   For any input n s.t. 1 <= n <= 10^5, your program should terminate within 5 seconds.
   */
  def PDAcheckB(n: Int): Int = {
    @tailrec def cal(m : Int, res : Int) : Int = {
      if(m==0) res
      else if(n%m==0) cal(m-1, m+res)
      else cal(m-1,res)
    }
    if(cal(n-1,0)==n) 0
    else if( cal(n-1,0)<n ) -1
    else 1
  }

  /*
   Exercise 2: Path count
   Given n, m, and three inaccessable points, compute the number of the North-East lattice paths
   from (0, 0) to (n, m).
   (See https://en.wikipedia.org/wiki/Lattice_path)
   */
  def count(n: Int, m: Int, x: (Int, Int), y: (Int, Int), z: (Int, Int)): BigInt = {
    if(n<0 || m<0) return 0;
    def comb ( a : Int, b : Int): BigInt = {
      if(a<b || a<0 || b<0 || (a==0 && b==0)) 0
      else if(a==0 || b==0) 1
      else if(b==1) a
      else comb(a-1,b-1)+comb(a-1,b)
    }
    def overlap: BigInt = {
      def docal(x : (Int, Int), y : (Int, Int)) : BigInt = {
        if(x._1*y._1 < 0 || x._2*y._2 <0) 0
        else if (x._1 >= y._1 && x._2 >= y._2) comb(x._1 - y._1 + x._2 - y._2, x._1 - y._1) * comb(y._1 + y._2, y._1) * comb(n - x._1 + m - x._2, n - x._1)
        else if (x._1 <= y._1 && x._2 <= y._2) comb(-x._1 + y._1 - x._2 + y._2, -x._1 + y._1) * comb(x._1 + x._2, x._1) * comb(n - y._1 + m - y._2, n - y._1)
        else 0
      }
      val a: BigInt = docal(x,y)
      val b: BigInt = docal(y,z)
      val c: BigInt = docal(x,z)
      a+b+c
    }
    def sum : BigInt = {
      def dosum(x : (Int, Int)) : BigInt =
        if(x._1<0 || x._2<0) 0 else comb(x._1+x._2,x._1)*comb(n+m-x._1-x._2, n-x._1)
      val a : BigInt = dosum(x)
      val b : BigInt = dosum(y)
      val c : BigInt = dosum(z)
      a+b+c
    }
    def ans = comb(n+m,m)-sum+overlap
    if(ans<0) 0
    else ans
  }

  /*
   Exercise 3: Newton's method
   Newton's method is root-finding algorithm which produces successively better
   approximations to the roots (or zeroes) of a real-valued function.
   (See https://en.wikipedia.org/wiki/Newton%27s_method)
   Given a function f, a derivative of f, and a start point, solve equation f(x) = 0
   using Newton's method starting from the given start point.
   For a solution x, |f(x)| should be smaller than 0.01.
   */
  def solver(f: Double => Double, diff_f: Double => Double, start: Double) = {
    def cal(a : Double): Double =
    {
      if(Math.abs(f(a))>0.001)
      {
        val sum = f(a)/diff_f(a)
        cal(a-sum)
      }
      else a
    }
    cal(start)
  }
}
