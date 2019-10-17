package pp201902.hw1test
import pp201902.hw1.Main._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1
  print_result(PDAcheckA(6) == 0)
  print_result(PDAcheckA(4) == -1)
  print_result(PDAcheckA(12) == 1)
  print_result(PDAcheckB(6) == 0)
  print_result(PDAcheckB(4) == -1)
  print_result(PDAcheckB(12) == 1)

  // Problem 2
  print_result(count(10, 10, (2, 6), (8, 6), (6, 6)) == 76991)
  print_result(count(5, 6, (2, 1), (2, 3), (4, 2)) == 124)

  // Problem 3
  // x^2 - 2 = 0 => x = sqrt(2) = 1.414 when starting point is 1
  println(solver((x: Double) => x * x - 2, (x: Double) => 2 * x, 1))
  // x^3 - 6x^2 + 11x - 6 = 0 => x = 1 when starting point is 0
  println(solver((x: Double) => x * x * x - 6 * x * x + 11 * x - 6,
                 (x: Double) => 3 * x * x - 12 * x + 11, 0))
}
