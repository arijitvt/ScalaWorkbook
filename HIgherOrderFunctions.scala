import scala.annotation.tailrec

object Main {

  def genericSum(x: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0 else x(a) + genericSum(x, a + 1, b)

  def genericSumTailRec(x: Int => Int, a: Int, b: Int) = {
    @tailrec
    def loop(a: Int, soFar: Int): Int = {
      if (a > b) soFar
      else {
        loop(a + 1, x(a) + soFar)
      }
    }
    loop(a, 0)
  }

  def genericSumTailRecWithCurry(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      @tailrec
      def loop(a: Int, soFar: Int): Int = {
        if (a > b) soFar else loop(a + 1, f(a) + soFar)
      }
      loop(a, 0)
    }
    sumF
  }

  def genericProductTailRecWithCurry(f: Int => Int): (Int, Int) => BigInt = {
    def productF(a: Int, b: Int): BigInt = {
      @tailrec
      def loop(a: Int, soFar: BigInt): BigInt = {
        if (a > b) soFar else loop(a + 1, f(a) * soFar)
      }
      loop(a, 1)
    }
    productF
  }
  
  def genericProductTailRecWithCurryWithSugar(f: Int => Int) (a:Int, b:Int) : BigInt = {
      @tailrec
      def loop(a: Int, soFar: BigInt): BigInt = {
        if (a > b) soFar else loop(a + 1, f(a) * soFar)
      }
      loop(a, 1)

  }
  
  def factorial(a:Int) =  {
   	if(a<=0) 1 else genericProductTailRecWithCurry((x)=>x)(1,a)
  }
  
  
   def factorialWithSugar(a:Int) =  {
   	if(a<=0) 1 else genericProductTailRecWithCurryWithSugar((x)=>x)(1,a)
  }


  def main(args: Array[String]): Unit = {
    println(genericSum((x) => x, 0, 1000))
    println(genericSumTailRec((x) => x, 0, 1000))
    println(genericSumTailRecWithCurry((x) => x)(0, 1000))
    println(genericSum((x) => x * x * x, 0, 10))
    println(genericSumTailRecWithCurry((x) => x * x * x)(0, 10))
    println(genericProductTailRecWithCurry((x)=>x)(1,10))
    println(factorial(100))
    println(factorialWithSugar(100))

  }
}

