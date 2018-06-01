import scala.util.control.Breaks._
import scala.collection.mutable.Queue
import scala.math._

object Calculator {
  private val NUMBER:Int = 0
  private val OPERATOR:Int = 1
  def main(args: Array[String]){
    val scanner = new java.util.Scanner(System.in)
    val inputAnalyzer = new InputAnalyzer
    val tkizer = new Tokenizer
    val rpnconverter = new RPNConverter
    val evaluator = new Evaluator
    
    print("Enter expression: ")
    var input = scanner.nextLine()
    
    while(input != ""){
      input = sanitize(input) // sanitize the input
      if(inputAnalyzer.analyze(input)){ // Analyze input
        var queue:Queue[Token] = tkizer.tokenize(input) // tokenize the input
        queue = rpnconverter.toRPN(queue) // arrange tokens in RPN
        printRPN(queue)
        println("Res: " + doubleToIntFormat(evaluator.evaluate(queue))) // evaluate RPN expression and print it
      }
      else println("Error")
      
      print("\nEnter expression: ")
      input = scanner.nextLine()
    }
    print("End of program...")
  }
  
  def printRPN(queue:Queue[Token]){
    print("RPN: ")
    for(token <- queue)
      if(token.vtype == NUMBER)
        print(doubleToIntFormat(token.value.asInstanceOf[Double]) + " ")
      else print(token.value.toString() + " ")
    println("")
  }
  
  def doubleToIntFormat(num: Double): String = {
    var d = num - floor(num)
    var res = ""
    if(d < 1 && d > 0 || d < 0 && d > -1)
      res = num.toString()
    else{
      res = floor(num).toString() 
      res = res.substring(0,res.length-2)
    }
    res
  }
  
   def sanitize(str:String): String = {
    var res:String = ""
    for(i <- 0 until str.length)
      if(str.charAt(i) != ' ')
        res += str.charAt(i)
    res
  }
}