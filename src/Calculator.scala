import scala.util.control.Breaks._
import scala.collection.mutable.Queue

object Calculator {
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
        println(evaluator.evaluate(queue)) // evaluate RPN expression and print it
      }
      else println("Error")
      
      print("\nEnter expression: ")
      input = scanner.nextLine()
    }
    print("End of program...")
  }
  
  def printRPN(queue:Queue[Token]){
    for(token <- queue)
      print(token.value.toString() + " ")
    println("")
  }
  
   def sanitize(str:String): String = {
    var res:String = ""
    for(i <- 0 until str.length)
      if(str.charAt(i) != ' ')
        res += str.charAt(i)
    res
  }
}