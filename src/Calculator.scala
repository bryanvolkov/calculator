import scala.util.control.Breaks._
import scala.collection.mutable.Queue

object Calculator {
  def main(args: Array[String]){
    val scanner = new java.util.Scanner(System.in)
    val automaton = new Automaton
    val tkizer = new Tokenizer
    val rpnconverter = new RPNConverter
    val evaluator = new Evaluator
    print("Enter expression: ")
    var input = scanner.nextLine()
    while(input != ""){
      input = sanitize(input)
      if(automaton.check_string(input)){
        var queue:Queue[Token] = tkizer.tokenize(input)
        queue = rpnconverter.toRPN(queue)
        var result: Double = evaluator.evaluate(queue)
        for(i <- queue)
          print(i.value + " ")
        println("")
        println(result)
      }
      else println("Error")
      
      print("\nEnter expression: ")
      input = scanner.nextLine()
    }
    print("End of program...")
  }
  
    def sanitize(str:String): String = {
    var res:String = ""
    for(i <- 0 until str.length)
      if(str.charAt(i) != ' ')
        res += str.charAt(i)
    res
  }
}