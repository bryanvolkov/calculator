import scala.util.control.Breaks._
import scala.collection.mutable.Queue

object Calculator {
  def main(args: Array[String]){
    val scanner = new java.util.Scanner(System.in)
    val automaton: Automaton = new Automaton
    val tkizer: Tokenizer = new Tokenizer
    val rpnconverter: RPNConverter = new RPNConverter
    print("Enter expression: ")
    var input = scanner.nextLine()
    while(input != ""){
      if(automaton.check_string(input)){
        var queue:Queue[Token] = tkizer.tokenize(input)
        queue = rpnconverter.toRPD(queue)
        for(i <- queue)
          println(i.value)

      }
      else print("Error")
      
      print("\nEnter expression: ")
      input = scanner.nextLine()
    }
    print("End of program...")
  }
  
  def rpn(str: String):String = {
    ""
  }
}