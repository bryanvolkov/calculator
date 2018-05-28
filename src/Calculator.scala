import scala.util.control.Breaks._

object Calculator {
  def main(args: Array[String]){
    val scanner = new java.util.Scanner(System.in)
    val automaton: Automaton = new Automaton
    val tkizer: Tokenizer = new Tokenizer
    print("Enter expression: ")
    var input = scanner.nextLine()
    while(input != ""){
      if(automaton.check_string(input)){
        //print("Accepted")
        tkizer.tokenize(input)
        //print(rpn(input))
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