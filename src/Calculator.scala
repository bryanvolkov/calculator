import scala.util.control.Breaks._
import scala.collection.mutable.Queue
import scala.math._


object Calculator {
  private val NUMBER:Int = 0 
  private val OPERATOR:Int = 1
  
  // main function that runs the whole program
  def main(args: Array[String]){
    val scanner = new java.util.Scanner(System.in) // scanner to read user's input
    val inputAnalyzer = new InputAnalyzer // this object is used to analyze user's input
    val tkizer = new Tokenizer // this object is used to tokenize user's input
    val rpnconverter = new RPNConverter // this object is used to convert user's input to reverse polish notation
    val evaluator = new Evaluator // this object is used to evaluate the reverse polish notation which gives the result of the mathematical expression
    
    // get user's input
    print("Enter expression: ")
    var input = scanner.nextLine()

    // repeat while the user's input is not an empty string
    while(input != ""){
      input = sanitize(input) // sanitize the input
      if(inputAnalyzer.analyze(input)){ // Analyze input, and if it's correct then
        var queue:Queue[Token] = tkizer.tokenize(input) // tokenize the input
        queue = rpnconverter.toRPN(queue) // arrange tokens in RPN
        printRPN(queue) // print the reverse polish notation version of the user's input
        println("Res: " + doubleToIntFormat(evaluator.evaluate(queue))) // evaluate RPN expression and print it
      }
      else println("Error") // if user's input is rejected then print the Error message
      
      // get user's input
      print("\nEnter expression: ")
      input = scanner.nextLine()
    }
    print("End of program...")// this message let's know the user that the program has ended
  }
  
  // This function prints the contents of a queue, a queue whose contents are in reseverse polish natation order
  def printRPN(queue:Queue[Token]){
    print("RPN: ")
    for(token <- queue) // get token by token from the queue
      if(token.vtype == NUMBER) // if the token is a number, then
        print(doubleToIntFormat(token.value.asInstanceOf[Double]) + " ") // print that number in its correct format
      else print(token.value.toString() + " ") // print the operator
    println("")
  }
  
  // This function returns the double number in integer format if the decimal part of the double number is 0
  def doubleToIntFormat(num: Double): String = {
    var d = num - floor(num) // calculate the decimal part of the double num
    var res = "" // back-up string
    if(d < 1 && d > 0 || d < 0 && d > -1) // if the decimal part is in (0,1) or (-1,0)
      res = num.toString() // return the double number as a string the way it is
    else{
      res = floor(num).toString() // get the whole number of the double number and make it as a string 
      res = res.substring(0,res.length-2) // remove the ".0" part of the string
    }
    res // return the bakc-up string
  }
  
  // This function removes the spaces from string str
   def sanitize(str:String): String = {
    var res:String = "" // back up string to return
    for(i <- 0 until str.length) // loop through the string
      if(str.charAt(i) != ' ')// if the character of the string at position i is not a space, then
        res += str.charAt(i) // put that character in the back up string
    res // return the back up string
  }
}