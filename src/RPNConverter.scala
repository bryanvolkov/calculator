import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

class RPNConverter{
  val NUMBER:Int = 0
  val OPERATOR:Int = 1
  val OPEN_PARENTHESIS:Int = 2
  val CLOSING_PARENTHESIS:Int = 3

  def toRPN(tokens: Queue[Token]): Queue[Token] = {
     var stack = Stack[Token]()
    var queue = Queue[Token]()
    for(i <- tokens) {
      val token:Token = i
      if(token.vtype == NUMBER)
        queue.enqueue(token)
      else if(token.vtype == OPERATOR) {
        if(stack.isEmpty)
          stack.push(token)
        else {
          while(!stack.isEmpty && checkPrecedence(stack.top.value.toString()) > checkPrecedence(token.vtype.toString()))
            queue.enqueue(stack.pop())
          stack.push(token)
        }
      }
      else if(token.vtype == OPEN_PARENTHESIS)
          stack.push(token)
      else if(token.vtype == CLOSING_PARENTHESIS) {
          while(stack.top.vtype != OPEN_PARENTHESIS) queue.enqueue(stack.pop())
          stack.pop()
        }
      else
        print("Token: " + token.value.toString() + " not identified, disregarding...\n")
    }
    while(!stack.isEmpty)
      if(stack.top.value.toString() == "(")
        stack.pop()
      else queue.enqueue(stack.pop())
    queue
  }

  def checkPrecedence(input: String) : Int = {
    val hi:Int = 2
    val med:Int = 1
    val low:Int = 0
    var pres: Int = 0
    if(input == "+" || input == "-") pres = low
    else if(input == "*" || input == "/") pres = med
    else if(input.equals("^")) pres = hi
    pres
  }
}