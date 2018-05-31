import scala.collection.mutable.Queue

class RPNConverter{
  private val NUMBER:Int = 0
  private val OPERATOR:Int = 1
  private val OPEN_PARENTHESIS:Int = 2
  private val CLOSING_PARENTHESIS:Int = 3

  private var stack = new Stack[Token]
  private var queue = Queue[Token]()
  
  private val whatToDo: Array[(Token) => Unit] = Array(ifNumber, ifOperator, ifOpenParenthesis, ifClosingParenthesis)
  
  def toRPN(tokens: Queue[Token]): Queue[Token] = {
    stack = new Stack[Token]
    queue = Queue[Token]()
    
    for(token <- tokens)
      whatToDo(token.vtype)(token)
      
    while(!stack.isEmpty)
      if(stack.peek.value.toString() == "(")
        stack.pop()
      else queue.enqueue(stack.pop())
    queue
  }

  private def ifNumber(token: Token){
    queue.enqueue(token)
  }
  
  private def ifOperator(token: Token){
    if(stack.isEmpty)
      stack.push(token)
    else{
      if(!stack.isEmpty && checkPrecedence(stack.peek.value.toString()) >= checkPrecedence(token.value.toString()))
        while(!stack.isEmpty && stack.peek.vtype != OPEN_PARENTHESIS)
          queue.enqueue(stack.pop())
      stack.push(token)
    }
  }
  
  private def ifOpenParenthesis(token: Token){
    stack.push(token)
  }
  
  private def ifClosingParenthesis(token: Token){
    while(stack.peek.vtype != OPEN_PARENTHESIS)
      queue.enqueue(stack.pop())
    stack.pop()
  }
  
  private def checkPrecedence(input: String) : Int = {
    val hi:Int = 2
    val med:Int = 1
    val low:Int = 0
    var pres: Int = 0
    if(input == "+" || input == "-") pres = low
    else if(input == "*" || input == "/") pres = med
    else if(input == "^") pres = hi
    pres
  }
}