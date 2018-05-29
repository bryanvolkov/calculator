import scala.collection.mutable.Queue
//23423.23+2343
class Tokenizer {
  val NUMBER:Int = 0
  val OPERATOR:Int = 1
  val OPEN_PARENTHESIS:Int = 2
  val CLOSING_PARENTHESIS:Int = 3
  
  def tokenize(expression: String):Queue[Token] = {
    var queue = new Queue[Token]
    var start:Int = 0 // index of initial character
    var i:Int = 0
    while(i < expression.length){
      if(is_number(expression.charAt(i))){
        start = i
        i+=1
        while(i < expression.length && is_number(expression.charAt(i))) i+=1
        queue.enqueue(new Token(expression.substring(start, i).toDouble, NUMBER))
      }
      else{
        if(is_operator(expression.charAt(i)))
          queue.enqueue(new Token(expression.charAt(i), OPERATOR))
        else if(expression.charAt(i) == '(')
          queue.enqueue(new Token(expression.charAt(i), OPEN_PARENTHESIS))
        else queue.enqueue(new Token(expression.charAt(i), CLOSING_PARENTHESIS))
        i+=1
      }
    }
    queue
  }
  
   private def is_number(c:Char):Boolean = {
     '0' <= c && c <= '9' || c == '.'
  }
  
  private def is_operator(c:Char):Boolean = {
    c == '-' || c == '+' || c == '/' || c == '*'
  }
}