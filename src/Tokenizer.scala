import scala.collection.mutable.Queue

class Tokenizer {
  val NUMBER:Int = 0
  val OPERATOR:Int = 1
  val OPEN_PARENTHESIS:Int = 2
  val CLOSING_PARENTHESIS:Int = 3
  
  def tokenize(expression: String):Queue[Token] = {
    var queue = new Queue[Token]
    var i:Int = 0
    while(i < expression.length){
      if(is_number(expression.charAt(i))){
        var start:Int = i // index of initial character
        i+=1
        while(i < expression.length && is_number(expression.charAt(i))) i+=1
        queue.enqueue(new Token(expression.substring(start, i).toDouble, NUMBER))
      }
      else{
        if(is_operator(expression.charAt(i))){
          if(expression.charAt(i) == '-' && ((i-1) < 0 || (i-1) >= 0 && (!is_number(expression.charAt(i-1)) && expression.charAt(i-1) != ')')))
            queue.enqueue(new Token('~', OPERATOR))
          else
            queue.enqueue(new Token(expression.charAt(i), OPERATOR))
        }
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