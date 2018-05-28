import scala.collection.mutable.Queue


//23423.23+2343

class Tokenizer {
  
  def tokenize(expression: String):Queue[Token] = {
    val queue = new Queue[Token]
    var start:Int = 0 // index of initial character
    var i:Int = 0
    while(i < expression.length){
      if(is_number(expression.charAt(i))){
        start = i
        i+=1
        while(i < expression.length && is_number(expression.charAt(i))) i+=1
        println(expression.substring(start, i))
      }
      else{
        if(is_operator(expression.charAt(i))){
          println(expression.charAt(i))
        }
        else if(expression.charAt(i) == '('){
          println(expression.charAt(i))
        }
        else
          println(expression.charAt(i))
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