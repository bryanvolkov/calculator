import scala.collection.mutable.Queue

class Evaluator {
  private val NUMBER:Int = 0
  private val OPERATOR:Int = 1
  
  private var stack = new Stack[Double]
  
  private val whatToDo: Map[Char, (Double, Double) => Unit] = Map('+' -> add, '-' -> sub, '*' -> mult, '/' -> div)
  
  def evaluate(queue: Queue[Token]):Double = {
    stack = new Stack[Double]
    for(token <- queue){              
      if(token.vtype == OPERATOR){
        var second = stack.pop()
        var first = stack.pop()
        whatToDo(token.value.asInstanceOf[Char])(first, second)
      }
      else if(token.vtype == NUMBER)
        stack.push(token.value.asInstanceOf[Double])
    }
    stack.pop()
  }
  
  private def add(x:Double, y:Double){
    stack.push(x + y)
  }
  
  private def sub(x:Double, y:Double){
    stack.push(x - y)
  }
  
  private def mult(x:Double, y:Double){
    stack.push(x * y)
  }
    
  private def div(x:Double, y:Double){
    stack.push(x / y)
  }
}