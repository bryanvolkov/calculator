import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

class Evaluator {
  val NUMBER:Int = 0
  val OPERATOR:Int = 1
  def evaluate(queue: Queue[Token]):Double = {
    var stack = Stack[Double]()
        for(i <- queue)
        {              
          if(i.vtype == OPERATOR){
            var s = i.value.asInstanceOf[Char]
            if(s == '+')
            {
              var second = stack.pop()
              var first = stack.pop()
              stack.push(first + second)
            }
            else if(s == '-')
            {
              var second = stack.pop()
              var first = stack.pop()
              stack.push(first - second)
            }
            else if(s == '*')
            {
              var second = stack.pop()
              var first = stack.pop()
              stack.push(first * second)
            }
            else if(s == '/')
            {
              var second = stack.pop()
              var first = stack.pop()
              stack.push(first / second)
            }
          }
          else if(i.vtype == NUMBER)
              stack.push(i.value.asInstanceOf[Double])
        }
    stack.pop()
  }
}