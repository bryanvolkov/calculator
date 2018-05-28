import scala.util.control.Breaks._

// The only function of this class is to accept or reject a mathematical expression
// that has any of the following characters:
// 0 1 3 4 5 6 7 8 9
// + - * /
// ( )
// .

class Automaton {
  private val q0 = 0
  private val q1 = 1
  private val q2 = 2
  private val q3 = 3
  private val q4 = 4
  private val q5 = 5
  private val q6 = 6
  private val q7 = 7
  private val q8 = 8
  
  private val check_state: Array[(Char) => Boolean] = Array(check_q0, check_q1, check_q2, check_q3, check_q4, check_q5, check_q6, check_q7, check_q8)
  
  private var stk:Stack[Char] = new Stack[Char]
  
  private def is_number(c:Char):Boolean = {
     '0' <= c && c <= '9'
  }
  
  private def is_operator(c:Char):Boolean = {
    c == '-' || c == '+' || c == '/' || c == '*'
  }
  
  private var state = q0
  
  def check_string(str:String):Boolean = {
    state = q0
    stk.push('$')
    var accept:Boolean = true
    breakable{
    for(i <- 0 until str.length)
      if(!check_state(state)(str.charAt(i))){
        accept = false
        break}
    }
    if(accept) accept = state == q2 || state == q4 || state == q8
    stk.empty_except(1)
    accept
  }
  
  private def check_q0(c:Char):Boolean = {
     if (is_number(c)) state = q2
     else if(c == '.') state = q3
     else if(c == '-') state = q1
     else if(c == '('){
        stk.push(c)
        state = q7}
    else return false
    true
   }
   
   private def check_q1(c:Char): Boolean = {
     if(is_number(c)) state = q2
    else if(c == '.') state = q3
    else if(c == '('){
        stk.push(c)
        state = q6}
    else return false
    true
    }
   
   private def check_q2(c:Char): Boolean = {
    if(is_number(c)) state = q2
    else if(c == '.') state = q3
    else if(is_operator(c)) state = q5
    else if(c == ')'){
        if(stk.pop()!='(') return false
        state = q8}
    else return false
    true
   }
   
    private def check_q3(c:Char): Boolean = {
      if(is_number(c)) state = q4
      else return false
      true
    }
      
     private def check_q4(c:Char): Boolean = {
    if(is_number(c)) state = q4
    else if(is_operator(c)) state = q5
    else if(c == ')'){
        if(stk.pop()!='(') return false
        state = q8}
    else return false
    true
    }
     
    private def check_q5(c:Char): Boolean = {
    if(is_number(c)) state = q2
    else if(c == '.') state = q3
    else if(c == '-') state = q1
    else if(c == '('){
        stk.push(c)
        state = q7}
    else return false
    true
  }
    
   private def check_q6(c:Char): Boolean = {
    if(is_number(c)) state = q2
    else if(c == '.') state = q3
    else if(c == '-') state = q1
    else if(c == '('){
        stk.push(c)
        state = q6}
    else return false
    true
    }
   
    private def check_q7(c:Char): Boolean = {
    if(is_number(c)) state = q2
    else if(c == '.') state = q3
    else if(c == '-') state = q1
    else if(c == '('){
        stk.push(c)
        state = q7}
    else return false
    true 
    }
    
   private def check_q8(c:Char): Boolean = {
    if(is_operator(c)) state = q5
    else if(c == ')'){
        if(stk.pop()!='(') return false
        state = q8}
    else if(c == '.') return false // BUG FIXED: expressions with dot after parenthesis is now rejected i.e. "(3)."
    true
    }
}