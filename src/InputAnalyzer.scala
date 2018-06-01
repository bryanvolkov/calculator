import scala.util.control.Breaks._

// The only function of this class is to accept or reject a mathematical expression.
// that has any of the following characters:
// 0 1 3 4 5 6 7 8 9
// + - * /
// ( )
// .
// This function is a Push Down Automata

class InputAnalyzer {
  private val q0 = 0 // state 0
  private val q1 = 1 // state 1
  private val q2 = 2 // state 2
  private val q3 = 3 // state 3
  private val q4 = 4 // state 4
  private val q5 = 5 // state 5
  private val q6 = 6 // state 6
  private val q7 = 7 // state 7
  private val q8 = 8 // state 8
  
  // This is an array of functions
  private val check_state: Array[(Char) => Boolean] = Array(check_q0, check_q1, check_q2, check_q3, check_q4, check_q5, check_q6, check_q7, check_q8)
  
  // This stack is used only to make sure that the opening parentheses of the mathematical expression have a matching closing parentheses
  private var stk:Stack[Char] = new Stack[Char]
  
  // this function returns true if the character is a number
  private def is_number(c:Char):Boolean = {
     '0' <= c && c <= '9'
  }
  
  // this function returns true if the character is an operator
  private def is_operator(c:Char):Boolean = {
    c == '-' || c == '+' || c == '/' || c == '*'
  }
  
  private var state = q0 // starting state of the push down automaton is State 0
  
  // This function analyzes a mathematical expression
  def analyze(str:String):Boolean = {
    state = q0// go back to the initial state
    stk.push('$') // end of stack
    var accept:Boolean = true // variable whose content will be returned
    breakable{ // breakable statement to be able to break the enclosed for loop 
    for(i <- 0 until str.length)// while the string has characters to be read
      if(!check_state(state)(str.charAt(i))){ // check the current state
        accept = false // do not accept the string
        break} // break for loop
    }
    // if string is to be accepted, then check if the last state that was checked is a final state
    if(accept) accept = state == q2 || state == q4 || state == q8
    stk.empty_except(1)
    accept // return true if the string was accepted
  }
  
  // check state 0
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
   
  // check state 1
   private def check_q1(c:Char): Boolean = {
     if(is_number(c)) state = q2
    else if(c == '.') state = q3
    else if(c == '('){
        stk.push(c)
        state = q6}
    else return false
    true
    }
   
     // check state 2
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
   
     // check state 3
    private def check_q3(c:Char): Boolean = {
      if(is_number(c)) state = q4
      else return false
      true
    }
      
    //   // check state 4
     private def check_q4(c:Char): Boolean = {
    if(is_number(c)) state = q4
    else if(is_operator(c)) state = q5
    else if(c == ')'){
        if(stk.pop()!='(') return false
        state = q8}
    else return false
    true
    }
     
     
       // check state 5
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
      // check state 6
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
   
     // check state 7
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
    
      // check state 8
   private def check_q8(c:Char): Boolean = {
    if(is_operator(c)) state = q5
    else if(c == ')'){
        if(stk.pop()!='(') return false
        state = q8}
    else if(c == '.') return false // BUG FIXED: expressions with dot after parenthesis is now rejected i.e. "(3)."
    true
    }
}