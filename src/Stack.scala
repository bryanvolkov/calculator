// This is a generic Stack class

class Stack[T]{
  var top:Node[T] = null
  var length:Int = 0
  
  def push(data:T){
    if(top == null){
      top = new Node[T](data)
    }
    else{
      var temp = new Node[T](data)
      temp.next = top
      top = temp
    }
    length+=1
  }
  
  def isEmpty():Boolean = {
    if(length == 0) true
    else false
  }
  
  def pop():T = {
    var res = top._data
    if(top != null){
      var temp:Node[T] = top
      top = top.next
      temp = null
      length-=1
    }
    res
  }
  
  def peek():T = {
    top._data
  }
  
  def empty_except(i:Int){
    while(length != i){
      var temp:Node[T] = top
      top = top.next
      temp = null
      length-=1
    }
  }
}