import scala.util.Random


object HelloWorld {
  def main(args: Array[String]){
    val scanner = new java.util.Scanner(System.in)
    print("Enter array size: ")
    var list:Array[Int] = new Array[Int](scanner.nextInt())
    println("Populating...")
    fill_array(list)
    println("Sorting...")
    //print_array(list)
    quickSort(list)
    println("Done..")
  //  println()
   // print_array(list)
  }
  
  def quickSort(arr:Array[Int]){
    if(arr.length == 1) return
    quick_sort(arr,0, arr.length - 1)
  }
  
  def quick_sort(arr:Array[Int], low: Int, high: Int){
    if(high < low) return
    var pivotpoint = partition(arr,low,high)
    quick_sort(arr, low, pivotpoint - 1)
    quick_sort(arr, pivotpoint + 1, high)
  }
  
  def partition(arr:Array[Int], low: Int, high: Int):Int = {
    var j:Int =  low
    for(i <- low+1 to high)
      if(arr(i) < arr(low)){
        j+=1
        var temp:Int = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      }
    var temp:Int = arr(low)
    arr(low) = arr(j)
    arr(j) = temp    
    j
  }
  
  def fill_array(arr:Array[Int]){
    val rand = new Random()
    for(i <- 0 to arr.length - 1)
        arr(i) = rand.nextInt(arr.length) + 1
  }
  
  def print_array(arr:Array[Int]){
    for(i <- 0 to arr.length - 1)
      print(arr(i) + ", ")
  }
}