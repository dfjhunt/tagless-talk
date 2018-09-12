package Addl

class One {
  println("Boolean Expressions")
  
  trait BExpr[T]{
    def const(i:Boolean):T
    def or(e1:T, e2:T):T
  }
 
  def program3[T](implicit B:BExpr[T]) = 
    B.or(B.const(true), B.const(false))
  
  implicit val evalB = new BExpr[Boolean]{
    def const(i:Boolean) = i
    def or(e1:Boolean, e2:Boolean) = e1||e2
  }
  
  implicit val printB = new  BExpr[String]{
    def const(b:Boolean) = s"$b"
    def or(e1:String, e2:String) = s"($e1 OR $e2)"
  }
  
  println(program3[Boolean])
  println(program3[String])
  println
}