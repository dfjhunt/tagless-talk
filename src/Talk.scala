

class Talk {

  
  
  
  // Flexible DSLs with Tagless Final
  //
  // Dan Hunt

   
  
  
  // References
  //
  // Finally Tagless, Partially Evaluated:
  // Tagless Staged Interpreters for Simpler Typed Languages
  // http://okmij.org/ftp/tagless-final/ 
  
  
  
  
  // Embedded DSL - A DSL that uses terms and values from,
  // and is interpreted and typed checked in, a 
  // (likely more powerful) host language
  
  println("Initial Encoding")
  
  sealed trait HRazor
  case class Const(i:Int) extends HRazor
  case class Add(e1:HRazor, e2:HRazor) extends HRazor
  
  val program = Add(Const(2), Const(3))
  
  def evalH(h:HRazor):Int = h match {
    case Const(i) => i
    case Add(e1, e2) => evalH(e1) + evalH(e2)
  }
  
  def printH(h:HRazor):String = h match {
    case Const(i) => s"$i"
    case Add(e1, e2) => s"(${evalH(e1)} + ${evalH(e2)})"
  }
  
  println(evalH(program))
  println(printH(program))
  println

  
  
  
  
  
  // The expression problem is a new name for an old problem.
  // The goal is to define a datatype by cases, where one can
  // add new cases to the datatype and new functions over the
  // datatype, without recompiling existing code, and while
  // retaining static type safety (e.g., no casts).
  //                        - Philip Wadler, 12 November 1998

  println("Tagless Final Endcoding")
  
//  Types and Kinds
  
//  1:Int
//  true:Boolean
  
//  Proper Types
//  Int : *
//  Boolean : *
//  List[Int] : *
  
//  Type Constructors
//  List : * -> *
//  Map : (*, *) -> *
 
//  Higher Kinded Types
//  IExpr : (*->*) -> *
  
  trait IExpr[T[_]]{
    def const(i:Int):T[Int]
    def add(e1:T[Int], e2:T[Int]):T[Int]
  }
  
  def program[T[_]](implicit I:IExpr[T]) = 
    I.add(I.const(2), I.const(3))
  
  type Id[X] = X
  
  case class ID[X](x:X)
    
  implicit val evalI = new IExpr[ID]{
    def const(i:Int) = ID(i)
    def add(e1:ID[Int], e2:ID[Int]) = ID(e1.x + e2.x)
  }
  
  implicit val evalI2 = new IExpr[Id]{
    def const(i:Int) = i
    def add(e1:Int, e2:Int) = e1+e2
  }
  
  type Str[X] = String
  
  implicit val printI = new IExpr[Str]{
    def const(i:Int) = s"$i"
    def add(e1:String, e2:String) = s"($e1 + $e2)"
  }
  
  println(program[Id])
  println(program[Str])
  println
  /*
  println("With Multiplication")
  
  trait MExpr[T]{
    def mult(e1:T, e2:T):T
  }
  def program2[T](implicit I:IExpr[T], M:MExpr[T]) = 
    M.mult(I.add(I.const(2), I.const(3)), I.const(3))
  
  implicit val evalM = new MExpr[Int]{
    def mult(e1:Int, e2:Int) = e1 * e2
  }
  
  implicit val printM = new MExpr[String]{
    def mult(e1:String, e2:String) = s"($e1 * $e2)"
  }

  println(program2[Int])
  println(program2[String])
  println
  
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
  */
  println("Mixed Type Expressions")
  
//  WTF type are these?
//  trait XExpr[T] {
//    def gt(e1:T, e2:T):T
//    def ifElse(p:T, e1:T, e2:T):T
//  }
  
  trait XExpr[T[_]] {
    def gt(e1:T[Int], e2:T[Int]):T[Boolean]
    def ifElse[A](p:T[Boolean], e1:T[A], e2:T[A]):T[A]
  }
  
  def max[T[_]](a:Int, b:Int)(implicit X:XExpr[T], I:IExpr[T])=
    X.ifElse(X.gt(I.const(a), I.const(b)), I.const(a), I.const(b))
  
  implicit val evalX = new XExpr[Id]{
    def gt(e1:Int, e2:Int) = e1 > e2
    def ifElse[A](p:Boolean, e1:A, e2:A) = if (p) e1 else e2
  }
  
  implicit val printX = new XExpr[Str]{
    def gt(e1:String, e2:String) = s"$e1 > $e2"
    def ifElse[A](p:String, e1:String, e2:String) = s"if ($p) $e1 else $e2"
  }
  
  println(max[Id](2, 3))
  println(max[Str](2, 3))
  println
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
object Talk {
  def main(args: Array[String]) {
    new Talk
  }
}
