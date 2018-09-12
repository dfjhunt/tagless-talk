package done

class Done {

  
  
  // Tagless Final
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
  
  def runRazor(h:HRazor):Int = h match{
    case Const(i) => i
    case Add(e1:HRazor, e2:HRazor) => runRazor(e1)+runRazor(e2)
  }
  
  def printRazor(h:HRazor):String = h match{
    case Const(i) => s"$i"
    case Add(e1:HRazor, e2:HRazor) => 
      s"(${printRazor(e1)} +  ${printRazor(e2)})"
  }
  
  println(runRazor(program))
  println(printRazor(program))
  println
  
  
  
  
  
  
  
  
  
  
  // The expression problem is a new name for an old problem.
  // The goal is to define a datatype by cases, where one can
  // add new cases to the datatype and new functions over the
  // datatype, without recompiling existing code, and while
  // retaining static type safety (e.g., no casts).
  //                        - Philip Wadler, 12 November 1998

  println("Final Encoding")
  
  trait IExpr[T[_]] {
    def const(i:Int):T[Int]
    def add(e1:T[Int], e2:T[Int]):T[Int]
  }
  
  def program2[T[_]](implicit I:IExpr[T])=I.add(I.const(2), I.const(3))
  
  type Id[A] = A
  
  implicit val runI = new IExpr[Id]{
    def const(i:Int) = i
    def add(e1:Int, e2:Int)=e1+e2
  }
  
  type Str[A] = String
  
  implicit val printI = new IExpr[Str]{
    def const(i:Int) = s"$i"
    def add(e1:String, e2:String)=s"($e1 + $e2)"
  }
  
  println(program2[Id])
  println(program2[Str])
  println
  
  
  
  println("With Multiplication")
  
  trait MExpr[T[_]]{
    def mult(e1:T[Int], e2:T[Int]):T[Int]
  }
  
  def program3[T[_]](implicit I:IExpr[T], M:MExpr[T])=
    M.mult(I.const(2), I.const(3))
    
  implicit val runM = new MExpr[Id]{
    def mult(e1:Int, e2:Int) = e1*e2
  }
  
  implicit val printM = new MExpr[Str]{
    def mult(e1:String, e2:String) = s"($e1 * $e2)"
  }
  
  println(program3[Id])
  println(program3[Str])
  println
  
  
  
  println("Boolean Expression")
  
  trait BExpr[T[_]]{
    def const(i:Boolean):T[Boolean]
    def or(e1:T[Boolean], e2:T[Boolean]):T[Boolean]
  }
 
  def program4[T[_]](implicit B:BExpr[T]) = B.or(B.const(true), B.const(false))
  
  implicit val runB = new BExpr[Id]{
    def const(i:Boolean) = i
    def or(e1:Boolean, e2:Boolean) = e1||e2
  }
  
  implicit val printB = new  BExpr[Str]{
    def const(b:Boolean) = s"$b"
    def or(e1:String, e2:String) = s"($e1 OR $e2)"
  }
  
  println(program4[Id])
  println(program4[Str])
  println
  
  
  
  println("Mixed Type Expression")
  
  trait XExpr[T[_]] {
    def gt(e1:T[Int], e2:T[Int]):T[Boolean]
    def ifElse[A](p:T[Boolean], e1:T[A], e2:T[A]):T[A]
  }
  
  implicit val runX = new XExpr[Id]{
    def gt(e1:Int, e2:Int) = e1>e2
    def ifElse[A](p:Boolean, e1:A, e2:A) = if(p) e1 else e2
  }
  
  implicit val printX = new XExpr[Str]{
    def gt(e1:String, e2:String) = s"$e1 > $e2"
    def ifElse[A](p:String, e1:String, e2:String) = 
      s"if($p) $e1 else $e2"
  }
  
  def max[T[_]](a:Int, b:Int)(implicit I:IExpr[T], X:XExpr[T]) = 
    X.ifElse(X.gt(I.const(a), I.const(b)), I.const(a), I.const(b))
    
 println(max[Id](2, 3))
 println(max[Str](2, 3))
 println
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
object Done {
  def main(args: Array[String]) {
    new Done
  }
}
