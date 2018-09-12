package done

class Talk3 {

  
  
  
  
  trait IExp[T[_]] {
    def const(i: Int): T[Int]
    def add(e1: T[Int], e2: T[Int]): T[Int]
    def neg(e1: T[Int]): T[Int]
  }

  def program[T[_]](implicit I: IExp[T]) =
    I.neg(I.add(I.const(2), I.neg(I.const(3))))
    
  type Str[A]=String
  
  implicit val printI = new  IExp[Str]{
    def const(i:Int)=s"$i"
    def add(e1:String, e2:String)=s"($e1 + $e2)"
    def neg(e1:String) = s"-$e1"
  }
      
  println(program[Str])
  
    
  
  
  
  
  
  
  
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx
  
  
  class PushNeg[T[_]](implicit I:IExp[T]) 
    extends IExp[({type f[x] = (Ctx => T[x])})#f ] {
    
    def const(i:Int) = ctx => ctx match {
      case Pos => I.const(i)
      case Neg => I.neg(I.const(i))
    }
    
    def add(e1:Ctx=>T[Int], e2:Ctx=>T[Int]) = ctx => 
     I.add(e1(ctx), e2(ctx))
    
    def neg(e1:Ctx=>T[Int]) = ctx => ctx match {
      case Pos => e1(Neg)
      case Neg => e1(Pos)
    }  
  }
  
  
  
  val pushNPrint = new PushNeg[Str]
  
  val programPush:Ctx=>String = 
    program[({type f[x] = (Ctx => Str[x])})#f ](pushNPrint)
    
  //println(programPush(Pos))
  
}








object Talk3 {
  def main(args:Array[String]){
    new Talk3
  }
}