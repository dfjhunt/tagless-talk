package done

class Talk3 {

  
  
  
  //Added negation as a term
  trait IExp[T[_]] {
    def const(i: Int): T[Int]
    def add(e1: T[Int], e2: T[Int]): T[Int]
    def neg(e1: T[Int]): T[Int]
  }

  def program[T[_]](implicit I: IExp[T]) =
    I.neg(I.add(I.const(2), I.neg(I.const(3))))//-(2 + -3)
    
  type Str[A]=String
  
  implicit val printI = new  IExp[Str]{
    def const(i:Int)=s"$i"
    def add(e1:String, e2:String)=s"($e1 + $e2)"
    def neg(e1:String) = s"-$e1"
  }
      
  println(program[Str])
  
    
  
  
  
  //We want to push all negation terms down to the consts, so convert our
  //program from -(2 + -3) to (-2 + 3).  But in tagless final the terms are
  //all compositional, the interpreting for the term knows nothing about
  //the parent or children in the structure.  We need some way to push that
  //contextual information from the parents to the children.  The first step
  //is to make the context explicit.
  
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx
  
  
  //In order to push the context down we're going to interpret our program
  //into a function that takes a Ctx and returns a T[X].  That way every
  //term is able to receive context from its parent and pass it to its
  //children.
  
  //The return type we want is Ctx->T[X] but tagless final needs a type
  //that is a type constructor so we need to leave a hole in the function 
  //type.  In some situations we could just do this
  //
  //type f[x] = Function1[Ctx, T[x]]
  //
  //In this case, we don't know what T is yet, it isn't in this scope, it 
  //will be in our interpreters scope so we'll have to use a type lambda.
  //This could be made cleaner with kind-projector.
  
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
  
  
  //Create the converting interpreter for the Str typeclass
  val pushNPrint = new PushNeg[Str]
  
  //Interpret the program
  val programPush:Ctx=>String = 
    program[({type f[x] = (Ctx => Str[x])})#f ](pushNPrint)
    
  //The outermost context is set to positive
  println(programPush(Pos))
  
}








object Talk3 {
  def main(args:Array[String]){
    new Talk3
  }
}