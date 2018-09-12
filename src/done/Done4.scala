package done


class Talk4 {

  trait IExp[T[_]] {
    def const(i: Int): T[Int]
    def add(e1: T[Int], e2: T[Int]): T[Int]
    def neg(e1: T[Int]): T[Int]
  }

  def program[T[_]](implicit I: IExp[T]) =
    I.neg(I.add(I.const(2), I.neg(I.const(3))))

  type Str[A] = String

  implicit val printI = new  IExp[Str]{
    def const(i:Int)=s"$i"
    def add(e1:String, e2:String)=s"($e1 + $e2)"
    def neg(e1:String) = s"-$e1"
  }

  type Id[A] = A

  implicit val runI = new IExp[Id] {
    def const(i: Int) = i
    def add(e1: Int, e2: Int) = e1 + e2
    def neg(e1: Int) = -1 * e1
  }

  println(program[Str])
  println(program[Id])
  println
  
  
  
  
  
  
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx

  
  
  
  
  
  
  trait >>>[F[_[_]], G[_[_]]] {
    def apply[T[_]](implicit f: F[T]): G[T]
  }

  class PushNeg
    extends IExp[({ type f[x] = (Ctx => IExp >>> ({ type g[y[_]] = Id[y[x]] })#g) })#f] {

    type idInt[t[_]] = Id[t[Int]]
    type returnType = IExp >>> idInt
    type expType = Ctx => returnType

    def const(i: Int) = ctx => new (returnType) {
      def apply[T[_]](implicit I: IExp[T]) = ctx match {
        case Pos => I.const(i)
        case Neg => I.neg(I.const(i))
      }
    }

    def add(e1: expType, e2: expType) = ctx => new (returnType) {
      def apply[T[_]](implicit I: IExp[T]) =
        I.add(e1(ctx)(I), e2(ctx)(I))
    }

    def neg(e1: expType) = ctx => new (returnType) {
      def apply[T[_]](implicit I: IExp[T]) = ctx match {
        case Pos => e1(Neg)(I)
        case Neg => e1(Pos)(I)
      }
    }
  }

  val programPushed =
    program[({ type f[x] = 
      (Ctx => IExp >>> ({ type g[y[_]] = Id[y[x]] })#g) })#f](new PushNeg)(Pos)

  println(programPushed[Str]) 
  println(programPushed[Id])
}




















object Talk4 {
  def main(args: Array[String]) {
    new Talk4
  }
}