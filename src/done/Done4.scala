package done

class Talk4 {

  //I didn't love the solution from Done3 because the conversion step
  //also locked in the interpreter.  This meant it created a function
  //from Ctx=>T[X] where T was already determined rather than a new 
  //program of type IExpr[T]=>T where T could still be determined by a 
  //different interpreter.  This solution attempts to do the latter.
  
  trait IExp[T[_]] {
    def const(i: Int): T[Int]
    def add(e1: T[Int], e2: T[Int]): T[Int]
    def neg(e1: T[Int]): T[Int]
  }

  def program[T[_]](implicit I: IExp[T]) =
    I.neg(I.add(I.const(2), I.neg(I.const(3))))

  type Str[A] = String

  implicit val printI = new IExp[Str] {
    def const(i: Int) = s"$i"
    def add(e1: String, e2: String) = s"($e1 + $e2)"
    def neg(e1: String) = s"-$e1"
  }

  type Id[A] = A

  implicit val runI = new IExp[Id] {
    def const(i: Int) = i
    def add(e1: Int, e2: Int) = e1 + e2
    def neg(e1: Int) = -1 * e1
  }

  println("Original")
  println(program[Str])
  println(program[Id])
  println

  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx

  //I stole some ideas from Shapeless here, we want to return a function
  //IExp[T] -> T  but we want T to be generic.  Since functions can't 
  //have generics, only methods, we create a class here.
  
  trait >>>[F[X[_]], G[X[_]]] {
    def apply[T[_]](implicit f: F[T]): G[T]
  }

 
  //So the desired type of the interpreter is:
  //
  //Ctx => IExp[T] => T
  //
  //I had to use a type lambda nested in a type lambda here and it is 
  //a bit of a mess.  I've tried a bunch of other ideas but so far none 
  //of them have both type checked and allowed T to stay generic.  I'm 
  //not sure this is completely type-safe any more.
  
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

  //This will interpret the original program into the function that
  //takes context to produce a new tagless final program.  It then 
  //applies the context.
  val programPushed =
    program[({ type f[x] = 
      (Ctx => IExp >>> ({ type g[y[_]] = Id[y[x]] })#g) })#f](new PushNeg)(Pos)

  //This shows that we can still interpret the converted program 
  //multiple ways.
  println("Restructured")
  println(programPushed[Str])
  println(programPushed[Id])
}

object Talk4 {
  def main(args: Array[String]) {
    new Talk4
  }
}