package done

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import cats.Id
import cats.Monad
import cats.catsInstancesForId
import cats.implicits.toFlatMapOps
import cats.implicits.toFunctorOps

class Talk2 {

  case class User(name: String, points: Int)

  
  
  
  trait UserAccountManager[T[_]] {
    def addUser(user: User): T[Unit]
    def getUser(name: String): T[Option[User]]
    def updateUser(user: User): T[Try[Unit]]
  }

  
  
  
  
  def incrementPoints[T[_]](username: String, points: Int)
    (implicit U: UserAccountManager[T], M: Monad[T]): T[Try[Unit]] =
    U.getUser(username).flatMap(_ match {
      case Some(user) => U.updateUser(User(username, user.points + points))
      case None       => M.pure(Failure(new Exception("No such user")))
    })

    
    
    
  def demo[T[_]](implicit U: UserAccountManager[T], M: Monad[T]) =
    for {
      _ <- U.addUser(User("Dan", 10))
      _ <- incrementPoints("Dan", 3)
      user <- U.getUser("Dan")
    } yield (user)

    
    
    
  implicit val testUserManager = new UserAccountManager[Id] {
    var users = scala.collection.mutable.Map[String, User]()

    def addUser(user: User) = users += (user.name -> user)

    def getUser(name: String) = users.get(name)

    def updateUser(user: User) =
      users.get(user.name) match {
        case Some(u) => Success(users(u.name)= user)
        case None    => Failure(new Exception("No such user"))
      }
  }
    
    

  println(demo[Id])
}











object Talk2 {
  def main(args: Array[String]) {
    new Talk2
  }
}