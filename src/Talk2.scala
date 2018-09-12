import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.concurrent._
import scala.concurrent.duration._

import cats.Id
import cats.instances.future._
import cats.Monad
import cats.catsInstancesForId
import cats.implicits.toFlatMapOps
import cats.implicits.toFunctorOps
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class Talk2 {

  
  
  
  case class User(name:String, points:Int)
  
  
  
  
  trait UserAccountManager{
    def addUser(user: User): Unit
    def getUser(name: String): Option[User]
    def updateUser(user: User): Try[Unit]
  }
  
  
  
  
  

  
  
  
  
  
  
  
//  def incrementPoints[T[_]](username: String, points: Int)
//    (implicit U: UserAccountManager[T], M: Monad[T]): T[Try[Unit]] =
//    U.getUser(username).flatMap(_ match {
//      case Some(user) => U.updateUser(User(username, user.points + points))
//      case None       => M.pure(Failure(new Exception("No such user")))
//    })
//
//    
//    
//    
//  def demo[T[_]](implicit U: UserAccountManager[T], M: Monad[T]) =
//    for {
//      _ <- U.addUser(User("Dan", 10))
//      _ <- incrementPoints("Dan", 3)
//      user <- U.getUser("Dan")
//    } yield (user)
//    
//   
//    
//    
//  implicit val asyncUserMgr = new UserAccountManager[Future] {
//    var users = scala.collection.mutable.Map[String, User]()
//
//    def addUser(user: User) = 
//      Future.successful(users += (user.name -> user))
//
//    def getUser(name: String) = Future.successful(users.get(name))
//
//    def updateUser(user: User) =
//      users.get(user.name) match {
//        case Some(u) => Future.successful(Success(users(u.name)= user))
//        case None    => Future.successful(Failure(new Exception("No such user")))
//      }
//  }
//    
//  implicit val testUserManager = new UserAccountManager[Id] {
//    var users = scala.collection.mutable.Map[String, User]()
//
//    def addUser(user: User) = users += (user.name -> user)
//
//    def getUser(name: String) = users.get(name)
//
//    def updateUser(user: User) =
//      users.get(user.name) match {
//        case Some(u) => Success(users(u.name)= user)
//        case None    => Failure(new Exception("No such user"))
//      }
//  }
//    
//
//  
//    
//  println(demo[Id])
//  println(Await.result(demo[Future], 10 seconds))
  
}









  // References
  //
  // Finally Tagless, Partially Evaluated:
  // Tagless Staged Interpreters for Simpler Typed Languages
  // http://okmij.org/ftp/tagless-final/


  // Me:         Dan Hunt
  // Email:      dfjhunt@gmail.com
  // LinkedIn:   linkedin.com/in/daniel-hunt-4825395a


















object Talk2 {
  def main(args: Array[String]) {
    new Talk2
  }
}