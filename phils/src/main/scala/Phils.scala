// Dining Philosophers

import io.threadcso._
import scala.language.postfixOps


object Phils
{
  // Philosophers' actions 
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec) 
  def Pause = sleep((200+random.nextInt(300))*milliSec)
 
//  val report = ManyOneBuf[String](20, "report")
  val report = N2NBuf[String](size=20, writers=0, readers=1, "report")
  val bultler = CountingSemaphore(available=N-1)

  // A single philosopher
  def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    repeat {
      report!(s"$me sits")
      Think
      left!Pick(me);  report!(me+" picks up left fork");  Pause
      right!Pick(me); report!(me+" picks up right fork"); Pause
      report ! (me+" eats"); Eat
      left!Drop(me);  report!(me+" drops left fork"); Pause
      right!Drop(me); report!(me+" drops right fork"); Pause
      report!(s"$me gets up"); Pause
    }
  }

  // A right-handed philosopher
  def PhilRightHand(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    repeat {
      report!(s"$me sits")
      Think
      right!Pick(me); report!(me+" picks up right fork"); Pause
      left!Pick(me);  report!(me+" picks up left fork");  Pause
      report ! (me+" eats"); Eat
      right!Drop(me); report!(me+" drops right fork"); Pause
      left!Drop(me);  report!(me+" drops left fork"); Pause
      report!(s"$me gets up"); Pause
    }
  }

  // A single philosopher that interacts with bultler
  def PhilwithBultler(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    repeat {
      bultler.acquire()
      report!(s"$me sits")
      Think
      left!Pick(me);  report!(me+" picks up left fork");  Pause
      right!Pick(me); report!(me+" picks up right fork"); Pause
      report ! (me+" eats"); Eat
      left!Drop(me);  report!(me+" drops left fork"); Pause
      right!Drop(me); report!(me+" drops right fork"); Pause
      bultler.release()
      report!(s"$me gets up"); Pause
    }
  }

  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork ${me} is with philosopher $owner" 
        register
    }
    serve
    {(
        left  =?=> { case Pick(x) => owner=s"$x"; left  ? { case Drop(y) => assert(y==x); owner="nobody"} }
     |  right =?=> { case Pick(x) => owner=s"$x"; right ? { case Drop(y) => assert(y==x); owner="nobody"} }
    )}
    println(s"FORK $me DIED: ${state}")
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork  = 
      for (i<-0 until N) yield 
          OneOne[Action]  (s"Phil($i) to Fork($i)")
  val philToRightFork = 
      for (i<-0 until N) yield 
          OneOne[Action]  (s"Phil($i) to Fork(${(N+i-1)%N})")
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)


  // Put the components together
  val AllPhils: PROC = || (
  for (i <- 0 until N) yield
    Phil( i, philToLeftFork(i), philToRightFork(i) )
)
  // Variant 1: a right-handed philosopher
  val AllPhilswOneRightHand: PROC = || (
    for (i <- 0 until N) yield
      if (i != N-1) Phil( i, philToLeftFork(i), philToRightFork(i) )
      else PhilRightHand( i, philToLeftFork(i), philToRightFork(i) )
  )

  // Variant 2: phils with bultler
  val AllPhilswithBultler: PROC = || (
    for (i <- 0 until N) yield
      PhilwithBultler( i, philToLeftFork(i), philToRightFork(i) )
  )

  val AllForks: PROC = || ( 
    for (i <- 0 until N) yield 
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) ) 
  )

  // Original system
  val System0: PROC = AllPhils || AllForks || component.console(report)

  // Variant 1: a right-handed philosopher
  val System1: PROC = AllPhilswOneRightHand || AllForks || component.console(report)

  // Variant 2: using a bultler
  val System2: PROC = AllPhilswithBultler || AllForks || component.console(report)


  // And run it
  def main(args : Array[String]) = 
  { println(debugger)
    System2()
  } 
}


object PhilswithBultler{
  // Philosophers' actions
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec)
  def Pause = sleep((200+random.nextInt(300))*milliSec)

  val report = N2NBuf[String](size=20, writers=0, readers=1, "report")

  def bultler (come : ?[Int], leave : ?[Int]) = proc {
    var served = 0
    serve ( (served <4 && come) =?=> { _=> served += 1}
      | (leave) =?=> { _=> served -= 1 }
    )
  }

//  // Bultler allows only 4 phils to be seated
//  class Bultler {
//    val monitor = new Monitor
//    val notFull = monitor.newCondition
//    var served = 0
//    val maximum = N - 1
//
//    def come = monitor withLock {
//      while (served == maximum) notFull.await()
//      served +=1
//    }
//
//    def leave = monitor withLock{
//      served -=1
//      notFull.signal()
//    }
//  }
//  val bultler = new Bultler()

  // A single philosopher
  def Phil(me: Int, left: ![Action], right: ![Action], come : ![Int], leave : ![Int]) = proc("Phil"+me) {
    repeat {
      report!(me+ " requests bulter to sit")
      come !(me); report!(me+ " requests bulter to sit")
      report!(s"$me sits")
      Think
      left!Pick(me);  report!(me+" picks up left fork");  Pause
      right!Pick(me); report!(me+" picks up right fork"); Pause
      report ! (me+" eats"); Eat
      left!Drop(me);  report!(me+" drops left fork"); Pause
      right!Drop(me); report!(me+" drops right fork"); Pause
      leave !(me); report!(me+ " informs bultler to leave")
      report!(s"$me gets up"); Pause
    }
  }


  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action] ) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork ${me} is with philosopher $owner"
      register
    }
    serve
    {(
      left  =?=> { case Pick(x) => owner=s"$x"; left  ? { case Drop(y) => assert(y==x); owner="nobody"} }
        |  right =?=> { case Pick(x) => owner=s"$x"; right ? { case Drop(y) => assert(y==x); owner="nobody"} }
      )}
    println(s"FORK $me DIED: ${state}")
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork  =
    for (i<-0 until N) yield
      OneOne[Action]  (s"Phil($i) to Fork($i)")
  val philToRightFork =
    for (i<-0 until N) yield
      OneOne[Action]  (s"Phil($i) to Fork(${(N+i-1)%N})")
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)

  // Put the components together
  val come = ManyOne[Int]()
  val leave = ManyOne[Int]()
  val runningBultler =  bultler(come,leave)
  val AllPhils: PROC = || (
    for (i <- 0 until N) yield
      Phil( i, philToLeftFork(i), philToRightFork(i), come, leave)
  )


  val AllForks: PROC = || (
    for (i <- 0 until N) yield
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) )
  )

  val System: PROC = AllPhils || AllForks || runningBultler|| component.console(report)

  // And run it
  def main(args : Array[String]) =
  { println(debugger)
    System()
  }
}


object PhilswithCommunications
{
  // Philosophers' actions
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action
  case class Commited(response:Boolean) extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec)
  def Pause = sleep((200+random.nextInt(300))*milliSec)

  val report = N2NBuf[String](size=20, writers=0, readers=1, "report")


  // Bi-directional channel. Phil could write to folk and folk could reply to phil.
  class biChannel[T](pi: Int, fi:Int) {
    val ptf = OneOne[T](s"Phil($pi) to Fork($fi)")
    val ftp = OneOne[T](s"Fork($fi) to Phil($pi)")
  }


  // A single philosopher
  def Phil(me: Int, left: biChannel[Action], right: biChannel[Action]) = proc("Phil"+me) {
    repeat {
      report!(s"$me sits")
      Think
      // phil asks left folk to commit
      left.ptf ! Pick(me);
      left.ftp ? match{
        case Commited(false) =>
          report ! (me + " cannot pick up left fork")
          Pause

        // left fork commited to phil
        case Commited(true) =>
          report!(me+" picks up left fork");  Pause
          // phil asks right fork to commit
          right.ptf ! Pick(me)
          report ! (me + " tries to pick up right fork")

          right.ftp ? match{
            // if right fork won't commit, drop left fork
            case Commited(false) =>
              report ! (me + " cannot to pick up right fork. Dropping left fork ")
              left.ptf ! Drop(me)
            // right fork commited, phil eats
            case Commited(true) =>
              report!(me+" picks up right fork");Pause
              report ! (me+" eats"); Eat
              left.ptf!Drop(me);  report!(me+" drops left fork"); Pause
              right.ptf!Drop(me); report!(me+" drops right fork"); Pause
              report!(s"$me gets up"); Pause
          }
      }

    }
  }

  // A single fork
  def Fork(me: Int, left: biChannel[Action], right: biChannel[Action]) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork $me is with philosopher $owner"
      register
    }
    serve
    {
      left.ptf  =?=> {
        case Pick(x) =>
          owner.synchronized {
            if (owner == "Nobody") {
              owner = s"$x";
              left.ftp ! Commited(true)
            } else {
              left.ftp ! Commited(false)
            }
          }
        case Drop(y) =>
          assert(s"$y" == owner)
          owner = "Nobody"
      }|
        right.ptf  =?=> {
          case Pick(x) =>
            owner.synchronized {
              if (owner == "Nobody") {
                owner = s"$x";
                right.ftp ! Commited(true)
              } else {
                right.ftp ! Commited(false)
              }
            }
          case Drop(y) =>
            assert(s"$y" == owner)
            owner = "Nobody"}
     }

    println(s"FORK $me DIED: ${state}")
  }


  // Channels to pick up and drop the forks:
  val philToLeftFork  =
    for (i<-0 until N) yield
      new biChannel[Action](i,i)
  val philToRightFork =
    for (i<-0 until N) yield
      new biChannel[Action](i,(N+i-1)%N)
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)

  // Put the components together
  val AllPhils: PROC = || (
    for (i <- 0 until N) yield
      Phil( i, philToLeftFork(i), philToRightFork(i) )
  )

  val AllForks: PROC = || (
    for (i <- 0 until N) yield
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) )
  )

  val System: PROC = AllPhils || AllForks || component.console(report)

  // And run it
  def main(args : Array[String]) =
  { println(debugger)
    System()
  }
}

object PhilswithTimeouts
{
  // Philosophers' actions
  abstract class Action {}
  case class Pick(who: Int)  extends Action
  case class Drop(who: Int)  extends Action

  val N = 5 // Number of philosophers

  val random = new scala.util.Random
  val timeOut = 3000*milliSec
  val mealsCount = 4000*milliSec

  // Simulate basic actions
  def Eat   = sleep(500*milliSec)
  def Think = sleep((200+random.nextInt(300))*milliSec)
  def Pause = sleep((200+random.nextInt(300))*milliSec)


  val report = N2NBuf[String](size=20, writers=0, readers=1, "report")

  // A single philosopher
  def Phil(me: Int, left: ![Action], right: ![Action]) = proc("Phil"+me) {
    var meals = 0
    var clock = nanoTime
    repeat {
      if (nanoTime > clock + mealsCount) {
        report ! (s"$me has eaten $meals times")
        clock = nanoTime
      }
//      report!(s"$me sits")
      Think
      left!Pick(me);
//       report!(me+" picks up left fork");
      Pause
      if (right.writeBefore(timeOut)(Pick(me))){
//        report ! (me+" eats");
        Eat; meals +=1
        left!Drop(me);
//        report!(me+" drops left fork");
        Pause
        right!Drop(me);
//        report!(me+" drops right fork");
        Pause
//        report!(s"$me gets up"); Pause
      }
      else{
        left!Drop(me)
//        report !(s"$me giving up on left fork after timeout")
        Pause
      }
    }
  }


  // A single fork
  def Fork(me: Int, left: ?[Action], right: ?[Action]) = proc("Fork"+me) {
    var owner: String="Nobody"
    val state = new Debuggable
    {   override def toString = s"Fork ${me} is with philosopher $owner"
      register
    }
    serve
    {(
      left  =?=> { case Pick(x) => owner=s"$x"; left  ? { case Drop(y) => assert(y==x); owner="nobody"} }
        |  right =?=> { case Pick(x) => owner=s"$x"; right ? { case Drop(y) => assert(y==x); owner="nobody"} }
      )}
    println(s"FORK $me DIED: ${state}")
  }

  // Channels to pick up and drop the forks:
  val philToLeftFork  =
    for (i<-0 until N) yield
      OneOne[Action]  (s"Phil($i) to Fork($i)")
  val philToRightFork =
    for (i<-0 until N) yield
      OneOne[Action]  (s"Phil($i) to Fork(${(N+i-1)%N})")
  // philToLeftFork(i)  is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)


  // Put the components together
  val AllPhils: PROC = || (
    for (i <- 0 until N) yield
      Phil( i, philToLeftFork(i), philToRightFork(i) )
  )

  val AllForks: PROC = || (
    for (i <- 0 until N) yield
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) )
  )

  val System: PROC = AllPhils || AllForks || component.console(report)

  // And run it
  def main(args : Array[String]) =
  { println(debugger)
    System()
  }
}