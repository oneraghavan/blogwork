package firestar.live.metrics

import java.util.Calendar

import com.sun.javafx.runtime.async.AsyncOperation
import org.mockito.Mockito.{spy, verify}
import org.mockito.{InOrder, Mockito}

import scala.collection.mutable


trait AsyncOperation {

  var timeToExpire: Int

  def expire: Unit

}

class DataBaseOperation(expireTime: Int) extends AsyncOperation {

  override var timeToExpire: Int = expireTime

  override def expire: Unit = {}
}

class TimeOutManager {

  private var currentTick: Int = 1

  private var timeWheel: mutable.Map[Int, List[(Int, AsyncOperation)]] = mutable.Map()

  (0 to 59).foreach((num) => {
    timeWheel.put(num, List())
  })

  def addOperation(asyncOperation: AsyncOperation): Unit = {
    val counter = asyncOperation.timeToExpire / 60
    val hashKey = asyncOperation.timeToExpire % 60

    timeWheel(hashKey) = timeWheel(hashKey) ++ List((counter, asyncOperation))
  }

  private def expireOperation: Unit = {

    timeWheel(currentTick) = timeWheel(currentTick).filter(expireTimedOutOperations).map((tuple: ((Int, AsyncOperation))) => {
      var counter = tuple._1
      val operation = tuple._2

      return (counter - 1, operation)
    })

    currentTick = currentTick + 1

  }

  private def expireTimedOutOperations(tuple: ((Int, AsyncOperation))): Boolean = {
    var counter = tuple._1
    val operation = tuple._2

    if (counter == 0) {
      operation.expire
      return false
    }
    true

  }

  def startTimingOut: Unit = {
    val t = new java.util.Timer()
    val task = new java.util.TimerTask {
      def run() = expireOperation
    }
    t.scheduleAtFixedRate(task, 0, 1000L)
  }

}

object SomeApp extends App {

  private val timeOutManager = new TimeOutManager()

  val jobExpiringTimes = List(102, 30, 44, 77, 3, 300)

  val job4 = new DataBaseOperation(3)
  val job1 = new DataBaseOperation(30)
  val job5 = new DataBaseOperation(35)
  val job3 = new DataBaseOperation(77)
  val job2 = new DataBaseOperation(102)


  timeOutManager.addOperation(job1)
  timeOutManager.addOperation(job2)
  timeOutManager.addOperation(job3)
  timeOutManager.addOperation(job4)
  timeOutManager.addOperation(job5)

  timeOutManager.startTimingOut

}


