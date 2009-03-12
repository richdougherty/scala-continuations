// $Id$

package scala.concurrent

import java.util.concurrent.atomic._
import jsr166y._

trait FJTaskRunners extends TaskRunners {
  
  type TaskRunner = FJTaskRunner
  def numWorkers: Int = java.lang.Runtime.getRuntime().availableProcessors()
  
  class FJTaskRunner(n: Int) extends AbstractTaskRunner {

    val pool = new ForkJoinPool(n)
    
    def submitTask(f:Task) {
      FJTaskWrapper.runOnCurrentThreadOrPool(new RecursiveAction[Unit] {
        def compute() = try {
          f()
        } catch { case e => e.printStackTrace() }
        // TODO: exception handling
      }, pool)
    }

    def waitUntilFinished() {
//      Thread.sleep(24*60*60*1000)
      pool.awaitTermination(Math.MAX_LONG,java.util.concurrent.TimeUnit.SECONDS)
      // FIXME: doesn't seem to work (?)
    }

  }
  
  def createDefaultTaskRunner() = new FJTaskRunner(numWorkers)

}