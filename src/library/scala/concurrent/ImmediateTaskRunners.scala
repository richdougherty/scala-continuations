// $Id$

package scala.concurrent

trait ImmediateTaskRunners extends TaskRunners {
  
  type TaskRunner = ImmediateTaskRunner
  
  class ImmediateTaskRunner extends AbstractSequentialTaskRunner {
    
    def submitTask(f: Task) = f()
    def waitUntilFinished() = {}
    
  }
  
  def createDefaultTaskRunner() = new ImmediateTaskRunner

}