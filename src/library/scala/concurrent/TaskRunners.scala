// $Id$

package scala.concurrent

trait TaskRunners {
  
  type Task = () => Unit
  type TaskRunner <: AbstractTaskRunner
  
  def createDefaultTaskRunner(): TaskRunner
  implicit val mainTaskRunner: TaskRunner = createDefaultTaskRunner()
  
}