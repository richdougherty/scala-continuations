package scala.concurrent

import scala.continuations._
import scala.continuations.ControlContext._

object cpsops {
  
  def spawn(ctx: =>(Any @cps[Unit,Any]))(implicit sched: AbstractTaskRunner): Unit = {
    sched.submitTask(() => run(ctx))
  }
  
  
  
  
}