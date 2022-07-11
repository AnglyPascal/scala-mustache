package Mustache 

/**
 * view helper trait 
 **/
trait MustacheHelperSupport {
  private val contextLocal = new java.lang.ThreadLocal[Any]()
  private val renderLocal  = new java.lang.ThreadLocal[Function1[String,String]]()

  protected def context:Any = contextLocal.get
  protected def render(template:String):Any = (renderLocal.get())(template)

  def withContextAndRenderFn[A](context:Any, render:(String)=>String)(fn: => A): A = {
    contextLocal.set(context)
    renderLocal.set(render)
    try { fn }
    finally { 
      contextLocal.set(null)
      renderLocal.set(null)
    }
  }
}
