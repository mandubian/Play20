package play.api.libs.json

object `package` {

  /**
   * Provided a Reads implicit for its type is available, convert any object into a JsValue
   */
  def toJson[T](o: T)(implicit tjs: Writes[T]): JsValue = tjs.writes(o)

  /**
   * Provided a Writes implicit for that type is available, convert a JsValue to any type
   */
  def fromJson[T](json: JsValue)(implicit fjs: Reads[T]): T = fjs.reads(json)


  def asProduct3[S, T1, T2, T3](f1: String, f2: String, f3: String)(apply : (T1, T2, T3) => S)(unapply : S => Product3[T1, T2, T3])(implicit bin1: Format[T1], bin2: Format[T2], bin3: Format[T3]) = new Format[S]{
	  def writes(s: S) = {
	    val product = unapply(s)
	    JsObject(
	      List(
	        (f1, toJson(product._1)), 
	        (f2, toJson(product._2)), 
	        (f3, toJson(product._3)) 
	    ))
	  }
	  def reads(js: JsValue) = js match {
	    case o:JsObject => // m is the Map
	     apply(
	        fromJson[T1](o\f1), 
	        fromJson[T2](o\f2), 
	        fromJson[T3](o\f3)
	      )
	    case _ => throw new RuntimeException("object expected")
	  } 
  }

  trait HasUnapply3[S <: Product3[T1,T2,T3], T1, T2, T3]  {
  	def unapply(s:S):Option[Product3[T1,T2,T3]]
  }

  def buildFormatter3[S, T1, T2, T3 ](f1: String, f2: String, f3: String)(apply : (T1, T2, T3) => S )(companion : { def unapply:S => Option[Product3[T1, T2, T3]] })(implicit bin1: Format[T1], bin2: Format[T2], bin3: Format[T3]) = new Format[S]{
	  def writes(s: S) = {
	    val product = companion.unapply(s).get
	    
	    JsObject(
	      List(
	        (f1, toJson(product._1)), 
	        (f2, toJson(product._2)), 
	        (f3, toJson(product._3)) 
	    ))
	    
	  }
	  def reads(js: JsValue) = js match {
	    case o:JsObject => // m is the Map
	     apply(
	        fromJson[T1](o\f1), 
	        fromJson[T2](o\f2), 
	        fromJson[T3](o\f3)
	      )
	    case _ => throw new RuntimeException("object expected")
	  }
  }

  def buildFormatter2[S, T1, T2](f1: String, f2: String)(apply : (T1, T2) => S)(unapply : S => Option[Product2[T1, T2]])(implicit bin1: Format[T1], bin2: Format[T2]) = new Format[S]{
	  def writes(s: S) = {
	    val product = unapply(s).get
	    
	    JsObject(
	      List(
	        (f1, toJson(product._1)), 
	        (f2, toJson(product._2))
	    ))
	    
	  }
	  def reads(js: JsValue) = js match {
	    case o:JsObject => // m is the Map
	     apply(
	        fromJson[T1](o\f1), 
	        fromJson[T2](o\f2)
	      )
	    case _ => throw new RuntimeException("object expected")
	  }
  }
}
