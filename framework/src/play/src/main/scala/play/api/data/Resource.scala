package play.api.data.resource

import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.LensConstructor._
import play.api.data.validation._

case class LensValidationError(lens: JsLens, message: String, args: Any*)

sealed trait JsValidationResult[T] {
  def fold[X](
    invalid: Seq[LensValidationError] => X, 
    valid: T => X) = this match {
    case JsValidationSuccess(v) => valid(v)
    case JsValidationError(e) => invalid(e)
  }

  def map[X](f: T => X): JsValidationResult[X] = this match {
    case JsValidationSuccess(v) => JsValidationSuccess(f(v))
    case JsValidationError(e) => JsValidationError[X](e)
  }

  def flatMap[X](f: T => JsValidationResult[X]): JsValidationResult[X] = this match {
    case JsValidationSuccess(v) => f(v)
    case JsValidationError(e) => JsValidationError[X](e)
  }

}

case class JsValidationSuccess[T](value: T) extends JsValidationResult[T]
case class JsValidationError[T](errors: Seq[LensValidationError]) extends JsValidationResult[T]

trait Validates[T] {
  self => 

  def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T]

  def and[V](other: Validates[V]): Validates[(T, V)] = new Validates[(T, V)] {
    def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[(T, V)] = {
      val selfVal = self.validates(json, rootLens)
      val otherVal = other.validates(json, rootLens)
      (selfVal, otherVal) match {
        case (JsValidationError(errors), JsValidationSuccess(_)) => JsValidationError[(T, V)](errors)
        case (JsValidationSuccess(_), JsValidationError(errors)) => JsValidationError[(T, V)](errors)
        case (JsValidationSuccess(t), JsValidationSuccess(v)) => JsValidationSuccess((t, v))
        case (JsValidationError(errors), JsValidationError(errors2)) => JsValidationError[(T, V)](errors ++ errors2)
      }
    }
  }

  def checking[A](other: Validates[A]) = {
    new Validates[T] {
      def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T] = {
        val otherVal = other.validates(json, rootLens)
        val selfVal = self.validates(json, rootLens)
        (selfVal, otherVal) match {
          case (JsValidationError(errors), JsValidationSuccess(_)) => JsValidationError[T](errors)
          case (JsValidationSuccess(_), JsValidationError(errors)) => JsValidationError[T](errors)
          case (JsValidationSuccess(t), JsValidationSuccess(_)) => JsValidationSuccess(t)
          case (JsValidationError(errors), JsValidationError(errors2)) => JsValidationError[T](errors ++ errors2)
        }
      }
    }
  }

}

object Validates {
  def apply[T, A1](c1: JsLensValidates[A1])
                  (apply: Function1[A1, T])(unapply: Function1[T, Option[A1]])
                  (implicit valA1: Validates[A1]) = {
    new Validates[T]{
      def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T] = {
        c1.validates(json, rootLens).map( a1 => apply(a1) )
      }
    }
  }

  def apply[T, A1, A2](c1: JsLensValidates[A1], c2: JsLensValidates[A2])
                      (apply: Function2[A1, A2, T])(unapply: Function1[T, Option[(A1, A2)]])
                      (implicit valA1: Validates[A1], valA2: Validates[A2]) = {
    new Validates[T]{
      def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T] = {
        (c1 and c2).validates(json, rootLens).map{ case (a1, a2) => apply(a1, a2) }
      }
    }
  }
}

object ValidateWrites {
  def apply[T, A1](c1: JsLensValidates[A1])
                  (apply: Function1[A1, T])(unapply: Function1[T, Option[A1]])
                  (implicit wA1: Writes[A1]) = {
    new Writes[T]{
      def writes(t: T): JsValue = {
        unapply(t) match {
          case Some(product) =>
            c1.lens.set(JsObject(Seq()), Json.toJson(product))
          case _ => throw new RuntimeException("product expected")
        }
      }
    }
  }

  def apply[T, A1, A2](c1: JsLensValidates[A1], c2: JsLensValidates[A2])
                      (apply: Function2[A1, A2, T])(unapply: Function1[T, Option[(A1, A2)]])
                      (implicit wA1: Writes[A1], wA2: Writes[A2]) = {
    new Writes[T]{
      def writes(t: T): JsValue = {
        unapply(t) match {
          case Some(product) =>
            c2.lens.set(c1.lens.set(JsObject(Seq()), Json.toJson(product._1)), Json.toJson(product._2))
          case _ => throw new RuntimeException("product expected")
        }
      }
    }
  }
}

case class JsLensValidates[T](lens: JsLens, constraints: Seq[Constraint[T]] = Nil)(implicit valT: Validates[T]) extends Validates[T] {
  def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T] = {
    val l = rootLens.andThen(lens)
    l.get(json) match {
      case JsUndefined(e) => JsValidationError[T](Seq(LensValidationError(l, e)))
      case js => valT.validates(js).flatMap( t => applyConstraints(l, t) )
    }
  }

  protected def applyConstraints(lens: JsLens, t: T): JsValidationResult[T] = {
    Option(collectErrors(lens, t))
      .filterNot(_.isEmpty)
      .map{ errors => val res: JsValidationResult[T] = JsValidationError(errors); res }
      .getOrElse( JsValidationSuccess(t))
  }

  protected def collectErrors(lens: JsLens, t: T): Seq[LensValidationError] = {
    constraints.map(_(t)).collect {
      case Invalid(errors) => errors.map{ e => LensValidationError(lens, e.message, e.args) }
    }.flatten
  }
}

object JsLensValidates {
  def apply[T](m: (JsLens, Constraint[T]))(implicit valT: Validates[T]) = new JsLensValidates[T](m._1, Seq(m._2))
}


sealed trait ResourceResult[T] {
  def map[X](f: T => X): ResourceResult[X] = this match {
    case ResourceSuccess(t) => ResourceSuccess(f(t))
    case ResourceValidationError(e) => ResourceValidationError[X](e)
    case ResourceOpError(e) => ResourceOpError[X](e)
  }

  def flatMap[X](f: T => ResourceResult[X]): ResourceResult[X] = this match {
    case ResourceSuccess(t) => f(t)
    case ResourceValidationError(e) => ResourceValidationError[X](e)
    case ResourceOpError(e) => ResourceOpError[X](e)
  }

  def fold[X](
    errorValid: Seq[ResourceErrorMsg] => X, 
    errorOp: Seq[ResourceErrorMsg] => X, 
    success: T => X) = this match {
    case ResourceSuccess(v) => success(v)
    case ResourceValidationError(e) => errorValid(e)
    case ResourceOpError(e) => errorOp(e)
  }

  def foldValid[X](
    invalid: Seq[ResourceErrorMsg] => X, 
    valid: T => X) = this match {
    case ResourceSuccess(v) => valid(v)
    case ResourceValidationError(e) => invalid(e)
    case _ => sys.error("unexpected state")
  }

  def foldOp[X](
    error: Seq[ResourceErrorMsg] => X, 
    success: T => X) = this match {
    case ResourceSuccess(v) => success(v)
    case ResourceOpError(e) => error(e)
    case _ => sys.error("unexpected state")
  }
}

case class ResourceErrorMsg(key: String, message: String, args: Any*)

case class ResourceSuccess[T](value: T) extends ResourceResult[T]
case class ResourceValidationError[T](errors: Seq[ResourceErrorMsg]) extends ResourceResult[T]
case class ResourceOpError[T](errors: Seq[ResourceErrorMsg]) extends ResourceResult[T]

trait ResourceTemplate[T] {
  def create(json: JsValue): ResourceResult[T]
  def fetch(json: JsValue): ResourceResult[T]
  //def update(s: S): T
  //def delete(s: S)
  //def getBatch(s: Enumerator[S]): Enumerator[T]
}


class Resource[T](tmpl: ResourceTemplate[JsValue], 
                  formatter: Validates[T],
                  val writer: Writes[T],
                  inputTransform: JsValue => JsValue = identity, 
                  outputTransform: JsValue => JsValue = identity,
                  queryTransform: JsValue => JsValue = identity){


  def create(json: JsValue): ResourceResult[T] = {
    formatter.validates(json).fold(
      invalid = { e => ResourceValidationError(e.map( e => ResourceErrorMsg(e.lens.toString, e.message, e.args:_*) )) },
      valid = { s => 
        tmpl.create(json).foldOp(
          error = { e => ResourceOpError(e.map( e => ResourceErrorMsg(e.key, e.message, e.args:_*) )) },
          success = { e => ResourceSuccess(s) }
        )
      }
    )
  }

  def fetch(json: JsValue): ResourceResult[T] = {
    tmpl.fetch(json).flatMap( js => 
      formatter.validates(js).fold[ResourceResult[T]](
        invalid = { e => ResourceValidationError(e.map( e => ResourceErrorMsg(e.lens.toString, e.message, e.args:_*) )) },
        valid = { t => ResourceSuccess(t) }
      )
    )
  }

  def checking[A](c: (JsLens, Constraint[A]))(implicit v:Validates[A]) = {
    new Resource(
      this.tmpl, 
      this.formatter.checking(JsLensValidates(c)), 
      this.writer,
      this.inputTransform, 
      this.outputTransform,
      this.queryTransform
    )
  }
  
  def transformInput( f: JsValue => JsValue ) = new Resource(this.tmpl, this.formatter, this.writer, f, this.outputTransform, this.queryTransform)
  def transformOutput( f: JsValue => JsValue ) = new Resource(this.tmpl, this.formatter, this.writer, this.inputTransform, f, this.queryTransform)
  def transformQuery( f: JsValue => JsValue ) = new Resource(this.tmpl, this.formatter, this.writer, this.inputTransform, this.outputTransform, f)
}

object Resource {
  def apply[T](tmpl: ResourceTemplate[JsValue])
              (implicit v: Validates[T], w: Writes[T]) = {
    new Resource[T](tmpl, v, w)
  }

  def apply[T, A1](c1: (JsLens, Constraint[A1]))
                  (apply: Function1[A1, T])(unapply: Function1[T, Option[A1]])
                  (tmpl: ResourceTemplate[JsValue])
                  (implicit valA1: Validates[A1], wA1: Writes[A1]): Resource[T] = {
    implicit val valT = Validates(JsLensValidates(c1))(apply)(unapply)
    implicit val wT = ValidateWrites(JsLensValidates(c1))(apply)(unapply)

    new Resource[T](tmpl, valT, wT)                
  }

  def apply[T, A1, A2](c1: (JsLens, Constraint[A1]), c2: (JsLens, Constraint[A2]))
                  (apply: Function2[A1, A2, T])(unapply: Function1[T, Option[(A1, A2)]])
                  (tmpl: ResourceTemplate[JsValue])
                  (implicit valA1: Validates[A1], valA2: Validates[A2], wA1: Writes[A1], wA2: Writes[A2]): Resource[T] = {
    implicit val valT = Validates(JsLensValidates(c1), JsLensValidates(c2))(apply)(unapply)
    implicit val wT = ValidateWrites(JsLensValidates(c1), JsLensValidates(c2))(apply)(unapply)

    new Resource[T](tmpl, valT, wT)                
  }

  def raw[A1](c1: (JsLens, Constraint[A1]))
                  (tmpl: ResourceTemplate[JsValue])
                  (implicit valA1: Validates[A1], v: Validates[JsValue], w: Writes[JsValue]): Resource[JsValue] = {
    new Resource[JsValue](tmpl, v.checking(JsLensValidates(c1)), w)                
  }
  
  def raw[A1, A2](c1: (JsLens, Constraint[A1]), c2: (JsLens, Constraint[A2]))
                  (tmpl: ResourceTemplate[JsValue])
                  (implicit valA1: Validates[A1], valA2: Validates[A2], v: Validates[JsValue], w: Writes[JsValue]): Resource[JsValue] = {
    new Resource[JsValue](tmpl, v.checking(JsLensValidates(c1)).checking(JsLensValidates(c2)), w) 
  }
}

object Validators {
  implicit object StringValidates extends Validates[String] {
    def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[String] = rootLens(json) match {
      case JsString(s) => JsValidationSuccess(s)
      case _ => JsValidationError(Seq(LensValidationError(rootLens, "validate.error.expected.string")))
    }
  }

  implicit object IntValidates extends Validates[Int] {
    def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[Int] = rootLens(json) match {
      case JsNumber(s) => JsValidationSuccess(s.toInt)
      case _ => JsValidationError(Seq(LensValidationError(rootLens, "validate.error.expected.int")))
    }
  }

  implicit object DefaultJsValidates extends Validates[JsValue] {
    def validates(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[JsValue] = rootLens(json) match {
      case JsUndefined(e) => JsValidationError[JsValue](Seq(LensValidationError(rootLens, e)))
      case js => JsValidationSuccess(js)
    }
  }
}

/**
 *
 * The Resource Controller to be plugged in your application
 *
 */
class ResourceController[T](res: Resource[T]) extends Controller {

  def create = Action(parse.json) { implicit request =>
    res.create(request.body).fold(
      errorValid = { errors => BadRequest(errors.toString) },
      errorOp = { errors => BadRequest(errors.toString) },
      success = { value => Ok("Created " + value) }
    )
  } 

  def fetch = Action {implicit request =>
    val json = request.queryString.foldLeft(JsObject(Nil))( (all: JsObject, elt: (String, Seq[String])) => 
      all ++ (if(elt._2.length == 1 ) Json.obj(elt._1 -> Json.toJson(elt._2(0))) else Json.obj(elt._1 -> Json.toJson(elt._2)))
    )
    res.fetch(json).fold(
      errorValid = { errors => BadRequest(errors.toString) },
      errorOp = { errors => BadRequest(errors.toString) },
      success = { value => Ok(Json.toJson(value)(res.writer)) }
    )
  } 
}



/*trait JsChecker[T] {
  self => 
  def check(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T] 

  def and[V](other: JsChecker[V]): JsChecker[(T, V)] = new JsChecker[(T, V)] {
    def check(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[(T, V)] = {
      self.check(json).fold( 
        valid = { t => other.check(json).fold(
          valid = { v => JsValidationSuccess((t, v)) },
          invalid = { otherErrors => JsValidationError(otherErrors) }
        )}, 
        invalid = { errors => other.check(json).fold(
          valid = { v => JsValidationError(errors) },
          invalid = { otherErrors => JsValidationError(errors ++ otherErrors) }
        )}
      )
    }
  }
}

case class JsLensChecker[T](lens: JsLens, constraints: Seq[Constraint[T]] = Nil)(implicit valT: Validates[T]) extends JsChecker[T] {
  def verifying(constraints: Constraint[T]*) = this.copy(constraints = this.constraints ++ constraints.toSeq)

  def check(json: JsValue, rootLens: JsLens = JsLens.ROOT): JsValidationResult[T] = {
    (rootLens.andThen(lens)).get(json) match {
      case JsUndefined(e) => JsValidationError[T](Seq(ValidationError(e)))
      case js => valT.validates(js).flatMap( t => applyConstraints(t) )
    }
  }

  protected def applyConstraints(t: T): JsValidationResult[T] = {
    Option(collectErrors(t))
      .filterNot(_.isEmpty)
      .map{ errors => val res: JsValidationResult[T] = JsValidationError(errors); res }
      .getOrElse( JsValidationSuccess(t))
  }

  protected def collectErrors(t: T): Seq[ValidationError] = {
    constraints.map(_(t)).collect {
      case Invalid(errors) => errors
    }.flatten
  }

}

object JsLensChecker {
  def apply[T](m: (JsLens, Constraint[T]))(implicit valT: Validates[T]) = new JsLensChecker[T](m._1, Seq(m._2))
}

case class JsValidator(checkers: Seq[JsChecker[_]] = Nil) extends Validates[JsValue] {
  def validates(json: JsValue, target: JsLens = JsLens.ROOT): JsValidationResult[JsValue] = {
    val errors = checkers.foldLeft(Seq[ValidationError]()){ (s, c) => c.check(json, target) match {
      case JsValidationError(errors) => s ++ errors
      case JsValidationSuccess(_) => s
    } }

    if(!errors.isEmpty) JsValidationError(errors)
    else JsValidationSuccess(json)
  }  

  def checking[A](c: JsChecker[A])(implicit v:Validates[A]) = this.copy(checkers = checkers ++ Seq(c))
  def checking[A](c: (JsLens, Constraint[A]))(implicit valA:Validates[A]): JsValidator = this.copy(checkers = checkers ++ Seq(JsLensChecker(c)))
}

object JsValidator {
  def apply[A1](c1: JsChecker[A1])(implicit valA1: Validates[A1]): JsValidator = {
    new JsValidator(Seq(c1))
  }

  def apply[A1, A2](c1: JsChecker[A1], c2: JsChecker[A2])(implicit valA1: Validates[A1], valA2: Validates[A2]): JsValidator = {
    new JsValidator(Seq(c1, c2))
  }

  def apply[A1](c1: (JsLens, Constraint[A1]))(implicit valA1: Validates[A1]): JsValidator = {   
    new JsValidator(Seq(JsLensChecker(c1)))
  }
}*/
