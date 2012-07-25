package play.api.libs.json.util

class ApplicativeOps[M[_],A](ma:M[A]){

  def ~>[B](mb: M[B])(implicit a:Applicative[M]):M[B] = a(a(a.pure((_:A) => (b:B) => b), ma),mb)
  def <~[B](mb: M[B])(implicit a:Applicative[M]):M[A] = a(a(a.pure((a:A) => (_:B) => a), ma),mb)
  def <~>[B,C](mb: M[B])(implicit witness: <:<[A,B => C],  a:Applicative[M]):M[C] = apply(mb)
  def apply[B,C](mb: M[B])(implicit witness: <:<[A,B => C],  a:Applicative[M]):M[C] = a(a.map(ma,witness),mb)
  def ~[B](mb:M[B])(implicit a:Applicative[M]):ApplicativeBuilder[M]#Builder2[A,B] = { val ab = new ApplicativeBuilder(a); new ab.Builder2(ma,mb) }

}

class ApplicativeBuilder[M[_]]( app:Applicative[M]){

  class Builder2[A1,A2](m1:M[A1], m2:M[A2]){

    def ~[A3](m3:M[A3]): Builder3[A1,A2,A3] = new Builder3(m1,m2,m3)

    def tupled:M[(A1,A2)] = app.apply(app.apply(app.pure((a1:A1) => (a2:A2) => (a1,a2)),m1),m2)

    def apply[B](f: (A1,A2) => B ):M[B] = app.apply(app.pure(f.tupled), tupled)

  }

  class Builder3[A1,A2,A3](m1:M[A1], m2:M[A2], m3:M[A3]){

    def ~[A4](m4:M[A4]): Builder4[A1,A2,A3,A4] = new Builder4(m1,m2,m3,m4)

    def tupled:M[(A1,A2,A3)] = app.apply(app.apply(app.apply(app.pure((a1:A1) => (a2:A2) => (a3:A3) => (a1,a2,a3)),m1),m2),m3)

    def apply[B](f: (A1,A2,A3) => B ):M[B] = app.apply(app.pure(f.tupled), tupled)

  }

  class Builder4[A1,A2,A3,A4](m1:M[A1], m2:M[A2], m3:M[A3], m4:M[A4]){

    def tupled:M[(A1,A2,A3,A4)] =  app.apply(app.apply(app.apply(app.apply(app.pure((a1:A1) => (a2:A2) => (a3:A3) => (a4:A4) => (a1,a2,a3,a4)),m1),m2),m3),m4)

    def apply[B](f: (A1,A2,A3,A4) => B ):M[B] = app.apply(app.pure(f.tupled), tupled)

  }

}

trait Applicative[M[_]]{

  def pure[A](a:A):M[A]
  def map[A,B](m:M[A], f: A => B):M[B]
  def apply[A,B](mf:M[A => B], ma:M[A]):M[B]

}

trait AlternativeOps[M[_],A]{

  def |[AA >: A ,B >: AA](alt2 :M[B])(implicit a:Alternative[M]):M[AA]

}

trait Alternative[M[_]] extends Applicative[M]{

  def |[A,B >: A](alt1: M[A], alt2 :M[B]):M[A]
  def empty:M[Nothing]
  //def some[A](m:M[A]):M[List[A]]
  //def many[A](m:M[A]):M[List[A]]

}
object `package` {

  implicit def toApplicativeOps[M[_],A](a:M[A]):ApplicativeOps[M,A] = new ApplicativeOps(a)

  implicit def applicativeOption:Applicative[Option] = new Applicative[Option]{

    def pure[A](a:A):Option[A] = Some(a)

    def map[A,B](m:Option[A], f: A => B):Option[B] = m.map(f)

    def apply[A,B](mf:Option[A => B], ma: Option[A]):Option[B] = mf.flatMap(f => ma.map(f))

  }
  import play.api.libs.json._
  implicit def applicativeReads:Applicative[Reads] = new Applicative[Reads]{

    def pure[A](a:A):Reads[A] = new Reads[A] { def reads(js: JsValue) = JsSuccess(a) }

    def map[A,B](m:Reads[A], f: A => B):Reads[B] = m.map(f)

    def apply[A,B](mf:Reads[A => B], ma: Reads[A]):Reads[B] = mf.flatMap(f => ma.map(f))

  }

}


