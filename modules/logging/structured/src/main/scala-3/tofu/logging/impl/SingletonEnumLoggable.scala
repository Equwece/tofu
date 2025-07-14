package tofu.logging

import scala.deriving.Mirror

trait SingletonEnumLoggable[A] extends SingleValueLoggable[A]

object SingletonEnumLoggable:
  inline def nameOf[A](using m: scala.deriving.Mirror.Of[A]): String =
   scala.compiletime.constValue[m.MirroredLabel]

  inline def makeInstance[T: Mirror.Of]: SingletonEnumLoggable[T] =
    new SingletonEnumLoggable[T]:
      def logValue(a: T): LogParamValue = StrValue(nameOf[T])

      override def putField[I, V, R, M](a: T, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
        receiver.addString(name, nameOf[T], input)

  inline def derived[T: Mirror.SumOf]: SingletonEnumLoggable[T] =
    SingletonEnumLoggableMacro.ensureSingletonEnum[T]
    makeInstance[T]
