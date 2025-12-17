package utils

import scala.reflect.ClassTag

extension [A: ClassTag](col: IterableOnce[A])
  def toIArray: IArray[A] =
    col match
      case arr: collection.immutable.ArraySeq[A]
          if arr.unsafeArray
            .getClass()
            .getComponentType() == implicitly[ClassTag[A]].runtimeClass =>
        IArray.unsafeFromArray(arr.unsafeArray.asInstanceOf[Array[A]])
      case it: Iterable[A] => IArray.unsafeFromArray(it.toArray)
      case it              => IArray.unsafeFromArray(it.iterator.toArray)
