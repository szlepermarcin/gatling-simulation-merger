package codec

import java.io.DataOutputStream
import java.io.DataInputStream
import util.StringInternals

import scala.collection.mutable

trait LogFileCodec[T] {

  def withRunStart[A](runStart: Option[Long])(fn: Long => A) = {
    runStart match
      case None     => throw new RuntimeException("no runStart value")
      case Some(rs) => fn(rs)

  }

  def write(
      t: T,
      os: DataOutputStream,
      cache: mutable.Map[String, Int],
      runStart: Option[Long]
  ): Unit
  def read(
      is: DataInputStream,
      cache: mutable.Map[Int, String],
      runStart: Option[Long]
  ): T
}

object LogFileCodec {
  def apply[T: LogFileCodec] = summon[LogFileCodec[T]]

  object Syntax {
    extension [T: LogFileCodec](t: T) {
      def write(
          os: DataOutputStream,
          cache: mutable.Map[String, Int],
          runStart: Option[Long]
      ) = LogFileCodec[T].write(t, os, cache, runStart)
    }

    extension (os: DataOutputStream) {
      def writeString(s: String) = {
        if (s.isEmpty()) then {
          os.writeInt(0)
        } else {
          val value = StringInternals.value(s)
          val length = value.length
          val coder = StringInternals.coder(s)

          os.writeInt(length)
          os.write(value)
          os.writeByte(coder)
        }
      }

      def writeCachedString(s: String, cache: mutable.Map[String, Int]) = {
        cache.get(s) match
          case None =>
            val nextIndex = cache.values.maxOption.getOrElse(0) + 1
            cache.put(s, nextIndex)
            os.writeInt(nextIndex)
            os.writeString(s)
          case Some(value) => os.writeInt(-value)
      }
    }

    extension (is: DataInputStream) {
      def readString(): String = {
        val length = is.readInt()
        if (length == 0) {
          ""
        } else {
          val value = is.readNBytes(length)
          val coder = is.readByte()
          StringInternals.newString(value, coder)
        }
      }

      def readCachedString(cache: mutable.Map[Int, String]): String = {
        val cachedIndex = is.readInt()

        if (cachedIndex >= 0) then {
          val string = is.readString()

          cache.put(cachedIndex, string)
          string
        } else {
          cache.get(-cachedIndex) match
            case None =>
              throw new RuntimeException(
                s"Cached string missing for ${-cachedIndex} index"
              )
            case Some(res) => res
        }
      }
    }

  }
}
