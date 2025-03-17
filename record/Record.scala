package record

import codec.LogFileCodec
import codec.LogFileCodec.Syntax.*

import java.io.DataInputStream
import java.io.DataOutputStream
import scala.collection.mutable

sealed trait Record extends Product with Serializable {
  def timestamp: Long
  def priority: Int
}

object Record {

  trait RecordCompanion[T <: Record] {
    def read(
        is: DataInputStream,
        cache: mutable.Map[Int, String],
        runStart: Option[Long]
    )(using LogFileCodec[T]): T = LogFileCodec[T].read(is, cache, runStart)
  }

  case class RunMessage(
      gatlingVersion: String,
      simulationClass: String,
      start: Long,
      runDescription: String,
      scenarios: Array[String],
      assertions: List[Array[Byte]]
  ) extends Record {

    def priority: Int = 5

    def timestamp: Long = start

    def isCompatible(other: RunMessage): Boolean = {
      gatlingVersion == other.gatlingVersion
      && simulationClass == other.simulationClass
      && (scenarios sameElements other.scenarios)
    }
  }

  object RunMessage extends RecordCompanion[RunMessage] {

    given LogFileCodec[RunMessage] with {

      override def write(
          t: RunMessage,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          runStart: Option[Long]
      ): Unit = {
        import t.*
        os.writeString(gatlingVersion)
        os.writeString(simulationClass)
        os.writeLong(start)
        os.writeString(runDescription)
        os.writeInt(scenarios.size)
        scenarios.toList.foreach(os.writeString)
        os.writeInt(assertions.size)
        assertions.foreach { a =>
          os.write(a)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          runStart: Option[Long]
      ): RunMessage = {
        val gatlingVersion = is.readString()
        val simulationClass = is.readString()
        val start = is.readLong()
        val runDesc = is.readString()
        val scenarions = Array.fill(is.readInt())(is.readString())
        val asserions = List.fill(is.readInt())(is.readNBytes(is.readInt()))

        RunMessage(
          gatlingVersion,
          simulationClass,
          start,
          runDesc,
          scenarions,
          asserions
        )
      }
    }
  }

  case class Request(
      group: List[String],
      name: String,
      start: Long,
      `end`: Long,
      status: Boolean,
      error: String
  ) extends Record {
    def priority: Int = 3
    def timestamp: Long = `end`
  }

  object Request extends RecordCompanion[Request] {

    given LogFileCodec[Request] with {

      override def write(
          t: Request,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          runStart: Option[Long]
      ): Unit = {
        import t.*

        withRunStart(runStart) { rs =>

          os.writeInt(group.length)
          group.foreach(os.writeCachedString(_, cache))
          os.writeCachedString(name, cache)
          os.writeInt((start - rs).toInt)
          os.writeInt((`end` - rs).toInt)
          os.writeBoolean(status)
          os.writeCachedString(error, cache)

        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          runStart: Option[Long]
      ): Request = {
        withRunStart(runStart) { rs =>
          val groups = List.fill(is.readInt())(is.readCachedString(cache))
          val name = is.readCachedString(cache)
          val start = is.readInt().toLong + rs
          val `end` = is.readInt().toLong + rs
          val status = is.readBoolean()
          val error = is.readCachedString(cache)

          Request(groups, name, start, `end`, status, error)
        }
      }
    }
  }

  case class User(scenario: Int, start: Boolean, timestamp: Long)
      extends Record {
    def priority: Int = if start then 4 else 1
  }

  object User extends RecordCompanion[User] {

    given LogFileCodec[User] with {

      override def write(
          t: User,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          runStart: Option[Long]
      ): Unit = {
        import t.*
        withRunStart(runStart) { rs =>
          os.writeInt(scenario)
          os.writeBoolean(start)
          os.writeInt((timestamp - rs).toInt)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          runStart: Option[Long]
      ): User = {
        withRunStart(runStart) { rs =>

          val scenario = is.readInt()
          val start = is.readBoolean()
          val timestamp = is.readInt().toLong + rs

          User(scenario, start, timestamp)
        }
      }
    }
  }

  case class Group(
      group: List[String],
      start: Long,
      `end`: Long,
      time: Int,
      status: Boolean
  ) extends Record {
    def priority: Int = 2
    def timestamp: Long = `end`
  }

  object Group extends RecordCompanion[Group] {

    given LogFileCodec[Group] with {

      override def write(
          t: Group,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          runStart: Option[Long]
      ): Unit = {
        import t.*

        withRunStart(runStart) { rs =>

          os.writeInt(group.length)
          group.foreach(os.writeCachedString(_, cache))
          os.writeInt((start - rs).toInt)
          os.writeInt((`end` - rs).toInt)
          os.writeInt(time)
          os.writeBoolean(status)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          runStart: Option[Long]
      ): Group = {
        withRunStart(runStart) { rs =>

          val groups = List.fill(is.readInt())(is.readCachedString(cache))
          val start = is.readInt().toLong + rs
          val `end` = is.readInt().toLong + rs
          val time = is.readInt()
          val status = is.readBoolean()

          Group(groups, start, `end`, time, status)
        }
      }
    }
  }

  case class Crash(msg: String, timestamp: Long) extends Record {
    def priority: Int = 0
  }
  object Crash extends RecordCompanion[Crash] {

    given LogFileCodec[Crash] with {

      override def write(
          t: Crash,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          runStart: Option[Long]
      ): Unit = {
        import t.*

        withRunStart(runStart) { rs =>

          os.writeCachedString(msg, cache)
          os.writeInt((timestamp - rs).toInt)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          runStart: Option[Long]
      ): Crash = {
        withRunStart(runStart) { rs =>

          val msg = is.readCachedString(cache)
          val timestamp = is.readInt().toLong + rs

          Crash(msg, timestamp)
        }
      }
    }
  }
}
