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
        ctx: Option[LogFileCodec.Context]
    )(using LogFileCodec[T]): T = LogFileCodec[T].read(is, cache, ctx)
  }

  case class RunMessage(
      gatlingVersion: String,
      simulationClass: String,
      start: Long,
      runDescription: String,
      scenarios: Array[String],
      assertions: List[Array[Byte]]
  ) extends Record
      with LogFileCodec.Context {

    def priority: Int = 5

    def timestamp: Long = start

    def isCompatible(other: RunMessage): Boolean = {
      gatlingVersion == other.gatlingVersion
      && simulationClass == other.simulationClass
    }

    def merge(other: RunMessage): RunMessage = {
      if isCompatible(other) then {
        copy(scenarios = (scenarios ++ other.scenarios).distinct)
      } else {
        throw new RuntimeException("Incompatible log file")
      }
    }

  }

  object RunMessage extends RecordCompanion[RunMessage] {

    given LogFileCodec[RunMessage] with {

      override def write(
          t: RunMessage,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          ctx: Option[LogFileCodec.Context]
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
          runStart: Option[LogFileCodec.Context]
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
          ctx: Option[LogFileCodec.Context]
      ): Unit = {
        import t.*

        withContext(ctx) { rs =>

          os.writeInt(group.length)
          group.foreach(os.writeCachedString(_, cache))
          os.writeCachedString(name, cache)
          os.writeInt((start - rs.timestamp).toInt)
          os.writeInt((`end` - rs.timestamp).toInt)
          os.writeBoolean(status)
          os.writeCachedString(error, cache)

        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          ctx: Option[LogFileCodec.Context]
      ): Request = {
        withContext(ctx) { rs =>
          val groups = List.fill(is.readInt())(is.readCachedString(cache))
          val name = is.readCachedString(cache)
          val start = is.readInt().toLong + rs.timestamp
          val `end` = is.readInt().toLong + rs.timestamp
          val status = is.readBoolean()
          val error = is.readCachedString(cache)

          Request(groups, name, start, `end`, status, error)
        }
      }
    }
  }

  sealed trait User extends Record {
    def scenario: String
    def timestamp: Long
    def start: Boolean
  }

  object User extends RecordCompanion[User] {

    def apply(scenario: String, start: Boolean, timestamp: Long): User =
      if start then UserStart(scenario, timestamp)
      else UserEnd(scenario, timestamp)

    case class UserStart(scenario: String, timestamp: Long) extends User {
      def start: Boolean = true
      def priority: Int = 4
    }

    case class UserEnd(scenario: String, timestamp: Long) extends User {
      def start: Boolean = false
      def priority: Int = 1
    }

    given LogFileCodec[User] with {

      override def write(
          t: User,
          os: DataOutputStream,
          cache: scala.collection.mutable.Map[String, Int],
          ctx: Option[LogFileCodec.Context]
      ): Unit = {
        import t.*
        withContext(ctx) { rs =>
          os.writeInt(rs.scenarioIndex(scenario))
          os.writeBoolean(start)
          os.writeInt((timestamp - rs.timestamp).toInt)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          ctx: Option[LogFileCodec.Context]
      ): User = {
        withContext(ctx) { rs =>

          val scenario = rs.scenarios(is.readInt())
          val start = is.readBoolean()
          val timestamp = is.readInt().toLong + rs.timestamp

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
          ctx: Option[LogFileCodec.Context]
      ): Unit = {
        import t.*

        withContext(ctx) { rs =>

          os.writeInt(group.length)
          group.foreach(os.writeCachedString(_, cache))
          os.writeInt((start - rs.timestamp).toInt)
          os.writeInt((`end` - rs.timestamp).toInt)
          os.writeInt(time)
          os.writeBoolean(status)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          ctx: Option[LogFileCodec.Context]
      ): Group = {
        withContext(ctx) { rs =>

          val groups = List.fill(is.readInt())(is.readCachedString(cache))
          val start = is.readInt().toLong + rs.timestamp
          val `end` = is.readInt().toLong + rs.timestamp
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
          ctx: Option[LogFileCodec.Context]
      ): Unit = {
        import t.*

        withContext(ctx) { rs =>

          os.writeCachedString(msg, cache)
          os.writeInt((timestamp - rs.timestamp).toInt)
        }
      }

      override def read(
          is: DataInputStream,
          cache: scala.collection.mutable.Map[Int, String],
          ctx: Option[LogFileCodec.Context]
      ): Crash = {
        withContext(ctx) { rs =>

          val msg = is.readCachedString(cache)
          val timestamp = is.readInt().toLong + rs.timestamp

          Crash(msg, timestamp)
        }
      }
    }
  }
}
