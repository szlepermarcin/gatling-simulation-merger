package io

import record.Record
import record.Record.*

import java.io.BufferedInputStream
import java.io.DataInputStream
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable

class LogReader(path: Path) extends AutoCloseable {

  private val is =
    new DataInputStream(
      new BufferedInputStream(Files.newInputStream(path))
    )

  private val stringCache: mutable.Map[Int, String] = mutable.Map()

  private var runMessage: Option[RunMessage] = None

  def read: Option[Record] = is.read().toByte match
    case 0 =>
      if (runMessage.nonEmpty)
        throw new RuntimeException("Unexpected record")
      runMessage = Some(RunMessage.read(is, stringCache, None))
      runMessage
    case 1 =>
      Some(Request.read(is, stringCache, runMessage))
    case 2 =>
      Some(User.read(is, stringCache, runMessage))
    case 3 =>
      Some(Group.read(is, stringCache, runMessage))
    case 4 =>
      Some(Crash.read(is, stringCache, runMessage))
    case -1 => None
    case _ =>
      throw new RuntimeException("Unsupported record")

  def iterator: Iterator[Record] =
    Iterator.unfold(())(_ => read.map(v => (v, ())))

  def close(): Unit = is.close()

}
