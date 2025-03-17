package io

import codec.LogFileCodec.Syntax.*
import record.Record
import record.Record.*

import java.io.BufferedOutputStream
import java.io.DataOutputStream
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable

class LogWriter(
    path: Path,
    runMessage: RunMessage
) extends AutoCloseable {

  val (os, stringCache) = {
    val os = new DataOutputStream(
      new BufferedOutputStream(
        Files.newOutputStream(path)
      )
    )
    val stringCache: mutable.Map[String, Int] = mutable.Map()
    os.write(0)
    runMessage.write(os, stringCache, None)
    (os, stringCache)
  }

  def write(rec: Record): Unit = {
    rec match
      case r: RunMessage =>
        if (!runMessage.isCompatible(r))
          throw new RuntimeException(
            "Incompatibile log file, can't write"
          )
      case r: Request =>
        os.writeByte(1)
        r.write(os, stringCache, Some(runMessage.start))
      case r: User =>
        os.writeByte(2)
        r.write(os, stringCache, Some(runMessage.start))
      case r: Group =>
        os.writeByte(3)
        r.write(os, stringCache, Some(runMessage.start))
      case r: Crash =>
        os.writeByte(4)
        r.write(os, stringCache, Some(runMessage.start))
  }

  def close(): Unit = {
    os.close()
  }
}
