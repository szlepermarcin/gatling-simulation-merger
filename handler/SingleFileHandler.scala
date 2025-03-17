package handler

import io.LogWriter
import io.MultiLogReader
import record.Record
import record.Record.RunMessage

import java.nio.file.Path
import scala.util.Using

class SingleFileHandler(outputFile: Path) extends Handler {

  override def handle(reader: MultiLogReader): Unit = {
    val it = reader.iterator.buffered

    def readRunMessage(acc: Option[RunMessage] = None): RunMessage = {
      (it.headOption, acc) match
        case (Some(_: RunMessage), acc) =>
          val rm = it.next().asInstanceOf[RunMessage]
          readRunMessage(acc.map(_ merge rm).orElse(Some(rm)))
        case (_, Some(result)) => result
        case (_, _) => throw new RuntimeException("Missing run message record")
    }

    val mergedRunMessage = readRunMessage()

    Using.resource(new LogWriter(outputFile, mergedRunMessage))(writer =>
      it.foreach(writer.write)
    )

    println(s"new simulation file saved: ${outputFile.toAbsolutePath()}")
  }
}

object SingleFileHandler {
  def apply(outputFile: Path): SingleFileHandler = new SingleFileHandler(
    outputFile
  )
}
