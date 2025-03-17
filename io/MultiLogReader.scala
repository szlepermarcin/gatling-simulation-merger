package io

import record.Record

import scala.collection.mutable
import java.nio.file.Path
import java.nio.file.Files

import scala.jdk.CollectionConverters.*
import handler.Handler
import scala.util.Using
import record.Record.RunMessage
import record.Record.Request
import record.Record.User.UserStart
import record.Record.User.UserEnd
import record.Record.Group
import record.Record.Crash

class MultiLogReader(logFiles: List[Path], prefetchSize: Int)
    extends AutoCloseable {

  given recordOrd: Ordering[Record] =
    List(
      Ordering.by[Record, Int] {
        case r: RunMessage => 1
        case _             => 0
      },
      Ordering.by[Record, Long](_.timestamp).reverse,
      Ordering.by[Record, Int](_.priority)
    ).reduce(_ orElse _)

  private val pq =
    new mutable.PriorityQueue[(Record, Iterator[Record])]()(Ordering.by(_._1))

  private val readers = logFiles.map(new LogReader(_))

  private val iterators =
    readers
      .map(_.iterator)
      .map { bi =>
        (1 to prefetchSize)
          .foreach(_ => if (bi.hasNext) then pq.enqueue((bi.next(), bi)))
        bi
      }

  def read: Option[Record] =
    Option.when(pq.nonEmpty) {
      val (t, bi) = pq.dequeue()
      if (bi.hasNext) then pq.enqueue((bi.next(), bi))
      t
    }

  def iterator: Iterator[Record] =
    Iterator.unfold(())(_ => read.map(v => (v, ())))

  override def close(): Unit = readers.foreach(_.close)

}

object MultiLogReader {
  def run(inputDir: Path, prefetchSize: Int)(using handler: Handler): Unit = {

    val f = Files
      .list(inputDir)
      .toList()
      .asScala
      .toList
      .filter(p => p.toString().endsWith(".log"))

    if (f.nonEmpty) then {

      println(s"found files: ${f.mkString(",")}")

      Using.resource(new MultiLogReader(f, prefetchSize = 20))(handler.handle)

    } else {
      println("no source files - skipping!")
    }

  }
}
