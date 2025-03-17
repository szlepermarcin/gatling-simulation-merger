package io

import record.Record

import scala.collection.mutable

class MultiLogReader(readers: List[LogReader], prefetchSize: Int)
    extends AutoCloseable {

  given recordOrd: Ordering[Record] = Ordering
    .by[Record, Long](_.timestamp)
    .reverse
    .orElse(Ordering.by(_.priority))

  private val pq =
    new mutable.PriorityQueue[(Record, Iterator[Record])]()(Ordering.by(_._1))

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
