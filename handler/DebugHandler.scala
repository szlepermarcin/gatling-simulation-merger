package handler

import io.MultiLogReader

object PrintHandler extends Handler {

  override def handle(reader: MultiLogReader): Unit =
    reader.iterator.foreach(println)

}
