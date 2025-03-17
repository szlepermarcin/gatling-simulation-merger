package handler

import io.MultiLogReader

trait Handler {
  def handle(reader: MultiLogReader): Unit
}
