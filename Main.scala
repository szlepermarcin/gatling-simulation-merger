import handler.SingleFileHandler
import io.MultiLogReader
import util.StringInternals
import args.Args
import handler.Handler
import handler.PrintHandler

object Main {

  def main(args: Array[String]): Unit = {

    StringInternals.checkAvailability()

    val arguments = Args.parse(args)

    given Handler = arguments.outputFile match
      case None        => PrintHandler
      case Some(value) => SingleFileHandler(value)

    MultiLogReader.run(
      arguments.inputDir,
      prefetchSize = 20
    )
  }
}
