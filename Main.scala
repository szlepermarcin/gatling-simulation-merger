import io.LogReader
import io.LogWriter
import io.MultiLogReader
import record.Record
import record.Record.*
import util.StringInternals

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.util.Using

object Main {

  case class Args(inputDir: Path, outputFile: Path)
  object Args {

    private case class Parsed(
        inputDir: Option[Path],
        outputFile: Option[Path]
    ) {
      def build: Args = {
        val argsOpt = for {
          id <- inputDir
          of <- outputFile
        } yield Args(id, of)

        argsOpt match
          case None =>
            println(
              "Usage scala-cli . -- -id/--input-dir <path> -of/--output-file <path>"
            )
            sys.exit(-1)
          case Some(value) =>
            value
      }
    }

    def parse(args: Array[String]): Args = parse(args.toList)

    private def parse(
        args: List[String],
        parsed: Parsed = Parsed(None, None)
    ): Args =
      args match
        case param :: value :: next if Set("-id", "--input-dir").apply(param) =>
          parse(next, parsed.copy(inputDir = Some(Paths.get(value))))
        case param :: value :: next
            if Set("-of", "--output-file").apply(param) =>
          parse(next, parsed.copy(outputFile = Some(Paths.get(value))))
        case _ :: next => parse(next, parsed)
        case Nil       => parsed.build

  }

  def main(args: Array[String]): Unit = {

    val arguments = Args.parse(args)

    StringInternals.checkAvailability()

    val f = Files
      .list(arguments.inputDir)
      .toList()
      .asScala
      .toList
      .filter(p => p.toString().endsWith(".log"))

    if (f.nonEmpty) then {

      println(s"found files: ${f.mkString(",")}")

      val earliestRunMessage = f
        .map(p => Using.resource(new LogReader(p))(_.read))
        .collect { case Some(v: RunMessage) => v }
        .minBy(_.start)

      Using.resource(LogWriter(arguments.outputFile, earliestRunMessage)) {
        writer =>
          val readers = f.map(new LogReader(_))
          Using(new MultiLogReader(readers, prefetchSize = 20))(
            _.iterator.foreach(writer.write)
          )
      }
      println(s"new simulation file saved: ${arguments.outputFile}")
    } else {
      println("no source files - skipping!")
    }
  }
}
