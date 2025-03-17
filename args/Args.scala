package args

import java.nio.file.Path
import java.nio.file.Paths
import handler.Handler
import handler.SingleFileHandler

case class Args(inputDir: Path, outputFile: Option[Path])
object Args {

  private case class Parsed(
      inputDir: Option[Path],
      outputFile: Option[Path]
  ) {
    def build: Args = {

      val argsOpt = for {
        id <- inputDir
        of = outputFile
      } yield Args(id, of)

      argsOpt match
        case None =>
          println(
            "Usage scala-cli . -- -id/--input-dir <path> [-of/--output-file <path>]"
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
      case param :: value :: next if Set("-of", "--output-file").apply(param) =>
        parse(next, parsed.copy(outputFile = Some(Paths.get(value))))
      case _ :: next => parse(next, parsed)
      case Nil       => parsed.build

}
