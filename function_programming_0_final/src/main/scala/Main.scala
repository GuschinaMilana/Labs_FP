package function_programming_0
import function_programming_0.commandLineInterface.Command
import function_programming_0.config.Loader
import function_programming_0.report.Logger
import java.nio.file.{Files, Paths}
// Главный модуль программы
object Main {
  def main(args: Array[String]): Unit = {
    val configPath = Paths.get(if (args.nonEmpty) args(0) else "config.yaml") // выбираем папку для конфиг-файла

    // создаём дефолтную конфигурацию, если файла нет
    if (!Files.exists(configPath)) {
      println(s"Конфигурация не найдена, будет использована конфигурация по умолчанию: $configPath")
      Loader.save(configPath, Loader.createDefaultConfig) match {
        case Right(_)  => Logger.info("Создана конфигурация по умолчанию.")
        case Left(err) => Logger.error(err)
      }
    }

    try {
      Command.run(configPath) // запускаем меню
    } catch {
      case e: Throwable =>
        Logger.error(s"Внутренняя ошибка: ${e.getClass.getName}: ${e.getMessage}.")
        System.err.println(s"Внутренняя ошибка: ${e.getMessage}.")
        sys.exit(1)
    }
  }
}