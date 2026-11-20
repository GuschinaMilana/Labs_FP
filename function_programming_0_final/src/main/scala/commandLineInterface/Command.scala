package function_programming_0.commandLineInterface

import java.nio.file.Path
// Точка входа в программу
object Command {
  def run(configPath: Path): Unit = new Menu(configPath).run()
}
