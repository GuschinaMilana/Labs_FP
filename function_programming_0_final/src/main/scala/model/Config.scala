package function_programming_0.model
// хранит настройки приложения

// Настройки программы
case class AppSettings(
                        allowedExtensions: List[String] = List(".txt", ".log", ".md", ".csv"), // доступные для анализа расширения файлов
                        preserveStructure: Boolean = true, // флаг - соранение структуры файловой системы
                        outputDir: String = "output", // название папки для преобразованных файлов
                        keepAliasMapping: Boolean = false, // сохранение словаря строка - псевдоним
                        logFile: String = "pdremover.log" // название логирующего файла
                      )

// Конфигурация программы
case class AppConfig(
                      settings: AppSettings = AppSettings(), // настройки программы
                      policies: Map[String, Policy] = Map.empty // хранилище политик название - сам объект
                    )

