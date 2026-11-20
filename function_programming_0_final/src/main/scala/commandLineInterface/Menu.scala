package function_programming_0.commandLineInterface
import function_programming_0.config.Loader
import function_programming_0.io.{FileProcessor, FileScanner, Selection}
import function_programming_0.model._
//import function_programming_0.model.PolicyValidator
import function_programming_0.report.{Logger, Report}
import model.{PolicyResolver, PolicyChecker, ResolvedPolicy}

import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime
import scala.io.StdIn
import scala.util.Try

// Пользовательский интерфейс программы менюшка
class Menu(initialConfigPath: Path) { //в конструкторе путь к файлу конфигурации
  private var pthCfg: Path = initialConfigPath //переменная с путём конфигурации
  private var conf: AppConfig = Loader.load(pthCfg).getOrElse(Loader.createDefaultConfig) // используем пользовательский конфиг, если его нет - конфиг по умолчанию

  // сохранение файла конфигурации
  // в conf хранится текущая конфигурация
  private def save(): Unit = {
    Loader.save(pthCfg, conf) match {
      case Right(_) => Logger.info(s"Конфигурация сохранена в папку: $pthCfg.")
      case Left(err) => Logger.error(err); println(err) // при ошибке в сохраняемом конфиге
    }
  }

  // повторная загрузка конфигурации
  private def reload(): Unit = {
    Loader.load(pthCfg) match {
      case Right(c) => conf = c; Logger.info("Конфигурация перезагружена.")
      case Left(e) => Logger.error(e); println(e) // при ошибке в загружаемом конфиге
    }
  }

  //метод для запуска пользовательского меню
  def run(): Unit = {
    Logger.init(Paths.get(conf.settings.logFile)) // инициализируем журнал
    Logger.info(s"Загружена конфигурация: $pthCfg.") //записываем в журнал информацию о загруженной конфигурации
    var keepRunning = true //true пока пользователь не выберет выход
    while (keepRunning) {
      printMainMenu()
      val pick = StdIn.readLine("> ").trim //считывание команды пользователя
      pick match {
        case "1" => listPolicies()
        case "2" => showPolicy()
        case "3" => createPolicy()
        case "4" => editPolicy()
        case "5" => deletePolicy()
        case "6" => composePolicy()
        case "7" => validateConfig()
        case "8" => processFiles()
        case "9" => previewMatches()
        case "10" => reload()
        case "11" => save()
        case "0" => keepRunning = false
        case other =>
          println(s"Неизвестная команда: $other.")
          Logger.warn(s"Неизвестная команда (ввод не логируется).")
      }
    }
    Logger.close() // закрываем журнал при выходе из цикла - конец программы
  }

  // вывод меню
  private def printMainMenu(): Unit = {
    println(
      """
        Для выбора действия введите цифру из меню:
        1 - Показать политики
        2 - Показать политику и правила
        3 - Создать новую политику
        4 - Изменить существующую политику
        5 - Удалить политику
        6 - Составить политику из существующих
        7 - Проверить конфигурацию системы
        8 - Обработать файлы
        9 - Предварительный просмотр совпадений
        10 - Перезагрузить конфигурацию
        11 - Сохранить конфигурацию
        0 - Выход из программы
        |""".stripMargin)
  }


  // вывод списка политик, которые есть в конфигурации
  private def listPolicies(): Unit = {
    println("Список всех политик:")
    conf
      .policies.toList
      .sortBy(_._1)
      .foreach { case (polName, curPol) => //сортируем политики по id
      // вывод информации о каждой политике
      println(s" - $polName: ${curPol.description} (правила=${curPol.rules.size}, включения=${curPol.includes.mkString(",")})")
    }
  }

  // показывает политику, которую введёт пользователь
  private def showPolicy(): Unit = {
    val polName = StdIn.readLine("Введите имя политики: ").trim
    conf.policies.get(polName) match { // поиск политики по имени
      case None => println(s"Политика с именем $polName не найдена.")
      case Some(curPol) => //найденную политику кладём в curPol
        println(s"Политика '$polName': ${curPol.description}")
        if (curPol.includes.nonEmpty) println(s"Включает: ${curPol.includes.mkString(", ")}")
        println("Собственные правила:")
        curPol.rules.foreach { r => //выводим все собственные правила политики
          println(s" [${if (r.active) "x" else " "}] id=${r.id} name=${r.name} prio=${r.priority}")
          println(s" desc: ${r.description}")
          println(s" search: ${r.search}")
          println(s" replace: ${r.replacement}")
        }
    }
  }

  // создание политики
  private def createPolicy(): Unit = {
    val polName = StdIn.readLine("Введите имя новой политики: ").trim
    if (polName.isEmpty) {
      println("Имя для политики не введено."); return
    }
    if (conf.policies.contains(polName)) {
      println("Политика с таким именем уже существует."); return
    }
    val polDesc = StdIn.readLine("Введите описание политики: ").trim
    conf = conf.copy(policies = conf.policies + (polName -> Policy(polName, polDesc))) // добавляем новую политику к существующим
    // copy создаёт копию объекта с изменённым полем
    // + добавляет новую пару в коллекцию
    //polName -> Policy(...) создаёт пару имя политики — объект политики
    Logger.info(s"Создана политика: $polName.") //запись событие создания в журнал
    println("Для добавления правил или включения других политик перейдите в режим 'Изменить политику'.")
    save()
  }

  // редактирование существующей политики
  private def editPolicy(): Unit = {
    val polName = StdIn.readLine("Введите имя политики для изменения: ").trim
    conf.policies.get(polName) match { //проверка существования политики
      case None => println(s"Политика с именем $polName не найдена.")
      case Some(curPol) =>
        println("Введите одну из цифр для выбора соответствущей опции:")
        println("1 - Изменить описание")
        println("2 - Добавить правило")
        println("3 - Удалить правило")
        println("4 - Переключить активность правила")
        println("5 - Добавить включение (include)")
        println("6 - Удалить включение (include)")
        StdIn.readLine("> ").trim match { //считываем выбор и проверям с помощью match
          case "1" =>
            val newDesc = StdIn.readLine("Введите новое описание: ").trim
            //Создаём копию политики с новым описанием, а затем создаём копию конфигурации с обновлённой политикой
            conf = conf.copy(policies = conf.policies + (polName -> curPol.copy(description = newDesc)))
            println("Описание политики успешно изменено.")
          case "2" =>
            readRule() match { // сборка правила из ввода пользователя
              case Some(r) => //если правило создано, получаем его в переменную r
                if (curPol.rules.exists(_.id == r.id)) println(s"Правило с id ${r.id} уже есть.")
                else {
                  conf = conf.copy(policies = conf.policies + (polName -> curPol.copy(rules = curPol.rules :+ r))) // новое правило добавляем в конец списка
                  Logger.info(s"Добавлено правило ${r.id} в политику $polName/")
                }
              case None => println("Правило не добавлено.")
            }
          case "3" =>
            val ruleId = StdIn.readLine("Введите id правила: ").trim
            conf = conf.copy(policies = conf.policies + (polName -> curPol.copy(rules = curPol.rules.filterNot(_.id == ruleId)))) // в конфиг заносим все правила, кроме этого
            Logger.info(s"Удалено правило $ruleId из политики $polName.")
          case "4" =>
            val ruleId = StdIn.readLine("Введите id правила: ").trim
            // map проходит по всем правилам и создаёт обновлённый список.
            conf = conf.copy(policies = conf.policies + (polName -> curPol.copy(rules = curPol.rules.map(r => if (r.id == ruleId) r.copy(active = !r.active) else r))))
            Logger.info(s"Переключена активность правила $ruleId в $polName.")
          case "5" =>
            val incName = StdIn.readLine("Введите имя включаемой политики: ").trim
            conf.policies.get(incName) match {
              case None => println(s"Политика с именем $polName не найдена.")
              case Some(innerPol) =>
                if (incName.nonEmpty && !innerPol.includes.contains(incName)) //проверяем что имя не пустое и что политика ещё не включена в список её собственных включений
                  //добавляем имя другой политики в список includes текущей политики.
                  conf = conf.copy(policies = conf.policies + (polName -> curPol.copy(includes = curPol.includes :+ incName)))
                Logger.info(s"Политика с имененм $incName включена в политику с имененм $polName.")
            }
          case "6" =>
            val incName = StdIn.readLine("Введите имя исключаемой политики: ").trim
            conf.policies.get(incName) match {
              case None => println(s"Политика с именем $polName не найдена.")
              case Some(_) => //если политика существует, её объект нам не нужен, поэтому используется _
                conf = conf.copy(policies = conf.policies + (polName -> curPol.copy(includes = curPol.includes.filterNot(_ == incName)))) //Удаляем имя указанной политики из списка включений
                Logger.info(s"Политика с имененм $incName исключена из политики с имененм $polName.")
            }
          case _ => println("Неизвестный режим.")
        }
        save() // сохраняем изменение
    }
  }

  // удаление политики
  private def deletePolicy(): Unit = {
    val polName = StdIn.readLine("Введите имя политики для удаления: ").trim
    if (conf.policies.contains(polName)) {
      conf = conf.copy(policies = conf.policies - polName)
      Logger.info(s"Удалена политика: $polName.")
      save()
    } else println(s"Политика с именем $polName не найдена.")
  }

  // Создание составной политики из нескольких уже существующих политик
  private def composePolicy(): Unit = {

    // Запрашиваем у пользователя название новой составной политики.
    // trim() удаляет лишние пробелы в начале и конце введённой строки.
    val polName = StdIn.readLine("Введите имя новой составной политики: ").trim

    // Проверяем, что название не пустое и политика с таким именем ещё не существует.
    // Если хотя бы одно условие нарушено, выводим сообщение и завершаем метод.
    if (polName.isEmpty || conf.policies.contains(polName)) {
      println("Некорректное или уже существующее имя.")
      return
    }

    // Показываем пользователю названия всех политик,
    // которые можно включить в новую составную политику.
    // keys получает ключи коллекции, а mkString объединяет их через запятую.
    println("Доступные для включения политики: " + conf.policies.keys.mkString(", "))

    // Получаем от пользователя названия политик, которые нужно объединить.
    // split(",") разделяет введённую строку по запятым.
    // map(_.trim) убирает пробелы вокруг каждого названия.
    // filter(_.nonEmpty) удаляет пустые элементы.
    // toList преобразует полученные значения в список.
    val incList = StdIn.readLine("Введите имена включаемых политик через запятую: ")
      .split(",").map(_.trim).filter(_.nonEmpty).toList

    // Проверяем, существуют ли все политики, указанные пользователем.
    // filterNot оставляет только те названия, которых нет в конфигурации.
    val notFound = incList.filterNot(conf.policies.contains)

    // Если найдены несуществующие политики, сообщаем об этом
    // и прекращаем создание составной политики.
    if (notFound.nonEmpty) {
      println(s"Не найдены политики, вследствие чего не будут добавлены: ${notFound.mkString(", ")}.")
      return
    }

    // Запрашиваем описание новой составной политики.
    val polDesc = StdIn.readLine("Введите описание составляемой политики: ").trim

    // Создаём новую версию конфигурации с добавленной составной политикой.
    // copy() создаёт копию конфигурации с изменённым полем policies.
    // Оператор + добавляет новую пару: название политики и её объект.
    // Policy создаётся с пустым списком собственных правил (Nil),
    // а incList содержит названия включённых политик.
    conf = conf.copy(
      policies = conf.policies + (polName -> Policy(polName, polDesc, Nil, incList))
    )

    // Записываем в журнал информацию о создании составной политики
    // и перечисляем политики, которые в неё включены.
    Logger.info(s"Создана составная политика: $polName (включения=${incList.mkString(",")}).")

    // Сохраняем обновлённую конфигурацию в файл.
    save()
  }

  // Создание нового правила на основе данных, введённых пользователем.
  // Метод используется при редактировании политики для добавления правила.
  // Option[Rule] означает, что метод может вернуть созданное правило (Some)
  // или отсутствие результата (None), если создание не удалось.
  private def readRule(): Option[Rule] = {

    // Выводим сообщение о начале создания нового правила.
    println("Создание нового правила")

    // Запрашиваем уникальный идентификатор правила.
    // trim() удаляет лишние пробелы.
    val ruleId = StdIn.readLine("Введите id правила (уникальный): ").trim

    // Если пользователь не ввёл идентификатор, прекращаем создание правила.
    // return None означает, что результат создания отсутствует.
    if (ruleId.isEmpty) return None

    // Запрашиваем название и описание нового правила.
    val ruleName = StdIn.readLine("Введите имя правила: ").trim
    val ruleDesc = StdIn.readLine("Введите описание правила: ").trim

    // Показываем доступные способы поиска данных в тексте.
    println("Укажите тип поиска (введите цифру с желаемой опцией):")
    println("1 - Точное значение")
    println("2 - Регулярное выражение")
    println("3 - Словарь значений")
    println("4 - Встроенный тип данных")

    // Считываем выбранный способ поиска и сохраняем его в переменную srchMeth.
    // SearchMethod описывает различные варианты поиска персональных данных.
    val srchMeth: SearchMethod = StdIn.readLine("> ").trim match {

      // Exact выполняет поиск конкретного значения, которое вводит пользователь.
      case "1" =>
        SearchMethod.Exact(StdIn.readLine("Введите значение: ").trim)

      // Regex выполняет поиск с помощью регулярного выражения.
      // Пользователь вводит само выражение и дополнительный флаг.
      // Например, флаг i позволяет выполнять поиск без учёта регистра.
      case "2" =>
        SearchMethod.Regex(
          StdIn.readLine("Введите выражение: ").trim,
          StdIn.readLine("Флаг (напр. i для регистронезависимости): ").trim
        )

      // Dictionary выполняет поиск значений из заданного пользователем списка.
      // Значения разделяются символом |.
      // split("\\|") разделяет строку по этому символу.
      // map(_.trim) удаляет лишние пробелы,
      // filter(_.nonEmpty) убирает пустые значения,
      // toList преобразует результат в список.
      case "3" =>
        SearchMethod.Dictionary(
          StdIn.readLine("Введите значения через | : ")
            .split("\\|").map(_.trim).filter(_.nonEmpty).toList
        )

      // Builtin использует один из заранее предусмотренных типов поиска.
      // Например: email, phone, ipv4, url, password или token.
      case "4" =>
        println("Имеющиеся встроенные типы: email, phone, ipv4, url, password, token")
        SearchMethod.Builtin(StdIn.readLine("Введите необходимый тип: ").trim)

      // Если введён неизвестный номер, прекращаем создание правила.
      case _ => return None
    }

    // Показываем пользователю доступные способы замены найденных данных.
    println("Введите необходимый тип замены (выберите нужную цифру):")
    println("1 - Замена константой")
    println("2 - Удаление поля")
    println("3 - Маскирование")
    println("4 - Замена псевдонимом")
    println("5 - Замена значением из списка")

    // Считываем выбранный способ замены и сохраняем его в replMeth.
    // Replacement описывает варианты изменения найденного фрагмента текста.
    val replMeth: Replacement = StdIn.readLine("> ").trim match {

      // Constant заменяет найденное значение на постоянный текст,
      // который пользователь вводит вручную.
      case "1" =>
        Replacement.Constant(StdIn.readLine("Введите значение: ").trim)

      // Delete означает полное удаление найденного фрагмента.
      case "2" =>
        Replacement.Delete

      // Mask маскирует часть найденного значения.
      case "3" =>

        // Запрашиваем количество символов в начале строки,
        // которые должны остаться видимыми.
        // Try безопасно преобразует введённую строку в число.
        // Если преобразование не удалось, используется значение 0.
        val pfxLen = Try(
          StdIn.readLine("Введите кол-во символов, которое не будет маскироваться в начале: ").trim.toInt
        ).getOrElse(0)

        // Аналогично запрашиваем количество символов в конце строки,
        // которые должны остаться видимыми.
        // При некорректном вводе также устанавливается 0.
        val sfxLen = Try(
          StdIn.readLine("Введите кол-во символов, которое не будет маскироваться в конце: ").trim.toInt
        ).getOrElse(0)

        // Создаём способ замены с помощью маскирования.
        // Передаём количество сохраняемых символов в начале и конце.
        Replacement.Mask(pfxLen, sfxLen)

      // Alias заменяет исходное значение псевдонимом.
      // Пользователь задаёт начало (префикс) будущего псевдонима.
      case "4" =>
        Replacement.Alias(StdIn.readLine("Введите начало псевдонима: ").trim)

      // FromList заменяет найденное значение одним из значений
      // заранее подготовленного списка.
      // Элементы списка разделяются символом |.
      case "5" =>
        Replacement.FromList(
          StdIn.readLine("Введите значения через | : ")
            .split("\\|").map(_.trim).filter(_.nonEmpty).toList
        )

      // Если пользователь ввёл неизвестный вариант замены,
      // прекращаем создание правила.
      case _ => return None
    }

    // Запрашиваем приоритет правила.
    // Он используется, в частности, при разрешении конфликтов совпадений.
    // Try преобразует введённое значение в число,
    // а при ошибке устанавливается приоритет 0.
    val prioVal = Try(
      StdIn.readLine("Введите приоритет правила в виде числа (по умолчанию 0): ").trim.toInt
    ).getOrElse(0)

    // Уточняем, должно ли правило быть активным.
    // toLowerCase переводит ответ в нижний регистр.
    // startsWith("y") проверяет, начинается ли ответ с буквы y.
    // Если да, правило считается активным.
    val isActive = StdIn.readLine("Ваше правило активно на данный момент? (y/n): ")
      .trim.toLowerCase.startsWith("y")

    // Создаём объект Rule и передаём ему все собранные данные:
    // ID, название, описание, способ поиска, способ замены,
    // приоритет и состояние активности.
    // Some означает, что правило успешно создано.
    Some(Rule(ruleId, ruleName, ruleDesc, srchMeth, replMeth, prioVal, isActive))
  }

  // Проверка текущей конфигурации программы на наличие ошибок.
  private def validateConfig(): Unit = {

    // Передаём текущую конфигурацию в PolicyChecker.
    // Метод validate проверяет её и возвращает список обнаруженных проблем.
    val issues = PolicyChecker.validate(conf)

    // Если список проблем пустой, конфигурация считается корректной.
    if (issues.isEmpty) {
      println("Конфигурация корректна.")
    } else {

      // Если ошибки или другие проблемы найдены,
      // выводим заголовок и перебираем все элементы списка.
      println("Обнаружены проблемы:")
      issues.foreach(e => println(" - " + e))
    }

    // Записываем в журнал результат проверки и количество проблем.
    Logger.info(s"Проверка конфигурации: ошибок=${issues.size}.")
  }

  // Выбор источника обработки: один файл, несколько файлов или каталог.
  // Метод используется в previewMatches() и processFiles().
  // Option означает, что выбор может быть отменён или оказаться некорректным.
  // Возвращается пара: выбранный источник Selection и дополнительный путь к каталогу.
  private def chooseSelection(): Option[(Selection, Option[Path])] = {

    // Показываем пользователю доступные варианты выбора источника.
    println("Выберите источник (введите подходящую цифру):")
    println("1 - Один файл")
    println("2 - Несколько файлов (вводятся через ;)")
    println("3 - Каталог (без вложенных папок)")
    println("4 - Каталог (с вложенными папками)")

    // Считываем выбор пользователя и проверяем его с помощью match.
    StdIn.readLine("> ").trim match {

      // Вариант 1: обработка одного файла.
      case "1" =>

        // Запрашиваем путь к файлу и преобразуем строку в объект Path.
        val singlePath = Paths.get(StdIn.readLine("Введите путь: ").trim)

        // Создаём Selection.SingleFile и возвращаем его в Some.
        // None означает, что дополнительный путь к каталогу не требуется.
        Some(Selection.SingleFile(singlePath) -> None)

      // Вариант 2: обработка нескольких файлов.
      case "2" =>

        // Получаем пути к файлам, разделённые точкой с запятой.
        // split(";") разделяет введённую строку.
        // map(_.trim) убирает лишние пробелы.
        // filter(_.nonEmpty) удаляет пустые элементы.
        // map(Paths.get(_)) преобразует каждую строку в Path.
        // toList формирует список путей.
        val manyPaths = StdIn.readLine("Введите пути через ; : ")
          .split(";").map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toList

        // Создаём Selection.MultipleFiles со списком выбранных файлов.
        // Дополнительный путь к каталогу здесь не используется.
        Some(Selection.MultipleFiles(manyPaths) -> None)

      // Вариант 3: обработка каталога без вложенных папок.
      case "3" =>

        // Запрашиваем путь к каталогу и преобразуем его в Path.
        val dirPath = Paths.get(StdIn.readLine("Введите каталог: ").trim)

        // recursive = false запрещает рекурсивный поиск во вложенных папках.
        // Возвращаем выбранный каталог и сохраняем его как дополнительный путь.
        Some(Selection.Directory(dirPath, recursive = false) -> Some(dirPath))

      // Вариант 4: обработка каталога вместе со всеми вложенными папками.
      case "4" =>

        // Получаем путь к каталогу.
        val dirPath = Paths.get(StdIn.readLine("Введите каталог: ").trim)

        // recursive = true включает рекурсивный поиск во вложенных папках.
        Some(Selection.Directory(dirPath, recursive = true) -> Some(dirPath))

      // Если введён неизвестный номер, возвращаем None.
      case _ => None
    }
  }

  // Выбор политики, которую пользователь хочет применить к файлам.
  // Метод используется при предварительном просмотре и обработке файлов.
  // Возвращает название политики в Some или None, если она не найдена.
  private def selectPolicy(): Option[String] = {

    // Выводим названия всех доступных политик из конфигурации.
    println("Доступные политики: " + conf.policies.keys.mkString(", "))

    // Запрашиваем название политики и удаляем лишние пробелы.
    val polName = StdIn.readLine("Введите имя политики: ").trim

    // Проверяем, существует ли политика с указанным названием.
    if (conf.policies.contains(polName)) {
      // Если политика существует, возвращаем её название.
      Some(polName)
    } else {
      // Если политика не найдена, сообщаем об этом и возвращаем None.
      println("Политика не найдена.")
      None
    }
  }

  // Разрешение составной политики и вывод информации о переопределениях.
  // Метод используется перед предварительным просмотром и обработкой файлов.
  // Возвращает готовую политику типа ResolvedPolicy либо None при ошибке.
  private def resolveOrReport(name: String): Option[ResolvedPolicy] = {

    // Вызываем PolicyResolver для получения итогового набора правил.
    // Передаём название выбранной политики и коллекцию всех политик.
    PolicyResolver.resolve(name, conf.policies) match {

      // Right означает, что политика успешно разрешена.
      case Right(r) =>

        // Перебираем все переопределения, обнаруженные при сборке политики,
        // и выводим информацию о каждом из них.
        r.overrides.foreach(o => println("Переопределение: " + o))

        // Возвращаем готовую политику в Some.
        Some(r)

      // Left означает, что при разрешении политики произошла ошибка.
      case Left(e) =>
        println("Ошибка: " + e)
        None
    }
  }

  // Предварительный анализ файлов и просмотр найденных совпадений.
  // Этот метод позволяет увидеть результаты поиска до анонимизации.
  private def previewMatches(): Unit = {

    // Предлагаем выбрать политику.
    // Если выбор не состоялся, завершаем метод.
    val pol = selectPolicy().getOrElse(return)

    // Разрешаем выбранную политику и получаем её итоговые правила.
    // Если возникла ошибка, прекращаем выполнение.
    val resPol = resolveOrReport(pol).getOrElse(return)

    // Предлагаем выбрать файлы или каталог для анализа.
    // srcSel содержит выбранный источник.
    // Второе значение здесь не используется, поэтому обозначено символом _.
    val (srcSel, _) = chooseSelection().getOrElse(return)

    // Запускаем поиск подходящих файлов.
    // Передаём источник, разрешённые расширения файлов и папку результатов.
    val fileList = FileScanner.scan(
      srcSel,
      conf.settings.allowedExtensions,
      Paths.get(conf.settings.outputDir)
    )

    // Выводим количество найденных файлов перед началом анализа.
    println(s"Найдено файлов: ${fileList.size}. Анализ...")

    // Перебираем каждый найденный файл и анализируем его.
    fileList.foreach { f =>

      // FileProcessor.analyze ищет совпадения по правилам политики.
      // Результат может быть успешным (Right) или содержать ошибку (Left).
      FileProcessor.analyze(f, resPol.rules) match {

        // Если анализ завершился успешно, получаем его результат в prev.
        case Right(prev) =>

          // Выводим имя анализируемого файла.
          println(s"\n- Файл: ${f.getFileName}.")

          // Показываем количество найденных совпадений и конфликтов.
          println(s"  Найдено совпадений: ${prev.matches.size}, конфликтов: ${prev.conflicts.size}.")

          // Берём первые 20 совпадений, чтобы не перегружать вывод.
          prev.matches.take(20).foreach { m =>

            // Не показываем само найденное значение, чтобы не раскрывать
            // потенциально конфиденциальные данные пользователя.
            // Выводим только ID правила, позиции и длину совпадения.
            println(s"    [${m.rule.id}] позиция ${m.start}-${m.end} (len=${m.end - m.start}).")
          }

          // Если совпадений больше 20, сообщаем о количестве остальных.
          if (prev.matches.size > 20) {
            println(s"    ... и ещё ${prev.matches.size - 20}.")
          }

        // Если анализ файла завершился ошибкой, выводим её.
        case Left(e) =>
          println(s"- Файл ${f.getFileName}: $e.")
      }
    }
  }

  // Полная обработка выбранных файлов: анализ, просмотр результатов,
  // подтверждение пользователя, анонимизация и создание отчёта.
  private def processFiles(): Unit = {

    // Сначала пользователь выбирает политику.
    // Если политика не выбрана, выполнение прекращается.
    val pol = selectPolicy().getOrElse(return)

    // Разрешаем выбранную политику и получаем итоговый набор правил.
    val resPol = resolveOrReport(pol).getOrElse(return)

    // Выбираем файлы или каталог для обработки.
    // srcSel хранит источник, baseDir — исходный каталог, если он выбран.
    val (srcSel, baseDir) = chooseSelection().getOrElse(return)

    // Получаем абсолютный нормализованный путь к папке результатов.
    // toAbsolutePath() преобразует путь в абсолютный.
    // normalize() убирает лишние элементы пути, например точки и переходы к родительским папкам.
    val outDir = Paths.get(conf.settings.outputDir).toAbsolutePath.normalize()

    // Находим подходящие файлы с помощью FileScanner.
    // Передаём выбранный источник, разрешённые расширения и папку результатов.
    val fileList = FileScanner.scan(
      srcSel,
      conf.settings.allowedExtensions,
      outDir
    )

    // Если подходящих файлов нет, сообщаем об этом и завершаем метод.
    if (fileList.isEmpty) {
      println("Нет подходящих файлов.")
      return
    }

    // Предварительный анализ всех найденных файлов перед анонимизацией.
    println(s"Найдено файлов: ${fileList.size}. Анализ...")

    // Анализируем каждый файл и сохраняем пару:
    // сам файл и результат его анализа.
    // map проходит по всем элементам списка и создаёт новый список результатов.
    val scans = fileList.map(f => f -> FileProcessor.analyze(f, resPol.rules))

    // collect выбирает только успешно проанализированные файлы.
    // Right(p) означает, что анализ завершился без ошибки.
    val goodScans = scans.collect {
      case (f, Right(p)) => f -> p
    }

    // Отдельно собираем файлы, при анализе которых возникли ошибки.
    // Left(e) содержит информацию об ошибке.
    val failedScans = scans.collect {
      case (f, Left(e)) => f -> e
    }

    // Суммируем количество совпадений во всех успешно проанализированных файлах.
    // _._2 обращается ко второму элементу пары, то есть к результату анализа.
    val cntMatches = goodScans.map(_._2.matches.size).sum

    // Аналогично суммируем количество конфликтов во всех файлах.
    val cntConflicts = goodScans.map(_._2.conflicts.size).sum

    // Выводим общую статистику предварительного анализа.
    println(s"Всего совпадений: $cntMatches, конфликтов: $cntConflicts.")

    // Предлагаем пользователю посмотреть подробности по одному из файлов.
    println("Показать детали по файлу? Введите номер файла или нажмите Enter для пропуска: ")

    // zipWithIndex добавляет к каждому успешно проанализированному файлу его индекс.
    // i начинается с нуля, поэтому при отображении ниже прибавляем единицу.
    goodScans.zipWithIndex.foreach { case ((f, p), i) =>

      // Выводим номер файла, его имя и количество совпадений и конфликтов.
      // %3d задаёт ширину поля для красивого выравнивания номера.
      println(f"  ${i + 1}%3d) ${f.getFileName} совпадения=${p.matches.size} конфликты=${p.conflicts.size}")
    }

    // Считываем номер файла, подробности которого хочет посмотреть пользователь.
    // Если пользователь просто нажмёт Enter, подробности выводиться не будут.
    val selIdx = StdIn.readLine("Введите номер или нажмите Enter для пропуска: ").trim

    // Проверяем, что пользователь действительно ввёл номер.
    if (selIdx.nonEmpty) {

      // Try безопасно преобразует строку в число.
      // toOption превращает результат в Option.
      // filter проверяет, что номер находится в диапазоне существующих файлов.
      // foreach выполняется только для корректного номера.
      Try(selIdx.toInt).toOption
        .filter(i => i >= 1 && i <= goodScans.size)
        .foreach { i =>

          // Получаем выбранный файл и результат его анализа.
          // Вычитаем единицу, потому что индексы списка начинаются с нуля.
          val (f, p) = goodScans(i - 1)

          // Выводим путь к выбранному файлу.
          println(s"Файл: $f.")

          // Показываем все найденные совпадения:
          // ID правила, начальную и конечную позиции, длину фрагмента.
          p.matches.foreach { m =>
            println(s"  [${m.rule.id}] позиция=${m.start}-${m.end} длина=${m.end - m.start}")
          }

          // Показываем конфликты:
          // позиции пересекающихся совпадений и ID правил, которые их обнаружили.
          p.conflicts.foreach { c =>
            println(s"  конфликт позиция=${c.start}-${c.end}: ${c.matches.map(_.rule.id).mkString(",")}")
          }
        }
    }

    // Запрашиваем подтверждение перед фактической анонимизацией.
    // Это позволяет пользователю отменить операцию после предварительного анализа.
    val confirm = StdIn.readLine("Применить политику и создать копии? (y/n): ")
      .trim.toLowerCase

    // Если ответ не начинается с y, отменяем обработку и завершаем метод.
    if (!confirm.startsWith("y")) {
      println("Операция отменена.")
      return
    }

    // Создаём выходную папку, если она ещё не существует.
    Files.createDirectories(outDir)

    // Запоминаем дату и время начала обработки.
    // Эти данные понадобятся для формирования итогового отчёта.
    val tStart = LocalDateTime.now()

    // Записываем в журнал начало обработки и количество успешно проанализированных файлов.
    Logger.info(s"Начало обработки политики '$pol', файлов=${goodScans.size}.")

    // Выполняем фактическую обработку каждого успешно проанализированного файла.
    // map создаёт список результатов обработки.
    val procResults = goodScans.map { case (f, p) =>

      // Передаём результат анализа и настройки обработки в FileProcessor.
      // p — результат предварительного анализа.
      // outDir — папка, куда будут записаны обработанные файлы.
      // baseDir — исходный каталог для сохранения структуры папок.
      // preserveStructure — настройка сохранения структуры каталогов.
      // keepAliasMapping — настройка сохранения соответствий псевдонимов.
      FileProcessor.process(
        p,
        outDir,
        baseDir,
        conf.settings.preserveStructure,
        conf.settings.keepAliasMapping
      )
    }

    // Запоминаем время окончания обработки.
    val tEnd = LocalDateTime.now()

    // Записываем в журнал количество файлов, обработанных без ошибок.
    // count подсчитывает результаты, у которых поле error пустое.
    Logger.info(s"Обработка завершена: успешно=${procResults.count(_.error.isEmpty)}.")

    // Формируем итоговый отчёт об операции.
    // Передаём использованную политику, время начала и окончания,
    // результаты обработки, количество файлов с ошибками анализа и выходную папку.
    val rpt = Report.build(
      resPol,
      tStart,
      tEnd,
      procResults,
      failedScans.size,
      outDir
    )

    // Отображаем готовый отчёт в консоли.
    println(rpt.render)

    // Создаём путь к файлу report.txt внутри выходной папки.
    val rptPath = outDir.resolve("report.txt")

    // Записываем текст отчёта в файл в кодировке UTF-8.
    Files.write(rptPath, rpt.render.getBytes("UTF-8"))

    // Сообщаем пользователю, где сохранён итоговый отчёт.
    println(s"Отчёт об операции сохранён в папке: $rptPath.")
  }
}