package function_programming_0.config
import function_programming_0.model._
import function_programming_0.report.Logger
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.DumperOptions
import org.yaml.snakeyaml.DumperOptions.{FlowStyle, ScalarStyle}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Try
import scala.jdk.CollectionConverters._
// загружает, сохраняет и создаёт конфигурацию по умолчанию

object Loader {
  //загрузка конфигурации из yaml файла
  def load(path: Path): Either[String, AppConfig] = {
    if (!Files.exists(path)) {
      Logger.warn(s"Конфигурационный файл не найден: $path")
      return Left(s"Файл не найден: $path")
    }
    Try {
      val text = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      val root = new Yaml().load[java.util.Map[String, Any]](text)
      parseAppConfig(root.asScala.toMap)  //преобразует Java-коллекцию в Scala-коллекцию
    }.toEither.left.map(e => s"Ошибка чтения конфигурации: ${e.getMessage}")
  }

  // запись конфигурации в yaml файл
  def save(path: Path, cfg: AppConfig): Either[String, Path] = {
    Try {
      val yaml = buildYaml()
      val root = appConfigToJava(cfg)
      val text = yaml.dump(root) //формирует yaml текст
      Files.write(path, text.getBytes(StandardCharsets.UTF_8))
    }.toEither.left.map(e => s"Ошибка записи конфигурации: ${e.getMessage}")
  }

  // создание конфигурации по умолчанию
  def createDefaultConfig: AppConfig = {
    val emailRule = Rule(
      id = "builtin.email", name = "Email", description = "Адрес электронной почты",
      search = SearchMethod.Builtin("email"), replacement = Replacement.Alias("EMAIL"), priority = 10, active = true
    )
    val ipRule = Rule(
      id = "builtin.ipv4", name = "IPv4", description = "IPv4-адрес",
      search = SearchMethod.Builtin("ipv4"), replacement = Replacement.Alias("IP"), priority = 10, active = true
    )
    val phoneNumberRule = Rule(
      id = "builtin.phone", name = "Телефон", description = "Номер телефона",
      search = SearchMethod.Builtin("phone"), replacement = Replacement.Alias("PHONE"), priority = 10, active = true
    )
    val tokRule = Rule(
      id = "builtin.token", name = "Токен", description = "Токен или ключ доступа",
      search = SearchMethod.Builtin("token"), replacement = Replacement.Alias("TOKEN"), priority = 20, active = true
    )
    val urlRule = Rule(
      id = "builtin.url", name = "URL", description = "URL-адрес",
      search = SearchMethod.Builtin("url"), replacement = Replacement.Alias("URL"), priority = 10, active = true
    )
    val passwordRule = Rule(
      id = "builtin.password", name = "Пароль", description = "Значение параметра, похожего на пароль",
      search = SearchMethod.Builtin("password"), replacement = Replacement.Constant("***"), priority = 20, active = true
    )
    val basePolicy = Policy(
      name = "base", description = "Стандартная политика со встроенными типами",
      rules = List(emailRule, phoneNumberRule, ipRule, urlRule, passwordRule, tokRule)
    )
    val strictPolicy = Policy(
      name = "strict", description = "Строгая политика: base + усиленная маскировка",
      includes = List("base"),
      rules = List(Rule(
        id = "custom.maskEmail", name = "Маскировать Email",
        description = "Полная маскировка email (переопределяет base)",
        search = SearchMethod.Builtin("email"), replacement = Replacement.Mask(0, 0, '*'), priority = 100, active = true
      ))
    )
    AppConfig(settings = AppSettings(), policies = Map("base" -> basePolicy, "strict" -> strictPolicy))
  }

  // --- YAML parsing helpers ---

  private type YMap = Map[String, Any]

  private def asMap(v: Any): YMap =
    v.asInstanceOf[java.util.Map[String, Any]].asScala.toMap //получение данных из yaml в удобном для scala формате

  private def asList(v: Any): List[Any] = {
    v.asInstanceOf[java.util.List[Any]].asScala.toList //преобразование Java List в Scala List
    // если в YAML указано несколько правил, этот метод позволяет получить их в виде Scala-списка
  }

  //определение способа поиска данных по настройкам YAML
  private def parseSearchMethod(m: YMap): SearchMethod = m("type").toString match {
    case "builtin"    => SearchMethod.Builtin(m("kind").toString) //Встроенный поиск
    case "exact"      => SearchMethod.Exact(m("value").toString) //Поиск точного совпадения
    case "regex"      => SearchMethod.Regex(m("pattern").toString, m.getOrElse("flags", "").toString) //Поиск по регулярному выражению
    case "dictionary" => SearchMethod.Dictionary(asList(m("values")).map(_.toString)) //Поиск по списку значений
    case other        => throw new RuntimeException(s"Unknown search type: $other")
  }

  //определение способа замены найденных данных
  private def parseReplacement(m: YMap): Replacement = m("type").toString match {
    case "alias"    => Replacement.Alias(m("prefix").toString)
    case "constant" => Replacement.Constant(m("value").toString)
    case "delete"   => Replacement.Delete
    case "mask"     => Replacement.Mask(
      m.getOrElse("keepPrefix", 0).asInstanceOf[Int],
      m.getOrElse("keepSuffix", 0).asInstanceOf[Int],
      m.getOrElse("maskChar", "*").toString.headOption.getOrElse('*')
    )
    case "fromList" => Replacement.FromList(asList(m("values")).map(_.toString))
    case other      => throw new RuntimeException(s"Unknown replacement type: $other")
  }

  //создание одного объекта Rule из данных YAML
  private def parseRule(m: YMap): Rule = Rule(
    id          = m("id").toString,
    name        = m("name").toString,
    description = m("description").toString,
    search      = parseSearchMethod(asMap(m("search"))),
    replacement = parseReplacement(asMap(m("replacement"))),
    priority    = m("priority").asInstanceOf[Int],   //приводит значение к типу java Map
    active      = m("active").asInstanceOf[Boolean]
  )

  //создание объекта политики из YAML
  //позволяет превратить описание политики base из YAML в объект Policy.
  private def parsePolicy(key: String, m: YMap): Policy = Policy(
    name        = m.getOrElse("name", key).toString,
    description = m.getOrElse("description", "").toString,
    rules       = m.get("rules").map(r => asList(r).map(v => parseRule(asMap(v)))).getOrElse(Nil),
    includes    = m.get("includes").map(i => asList(i).map(_.toString)).getOrElse(Nil)
  )

  //создание полной конфигурации приложения из разобранного YAML
  //создаёт итоговый объект AppConfig
  private def parseAppConfig(root: YMap): AppConfig = {
    val sm = asMap(root("settings"))
    val settings = AppSettings(
      allowedExtensions = asList(sm("allowedExtensions")).map(_.toString),
      preserveStructure = sm("preserveStructure").asInstanceOf[Boolean],
      outputDir         = sm("outputDir").toString,
      keepAliasMapping  = sm("keepAliasMapping").asInstanceOf[Boolean],
      logFile           = sm("logFile").toString
    )
    val policies = asMap(root("policies")).map { case (key, v) => key -> parsePolicy(key, asMap(v)) }
    AppConfig(settings = settings, policies = policies)
  }

  //----------методы преобразования в yaml

  //создание и настройка YAML-записывателя
  private def buildYaml(): Yaml = {
    val opts = new DumperOptions()
    opts.setDefaultFlowStyle(FlowStyle.BLOCK)
    opts.setDefaultScalarStyle(ScalarStyle.PLAIN)
    opts.setAllowUnicode(true)
    new Yaml(opts)
  }

  //преобразование способа поиска из Scala в Java Map
  private def searchToJava(s: SearchMethod): java.util.Map[String, Any] = {
    val m = new java.util.LinkedHashMap[String, Any]()
    s match {
      case SearchMethod.Builtin(kind)      => m.put("type", "builtin"); m.put("kind", kind)
      case SearchMethod.Exact(value)       => m.put("type", "exact"); m.put("value", value)
      case SearchMethod.Regex(pat, flags)  => m.put("type", "regex"); m.put("pattern", pat); m.put("flags", flags)
      case SearchMethod.Dictionary(values) => m.put("type", "dictionary"); m.put("values", values.asJava)
    }
    m
  }

  //преобразование способа замены из Scala в Java Map
  private def replacementToJava(r: Replacement): java.util.Map[String, Any] = {
    val m = new java.util.LinkedHashMap[String, Any]()
    r match {
      case Replacement.Alias(prefix)      => m.put("type", "alias"); m.put("prefix", prefix)
      case Replacement.Constant(value)    => m.put("type", "constant"); m.put("value", value)
      case Replacement.Delete             => m.put("type", "delete")
      case Replacement.Mask(kp, ks, mc)   => m.put("type", "mask"); m.put("keepPrefix", kp); m.put("keepSuffix", ks); m.put("maskChar", mc.toString)
      case Replacement.FromList(values)   => m.put("type", "fromList"); m.put("values", values.asJava)
    }
    m
  }

  //преобразование одного объекта Rule в Java Map
  private def ruleToJava(rule: Rule): java.util.Map[String, Any] = {
    val m = new java.util.LinkedHashMap[String, Any]()
    m.put("id", rule.id)
    m.put("name", rule.name)
    m.put("description", rule.description)
    m.put("search", searchToJava(rule.search))
    m.put("replacement", replacementToJava(rule.replacement))
    m.put("priority", rule.priority)
    m.put("active", rule.active)
    m
  }

  //преобразование объекта Policy в Java Map
  private def policyToJava(policy: Policy): java.util.Map[String, Any] = {
    val m = new java.util.LinkedHashMap[String, Any]()
    m.put("name", policy.name)
    m.put("description", policy.description)
    m.put("includes", policy.includes.asJava)
    m.put("rules", policy.rules.map(ruleToJava).asJava)
    m
  }

  //преобразование всей конфигурации приложения в Java Map
  private def appConfigToJava(cfg: AppConfig): java.util.Map[String, Any] = {
    val s = cfg.settings
    val settingsMap = new java.util.LinkedHashMap[String, Any]()
    settingsMap.put("allowedExtensions", s.allowedExtensions.asJava)
    settingsMap.put("preserveStructure", s.preserveStructure)
    settingsMap.put("outputDir", s.outputDir)
    settingsMap.put("keepAliasMapping", s.keepAliasMapping)
    settingsMap.put("logFile", s.logFile)

    val policiesMap = new java.util.LinkedHashMap[String, Any]()
    cfg.policies.foreach { case (key, policy) => policiesMap.put(key, policyToJava(policy)) }

    val root = new java.util.LinkedHashMap[String, Any]()
    root.put("settings", settingsMap)
    root.put("policies", policiesMap)
    root
  }
}
