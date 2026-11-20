package function_programming_0.model

//описывает правила поиска и способы замены данных

// Правила анонимизации
case class Rule(
                 id: String, // id правила
                 name: String, // название
                 description: String, // описание
                 search: SearchMethod, // способ поиска значений
                 replacement: Replacement, // способ замены значений
                 priority: Int, // приоритет
                 active: Boolean // флаг активации
               )

// Способы поиска данных
sealed trait SearchMethod
object SearchMethod {
  case class Exact(value: String) extends SearchMethod // точное значение
  case class Regex(pattern: String, flags: String = "") extends SearchMethod // регулярное выражение
  case class Dictionary(values: List[String]) extends SearchMethod // значение из словаря
  case class Builtin(kind: String) extends SearchMethod // встроенный тип
}

// Спобобы замены полей
sealed trait Replacement
object Replacement {
  case class Constant(value: String) extends Replacement // константа
  case object Delete extends Replacement // полное удаление
  case class Mask(keepPrefix: Int, keepSuffix: Int, maskChar: Char = '*') extends Replacement // подстановка маски
  case class Alias(prefix: String) extends Replacement // псевдоним
  case class FromList(values: List[String]) extends Replacement // значение из списка
}
