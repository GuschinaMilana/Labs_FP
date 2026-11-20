package function_programming_0.matching
import function_programming_0.report.Logger
// "Разрешатель" конфликтов совпадений правил для полей текста
// объект конфликта
case class Conflict(start: Int, end: Int, matches: List[Match])
// результат решения конфликта
case class ResolutionResult(
                             accepted: List[Match], // "победившие" совпадения
                             conflicts: List[Conflict] // нерешённые конфликты
                           )

object ConflictKiller {
  // Решаем пересечения внутри одной связной группы. После выбора победителя
  // рассматриваем оставшиеся совпадения, которые с ним не пересекаются.
  def resolve(all: List[Match]): ResolutionResult = {
    val sorted = all.sortBy(m => (m.start, -m.end, -m.rule.priority, m.rule.id))
    val accepted = scala.collection.mutable.ArrayBuffer.empty[Match]
    val conflicts = scala.collection.mutable.ArrayBuffer.empty[Conflict]

    def overlaps(left: Match, right: Match): Boolean =
      left.start < right.end && right.start < left.end

    def resolveGroup(group: List[Match]): Unit = {
      var remaining = group

      while (remaining.nonEmpty) {
        if (remaining.size == 1) {
          accepted += remaining.head
          remaining = Nil
        } else {
          val maxPriority = remaining.map(_.rule.priority).max
          val byPriority = remaining.filter(_.rule.priority == maxPriority)
          val maxLength = byPriority.map(m => m.end - m.start).max
          val finalists = byPriority.filter(m => m.end - m.start == maxLength)

          val winner = if (finalists.size == 1) {
            Some(finalists.head)
          } else if (finalists.map(_.rule.id).distinct.size == 1) {
            // Одно правило может вернуть дублирующиеся совпадения одного участка.
            Some(finalists.head)
          } else {
            None
          }

          winner match {
            case Some(selected) =>
              accepted += selected
              // Не удаляем всю связную группу: исключаем только реальные
              // пересечения с победителем, остальные кандидаты решаем дальше.
              remaining = remaining.filterNot(m => m == selected || overlaps(selected, m))
            case None =>
              val start = remaining.map(_.start).min
              val end = remaining.map(_.end).max
              val ruleIds = finalists.map(_.rule.id).distinct
              conflicts += Conflict(start, end, remaining)
              Logger.warn(s"Конфликт совпадений в диапазоне [$start,$end), правила: ${ruleIds.mkString(", ")}")
              remaining = Nil
          }
        }
      }
    }

    var index = 0
    while (index < sorted.length) {
      val group = scala.collection.mutable.ArrayBuffer(sorted(index))
      var next = index + 1
      var groupEnd = sorted(index).end

      // Собираем все транзитивно пересекающиеся интервалы.
      while (next < sorted.length && sorted(next).start < groupEnd) {
        group += sorted(next)
        groupEnd = math.max(groupEnd, sorted(next).end)
        next += 1
      }

      resolveGroup(group.toList)
      index = next
    }

    ResolutionResult(accepted.sortBy(m => (m.start, m.end, m.rule.id)).toList, conflicts.toList)
  }
}
