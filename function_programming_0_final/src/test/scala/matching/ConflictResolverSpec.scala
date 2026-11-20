package function_programming_0.matching

import function_programming_0.model.{Replacement, Rule, SearchMethod}
import org.scalatest.funsuite.AnyFunSuite

class java -versionConflictResolverSpec extends AnyFunSuite {
  private def matchAt(id: String, start: Int, end: Int, priority: Int): Match = {
    val rule = Rule(
      id = id,
      name = id,
      description = "test rule",
      search = SearchMethod.Exact(id),
      replacement = Replacement.Constant("[REDACTED]"),
      priority = priority,
      active = true
    )
    Match(rule, start, end, id)
  }

  test("keeps a match outside the selected winner even in a transitive overlap group") {
    val first = matchAt("first", 0, 5, 10)
    val middle = matchAt("middle", 3, 8, 5)
    val last = matchAt("last", 7, 10, 4)

    val result = ConflictResolver.resolve(List(first, middle, last))

    assert(result.accepted == List(first, last))
    assert(result.conflicts.isEmpty)
  }

  test("reports an unresolved tie between different rules") {
    val first = matchAt("first", 0, 5, 10)
    val second = matchAt("second", 2, 7, 10)

    val result = ConflictResolver.resolve(List(first, second))

    assert(result.accepted.isEmpty)
    assert(result.conflicts.size == 1)
    assert(result.conflicts.head.matches.toSet == Set(first, second))
  }

  test("prefers higher priority for overlapping matches") {
    val lower = matchAt("lower", 0, 6, 10)
    val higher = matchAt("higher", 2, 8, 20)

    val result = ConflictResolver.resolve(List(lower, higher))

    assert(result.accepted == List(higher))
    assert(result.conflicts.isEmpty)
  }
}
