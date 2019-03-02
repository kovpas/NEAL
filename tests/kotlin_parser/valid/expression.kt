// RUN: %neal-kotlin

var test1 = a || b;

var test2 = a || b || (c < 3 && c > 4 && c <= 4 && c >= 7)

var test3 = a || true && (d && e == false || e === false || e != true || e !== true)

var test4 = a ?: true ?: null ?: 'a'

var test5 = 5 in 6..123 || 6 !in 23.5.."string" || 12 is Float || 7 !is String

fun wrap() {
  test6 += "string"
  test7 -= 1
  test8 /= 6
  test9 *= 0
  test10 %= 1
  1 + 2
    - 3 /
    4   *
     (5)
}
