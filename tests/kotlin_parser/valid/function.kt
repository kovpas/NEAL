// RUN: %neal-kotlin

fun test1()

fun test2(param1: String);

private noinline fun test3(param1: String, param2: Int): String {}

public fun test4(): String = "Some string"

public fun Int.test5() = 7

public fun More.Than.One<Word>.test6() = 7.5e11