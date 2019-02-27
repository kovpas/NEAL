#!/path/to/kotlin choo-choo

@file:[foo bar]
@file:baz

package com.mu.nism

import foo.f.lo
import foo.foo.*;
import baz;
import bar.ba.pa.pa as loo;

class test<A>

private interface test<A>

private sealed tailrec class test;

class test<A,
            in reified R
            , in out I
            ,
             out o: suspend ((Java.Lib.String)?)?,
            f: suspend receiverType.(param: J.String?,
                                    f: (b: (LalaType)?) -> Unit) -> ReturnType,
            d: dynamic>

class test<f: suspend receiverType.(param: J.String?) -> ReturnType>
class test2<f: (JustType, b: (LalaType)?) -> Unit>
class test3<f: () -> (ParamType) -> Unit>

class test constructor ( enum val prop1: Type1 )

class test private constructor(enum val prop1: Type1 = a || c && b ?: d as e)

class CountingSet(private val delegate: MutableSet<out Nothing, *, in out Any, CoolType<NestedTypeArgument>.Subtype>)
class CountingSet(private val delegate: MutableSet<Long>) : MutableSet<Long> by delegate where A: B<T>, C: (D?) -> Unit
class test: A by delegate where A: B<T>, C: dynamic

class c: a by a
class d {
  class test constructor ( enum val prop1: Type1 )
}

class test private constructor(enum val prop1: Char = '\$', var collectionProp: Coll = [a, b],
                                val stringTest: String = "   tests  ${b as c} another test ${a * 3 + b} $abc",
                                val mlStringTest: String = "a" + """
                                  test stringg
                                  more multilline
                                  ${expression == true}
                                  $variable
                                """) {

                                }

class CountingSet(private val delegate: MutableSet<out Nothing, *, in out Any, CoolType<NestedTypeArgument>.Subtype>) {
}

class compTest {
  companion object Boo {
    class CountingSet(private val delegate: MutableSet<out Nothing, *, in out Any, CoolType<NestedTypeArgument>.Subtype>)
  }
}

class InitOrderDemo(name: String) {
    private constructor(firstName: String = a || c && b ?: d as e, secondName: String<String>): this(arg1, arg2) {
      a = b<T>
      b -= a || b

      while (a != b) {
        a = b
      }

      for ((i, k: String) in 1..5) {
        a = b<T>
        b -= a || b
      }
      A::ident

      if (a != b);
      if (a != b) {}
      if (a != b) {} else;
      if (a != b) else;
      if (a != b) {
        while (a != b) {
          a = b
        }
      } else {}

      { (a: String, b: String): AnotherType, i: Int ->
        a = b<T>
      }

      fun String.(something: Int): MyType<Generic> where MyType: B<M> = b<T>
      fun String.(something: Int): MyType<Generic> where MyType: B<M> { b<T> }

      object: MyString by Delegate {
        private constructor(firstName: String = a || c && b ?: d as e, secondName: String<String>): this(arg1, arg2) {
        }
      }

      object {
        private constructor(firstName: String = (a > c && b >= d), secondName: String<String>): this(arg1, arg2) {
        }
      }

      this
      this@

      super
      super<Type>@identifier
      super<Type>
      super@

      try {
        someting = ba
      } catch (b: Exception1) {
        fun String.(something: Int): MyType<Generic> where MyType: B<M> = b<T>
      } catch (c: Exception2) {
        object {
          private constructor(firstName: String = a || c && b ?: d as e, secondName: String<String>): this(arg1, arg2) {
          }
        }
      } catch (d: Exception3) {
        body1 as d
      } finally {

      }

      when (x) {
          1 -> a
          2, 3..5, "string" -> b
          in 23..197 -> b
          is String<(a: Stirng) -> Unit> -> b
          else -> c
      }

      throw someExpression
      return true
      return@ false
      continue
      continue@
      break
      break@
    }
}

public fun<E> fullName(firstName: String = "John", lastName: String = "Doe"): String {
  return firstName + " " + lastName
}

private object MyClass: A by delegateA, B by delegateB {
  var a: String = "John"
    get() = field
    set(noinline vararg value: String) {
      field = value
    }

  val b: String = "Doe"
    set(noinline vararg value: String) = field
    get() {
      blah = value
    }

  typealias b = c
}


enum class Direction {
    NORTH, SOUTH, WEST, EAST
}
enum class Numbers(val num: Int) {
        HEX(0xFF0000),
        BINARY(0b001100),
        INTEGER(197),
}

enum class ProtocolState {
    WAITING {
        override fun signal() = TALKING
    },

    TALKING {
        override fun signal() = WAITING
    };

    abstract fun signal(): ProtocolState
}