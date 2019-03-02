// RUN: %neal-kotlin

class Test1
class Test1;

class Test2()

class Test3(param1: String,
            param2: Int)

class Test4 constructor(param1: String
                        , param2: Int) {}

private class Test5 internal constructor(param1: String
                                         ,
                                         param2: Int);

sealed data class Test6(val prop1: String, var prop2: Int) {
  constructor(prop3: Double, prop4: Generic<Type>) {}
}

// class name can be a reserved word
class private