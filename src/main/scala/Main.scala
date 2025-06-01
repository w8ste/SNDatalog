import sn.*

@main def hello(): Unit =

  // Predicate definitions
  val parent = Predicate("parent", 2)
  val ancestor = Predicate("ancestor", 2)

  // edb definitions
  val edb: Map[Predicate, Relation] = Map(
    parent -> Set(
      List("Alice", "Bob"),
      List("Bob", "Charlie"),
      List("Charlie", "Delta")
    ))

  // rule definitions
  val rules = List(
    Rule(Atom(ancestor, List("x", "y")), List(Atom(parent, List("x", "y")))),
    Rule(Atom(ancestor, List("x", "y")), List(Atom(ancestor, List("x", "z")), Atom(parent, List("z", "y")))),
  )

  val result: Map[Predicate, Relation] = sn.semiNaiveEvaluation(rules, edb)

  println("--- Derived Facts ---")
  for ((pred, facts) <- result) {
    facts.foreach(f => println(s"${pred}(${f.mkString(", ")})"))
  }
