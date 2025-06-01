import sn.*

@main def hello(): Unit =
  val parent = Predicate("parent", 2)
  val ancestor = Predicate("ancestor", 2)
  val reachable = Predicate("reachable", 2)
  val link = Predicate("linked", 2)
  val edb: Map[Predicate, Relation] = Map(
    parent -> Set(
      List("Alice", "Bob"),
      List("Bob", "Charlie"),
      List("Charlie", "Delta")
    ),
    link -> Set(
      List("Alice", "Bob"),
      List("Bob", "Charlie"),
      List("Charlie", "Delta")
    ))

  val rules = List(
    Rule(Atom(ancestor, List("x", "y")), List(Atom(parent, List("x", "y")))),
    Rule(Atom(ancestor, List("x", "y")), List(Atom(ancestor, List("x", "z")), Atom(parent, List("z", "y")))),
    Rule(Atom(reachable, List("x", "y")), List(Atom(link, List("x", "y")))),
    Rule(Atom(reachable, List("x", "y")), List(Atom(reachable, List("x", "z")), Atom(link, List("z", "y"))))
  )

  val result = sn.semiNaiveEvaluation(rules, edb)

  println("--- Derived Facts ---")
  for ((pred, facts) <- result) {
    facts.foreach(f => println(s"${pred}(${f.mkString(", ")})"))
  }
