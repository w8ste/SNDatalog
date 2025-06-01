## SNDatalog


## How to Define Your Datalog Program

To specify your Datalog facts and rules, define the following in your Scala file (e.g., `main.scala`):

```scala
// Predicate definitions
val parent = Predicate("parent", 2)
val ancestor = Predicate("ancestor", 2)

// EDB (Extensional Database) definitions
val edb: Map[Predicate, Relation] = Map(
  parent -> Set(
    List("Alice", "Bob"),
    List("Bob", "Charlie"),
    List("Charlie", "Delta")
  )
)

// Rule definitions
val rules = List(
  Rule(Atom(ancestor, List("x", "y")), List(Atom(parent, List("x", "y")))),
  Rule(Atom(ancestor, List("x", "y")), List(
    Atom(ancestor, List("x", "z")),
    Atom(parent, List("z", "y"))
  ))
)
```

- Use `Predicate(name: String, arity: Int)` to declare your predicates.
- `edb` maps predicates to sets of facts (`Relation` is expected to be a `Set[List[String]]`).
- `rules` is a list of `Rule(head: Atom, body: List[Atom])`.



### Build Instructions
* You need a recent installation of
  1. [Scala3](https://docs.scala-lang.org/getting-started/install-scala.html)
  2. [sbt](https://www.scala-sbt.org/)
* Clone this repository by running:
  `git clone https://github.com/w8ste/SNDatalog`

  And then, from the cloned directory
  * compile and run with:
  `sbt run`
