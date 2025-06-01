

class Predicate(val name: String, val arity: Int):
    override def toString(): String = s$"name"


case class Atom(val predicate: Predicate, val termList: List[String])
case class Rule(val head: Atom, val body: List[Atom])

type Fact     = List[String]
type Relation = Set[Fact]
n
