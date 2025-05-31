package sn

case class Predicate(val name: String, val arity: Int)
case class Atom(val predicate: Predicate, val termList: List[String])
case class Rule(val head: Atom, val body: List[Atom])

type Fact     = List[String]
type Relation = Set[Fact]

// path(X, Y) -> Atom(relation: Predicate(name: "path", arity: 2), termList: [String(name: "X"), String(name: "Y")])

def join(lhs: Set[Map[String, String]], rhs: Set[Map[String, String]]): Set[Map[String, String]] = 
  for {
    l <- lhs
    r <- rhs
    if l.keySet.intersect(r.keySet).forall(k => l(k) == r(k))
  } yield l ++ r

def evaluateAtom(atom: Atom, db: Map[Predicate, Relation]): Set[Map[String, String]] = {
  val rel = db.getOrElse(atom.predicate, Set())
  rel.map { fact => 
    atom.termList.zip(fact).toMap
  }
}

def evaluateRule(rule: Rule, db: Map[Predicate, Relation]): Set[Fact]= 
  val initial: Set[Map[String, String]] = Set(Map())
  val joined: Set[Map[String, String]] = rule.body.foldLeft(initial) { (acc, atom) =>
    val litEval = evaluateAtom(atom, db)
    join(acc, litEval)
  }

  joined.map(m => project(m, rule.head.termList))

def project(mapping: Map[String, String], termList: List[String]): Fact = 
  termList.map(t => mapping(t))



def semiNaiveEvaluation(rules: List[Rule], edb: Map[Predicate, Relation]): Map[Predicate, Relation] = {
    // 2. S^0 to an empty set, for each idb predicate S
    val idbPreds = rules.map(_.head.predicate).toSet
    var idb: Map[Predicate, Relation] = idbPreds.map(p => p -> Set.empty[Fact]).toMap
    var delta: Map[Predicate, Relation] = idb


    // 1. Set P' to be the rules in P without a idb predicate in the body
    // 3. delta^1_s to P'(I)(S), for each idb predicate S
    for (rule <- rules if rule.body.forall(l => !idbPreds.contains(l.predicate))) {
      val facts = evaluateRule(rule, edb)
      idb = idb.updated(rule.head.predicate, idb(rule.head.predicate) ++ facts)
      delta = delta.updated(rule.head.predicate, delta(rule.head.predicate) ++ facts)
    }

    // we can skip 4. as it is we dont need a round counter

    var deltaNotEmpty = true
    
    // 5. Iterate whilst delta^i_s is not empty
    while (deltaNotEmpty) {
      // Collect all new facts derived in this round
      val newDelta = scala.collection.mutable.Map[Predicate, Relation]().withDefaultValue(Set())
      for (rule <- rules if rule.body.exists(l => idbPreds.contains(l.predicate))) {
        val body = rule.body
        for (i <- body.indices if idbPreds.contains(body(i).predicate)) {
          val bodyDb = body.indices.map { j =>
            val atom: Atom = body(j)
            if (j == i) atom.predicate -> delta(atom.predicate)
            else if (idbPreds.contains(atom.predicate)) atom.predicate -> idb(atom.predicate)
            else atom.predicate -> edb.getOrElse(atom.predicate, Set())
          }.toMap

          // evaluate and subtract existing facts
          val facts = evaluateRule(rule, bodyDb)
          val newFacts = facts -- idb(rule.head.predicate)

          // delta^i+1_S := new inferences
          if (newFacts.nonEmpty)
            newDelta(rule.head.predicate) = newDelta(rule.head.predicate) ++ newFacts
        }
      }

      // check for loop condition
      deltaNotEmpty = newDelta.exists(_._2.nonEmpty)

      // 5. S^i := S^i-1 union delta^i_S
      for ((predicate, facts) <- newDelta) {
        idb = idb.updated(predicate, idb(predicate) ++ facts)
      }

      delta = newDelta.toMap.withDefaultValue(Set())
    }

    // 6. Return S^i for each idb predicate S
    idb
  }

  
