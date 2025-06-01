package sn

class Predicate(val name: String, val arity: Int):
    override def toString(): String = s"$name"

case class Atom(val predicate: Predicate, val termList: List[String])
case class Rule(val head: Atom, val body: List[Atom])

type Fact     = List[String]
type Relation = Set[Fact]

/**
 * Performs a natural join between two sets of Map[String, String]
 *
 * A natural join combines pairs of maps from `lhs` and `rhs` where all keys that exist
 * in both maps have the same corresponding values. The result is a new set containing
 * the merged maps from `lhs` and `rhs` that satisfy this condition.
 *
 * @param lhs The left-hand set of records to join.
 * @param rhs The right-hand set of records to join.
 * @return A set of merged maps where overlapping keys have equal values.
 */
def join(lhs: Set[Map[String, String]], rhs: Set[Map[String, String]]): Set[Map[String, String]] = 
  for {
    l <- lhs
    r <- rhs
    if l.keySet.intersect(r.keySet).forall(k => l(k) == r(k))
  } yield l ++ r

/**
 * Evaluates a logical atom against a database and returns all matching substitutions.
 *
 * The atom consists of a predicate and a list of terms (constants or variables).
 * The database maps each predicate to a relation (set of facts), where each fact
 * is a list of strings corresponding to the predicate's arity.
 *
 * For each fact in the relation associated with the atom's predicate, this function
 * pairs the terms in the atom with the values in the fact, producing a substitution
 * (represented as a `Map[String, String]`).
 *
 * @param atom The atom to evaluate, containing a predicate and a list of terms.
 * @param db A database mapping predicates to relations (sets of fact tuples).
 * @return A set of substitutions mapping terms to corresponding fact values.
 */
def evaluateAtom(atom: Atom, db: Map[Predicate, Relation]): Set[Map[String, String]] = {
  val rel = db.getOrElse(atom.predicate, Set())
  rel.map { fact => 
    atom.termList.zip(fact).toMap
  }
}

/**
 * Evaluates a Datalog rule against a database and derives a set of facts.
 *
 * A rule consists of a head atom and a body (a list of atoms). This function
 * evaluates the rule body against the database to find variable bindings
 * that satisfy all atoms (i.e., the conjunction). It then applies those bindings
 * to the head of the rule to produce new facts.
 *
 * @param rule The rule to evaluate, consisting of a head and a body of atoms.
 * @param db The database mapping predicates to relations (sets of fact tuples).
 * @return A set of derived facts (tuples) for the rule's head predicate.
 */
def evaluateRule(rule: Rule, db: Map[Predicate, Relation]): Set[Fact]= 
  val initial: Set[Map[String, String]] = Set(Map())
  val joined: Set[Map[String, String]] = rule.body.foldLeft(initial) { (acc, atom) =>
    val litEval = evaluateAtom(atom, db)
    join(acc, litEval)
  }

  joined.map(m => project(m, rule.head.termList))

/**
 * Projects a substitution mapping onto a list of terms to produce a fact.
 *
 * Each term in the list is assumed to be a variable present in the substitution map.
 * The result is a fact (list of strings) where each element is the value associated
 * with the corresponding term in the mapping.
 *
 * @param mapping A substitution mapping variables to their values.
 * @param termList A list of terms (typically variables) to be substituted.
 * @return A fact (list of strings) obtained by applying the substitution to each term.
 */
def project(mapping: Map[String, String], termList: List[String]): Fact = 
  termList.map(t => mapping(t))


/**
 * Performs semi-naive evaluation of a set of Datalog rules over an extensional database (EDB).
 *
 * The semi-naive evaluation strategy incrementally derives new facts (IDB - intensional database)
 * by avoiding redundant re-derivation. It tracks the "delta" — the set of facts newly derived
 * in each iteration — and uses it to compute only new derivations in subsequent rounds.
 *
 * This function assumes rules are stratified and that recursion is well-founded.
 *
 * @param rules The list of Datalog rules to evaluate. Each rule defines how to derive new facts.
 * @param edb The extensional database: a map from predicates to known base facts (relations).
 * @return A map from each intensional predicate (IDB) to the set of facts derived for it.
 */
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

  
