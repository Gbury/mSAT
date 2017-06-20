
# Equality in McSat

## Basics

McSat theories have different interfaces and requirements than classic SMT theories.
The good point of these additional requirements is that it becomes easier to combine
theories, since the assignments allow theories to exchange information about
the equality of terms. In a context where there are multiple theories, they each have
to handle the following operations:

- return an assignment value for a given term
- receive a new assignment value for a term (the assignment may, or not, have been
  done by another theory)
- receive a new assertion (i.e an atomic formula asserted to be true by the sat solver)

With assignments, the reason for a theory returning UNSAT now becomes when
some term has no potential assignment value because of past assignments
and assertions, (or in some cases, an assignments decided by a theory A is
incompatible with the possible assignments of the same term according to theory B).

When returning UNSAT, the theory must, as usual return a conflict clause.
The conflict clause must be a tautology, and such that every atomic proposition
in it must evaluate to false using assignments.


## Equality of uninterpreted types

To handle equality on arbitrary values efficiently, we maintain a simple union-find
of known equalities (*NOT* computing congruence closure, only the reflexive-transitive
closure of the equalities), where each class can be tagged with an optional assignment.

When receiving a new assertions by the sat, we update the union-find. When the theory is
asked for an assignment value for a term, we lookup its class. If it is tagged, we return
the tagged value. Else, we take an arbitrary representative $x$ of the class and return it.
When a new assignment $t \mapsto v$ is propagated by the sat solver, there are three cases:

- the class of $t$ if not tagged, we then tag it with $t \mapsto v$ and continue
- the class of $t$ is already tagged with $\_ mapsto v$, we do nothing
- the class of $t$ is tagged with a $t' \mapsto v'$, we raise unsat,
  using the explanation of why $t$ and $t'$ are in the same class and the equality
  $t' = v'$

Additionally, in order to handle disequalities, each class contains the list of classes
it must be distinct from. There are then two possible reasons to raise unsat, when
a disequality $x <> y$ is invalidated by assignemnts or later equalities:

- when two classes that should be distinct are merged
- when two classes that should be distinct are assigned to the same value

in both cases, we use the union-find structure to get the explanation of why $x$ and $y$
must now be equal (since their class have been merged), and use that to create the
conflict clause.


## Uninterpreted functions

The uninterpreted function theory is much simpler, it doesn't return any assignemnt values
(the equality theory does it already), but rather check that the assignemnts so far are
coherent with the semantics of uninterpreted functions.

So for each function asignment $f(x1,...,xn) \mapsto v$, we wait for all the arguments to
also be assigned to values $x1 \mapsto v1$, etc... $xn \mapsto vn$, and we add the binding
$(f,v1,...,vn) \mapsto (v,x1,...,xn)$ in a map (meaning that in the model $f$ applied to
$v1,...,vn$ is equal to $v$). If a binding $(f,v1,...,vn) \mapsto (v',y1,...,yn)$ already
exists (with $v' <> v$), then we raise UNSAT, with the explanation:
$( x1=y1 /\ ... /\ xn = yn) => f(x1,...,xn) = f(y1,...,yn)$

