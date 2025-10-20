# Lazy Let and Let-Floating Transformation

## Overview

Let in relaxed ordering (or lazy let) in Scalus SIR is a let expression with lazy flags.
The essence of relaxed ordering: the value is generated before first usage and not generated when it is not needed.

For the V3 lowering backend we handle this via generation of UPLC let's on demand (as in Sea of Code architecture). For Simple backends we use a source-to-source transformation called **Let-Floating**.

This transformation accepts SIR and outputs transformed SIR where lazy let's are:
- Reordered closer to their usage points (floating inward)
- Deleted when the value is not used at all
- Prevented from crossing lambda boundaries when the binding requires computation

## Algorithm

The let-floating transformation works in three phases:

### Phase 1: Enrichment Pass (Down-Traversal)

Traverse the SIR tree from top to bottom, building an enriched representation that tracks:
- **Unique index** for each node
- **Child indices** for each node
- **Used variables** in each subtree (variable name + defining node index)
- **Defined variables** and their dependencies for each Let expression
- **Lambda context**: nearest enclosing lambda for each node
- **Direct usage tracking**: for each variable, track where it's directly referenced
- **Multiple children usage**: variables used in 2+ child subtrees of a node

### Phase 2: Analysis Pass

Using the tracked information from Phase 1, compute the target insertion point for each lazy let binding:

1. **Initial target computation**: For each lazy variable X, find the minimum dominating node where:
   - X is directly used, OR
   - X is used in multiple children (duplication would occur)

2. **Lambda barrier application**: If the binding value is not "effortless", constrain the target to not cross lambda boundaries:
   - **Effortless bindings** (Const, Var, ExternalVar, LamAbs): Can cross lambda boundaries freely
   - **Non-effortless bindings** (Apply, IfThenElse, etc.): Must stay outside of lambdas
   - When crossing would occur, stop at the outermost lambda boundary

3. **Transitive dependency propagation**: Using fixpoint iteration, propagate constraints from dependent variables to their dependencies. If variable Y depends on X, and Y must be placed at node N, then X must also be placed at node N (or shallower) to be available when Y's binding is evaluated.

### Phase 3: Output Generation Pass (Down-Traversal)

Traverse the enriched SIR tree from top to bottom, reconstructing the output SIR:
- When encountering a lazy let with computed target deeper in the tree: float the binding to pending list
- When reaching the target node: insert floated bindings (with renamed variables to avoid shadowing)
- Non-lazy lets are kept in place
- Unused bindings (target = ∞) are eliminated
- Floated bindings are topologically sorted by dependencies

## Lambda Barrier

The lambda barrier is a crucial optimization that prevents work duplication:

```scala
let lazy x = expensiveComputation() in
  lambda y =>
    x + y
```

Without the lambda barrier, `x` would be floated into the lambda body. If the lambda is called multiple times, `expensiveComputation()` would be executed multiple times.

With the lambda barrier, non-effortless bindings stay outside the lambda:
```scala
let x = expensiveComputation() in
  lambda y =>
    x + y
```

However, effortless bindings (constants, variables) can still cross:
```scala
let lazy x = 42 in         // Effortless constant
  lambda y =>
    x + y

// Transforms to:
lambda y =>
  let x = 42 in
    x + y
```

This is safe because copying the constant has no runtime cost.

## Implementation

The implementation is in `scalus-core/shared/src/main/scala/scalus/sir/simpleLowering/LetFloating.scala`.

Tests are in `scalus-core/shared/src/test/scala/scalus/sir/simpleLowering/LetFloatingTest.scala`.

## References

Similar transformation, known as 'Let floating' (or more specifically: 'Floating inward') is described in:

Simon Peyton Jones, Will Partain, and André Santos. 1996. Let-floating: moving bindings to give faster programs. In Proceedings of the first ACM SIGPLAN international conference on Functional programming (ICFP '96). Association for Computing Machinery, New York, NY, USA, 1–12. https://doi.org/10.1145/232627.232630


