# Building
To build, run `cabal build --enable-tests`. You can then run the tests with `cabal test`

# The assignment
## Works sample 

Below is the technical test. Do not hesitate to ask if there are any questions or if you need some clarifications.


================== Technical Exercise =============================

For this exercise, use haskell. There are 2 parts of this exercise. You're welcome, but not required, to embellish or add extra requirements/functionality.

With the given types:
```
data NodeInfo = NodeInfo
{ cost :: Cost
, nodeInfoName :: NodeName
}

data Tree
= Tree_TypeA NodeInfo
String
[Tree]
| Tree_TypeB TypeB

newtype NodeName =
NodeName String

newtype Cost =
Cost Float

data TypeB =
TypeB Cost
NodeName
[TypeB]

getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa _ _ = undefined

```

1. Implement the function getCommonNodeNamesExceptBepa.
    - The function should return all node names that exists in both the trees (the name does not need to be in the same place in the tree)
    - Names that contains the string "Bepa" (case sensitive) should never be included in the result

2. Implement "property-based tests" to the function getCommonNodeNamesExceptBepa
    - Write generators for all the types described above
    - Identify as many properties as possible and write tests for them
    - See if you can find ways to update the types (possibly with new types) to reduce the number of tests needed to ensure the properties
    - Finally, write a test for the property  "Nodes with the name "bepa" (case insensitive) should never be present in the results"

   This property should fail without changing the implementation.

If you want to use GDP (Ghosts of departed proofs) or Refinement Types, please do. It is not a requirement and not 100% that it is suitable for this task and your solution.

Please work in a git-repo and when you're done, send the link back and I'll check the results.
This is a quite crude specification so if you have any questions or something in the description is unclear/contradictory just ask.

Good luck!

