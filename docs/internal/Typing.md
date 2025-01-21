


Supported scala types:

- Scala Primitives types, mapped to appropriate SIR Primitive types.
- scalus.Builtin.Data, mapped to SIRType.Data
- scala case classes 

Type:
```
package x
case class A(x:BigInt, y:ByteString)
```
should be translated to
```
 CaseClass(ConstrDecl("A",
                      TypeBindings("x"-> TypeInteger, "y" -> TypeByteString), 
                      Nil),
           Nil)
```

Q:  A or x.A or PackagedName("x","A")


Type:
```
sealed trait A
case class B(x:BigInt, y: BigInt) extends A
case class C(x:ByteString, b:B) extends A
case object D extends A
```

should be
```
A:  Type = SumCaseClass(dataDeclA)
  dataDeclA = DataDecl("A",List(consrB,constrC,constrD), Nil, Nil)
B:  Type = CaseClass(constrB)
  constrB = ConstrDecl("B", st, List("x"->Integer,"y"->Integer), Nil)
C:  Type = CaseClass(constrC)
  constrC = ConstrDecl("C", st, List("x"->ByteString,"b"->B.Type), Nil)
D:  Type = CaseClass(constrD)
  constrD = ConstrDecl("D", st, List("x"->ByteString,"y"->ByteString), Nil)

```


Hierarchy
```
 package x

 sealed trait BaseA
 sealed trait ChildA1BaseB extends Base
 case class B1(x: BigInt) extends ChildA1BaseB 
 case class B2(y: BigInt) extends ChildA1BaseB 
 case object C extends BaseA

```
 Now is officially not supported.
 Technically can be translated to

```
  BaseA: dataDeclA = DataDecl("BaseA",List(constrB1,constrB2,constrC),Nil)
     Type: SumCaseClass(dataDeclA)

  ChildA1BaseB: dataDeclA1B = DataDecl("ChildA1BaseB",List(constrB1,constrB2),Nil,Nil)
     Type: SumCaseClass(dataDeclA1B)

  B1: constrB1 = ConstrDecl("B1",List("x"-> TypeInteger), Nil, Nil)
     Type = CaseClass(constrB1,Nil)
  B2: constrB2 = ConstrDecl("B2",List("y"-> TypeInteger), Nil, Nil)
     Type = CaseClass(constrB1,Nil)

  C: constrC = ConstrDecl("C",Nil,Nil,Nil)

```

But here we have a problem, which illustrated by example with type params (later)



 In theory, can be translated to:
```

  BaseA: dataDeclA = DataDecl("BaseA",List(constrA1B,constrC),Nil)
  ChildA1BaseB: 
       constrA1B: ConstrDecl("ChildA1BaseB_BaseA", List("_value" -> ChildA1BaseB)), flag="syntetic"
       dataDeclA1B = DataDecl("ChildA1BaseB",List(constrB1,constrB2),Nil,Nil)
       Type = SumCaseClass(dataDecl1A1B, .. parentConstr = constrA1B)
  B1: constrB1 (same as prev)  
  B2: constrB2 (same as prev)  

```

With type parameters:

```
package x

sealed trait BaseA[X]
sealed trait BaseB[X] extends BaseA[Option[X]]
case class B1(x: BigInt) extends BaseB[BigInt]
case class B2(y: BigInt) extends BaseB[BigInt]
case object C extends BaseA[Nothing]
```

Now if we have class B1, then type-parameter that this is BaseB[BigInt] is lost. 
(and we can put same constr in two places)

Now:
```
  BaseA: dataDeclBaseA = DataDecl("BaseA",List(constrB1,constrB2,constrC),List(TypeVar("X")))
    Type = TypeLambda:X =>> SumCaseClass(dataDeclBaseA,X)
```



Should be:
```
  BaseA: 
    dataDeclBaseA = DataDecl("BaseA",List(constrBaseB,constrC),List(TypeVar("X",1)))
    Type = TypeLambda:X =>> SumCaseClass(dataDeclBaseA,X)

  BaseB: 
    dataDeclBaseB = DataDecl("BaseB",List(constrB1, constrB2), List(TypeVar("X",2)))
    constrB = ConstrDecl("BaseB_BaseA", List("_value" -> ChildA1BaseB), List(TypeVar("X",3), List(`MayBe[X]`)), flag="syntetic"
 
  B1: constrB1 = ConstrDecl("B1",List("x"-> TypeInteger), Nil, List(TypeInteger))
    Type = CaseClass(constrB1)
  B2: constrB2 = ConstrDecl("B2",List("y"-> TypeInteger), Nil, List(TypeInteger))
    Type = CaseClass(constrB2)
  C: constrC = ConstrDecl("C",List("y"-> TypeInteger), Nil, List(TypeInteger))
```





TODO:

 enums

 Type Parameter
