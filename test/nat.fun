Unit := for(A : Type)(x : A). A
unit := lam A x. x

Nat := exi(b : Bool). if b Nat Unit
zero := pair false unit

main := exi(n : Nat). n == zero