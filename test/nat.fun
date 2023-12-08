Unit := for(A : Type)(x : A). A
unit := lam A x. x

Nat := exi(b : Bool). if b Nat Unit
zero := pair false unit
succ := lam n. pair true n

impl := lam A B. for(x : A). B
and := lam A B. for(b : Bool). if b A B
or := lam A B. exi(b : Bool). if b A B

sum := lam n m k.
    or
        (and (n == zero) (m == k))
        (exi(j : Nat)(r : Nat). Unit)

main := sum zero zero zero