
  | Lesschar (a, b) -> semlesschar (sem a, sem b)
  | Eqchar (a, b) -> semeqchar (sem a, sem b)
  | Or (b1, b2) -> semor (sem b1, sem b2)
  | And (b1, b2) -> semand(sem b1, sem b2)
  | Not (b) -> semnot (sem b)
  | Ifthenelse(a, b, c) -> semifthenelse(sem a,sem b,sem c)
;;

Ifthenelse (Ebool(2==2),Eint(3),Eint(4));;
