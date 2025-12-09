(*
Exercice 8. D´efinir un type de donn´e date comme ´etant une valeur de type int * int * int.
Les exemples de triplet de type date incluent (5, 6, 2024) et (0, 0, 1000). Une date est un triplet
de type date dont la premi`ere partie est un jour entre 1 et 31 (ou 30, 29 ou 28, selon le mois
et l’ann´ee), la deuxi`eme partie est un mois entre 1 et 12 et la troisi`eme partie est une ann´ee
positive (c’est-`a-dire une ann´ee de l’`ere commune), Donc . (5, 6, 2024) est une date ; (0, 0, 1000)
ne l’est pas.
1. Ecrire trois fonctions pour extraire le jour, le mois, l’ann´ee.
2. Ecrire une fonction pour d´eterminer si une date est valide (Attention aux ann´ee bis￾sextiles : les ann´ees sont bissextiles si elles sont multiples de quatre, mais pas si elles
sont multiples de cent, `a l’exception des ann´ees multiples de quatre cents qui, elles, sont
´egalement bissextiles.)
3. Ecrire une fonction ´ is_before qui prend deux dates en entr´ee et ´evalue `a true ou false.
Elle ´evalue `a true si le premier argument est une date qui pr´ec`ede le deuxi`eme argument.
Si les deux dates sont identiques, le r´esultat est false. Utiliser les type option pour g´erer
aussi les dates non valides.

*)
type date = int * int * int

let jour (j,_,_) = j
let mois (_,m,_) = m
let annee (_,_,a) = a


let () = assert (jour (5, 6, 2024) = 5)
let () = assert (mois (5, 6, 2024) = 6)
let () = assert (annee (5, 6, 2024) = 2024)

let est_bissextile a =
  (a mod 4 = 0 && a mod 100 <> 0) || (a mod 400 = 0)

let () = assert (est_bissextile 2020 = true)   (* normal leap year *)
let () = assert (est_bissextile 2000 = true)   (* divisible by 400 *)
let () = assert (est_bissextile 1900 = false)  (* divisible by 100, not 400 *)
let () = assert (est_bissextile 2023 = false)

let date_est_valide (j, m, a) =
  if a <= 0 || m < 1 || m > 12 || j < 1 then false
  else
    let max_jour =
      match m with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | 4 | 6 | 9 | 11 -> 30
      | 2 -> if est_bissextile a then 29 else 28
      | _ -> assert false
    in
    j <= max_jour



let () = assert (date_est_valide (1, 1, 2024) = true)
let () = assert (date_est_valide (31, 1, 2024) = true)
let () = assert (date_est_valide (30, 4, 2024) = true)
let () = assert (date_est_valide (29, 2, 2020) = true)  (* leap year *)
let () = assert (date_est_valide (28, 2, 2023) = true)

let () = assert (date_est_valide (0, 1, 2024) = false)
let () = assert (date_est_valide (32, 1, 2024) = false)
let () = assert (date_est_valide (31, 4, 2024) = false)  (* April has 30 *)
let () = assert (date_est_valide (29, 2, 2023) = false)  (* non leap year *)


let is_before d1 d2 =
  if not (date_est_valide d1) || not (date_est_valide d2) then None
  else
    let j1, m1, a1 = d1 in
    let j2, m2, a2 = d2 in
    if a1 < a2 then Some true
    else if a1 > a2 then Some false
    else if m1 < m2 then Some true
    else if m1 > m2 then Some false
    else if j1 < j2 then Some true
    else Some false


let () = assert (is_before (0,1,2023) (1,1,2023) = None)
let () = assert (is_before (1,1,2023) (32,1,2023) = None)
let () = assert (is_before (29,2,2023) (1,3,2023) = None)


let () = assert (is_before (1,1,2023) (2,1,2023) = Some true)
let () = assert (is_before (31,1,2023) (1,2,2023) = Some true)
let () = assert (is_before (1,12,2023) (1,1,2024) = Some true)

let () = assert (is_before (2,1,2023) (1,1,2023) = Some false)
let () = assert (is_before (1,3,2024) (1,2,2024) = Some false)
let () = assert (is_before (1,1,2025) (31,12,2024) = Some false)


let () = assert (is_before (10, 10, 2023) (10, 10, 2023) = Some false)


let () = assert (is_before (28,2,2020) (29,2,2020) = Some true)
let () = assert (is_before (29,2,2020) (1,3,2020) = Some true)
