# la-methode-des-tableaux.

La méthode des tableaux est une solution simple permettant de déterminer la satisfaisabilité d’une formule sans nécessairement construire l’intégralité 
d’une table de vérité. La formule est traitée en simulant la construction d’une forme clausale disjonctive (et non conjonctive) 
afin de déterminer si chaque clause obtenue est contradictoire. Si une clause (conjonctive) est satisfaisable, alors la formule l’est également. 
Le but de ce projet est d’implanter cet algorithme en OCaml ainsi qu’une méthode permettant de tester empiri- quement vos résultats à l’aide de formules construites (pseudo-)aléatoirement.
