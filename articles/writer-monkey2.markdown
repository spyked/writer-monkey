să scriem împreună un generator de text markov (ii)
===================================================

În cadrul [articolului introductiv](TODO-link) am prezentat pe scurt ideea de a
implementa un generator de text Markov ca exercițiu pur didactic, am explicat
câteva din conceptele teoretice fundamentale pe care se constituie aplicația și
nu în ultimul rând am definit o structură de date în Haskell, structură care se
mapează unu la unu pe cea a unui lanț Markov. Mai departe voi da un exemplu de
construcție (non-algoritmică) a unui obiect de tipul `Chain`, după care vom
porni spre a programa un simulator de procese Markov, definit printr-o
interfață oarecare, fixă, interfață care la rândul ei se constituie pe baza
unor funcții Haskell.

O primă și importantă funcționalitate a acestei interfețe o reprezintă aceea de
construire a lanțurilor Markov. Modulul
[`Data.Map`](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html
"Data.Map") oferă tot soiul de modalități de construire a dicționarelor,
printre care inserarea, reuniunea etc. Dat fiind că în programarea funcțională
cea mai naturală metodă de a reprezenta chestii este lista, noi vom defini o
funcție `fromList`, care se va folosi de omoloaga din `Data.Map` pentru a
construi lanțuri Markov din asocieri stare-listă (de tupluri
stare-probabilitate):

<p lang="haskell">
fromList :: Ord a => [(a, [(a, Float)])] -> Chain a
fromList = M.fromList
</p>

Signatura de tip pare un pic încurcată, o ocazie numai bună pentru a o descurca
noi. În primul rând, restricția `Ord a` impune ca tipul `a` să aibă definită o
relație de ordine totală ((Sau mai bine zis o așa-zisă „ordonare naturală”.
Pentru mai multe detalii consultați documentația asociată [clasei de tipuri
Ord](http://www.haskell.org/ghc/docs/6.10.2/html/libraries/base/Data-Ord.html
"Data.Ord").)), din rațiuni de implementare a tipului de date dicționar în
Haskell. În al doilea rând că lista primită ca argument e o listă de asocieri
cheie-valoare, asocieri ale cărei valori sunt la rândul lor liste. Întâmplător,
listele în cauză conțin și ele asocieri cheie-valoare (cheia fiind starea și
valoarea probabilitatea), ceea ce face structura noastră de date, într-un
anumit sens, un dicționar de dicționare. Nu am folosit însă `Map` pentru lista
de tupluri stare-probabilitate deoarece aceasta e oricum parcursă de la un cap
la celălalt, căutarea fiind în acest caz o funcționalitate cvasi-inutilă.

**Exercițiu** **(p2.1)**: Redefiniți câmpul din tipul `Chain` asociat valorilor
dicționarului, folosind alias-uri de tip - hint: tipul `Chain` este însuși un
alias de tip. Îmbunătățirea este una pur estetică, oferind programatorului o
înțelegere mai bună asupra codului.

Având astfel la dispoziție o modalitate de a construi lanțuri Markov prin
explicitarea nodurilor și a arcelor, să definim un modul Haskell nou în care să
creăm o valoare de tipul `Chain Weather`, unde `Weather` e tipul explicitat în
cele ce urmează:

<p lang="haskell" line="1">
module Markov.Examples where

import Markov.Chain

-- possible states of the weather
data Weather = Sunny | Rainy deriving (Eq, Ord, Show)

-- a "Weather Markov chain"
weatherChain :: Chain Weather
weatherChain = fromList [
    (Sunny, [(Sunny, 0.6), (Rainy, 0.4)]),
    (Rainy, [(Sunny, 0.7), (Rainy, 0.3)])
    ]
</p>

Observăm că am definit un tip nou de date `Weather`, cu două valori posibile,
`Sunny` și `Rainy`. De asemenea am definit lanțul Markov din exemplul dat în
articolul anterior, exprimabil prin graful:

**TODO**: link to Markov chain graph.

unde $latex A \equiv \text{Sunny}$ și $latex E \equiv \text{Rainy}$.

Mai departe avem nevoie de o funcție care să întoarcă lista stărilor accesibile
dintr-o stare dată, primind un lanț Markov ca valoare. Forma funcției poate fi
dată imediat prin folosirea funcției
[`lookup`](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html#v%3Alookup
"Data.Map: lookup"):

<p lang="haskell">
accessibleStates :: Ord a => Chain a -> a -> [(a, Float)]
accessibleStates c s = case M.lookup s c of
    Just accs -> accs
    Nothing -> []
</p>

Observăm că funcția în sine nu ne este foarte utilă. Pentru programator, scopul
final al definirii unui lanț Markov este acela de a-l folosi pentru simularea
proceselor cu același nume. Cu alte cuvinte dorim să plecăm dintr-o stare dată
(aleasă după criterii date sau aleator) și să executăm așa-zise „[plimbări
aleatoare](http://en.wikipedia.org/wiki/Random_walk "Random walk")” prin graful
dat. Utilitatea funcției `accessibleStates` e aici aceea de furnizor de
distribuție de probabilități pentru o stare dată, urmând ca mai departe să
folosim distribuția asociată stării curente pentru a alege aleator următoarea
stare. Pentru aceasta vom da signatura funcției `next`:

<p lang="haskell">
next :: Ord a => Chain a -> a -> IO a
</p>

`next` primește ca argumente un lanț Markov și o stare a acestuia și întoarce
următoarea stare „înfășurată” în monada `IO` ((Tipul `IO` e un tip ceva mai
dubios din Haskell, care îi permite utilizatorului să facă tot soiul de chestii
practice precum citirea de la tastatură, scrierea într-un fișier sau desenarea
unei interfețe grafice. Întâmplător generarea unui număr aleator presupune
folosirea facilităților oferite de sistemul de operare, iar starea următoare
fiind aleatoare nu poate fi furnizată către utilizator decât sub această formă. 

Aici discuția se poate întinde asupra motivului pentru care am ales să
proiectez astfel această funcție și alte considerente, discuție pe care o putem
muta într-un articol separat la cerere. Pe moment, cei interesați de monade în
general și de IO în particular pot consulta [Wiki-ul
Haskell](http://www.haskell.org/haskellwiki/IO_inside "IO inside").)). Vom
folosi funcția pentru a defini o așa-zisă mașină cu stări, care să se plimbe
prin graf. Mașina, pe care o vom numi `randomWalk`, va fi principala metodă de
a simula un proces Markov, constituindu-se deci drept un element important al
interfeței oferită de modulul `Markov.Chain`; de asemenea orice plimbare va
avea un număr bine determinat de pași, motiv pentru care mai avem nevoie de un
argument (pe lângă lanțul Markov și starea inițială) pentru a defini noua
funcție; în plus, `randomWalk` va întoarce nu o stare, ci o listă de stări
reprezentând toate stările parcurse de la începutul plimbării și până la
încheierea acesteia. Mai jos puteți observa definiția funcției `randomWalk`:

<p lang="haskell">
randomWalk :: Ord a => Chain a -> a -> Int -> IO [a]
randomWalk c s n = if n <= 0
    then return []
    else do
        ns <- next c s
        rw' <- randomWalk c ns (n - 1)
        return $ s : rw'
</p>

Forma este tipică programării funcționale: dacă mai am de mers zero (sau mai
puțini) pași înseamnă că plimbarea s-a terminat, deci întorc o listă vidă.
Altfel iau următoarea stare pornind din starea curentă și merg mai departe. De
întors întorc starea curentă plus restul stărilor întoarse de apelul recursiv
al lui `randomWalk`.

Observați însă că am omis să dau definiția lui `next`, și am făcut-o
intenționat deoarece aceasta necesită o discuție un pic mai amplă. `next`
trebuie să facă două lucruri: să genereze un număr aleator între 0 și 1
(asociat unei probabilități) și să îl folosească pentru a alege starea
următoare. Prima parte poate fi comisă cu ajutorul funcției
[`randomRIO`](http://hackage.haskell.org/packages/archive/random/1.0.0.3/doc/html/System-Random.html#g:4
"Random values of various types"). A doua parte e un pic mai complicată și
necesită două funcții auxiliare, după cum se poate vedea în definiția de mai
jos:

<p lang="haskell">
next c s = do
    rn <- randomRIO (0, 1)
    return $ next' rn 0 $ sortByProb $ accessibleStates c s
</p>

În primul rând `next` ia stările accesibile din starea curentă și le sortează
după probabilitate, folosind funcția `sortByProb`:

<p lang="haskell">
sortByProb :: [(a, Float)] -> [(a, Float)]
sortByProb = sortBy (.<.)
    where
    sp .<. sp' = snd sp' `compare` snd sp
</p>

Apoi `next'` ia lista de stări sortată și face un sampling după [algoritmul
ruletei](http://en.wikipedia.org/wiki/Fitness_proportionate_selection "Roulette
selection"), care ia cea mai probabilă stare și verifică dacă numărul generat
se încadrează în aceasta. Dacă da atunci o selectează, altfel adună
probabilitatea asociată stării-țintă eșuată și verifică din nou, folosind
următoarea stare ca probabilitate și așa mai departe. Codul pentru `next'` este
dat în următorul paragraf:

<p lang="haskell">
    next' _ _ [] = error "No states available."
    next' _ _ (sp : []) = fst sp
    next' rn acc ((s, p) : sps) = -- sample
        if rn <= acc + p
            then s
            else next' rn (acc + p) sps
</p>

Parametrul `acc` e folosit pentru „incrementarea” ruletei, cu observația că nu
se fac verificări legate de depășirea valorii 1. Cu alte cuvinte nu se verifică
normalizarea distribuției de probabilitate, această povară fiind lăsată pe
seama utilizatorului. Altfel și `next'` e tipică programării funcționale,
neexistând aspecte deosebite în legătură cu aceasta.

TODO: conclusions and exercises
