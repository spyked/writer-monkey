să scriem împreună un generator de text markov (iv)
===================================================

[markov-iii]: TODO-link
[markov-ii]: http://lucian.mogosanu.ro/bricks/sa-scriem-impreuna-un-generator-de-text-markov-ii "să scriem împreună un generator de text markov (ii)"
[lsa]: http://en.wikipedia.org/wiki/Latent_semantic_analysis "Latent semantic analysis"
[free-monoid]: http://en.wikipedia.org/wiki/Free_monoid "Free monoid"
[monkey-util-processing]: https://github.com/spyked/writer-monkey/blob/article-base/Monkey/Util/Processing.hs "Monkey.Util.Processing"
[monkey-analyzer]: https://github.com/spyked/writer-monkey/blob/article-base/Monkey/Analyzer.hs "Monkey.Analyzer"
[catamorphism]: http://en.wikipedia.org/wiki/Catamorphism "Catamorphism"

Am stabilit în cadrul [părții a treia][markov-iii] a seriei că urmează să construim un generator de text Markov format din două componente:

* O componentă care primește la intrare un model statistic, adică un lanț Markov, și întoarce un text generat aleator. Aceasta a constituit subiectul [celei de-a doua părți][markov-ii] a tutorial-ului.
* O componentă care primește la intrare unul sau mai multe texte într-o limbă oarecare și construiește pe baza lui, respectiv a lor, lanțul Markov necesar generării de text. În cele ce urmează vom implementa această a doua parte, care este de fapt prima parte a algoritmului în totalitatea sa.

Am observat de asemenea că problema construirii unui model pentru generarea propozițiilor într-o limbă oarecare este în practică intractabilă. Pe de o parte un „corpus al limbii” complet ar putea fi constituit doar din totalitatea textelor și cuvintelor scrise în limba respectivă. Pe de altă parte limba este o unealtă flexibilă, cu reguli și excepții care pot fi încălcate în diverse contexte, și a cărei formalizare este un subiect de cercetare intens la ora actuală în știința lingvisticii.

Prima dificultate este deci cea a timpului și spațiului necesar analizei unui set imens de date, presupunând că avem așa ceva la dispoziție ((Iar Internetul este un astfel de set de date. Apropo de asta, s-ar putea spune că Google este compania care dispune de cele mai sofisticate metode de analiză automată a datelor pe Internet, unde prin date înțelegem inclusiv text scris. Cu alte cuvinte, dacă ar fi putut exista un generator perfect de text într-o limbă anume, probabil ei ar fi fost primii care l-ar fi conceput.)). A doua dificultate este cea a utilizării uneltelor lingvistice actuale, unelte cum ar fi arborii sintactici, a căror construire și parcurgere impune costuri computaționale inerente. Din fericire această a doua dificultate este atacată în cadrul domeniului prelucrării limbajului natural, care însă le vine mai degrabă în ajutor celor ce au apucat să îi parcurgă literatura vastă ((Pomeneam la începutul seriei de utilizarea generatoarelor de text Markov pentru generarea de spam. Am observat, pe marginea comentariilor spam primite pe blog, că adepții acestui fenomen au ajuns să folosească unelte care au un grad destul de mare de sofisticare. Am primit comentarii care păreau de-a dreptul legitime, pentru a afla apoi că acele comentarii erau bucăți de text din alte articole de pe Internet, obținute automat cu ajutorul unor tehnici cum ar fi [analiza semantică latentă][lsa].)).

Drept urmare, vom folosi drept model pentru implementarea analizorului nostru următoarea problemă, echivalentă de altfel cu cea a construirii unui lanț Markov pe baza unui text dat. Fie `$latex \Sigma$` alfabetul latin, extins eventual cu diacritice. Vom nota cu `$latex \Sigma^*$` mulțimea șirurilor formate din litere din `$latex \Sigma$` ((Cu alte cuvinte [monoidul liber][free-monoid] peste mulțimea `$latex \Sigma$`.)) și cu `$latex w, w_1, w_2, \dots \in \Sigma^*$` cuvinte specifice din `$latex \Sigma^*$`. Notăm de asemenea în acest context operația de concatenare a două șiruri cu `$latex \cdot$` ((În Haskell concatenarea se notează cu operatorul `(++)`. Punctul e folosit mai degrabă pentru a desemna o lege de compoziție în structura algebrică de monoid.)) și caracterul/cuvântul „spațiu” cu `$latex \textvisiblespace$`.

Fiind dat un text oarecare și două cuvinte `$latex w_1$` și `$latex w_2$`, spunem că `$latex w_2$` este *succesorul* lui `$latex w_1$` dacă în text apare concatenarea `$latex w_1 \cdot \textvisiblespace \cdot w_2$`.

Putem astfel reduce problema la următoarea întrebare: fiind dat un text oarecare `$latex T$` și mulțimea `$latex W$` de cuvinte asociată lui `$latex T$`, care este probabilitatea ca două cuvinte oarecare `$latex w_1, w_2 \in W$` să se succeadă în `$latex T$`?

Dacă alegem de exemplu `$latex T = \text{Ana}\;\text{Ana}\;\text{Ana}\;\dots$`, atunci `$latex W = \{\text{Ana}\}$` și modelul rezultat va fi un lanț Markov cu `$latex p(\text{Ana}|\text{Ana}) = 1$`. Dacă alegem în schimb `$latex T = \text{Ana}\;\text{are}\;\text{Ana}\;\text{nu}$`, atunci `$latex W = \{\text{Ana},\text{are},\text{nu}\}$` și `$latex p(\text{are}|\text{Ana}) = 0.5$` și `$latex p(\text{nu}|\text{Ana}) = 0.5$`. Generalizând, am putea să modelăm distribuția `$latex p(w|\text{Ana})$` după cum ne îndeamnă inima, însă din exemplele de mai sus constatăm că distribuția uniformă e o alegere naturală și foarte la îndemână.

Un prim pas al implementării îl reprezintă deci, având la dispoziție textul `t` -- reprezentat în Haskell ca o listă de cuvinte -- și un cuvânt `w` oarecare, obținerea tuturor cuvintelor din `t` care îi succed lui `w`:

<pre lang="haskell">
-- gets all the elements that follow a given element
-- in (or not in) the list
consecutivesOf :: Eq a => a -> [a] -> [a]
consecutivesOf x (x' : x'' : xs)
    | x == x' = x'' : consecutivesOf x (x'' : xs)
    | otherwise = consecutivesOf x (x'' : xs)
consecutivesOf _ _ = []
</pre>

Observăm că funcția este în fapt destul de generală încât să poată fi aplicată pe variabile de orice tip care asigură comparația la egalitate, fie el `String`, `Char` sau Porc. Rezultatele funcției pot fi apoi folosite pentru a calcula distribuția urmașilor unui cuvânt oarecare, sau mai exact numărul de apariții ale fiecărui urmaș în listă:

<pre lang="haskell">
occurenceList :: Eq a => [a] -> [(a, Int)]
occurenceList xs = zip uniqs $ map (flip countOccurences $ xs) uniqs
    where
    uniqs = nub xs
</pre>

`occurenceList` întoarce o listă cu tupluri formate din cuvinte (unice) și numărul lor de apariții în lista inițială. Atât `consecutivesOf` cât și `occurenceList` și `countOccurences` -- a cărei implementare nu am pus-o aici -- pot fi vizualizate în context în modulul [`Monkey.Util.Processing`][monkey-util-processing] al proiectului.

Fiind dat un cuvânt `w` dintr-un text `t` oarecare, cele două funcții vor duce practic greul procesării asupra lui `w` în raport cu `t`. O observație este aceea că distribuția trebuie normalizată, deoarece tipul `Chain a` definit mai devreme folosește probabilități reprezentate ca `Float`-uri subunitare. Funcția `normalizeFollowers` mapează pe al doilea element al fiecărui tuplu împărțirea la suma tuturor, astfel:

<pre lang="haskell">
-- convert integers into probabilities
-- by default, states without followers loop into themselves (first equation)
normalizeFollowers :: Eq a => a -> [(a, Int)] -> [(a, Float)]
normalizeFollowers x [] = [(x,1)]
normalizeFollowers _ fs = map (./. total) fs
    where
    total = fromIntegral . sum $ map snd fs
    (x, occ) ./. n = (x, fromIntegral occ / n)
</pre>

Analiza unui cuvânt decurge astfel natural prin compoziția celor trei funcții definite până acum:

<pre lang="haskell">
-- given a token and its context, construct a follower-probability
-- model
analyzeToken :: Eq a => a -> [a] -> [(a, Float)]
analyzeToken x = normalizeFollowers x . occurenceList . consecutivesOf x
</pre>

Având la dispoziție `analyzeToken`, putem să testăm exemplele de mai sus. Primul exemplu:

<pre lang="haskell">
\*Monkey.Analyzer> analyzeToken "Ana" ["Ana", "Ana", "Ana", "Ana", "Ana"]
[("Ana",1.0)]
</pre>

rezultă evident într-un lanț Markov format dintr-o singură buclă. Al doilea exemplu:

<pre lang="haskell">
\*Monkey.Analyzer> analyzeToken "Ana" ["Ana", "are", "Ana", "nu"]
[("are",0.5),("nu",0.5)]
</pre>

dă de asemenea rezultatul așteptat, o distribuție uniformă a celor două cuvinte.

Dorim să mapăm operația peste toate cuvintele unice din text. În plus, dorim să împerechem fiecare cuvânt unic cu distribuția succesorilor săi și să convertim lista obținută într-un lanț Markov, folosind `fromList` definit în a doua parte.

<pre lang="haskell">
-- we assume that a "text" is actually a sequence of tokens
analyze :: (Eq a, Ord a) => [a] -> Chain a
analyze text = fromList $ zip uniqs $ map doAnalyze uniqs
    where
    uniqs = nub text
    doAnalyze = flip analyzeToken $ text
</pre>

Aplicând `analyze` -- pe care am pus-o în modulul [`Monkey.Analyzer`][monkey-analyzer] -- pe cel de-al doilea exemplu de mai devreme, obținem:

<pre lang="haskell">
\*Monkey.Analyzer> analyze ["Ana", "are", "Ana", "nu"]
fromList [("Ana",[("are",0.5),("nu",0.5)]),("are",[("Ana",1.0)]),("nu",[("nu",1.0)])]
</pre>

Propun de asemenea să ilustrăm puterea generatorului pe un exemplu mai interesant:

<pre lang="haskell">
*Monkey.Analyzer> :m +Markov.Chain 
*Monkey.Analyzer Markov.Chain> words "lungimea medianei corespunzatoare ipotenuzei este egala cu jumatate din lungimea ipotenuzei"
["lungimea","medianei","corespunzatoare","ipotenuzei","este","egala","cu","jumatate","din","lungimea","ipotenuzei"]
*Monkey.Analyzer Markov.Chain> analyze $ words "lungimea medianei corespunzatoare ipotenuzei este egala cu jumatate din lungimea ipotenuzei"
fromList [("corespunzatoare",[("ipotenuzei",1.0)]),("cu",[("jumatate",1.0)]),("din",[("lungimea",1.0)]),("egala",[("cu",1.0)]),("este",[("egala",1.0)]),("ipotenuzei",[("este",1.0)]),("jumatate",[("din",1.0)]),("lungimea",[("medianei",0.5),("ipotenuzei",0.5)]),("medianei",[("corespunzatoare",1.0)])]
*Monkey.Analyzer Markov.Chain> randomWalk (analyze $ words "lungimea medianei corespunzatoare ipotenuzei este egala cu jumatate din lungimea ipotenuzei") "lungimea" 10
["lungimea","ipotenuzei","este","egala","cu","jumatate","din","lungimea","medianei","corespunzatoare"]
</pre>

Ei, și acum imaginați-vă ce minunății de texte ar putea genera artificial algoritmul (relativ simplu) prezentat până acum dacă am avea la dispoziție niște texte mai mari. Două-trei articole culese la întâmplare de pe Wikipedia s-ar putea dovedi a fi de ajuns pentru a scoate texte dacă nu interesante, atunci măcar amuzante.

Firește că ce am făcut până acum e departe de a fi un program complet. O îmbunătățire importantă ar putea fi adăugarea unui modul principal, care să lege `Monkey.Analyzer` și `Markov.Chain` într-un singur program care ia fișiere, le analizează și produce text pe baza lor. O altă îmbunătățire este prelucrarea semnelor de punctuație și a diacriticelor, dat fiind faptul că dorim pe cât posibil să lucrăm doar cu cuvinte. În fine, îmi vin în minte tot felul de mici chestii care ar putea să facă programul mai utilizabil, plus câteva îmbunătățiri mai mari, care probabil că ar face generatorul o idee mai inteligent.

Înainte de a încheia articolul, vă propun două probleme mai mult sau mai puțin interesante, care ar putea da naștere unor fire de dezvoltare ulterioară și eventual unor articole:

* **p4.1**: Stând un pic și cugetând, mi-am dat seama că reprezentarea probabilităților ca numere în virgulă mobilă nu e așa de utilă pe cât mă așteptam. În primul rând că numerele întregi sunt exacte, în timp ce o împărțire oricât de banală în virgulă mobilă poate genera erori. În al doilea rând, distribuția numerelor fiind discretă și mai ales uniformă, nu își are rostul folosirea împărțirii adiționale mai mult decât raportarea la lungimea listei succesorilor. Sunt curios deci dacă folosirea numerelor întregi în cadrul tipului `Chain a` rezultă într-o simplificare a codului.
* **p4.2**: Construirea lanțurilor Markov folosind `fromList` servește perfect scopului pe care ni l-am propus la începutul seriei. Implementarea poate avea însă probleme de performanță datorate conversiei din liste posibil foarte mari în dicționare, ceea ce denotă în același timp o lipsă de naturalețe a abordării. Doresc astfel să speculez că dicționarele/lanțurile pot fi construite incremental, ceea ce ar reduce analiza la un simplu fold ((Adică la un [catamorfism][catamorphism], care are o semnificație algebrică foarte importantă, pe lângă faptul că ar putea îmbunătăți performanța componentei de analiză.)). Mai mult, cred că este posibilă compunerea lanțurilor Markov, deci transformarea întregii povești într-o structură de monoid. Cercetarea acestui aspect ar putea avea implicații serioase din punct de vedere teoretic, putând fi de ajutor de exemplu la examinarea calității unui model în raport cu altul și îmbunătățirea per total a generării de text, fapt care nu a fost tratat în articolele din această serie.
