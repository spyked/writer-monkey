să scriem împreună un generator de text markov
==============================================

Invit cititorul de Cărămizi să ia parte la o inițiativă 

* **(a)** destul de rar întâlnită pe blog-urile românești,
* **(b)** pur didactică,
* **(c)** destinată programatorilor amatori, profesioniști sau pur și
  simplu oamenilor cărora le place să-și folosească chestia aia dintre umeri,
  și nu în ultimul rând
* **(d)** aparent inutilă.

De fapt punctul **(d)** e în mare parte fals; stând un pic și cugetând,
realizez că ar fi foarte util să urmăriți tutorialul dacă doriți de exemplu să
învățați Haskell sau să vă faceți o idee legată de analiză statistică și
lanțuri Markov, sau pur și simplu dacă vreți să vă dezvoltați un generator de
spam ((Nu văd care-i problema. Scopul meu e acela de a le arăta oamenilor cum
să folosească cuțitul, nu să le explic cum ar putea să-l vâre în alți
indivizi.)).

Acestea fiind spuse, să începem prin a explica câteva concepte teoretice care
stau la baza aplicației noastre. Primul și cel mai important e acela de
probabilitate, pe care nu îl voi explica deoarece consider că cititorii mei
sunt mai deștepți decât o oaie. Al doilea este acela de [probabilitate
condiționată](http://en.wikipedia.org/wiki/Conditional_probability "Conditional
probability"), care pleacă de la următorul raționament simplu:

Fie $latex A$ și $latex B$ două evenimente. Notăm cu $latex p(A)$
probabilitatea ca $latex A$ să se fi produs. De asemenea presupunem că am
observat producerea lui $latex B$. Acest fapt ne poate oferi informații despre
$latex p(A)$, dat fiind faptul că nu știm dacă $latex A$ și $latex B$ sunt
legate cauzal sau corelate sau mai știu eu ce. În cazul ăsta notăm
probabilitatea ca $latex A$ să se fi produs condiționată de $latex B$ drept
$latex p(A|B)$, definită drept raportul dintre probabilitatea conjuncției celor
două evenimente și $latex p(B)$.

Privind invers problema, probabilitatea ca atât $latex A$ cât și $latex B$ să
se fi produs depinde de:

* Probabilitatea ca $latex B$ să se fi produs $p(B)$ și
* Probabilitatea ca $latex A$ să se fi produs în cazul în care s-a observat
  $latex B$, deci $latex p(A|B)$.

Din a doua definiție observăm că putem inversa $latex A$ cu $latex B$ în
definiția conjuncției de probabilități, ceea ce ne duce cu gândul că e cam greu
de găsit relații de cauzalitate în gândirea probabilistică, dar în fine,
astea-s chestii filosofice care nu ne interesează pe noi.

Dacă tot am ajuns aici, să dăm și un exemplu: să presupunem evenimentele mutual
exclusive $latex \text{AziSoare}$ și $latex \text{AziPloaie}$, având
semnificațiile evidente „azi e soare” și „azi plouă”. Analog putem defini
$latex \text{MaineSoare}$ și $latex \text{MainePloaie}$ cu semnificații
similare. E clară deci relația dintre perechile de evenimente, însă nu sunt
clare relațiile inter-perechi, de exemplu dependența dintre $latex
\text{AziSoare}$ și $latex \text{MainePloaie}$.

Dat fiind că ziua de mâine o succede pe cea de azi, putem observa la un moment
dat starea de azi a vremii, informație pe care apoi o vom putea folosi pentru a
face predicții mai bune asupra vremii de mâine. Generalizând, putem folosi
probabilitățile condiționate ((Alese la întâmplare sau obținute din niște date
statistice. Aici discuția se poate întinde pe parcursul a cărți întregi. Cert e
că există metode de a obține niște probabilități condiționate fixe.)) pentru a
face predicții legate de starea vremii în viitor bazate pe starea curentă a
vremii. Orice proces asemănător cu cel de mai sus se numește lanț Markov și
poate fi reprezentat printr-un graf, după cum se poate vedea în link-ul de pe
Wikipedia. Graful va avea pe arce probabilități condiționate, un arc $latex B
\to A$ având asociată o valoare `$latex p(A_{t}|B_{t-1})$`, unde $latex t$ e
timpul reprezentat explicit.

Observăm de asemenea că putem reprezenta un astfel de graf folosind o tabelă
unde pe fiecare linie și coloană avem valori/stări posibile ale
variabilei/evenimentului care ne interesează. În termeni de știința
calculatoarelor orice tabelă bidimensională poate fi foarte ușor reprezentată
sub forma unui [dicționar](http://en.wikipedia.org/wiki/Associative_array
"Associative array") având drept cheie o stare posibilă și drept valoare
coloana (sau rândul) asociată acesteia, pe care noi o vom reprezenta printr-o
listă de asocieri stare-probabilitate.

În Haskell structura de date de tip dicționar se numește
[Map](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html
"Data.Map") și se construiește pe baza a două tipuri de date, tipul asociat
cheii și cel asociat valorii. Plecând de la cele de mai sus, putem defini un modul
pe nume `Markov.Chain` în care să definim o structură de tip dicționar având
proprietățile descrise de noi, după cum urmează:

<p lang="haskell" line="1">
module Markov.Chain where

import Data.Map as M

-- Markov Chain: DAG with probabilities on arcs
-- also, a dictionary that associates a
-- probability to each successor
type Chain a = M.Map a [(a, Float)]
</p>

A se nota că am importat `Data.Map` cu numele `M` pentru a simplifica folosirea
funcțiilor din modul și a structurii de date `Map`.

Ca o observație, conținutul acestui prim articol mi se pare suficient de
abstract și arid încât să îi sperie pe diletanți. Nu vă panicați totuși, de
aici încolo încep exemplele, mai exact exemplul, adică aplicația de generat
text. Am mai pomenit de câteva ori [dadadodo](http://www.jwz.org/dadadodo
"dadadodo") aici, iar întâmplarea face că rezultatul obținut din tutorial va
avea exact aceeași funcționalitate. Procesul de dezvoltare va fi explicat pe
parcursul mai multor articole, toate astea ilustrând cât de simplu se poate
dezvolta o chestie aparent atât de complicată cum e generatorul de text Markov.

Data viitoare vom defini o interfață minimală pentru noua noastră structură de
date, interfață pe care mai apoi o vom folosi pentru a rula câteva exemple de
lanțuri Markov. Cu ocazia asta vom observa și motivul pentru care meteorologii
o dau în bară în mod sistematic.

**Post Scriptum**: Atât articolele cât și codul sunt disponibile pe
[github](https://github.com/spyked/writer-monkey "spyked/writer-monkey"), de
unde pot fi descărcate, testate și modificate, fapt pe care autorul (anume eu)
îl încurajează din toată inima. După încheierea procesului de dezvoltare -
încheiere ce se va concretiza prin publicarea ultimului articol - voi accepta
contribuții la proiect indiferent de natura acestora, cât timp le voi considera
pozitive.
