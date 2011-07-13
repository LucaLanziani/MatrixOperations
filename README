 
Il progetto è formato principalmente da due parti separate.

La prima lavora su di una struttura circolare usando l'approccio SIMD per effettuare le operazioni di inversa e risoluzioni di sistemi lineari o semplicemente per effettuare una serie di pivot sulla diagonale principale.

La seconda lavora invece su una struttura a fiore dove un singolo dispatcher si occupa di inviare il lavoro a più worker (somma, sottrazione o prodotto) per poi attendere la risposta.


Nodi di distribuzione/preparazione : dispatcher
Nodi che effettuano le operazioni: worker


===Dettagli Circle:===


Nel primo caso la struttura come è stato detto è di tipo circolare, ogni anello del cerchio (dispatcher) si occupa di preparare la matrice (scambiare eventualmente le righe e dividerla nei giusti modi) per poi comunicare con una serie di Worker che effettuano effettivamente l'operazione di pivor.
In effetti ogni Dispatcher effettua attraverso i sui Worker l'operazione di pivot su una singola riga per poi passare la matrice al prossimo Dispatcher dell'anello che si occuperà delle righe successive.
Nell'anello possono essere messe in circolo più matrici contemporaneamente.

Istruzioni:

Per lanciare il sistema circle si usa il modulo launcher effettuando:

> launcher:start()    

#si occupa di lanciare il server che metterà i lavori in circolo nell'anello

> launcher:startSupervisor({circle,D,W})  	

#lancia il Supervisor che si occupa a sua volta di creare la struttura e mantenerla in piedi. 
#Vanno specificati D e W come due interi che rappresentano rispettivamente il numero D di dispatcher da creare (quindi sapremo su quante matrici contemporaneamente potremo lavorare) e W il numero dei Worker per ogni Dispatcher (danno un idea del livello di parallelismo su ogni singolo pivot).

> A=smm:import("mr100100.txt")

#importiamo una matrice 100x100 randomica salvata precedentemente su file

> launcher:sendwork(TypeOfWork,A).  

#effettuo il lavoro effettivo TypeOfWork è un atomo e più assumere i valori (inverse | symlin | pivot)
#il sistema ci risponderà comunicandoci il numero assegnatoci per il nostro lavoro.
#Al termine potremmo recuperare il risultato con

> launcher:getWork(Nwork) 

#Nwork deve essere esattamente il numero comunicatoci dal comando precedente.

 

===Dettagli Flower===

Qui abbiamo un solo Dispatcher che dividerà la matrice tra i worker a seconda dell'operazione richiesta, e quanto questi ultimi avranno terminato riassemblerà i risultati.

In questo caso è necessario avviare da prima il supervisor  

> flowersv:start({flower,Worker}).

#La variabile Worker deve essere un intero che indica quanti worker avviare
#Da qui in poi si può parlare con il dispatcher che si occuperà di effettuare i lavori per noi

> flower_dispatcher:operation4Row(prod,A,B);  

#A e B rappresentano le matrici su cui effettuare le operazioni
#il primo termine è un atomo che può assumere i valori (prod | sum | sub)


