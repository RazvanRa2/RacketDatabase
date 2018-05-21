#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    null)) ; init database este null

(define create-table
  (λ (table columns-name)
    (cons table (list columns-name)))) ; un tabel gol e o lista cu numele (car) si o lista de capete de coloana (cdr)

(define get-name
  (λ (table)
    (car table))) ; primul element din tabel e mereu numele tabelului

(define get-columns
  (λ (table)
    (cadr table))) ; (car tabel) => nume. (car (cdr tabel)) => capete de coloane

(define get-tables
  (λ (db)
     db)) ; tabelele dintr-o baza de date sunt, in acest caz, tocmai baza de date

(define (find-table-in-list name tables)
  (if (null? tables) ; daca a ajuns la capat, tabelul nu exista, prin conventie return '()
      '()
      (if (equal? (get-name (car tables)) name) ; daca car e tabelul, il returneaza, altfel se uita mai departe
          (car tables)
          (find-table-in-list name (cdr tables)))))

(define get-table
  (λ (db table-name)
    (let ([tables (get-tables db)])
      (find-table-in-list table-name tables))))

(define add-table
  (λ (db table)
    (cons table db)))  ; se dauga tabelul in lista de tabele (db)

(define remove-table
  (λ (db table-name)
    (let ([deleted (get-table db table-name)])
      (remove deleted db)))) ; pentru a scoate un tabel, identificam elementul dupa nume, apoi folosim remove 

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db '(("Studenți"
   ("Număr matricol" "Nume" "Prenume" "Grupă" "Medie")
   (123 "Ionescu" "Gigel" "321CA" 9.82) (124 "Popescu" "Maria" "321CB" 9.91)
   (125 "Popa" "Ionel" "321CC" 9.99) (126 "Georgescu" "Ioana" "321CD" 9.87) )
             ("Cursuri" ("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme")
                        ("I" "I" "Programarea calculatoarelor" 5 2)
                        ("II" "II" "Paradigme de programare" 6 3)
                        ("III" "I" "Algoritmi paraleli și distribuiți" 5 3)
                        ("IV" "I" "Inteligență artificială" 6 3)
                        ("I" "II" "Structuri de date" 5 3)
                        ("III" "II" "Baze de date" 5 0))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define (find-value record word)
  (if (null? record)
      '()
      (if (equal? (car (car record)) word) ; daca "Name" din ("Name" . "value") e egal cu capul de tabela
          (cdr (car record)) ; am gasit valoarea
          (find-value (cdr record) word))))  ; altfel, mai cautam

(define (make-table-entry columns record res) ; creem o intrare in tabel
  (if (null? columns) ; daca s-a ajuns la finalul capetelor de tabel, returnam rezultatul
      res
      (if (equal? '() (find-value record (car columns))) ; cautam valoarea pentru pozitia column si apoi o inseram
          (make-table-entry (cdr columns) record (append res (list NULL)))  ; daca valoarea nu a fost definita, punem NULL
          (make-table-entry (cdr columns) record (append res (list (find-value record (car columns))))))))

(define insert  
  (λ (db table-name record)
    (let ([table (get-table db table-name)]) ; stabilim care e tabelul in care se adauga
      (add-table (remove-table db table-name) ; scoatem tabelul care nu contine elementul de adaugat
                 (append table (list (make-table-entry (get-columns table) record '() ))))))) ; construim un nou tabel si il inseram


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define (col-no header col no) ; cauta coloana cu numele col in header si returneaza index-ul ei (a cata coloana e)
  (if (equal? col (car header)) ; (indexes start at 1)
      no
      (col-no (cdr header) col (+ 1 no))
      ))

(define (select-helper-helper no entry poz) ; method has a bad name just cause i thought it'd be funny
  (if (equal? poz no) ; cauta elementul cu nr no intr-o intrare din tabel. Practic, cauta elementul de pe coloana nr. "no"
      (car entry) 
      (select-helper-helper no (cdr entry) (+ 1 poz))))

(define (select-helper minTable no ans)
  (if (null? minTable) ; folosind helper-helper, scoate toate elementele de pe o anumita coloana din tabel
      ans
      (select-helper (cdr minTable) no (append ans (list (select-helper-helper no (car minTable) 1))))))

(define (select-column-by-name table name)  ; alege coloana din tabel, dupa nume (calculand nr coloanei)
  (select-helper (cddr table)   (col-no (get-columns table) name 1) '())) 

(define (do-simple-select table columns acc)  
  (if (null? columns)
      acc
      (do-simple-select table (cdr columns) (append acc (list (select-column-by-name table (car columns)))))
      ))

(define (check-empty-select list) ; verificare selectie goala. o selectie de tipul '('() '() '()) devine '()
  (if (equal? #t (andmap (lambda (elem)
         (equal? '() elem)) list)) ; daca toate elementele sunt '(), atunci returneaza '(), daca nu, returneaza lista
      '()
      list))

(define simple-select
  (λ (db table-name columns)
    (let ([table (get-table db table-name)])  ; scoatem tabel din db
      (check-empty-select (do-simple-select table columns '())) ; alegem coloanele din tabela (+ verificam sa nu fie goala selectia)
      )))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define (simplify-conditions header conditions res) ; transforma o conditie de tipul (semn "nume" val) in (semn nr_col val)
  (if (null? conditions)  
      res
      (simplify-conditions header (cdr conditions) (append res (list (list (caar conditions) (col-no header (cadar conditions) 1) (caddar conditions)))))
      ))

(define (get-val entry pos cnt) ; ia valoarea de pe pozitia pos dintr-o intrare
  (if (equal? pos cnt)
  (car entry)
  (get-val (cdr entry) pos (+ 1 cnt))))

(define (check-cond cond entry) ; verifica o conditie pentru o intrare din tabela (face (operator val_intrare val_cond))
  ((car cond)  (get-val entry (cadr cond) 1) (caddr cond) ))  

(define (filter-one-cond entries cond res)
  (if (null? entries)
      res
      (filter-one-cond (cdr entries) cond
                       (if (equal? (check-cond cond (car entries)) #t) 
                       (append res (list (car entries)))
                       res)
                       )))

(define (filter-table entries conditions)  ; filtreaza pe rand intrarile din tabela pe baza conditiilor, luand
  (if (null? conditions) ; conditiile una cate una
      entries
      (filter-table (filter-one-cond entries (car conditions) '()) (cdr conditions) ))) ;aici

(define (make-my-table table toInsert)  ; creeaza un tabel inserand intrari in el
  (if (null? toInsert)
      table
      (make-my-table (append table (list (car toInsert))) (cdr toInsert))))

(define (do-select table columns conds)  ; face selectia
  (simple-select (add-table (init-database)  (make-my-table (create-table (car table) (cadr table)) (filter-table (cddr table) conds))) (car table) columns))

(define (simple-cols columns res)
  (if (null? columns)
      res
      (simple-cols (cdr columns) (append res (list (if (pair? (car columns)) ; transforma intrarile de tipul "min "coloana" in "coloana"
                                                       (cdr (car columns))
                                                       (car columns)))))))
(define (get-filters columns res cnt)
  (if (null? columns)
      res
      (get-filters (cdr columns) (append  res (list (if (pair? (car columns))
                                                        (cons (caar columns) cnt)
                                                        cnt))) (+ 1 cnt))))
(define (do-min lst minim)  ; urmatoarele functii sunt utile pentru min, max, count, sum, average
    (if (null? lst)
        minim
        (do-min (cdr lst) (min (car lst) minim))))

(define (do-max lst maxim)
    (if (null? lst)
        maxim
        (do-max (cdr lst) (max (car lst) maxim))))

(define (do-count lst cnt)
  (if (null? lst)
      cnt
      (do-count (cdr lst) (+ 1 cnt))))

(define (do-sum lst sum)
  (if (null? lst)
      sum  
      (do-sum (cdr lst) (+ sum (car lst)))))

(define (do-avg lst)
  (/ (do-sum lst 0) (do-count lst 0)))

(define (use-filter operation input) ; pentru filtrare in functie de operatie, alege metoda corecta
  (if (equal? 'min operation)
      (do-min input 99999)
      (if (equal? 'max operation)
          (do-max input -99999)
          (if (equal? 'count operation)
              (do-count (remove-duplicates input) 0)
              (if (equal? 'sum operation)
                  (do-sum input 0)
                  (if (equal? 'avg operation)
                      (do-avg input)
                      (if (equal? 'sort-asc operation)
                          (sort input <)
                          (if (equal? 'sort-desc operation)
                              (sort input >)
                              '() ))))))))

(define (apply-filters res filters input) ; metoda aplica filtrele definie in operatia select (min, max, average etc)
  (if (null? filters)
      res
      (apply-filters (if (pair? (car filters))
                        (append res (list (use-filter (caar filters) (car input))))
                        (append res (list (car input))))  
                    (cdr filters) (cdr input))))

(define select
  (λ (db table-name columns conditions)  
      (apply-filters '() (get-filters columns '() 1) (do-select (get-table db table-name) (simple-cols columns '()) (simplify-conditions (cadr (get-table db table-name)) conditions '())))))  

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
; update seamana f mult cu select, practic se comporta la fel, dar cand gaseste o intrare ce trebuie updatata, o updateaza
(define (get-updated-entry entry valpair res cnt)  ; creeaza un entry cu date updatate
  (if (null? entry)
      res
      (get-updated-entry (cdr entry) valpair (if (equal? cnt (car valpair)) (append res (cdr valpair)) (append res (list (car entry)))) (+ 1 cnt ))))

(define (update-values values entry) ; updateaza o toate coloanele unei intrari din tabela (cele care trebuie updatate)
  (if (null? values)
      entry
      (update-values (cdr values) (get-updated-entry entry (car values) '() 1)))) 

(define (filter-upd-entry conditions entry) ; verifica daca o intrare indeplineste toate conditiile pentru a fi updatata
  (if (null? conditions)
      entry
      (if (equal? #t (check-cond (car conditions) entry)) ; check-cond definit anterior
          (filter-upd-entry (cdr conditions) entry)
          '()
           )))

(define (filter-upd-table entries conditions res values) ; filtreaza intrarile din tabel, dupa conditii
  (if (null? entries)
      res
      (filter-upd-table (cdr entries) conditions (if (equal? '() (filter-upd-entry conditions (car entries)))
                                                     (append res (list (car entries)) )
                                                     (append res (list (update-values values (car entries))))  ) values)))

(define (do-upd-select table columns conds values)  
  (simple-select (add-table (init-database)  (make-my-table (create-table (car table) (cadr table)) (filter-upd-table (cddr table) conds '() values))) (car table) columns))

(define upd-select
  (λ (db table-name columns conditions values)  
      (do-upd-select (get-table db table-name) columns (simplify-conditions (cadr (get-table db table-name)) conditions '()) values)))

(define (exists-col name header)  ; verifica daca exista o coloana cu numele name.
  (if (null? header)
      #f
      (if (equal? name (car header))
          #t
          (exists-col name (cdr header)))))
  

(define (easy-values values header res) ; modifica perechile ("nume coloana" valoare") in (numar_col valoare)
  (if (null? values)
      res
      (easy-values (cdr values) header (if (equal? #t (exists-col (caar values) header)) (append res (list (list (col-no header (caar values) 1) (cdr (car values))))) res ))))

(define update
  (λ (db table-name values conditions)
    (list (cons table-name (cons (get-columns (get-table db table-name)) (transpose (upd-select db table-name (get-columns (get-table db table-name)) conditions
                                                                                                (easy-values values (get-columns (get-table db table-name)) '() ))))))))  

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
; remove e implementat ca un select cu conditii intoarse. Practic, aleg toate elementele care indeplinesc conditia opusa.
(define (notequal a b)  ; needed for not equal as a parameter in a list. this is all i could come up with.
  (not (equal? a b)))

(define (flip-one-cond cond res) ; method returns flipped conditions
  (if (equal? (car cond) <)
      (list >= (cadr cond) (caddr cond))
      (if (equal? (car cond) >)
          (list <= (cadr cond) (caddr cond))
          (if (equal? (car cond) <=)
              (list > (cadr cond) (caddr cond))
              (if (equal? (car cond) >=)
                  (list < (cadr cond) (caddr cond))
                  (if(equal? (car cond) equal?)
                     (list notequal (cadr cond) (caddr cond))
                     '() ))))))

(define (flip-conds conds res) ; method flips all conditions
  (if (null? conds)
      res
      (flip-conds (cdr conds)  (append res (list (flip-one-cond (car conds) '() ))) )))

(define (transpose matrix) ; transpunsa unei matrice, necesara pentru a transforma coloanele in linii (in tabele)
  (if (null? matrix)
      '()
      (apply map list matrix)))

(define (filter-one-entry conditions entry) ; filtreaza o intrare dupa toate conditiile impuse
  (if (null? conditions) ; daca nu mai exist aconditii de verificat, intoarce intrarea
      entry
      (if (equal? #t (check-cond (car conditions) entry)) ; daca conditia nu e indeplinita, intoarce '()
          '()
          (filter-one-entry (cdr conditions) entry) ))) ; altfel, continua sa filtreze dupa restul conditiilor

(define (filter-del-table entries conditions res) ; filtreaza toate intrarile dupa conditii
  (if (null? entries)
      res
      (filter-del-table (cdr entries) conditions (if (equal? '() (filter-one-entry conditions (car entries)))
                                                     (append res (list (car entries)) )
                                                     res  ))))


(define (do-del-select table columns conds)  ; declanseaza filtrarea dupa conditii
  (simple-select (add-table (init-database)  (make-my-table (create-table (car table) (cadr table)) (filter-del-table (cddr table) conds '()))) (car table) columns))

(define del-select
  (λ (db table-name columns conditions)  ; face selectarea coloanelor, cu conditii simplificate de simplify-conditions
      (do-del-select (get-table db table-name) columns (simplify-conditions (cadr (get-table db table-name)) conditions '())))) 


(define delete
  (λ (db table-name conditions)  
    (list (cons table-name (cons (get-columns (get-table db table-name)) (transpose (del-select db table-name (get-columns (get-table db table-name)) (flip-conds conditions '()))))))))

