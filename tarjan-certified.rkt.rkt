#lang r7rs

(import (scheme base)
        (scheme write)         
        (prefix (a-d stack linked) stack:)
        (a-d graph-algorithms directed topological-sorting)
        (a-d graph unweighted adjacency-matrix)
        (only (a-d scheme-tools) random-inbetween)
        (a-d graph-traversing dft-unweighted)
        (only (racket base) format exact->inexact))



(define (tarjan-certified g)
  (define preorder-time 0)
  (define preorder-numbers (make-vector (order g) -1)) 
  (define highest-back-edge (make-vector (order g) -1)) 
  (define sc-components (make-vector (order g) -1))
  (define included (make-vector (order g) #f))
  (define nr-of-components 0)
  (define stack (stack:new))
  (define fw (new #t (order g)))
  (define bw (new #t (order g)))
  (dft
   g
   root-nop
       
   (lambda (node) ;node-discovered
     (stack:push! stack node)
     (vector-set! preorder-numbers node preorder-time)
     (vector-set! highest-back-edge node preorder-time)
     (set! preorder-time (+ preorder-time 1)))
       
   (lambda (node) ;node-processed
     (when (= (vector-ref highest-back-edge node)
              (vector-ref preorder-numbers node)) 
       (set! nr-of-components (+ 1 nr-of-components))
       (let loop
            
         ((t (stack:pop! stack)))
          
         (vector-set! sc-components t nr-of-components)
         (vector-set! included t #t)
         (unless (eq? t node)
           (loop (stack:pop! stack))))))

       
   edge-nop  ; before  
       
   (lambda (from to) ; after => 
     (if (not (vector-ref included to)) 
         (let ((n (vector-ref highest-back-edge from)))
           ;Elke keer een nieuw aangrenzend knoop wordt bezocht in de fw tree zetten
           (add-edge! fw from to)
           (vector-set! highest-back-edge
                        from (min n (vector-ref highest-back-edge to)))
           (if (not (eq? n (vector-ref highest-back-edge from)))
             ;als de lage nummer van een knoop wordt bijgewerkt in de bw tree zetten    
               (add-edge! bw from to)))))
       

   (lambda (from to) ; bump  => avoid cross-edges
     (if (not (vector-ref included to))
         (let ((n (vector-ref highest-back-edge from)))
           (vector-set! highest-back-edge
                        from (min (vector-ref highest-back-edge from)
                                  (vector-ref preorder-numbers to)))
           (if (not (eq? n (vector-ref highest-back-edge from)))
           ;als de lage nummer van een knoop wordt bijgewerkt in de bw tree zetten    

               (add-edge! bw from to))))))
     
    
    
  (cons  (cons nr-of-components sc-components) (list fw bw (construct-dag nr-of-components sc-components g))))
      









(define (construct-dag nr-of-components sc-components g)
  (define dag (new #t nr-of-components))
  (define res '())
  
  ;; Loop door elke knoop van de oorspronkelijke graaf
  (let loop
    ((from 0))
    (for-each-edge g from
                   (lambda (to)
    
                     (let ((from (vector-ref sc-components from))
                           (to  (vector-ref sc-components to)))
                      
                       (if (and (not (eq?  from to)) ; Controleer of de start- en eindknoop van de rand verschillend zijn 
                                (not (adjacent? dag(- from 1) (- to 1)))) ; Controleer of er nog geen directe verbinding is tussen dezelfde SCC's in de DAG
                           (add-edge! dag (- from 1) (- to 1))))))
                       
    ;; Als nog niet alle knopen zijn doorlopen, ga door met de volgende knoop
    (if (not (= from (- (order g) 1)))
        (loop (+ from 1))
        ;; Anders, retourneer de resulterende DAG
        dag)))
               








(define (check-tarjan g scc-result certificate)

  (define fw (car certificate))
  (define bw (cadr certificate)) 
  (define scc (cdr scc-result))
  (define h (reverse (dfs-topological-sort (car (cddr certificate)))))
  (define v '())
  (define res #t)
 
  
  (define (nodes-component c)
    (let loop
      ((res '())
       (cntr 0))
      (cond ((= cntr (vector-length scc))   res)
            ((eq? (vector-ref scc cntr) c) (loop (append res (list cntr)) (+ cntr 1)))
            (else
             (loop res (+ cntr 1))))))
  
  
  (define (nodes-tree tree nodes)
    (define res '())
    (for-each-node tree (lambda (node) (if (and (member node nodes) (not (member node res)))
                                           (set! res (cons node res)))))
    res)
  
  
  (define (check-nodes-in-graph? nodes)
    (define res #t)
    ;; Voor elk knooppunt in de lijst van knooppunten
    (for-each
     (lambda (node)
       ;; Controleer of het knoop in de graaf zit
      
  (if (>= node (order g))
         (set! res #f)))
     nodes)
    res)

  
  (define (check-reachability? component-nodes direction-tree . roots)

    (define reachable (make-vector (order g) #f))
  

    (if (not (null? roots))
        ;; Voer een diepte-eerst doorzoeken uit op basis van de gegeven richting-boom
        (dft direction-tree
             root-nop 
             ;; Markeer elke bezochte knoop
             (lambda (node) (vector-set! reachable node #t))
             node-nop 
             edge-nop 
             edge-nop 
             edge-nop
             ;;gebruik de geven root
             (car roots))
    
     
        (dft direction-tree
             root-nop 
             (lambda (node) (vector-set! reachable node #t))
             node-nop 
             edge-nop 
             edge-nop 
             edge-nop))

    (let loop
      ((nodes component-nodes))
      (cond ((null? nodes)  #t)
            ((vector-ref reachable (car nodes)) (loop (cdr nodes)))
            (else #f))))
  


  (define (remove-component-from-graph! nodes-component)
    ;; Begin loop over alle knopen in de graaf
    (let loop
      ((from 0))
      (if (not (= from (order g)))
          (begin
            ;; Verwijder alle randen die verbonden zijn met knopen in de component

            (for-each-edge g from (lambda (to) 
                                    (if (or (member from nodes-component) (member to nodes-component))
                                        (delete-edge! g from to))))
    
            (loop (+ from 1))))))


  

   
  (for-each-node g (lambda (node) (set! v (cons node v))))
      



  (let loop
    ((c (+ 1(car h)))
     (dag (cdr h)))
    (let*
        ((nodes-component  (nodes-component c))
         (forward-nodes  (nodes-tree fw nodes-component))
         (backward-nodes  (nodes-tree bw nodes-component)))
         
  
      (if (not (null? dag))
          (begin

            ;;C1
            
            (unless (equal? forward-nodes backward-nodes) 
             (set! res #f)
              (loop c '())) 

            (unless (check-nodes-in-graph? nodes-component) ;Controleer of alle knooppunten van de component aanwezig zijn in de graaf.
              (set! res #f)
              (loop c '()))
;Als het geldt voor knoop-componenten, geldt het ook voor fw en bw, omdat ze deel uitmaken van knoop-componenten.

            ;; C2
    
            (let loop2 
              ((fw-n forward-nodes)
               (bw-n backward-nodes))
    
   
 
              (if (not (null? fw-n))
                  (begin
                    ;; Controleer dat elke rand in de forward boom in g zit
                    (for-each-edge fw (car fw-n)  (lambda (to) 
                                             
                                                    (if (not (adjacent? g (car fw-n) to))
                                                        (begin
                                                          (set! res #f)
                                                          (loop c '())))))
                                                       
                    ;; Controleer dat elke rand in de backward boomin g zit
                    (for-each-edge bw (car bw-n)(lambda (to) (if (not (adjacent? g (car bw-n) to))
                                                                 (begin
                                                                   (set! res #f)
                                                                   (loop c '())))))
                    (loop2 (cdr fw-n)(cdr bw-n)))))


            ;; C3
     
 
            (let ((reachable-fw? (check-reachability? nodes-component fw)) 
                  (reachable-bw? (check-reachability? nodes-component bw)))


              (unless (and reachable-fw? reachable-bw?)
                (set! res #f)
                (loop c '())))



            ;;; C4

            (unless (check-reachability? nodes-component g (list (car nodes-component)))
              (set! res #f)
              (loop c '()))


            (remove-component-from-graph! nodes-component)
 
       
            (loop (+ 1(car dag)) (cdr dag)))

       res))))







;;test

(define (generate-random-graph num-nodes num-edges)
  (define graph (new #t  num-nodes))
  
  (do ((i 0 (+ i 1)))
      ((= i num-edges))
    (let* ((from (random-inbetween 0  (- num-nodes 1)))
           (to (random-inbetween 0 (- num-nodes 1))))
      (add-edge! graph from to)))
  
  graph)




(define (demo-checker n)
  (define count-accept 0)
  (define count-reject 0)
  (do ((i 0 (+ i 1)))
    ((= i n)  )
    (let* ((num-nodes (random-inbetween 5 20))
           (max-edges (* num-nodes (- num-nodes 1))) ; Maximum aantal mogelijke randen
           (num-edges (random-inbetween 1 (/ max-edges 2)))
           (g (generate-random-graph num-nodes num-edges))
           (res  (tarjan-certified g))) 
      (display (format "--- Checking Tarjan(~a) ---~nOutput: ~a~nCertificate: ~a~n" g (car res) (cdr res)))
      (let ((check-res (check-tarjan g 
                                  (car res)
                                  (cdr res))))
        (if check-res
            (set! count-accept (+ count-accept 1))
            (set! count-reject (+ count-reject 1)))
        (display (format "Checker result: ~a~n~n" check-res )))))
  (display (format "Demo results: ~a% ACCEPT, ~a% REJECT" (exact->inexact (* (/ count-accept n) 100)) (exact->inexact (* (/ count-reject n) 100)))))

(demo-checker 100)



