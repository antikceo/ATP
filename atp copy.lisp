;;params used throughout the program
(defparameter root NIL)
(defparameter fringe NIL)
(defparameter s-state NIL)
(defparameter s-action NIL)
(defparameter s-cost NIL)
(defparameter a-e-binding NIL)
(defparameter sub-set NIL)
(defparameter result NIL)
(defparameter answer NIL)
(defvar *nodes-expanded* 0)
(defvar *global-theta* NIL)

(defstruct (compound (:conc-name nil)) op args)

(defun make-node (&key state parent action (path-cost 0) (depth 0))
 (list state parent action path-cost depth))

(defun node-state (node) (car node))
(defun node-parent (node) (cadr node))
(defun node-action (node) (caddr node))
(defun node-path-cost (node) (cadddr node))
(defun node-depth (node) (car (cddddr node)))

(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
  (last nil)
  (elements nil))

(defun q-emptyp (q)
  "Returns T if queue is empty."
  (= (length (q-elements q)) 0))       ; (length x) works for both lists and arrays with fill-pointers

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))              ; (elt x n) works for both lists and arrays

(defun q-remove (q)
  "Removes the element from the front of the queue and returns it."
  (if (listp (q-elements q))
      (pop (q-elements q))             ; (pop x) alters x by removing the car, then returns the item removed
    (heap-pop (q-elements q) (q-key q))))

;;inserts 'items' into the q
(defun q-insert (q items)
  (funcall (q-enqueue q) q items)
  q)

;;;; The Three Enqueing Functions

(defun enqueue-LIFO (q items)
  "Adds a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q)))  ; (nconc x y) is destructive version of (append x y)
  items
  )

(defun enqueue-FIFO (q items)
  "Adds a list of items to the end of the queue."
  (if (q-emptyp q) 
      (setf (q-elements q) items)
    (setf (cdr (q-last q)) items))
  (setf (q-last q) (last items))
  items
  )


(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, moves heap[i] down where it belongs."
  (unless (heap-leafp heap i)
    (let ((l (heap-left i))
	  (r (heap-right i)))
      (let ((smaller-child (if (and (< r (length heap))  (< (heap-val heap r key) (heap-val heap l key)))
			    r 
			    l
			    )
	      )
	    )
	(when (> (heap-val heap i key) (heap-val heap smaller-child key))
	  (rotatef (elt heap i) (elt heap smaller-child))    ; (rotatef x y) swaps values of x and y
	  (heapify heap smaller-child key))))
    )
)

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))        ; (decf x) decrements x
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)       ; (vector-push-extend value array) adds the value to the next
                                      ; available position in the array, incrementing the fill-pointer
                                      ; and increasing the size of the array if necessary.
  (setf (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key)) 
	item)

  )

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items down in the process."
  (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
	 i)
	(t
	 (setf (elt heap i) (elt heap (heap-parent i)))
	 (heap-find-pos heap (heap-parent i) val key))
	))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

;heuristic search function
(defun search-heuristic (kb nq successorf goalp samep f-function &optional (enqueuef #'append))
  ;;new node = root
  (setq root (make-node :state (list NIL NIL nq) :action kb))
  ;;new queue = fringe
  (setq fringe (make-q :enqueue #'enqueue-priority :key f-function))
  ;;insert root node into queue
  (q-insert fringe (list root)) 
  (graph-search fringe nil successorf goalp samep enqueuef))

;;graph search function
(defun graph-search (fringe closed successorf goalp samep enqueuef)
  (when fringe
    ;;pops front value from queue and stores in 'node'
    (let ((node (q-front fringe))  
          (fringe (fringe-create fringe)))
      ;;return stored path if at goal node
      (cond ((funcall goalp (node-state node)) (action-sequence node))
            ;;checks if node has already been expanded. does nothing to it if true.
            ((member node closed :test (lambda (x y) (funcall samep (node-state x) (node-state y))))
                        (graph-search fringe closed successorf goalp samep enqueuef))
            ;;else, add successor nodes for 'node' to queue
            (t (graph-search (fringe-successors fringe node enqueuef successorf) (cons node closed) successorf goalp samep enqueuef))))))

;;Returns path of successful solution
(defun action-sequence (node &optional (temp '(proven!)))
  (if (node-parent node)
      ;;add all parent nodes to our list stored in 'temp'
    (action-sequence (node-parent node) 
                     (cons (node-state node) (cons '=====> temp)))
    (format t "~{~a~^~% ~}" temp)
    ))

;create new create with all but top element
(defun fringe-create (fringe) 
 (q-remove fringe)
 fringe
)

;add 'node's' successors our queue
(defun fringe-successors (fringe node enqueuef successorf)
 (funcall enqueuef fringe (expand successorf node))
 fringe
)

;;Inserts items into queue by priority
(defun enqueue-priority (q items)  
  ;;if has no previous elements, create the heap
  (when (null (q-elements q)) (setf (q-elements q) (make-heap)))
  ;;insert items into the heap
  (mapc (lambda (item) (heap-insert (q-elements q) item (q-key q))) items))

;make action-state-cost list
(defun successor (node)
;;increments *nodes-expanded* to reflect current no. of nodes expanded
 (setq *nodes-expanded* (+ *nodes-expanded* (length (node-action node))))
 (remove NIL (mapcar (lambda (actions)
                       ;;pass our knowledge base and nq into the resolution function
                       (setq s-state (list actions (caddr (node-state node)) (resolution actions (caddr (node-state node)))))
		       (setq s-action (node-action node))
		       (setq s-cost 1)

                       ;;if resolution fails, ignore node
                       (cond ((equal (caddr s-state) 'fail) NIL) 
			     ((equal (length (caddr s-state)) (+ (length actions) (length (caddr (node-state node))))) NIL)
			     (T (list s-action s-state s-cost))
		       ))
		     (node-action node))))

;the goal is achieved when the state equals to NIL
(defun goalp (current-state)
 (if (equal (caddr current-state) NIL)
  T
  NIL))

;two states are equal when equal function returns T
(defun samep (x y)
 (if (equalp (caddr x) (caddr y))
  T
  NIL))

;KISS heuristic function
(defun heur (node)
 ;1
 (if (or (equal (length (car (node-state node))) 1) (equal (length (cadr (node-state node))) 1))
   (node-path-cost node)
   (1+  (node-path-cost node))))

;make a successor node from action-state-cost list
(defun expand (successorf node)
  ;;arg for lambda function is action-state-node list returned from the successor function
  (mapcar (lambda (asc)
              (let ((action (car asc))  (state (cadr asc))  (cost (caddr asc)))
                (make-node :state state 
                           :parent node
                           :action action 
                           ;;add 1 to cost and depth after expansion
                           :path-cost (+ (node-path-cost node) cost)
                           :depth (1+ (node-depth node)))))
           (funcall successorf node)))

;resolution of kb and nq
(defun resolution (x y &optional (theta NIL) (ol-x x))
  ;;
  (if (not y) (resolution-end x theta ol-x)
    (if (not x) (resolution y NIL theta ol-x)
      (resolution (resolution-each (car y) x theta) (cdr y) (append theta (nth-value 1 (resolution-each (car y) x theta))) ol-x))))

;process resolution results
(defun resolution-end (x theta ol-x)
  (if (equalp x ol-x) ol-x 
    (apply-unify theta x) 
 ))

;use each predicate to resolute a list
(defun resolution-each (y x theta &optional (result NIL) (old-x x))
 (if (not x) (values-list (list (resolution-each-end result y old-x) theta))
  (if (resolution-able y (car x) theta) (resolution-each y (cdr x) (append theta (unify (remove-neg y) (remove-neg (car x)))) result old-x)
   (if (same-resolution-able y (car x) theta) 
       (resolution-each y (cdr x) (append theta (unify (remove-neg y) (remove-neg (car x)))) (append result (list y)) old-x)
    (resolution-each y (cdr x) theta (append result (list (car x))) old-x)
   ))))

;process resolution-each results
(defun resolution-each-end (result y old-x)
 (if (and (equalp result old-x) (not (member y result)))
  (append result (list y))
  result
 ))

;judge whether two predicates can be resolved
(defun resolution-able (x y &optional (theta NIL))
  (if (equal x NIL)
      NIL  
    (if (and (equal (get-pn x) (get-pn y)) 
             (not (equal (whether-positive x) (whether-positive y))) 
             (not (equal 'fail (unify (remove-neg x) (remove-neg y) theta)))
             ) T
      NIL
      )))

;get predicates name
(defun get-pn (x)
 (if (listp x)
  (op (cadr x))
  (op x)))

;judge whether same term to be resolved
(defun same-resolution-able (x y &optional (theta NIL))
 (if (equal x NIL)
  NIL
  (if (and (equal (get-pn x) (get-pn y))
           (equal (whether-positive x) (whether-positive y))
           (not (equal 'fail (unify (remove-neg x) (remove-neg y) theta))) 
      )
   T
   NIL
  )))

;judge whether a predicate is positive
(defun whether-positive (x)
 (if (listp x)
  NIL
  T
 ))

;apply unification
(defun apply-unify (theta x)
  ;;
  (if (not theta) (remove-same-term x)
    (apply-unify (cdr theta) (apply-unify-each (car theta) x))
    ))

;remove same term in the clauses
(defun remove-same-term (x)
  ;;consider delete-duplicates
  (remove-duplicates x :test #'equalp))

;apply unifications in theta individually
(defun apply-unify-each (theta-item x &optional (result NIL))
 (cond ((not x) result)
       ;if it is negative
       ((listp (car x)) (apply-unify-each theta-item (cdr x) 
                   (append result (list (list '~ (make-compound :op (op (cadar x)) :args (substitute (cdr theta-item) (car theta-item) (args (cadar x)))))))))
       ;if it is positive
       ((compound-p (car x)) (apply-unify-each theta-item (cdr x) 
                   (append result (list (make-compound :op (op (car x)) :args (substitute (cdr theta-item) (car theta-item) (args (car x)))))))) 
 ))

(defun unify (x y &optional (theta nil))
   (cond ((eql theta 'fail) 'fail)
         ((eql x y) theta)
         ((varp x) (unify-var x y theta))
         ((varp y) (unify-var y x theta))
         ((and (compound-p x) (compound-p y)) 
                     (unify (args x) (args y) (unify (op x) (op y) theta)))
         ((and (listp x) (listp y)) 
                    (unify (cdr x) (cdr y) (unify (car x) (car y) theta)))
         (t 'fail)))

(defun unify-var (var x theta)
   (let ((varbinding (assoc var theta))
         (xbinding (assoc x theta))
         (xsub (sub theta x)))
        (cond (varbinding (unify (cdr varbinding) x theta))
              (xbinding (unify var (cdr xbinding) theta)) 
              ((occurs-p var xsub) 'fail)
              (t (cons (cons var xsub) theta))
        )))

;whether occurs
(defun occurs-p (var xsub)
 (cond ((and (compound-p xsub) (member var (args xsub))) T)
       (T NIL)
 ))

;;checks for whether symbol is variable
;;vars start with '?' not '*'
(defun varp (var) 
 (if (and (atom var) (equal (subseq (prin1-to-string var) 0 1) "?"))
  T
  NIL
 ))

;apply each one in the theta to the x
;var means the current substitue pair
(defun sub-var (args var theta)
 (if (eq var NIL)
  args
  (if (equal (assoc (car var) theta) NIL)
   (sub-var args (cdr var) theta)
   (sub-var (substitute (cdr (assoc (car var) theta)) (car var) args) (cdr var) theta))))

;apply theta to x
(defun sub (theta x)
 (if (compound-p x)
  (make-compound :op (op x) :args (sub-var (args x) (args x) theta))   
  x ))

;elminate the '~'
(defun remove-neg (x)
 (if (whether-positive x)
  x
  (cadr x)
 ))

;;prover function, accepts knowledge base and nq
(defun atp (kb nq)
 (setq *nodes-expanded* 1)  
;;sets param 'answer' to the value returned by heuristic
 (setq answer (search-heuristic kb nq #'successor #'goalp #'samep #'heur #'enqueue-priority))
 answer
)


;;extra credit
;next var characters to assign
(defvar *var-to-assign* -1)

;next constant characters to assign
(defvar *constant-to-assign* -1)

;next function characters to assign
(defvar *function-to-assign* -1)

;process =>
(defun remove-im (x)
  (cond ((and (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (append (list (car x)) (remove-im (cdr x))))
        ((and (listp (car x)) (equal (length x) 1)) (list (remove-im (car x))))
        ((and (listp (car x)) (not (equal (length x) 1))) x)
        ((equal (car x) '=>) (list '|| (list '~ (cadr x)) (caddr x)))
        (T x)
  )
)

;process im=>
(defun process-im (x)
 (cond ((equal x NIL) x)
       ((compound-p x) x)
       ((and (listp (remove-im x)) (listp (car x))) (append (list (process-im (car (remove-im x)))) (process-im (cdr (remove-im x)))))
       ((and (listp (remove-im x)) (not (listp (car x)))) (append (list (car x)) (process-im (cdr (remove-im x)))))
       (T x) 
 )
)


;process 'all and 'exist
(defun a-e (x)
 (setq a-e-binding (extract-a-e x))
 (append a-e-binding (list (remove-a-e x))) 
)

;extract 'all and 'exist
(defun extract-a-e (x &optional (bind NIL))
 (cond ((or (compound-p x) (and (listp x) (compound-p (car x)))) bind)
       ((and (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (extract-a-e (cadr x) (append bind (list (car x)))));if have all or exist
       ((listp (car x)) (extract-a-e (car x) bind))
       ((equal (car x) '~) (extract-a-e (cdr x) bind));if ~
       ((or (equal (car x) '=>) (equal (car x) '<=>) (equal (car x) '=)) (append bind (extract-a-e (cadr x)) (extract-a-e (caddr x))))
       ((or (equal (car x) '||) (equal (car x) '&)) (append bind (extract-a-e (cadr x)) (extract-a-e (caddr x))))
       ((atom (car x)) bind)
       (T 'FAIL)
 )
)


;remove a-e elements
(defun remove-a-e (x)
 (cond ((or (compound-p x) (and (listp x) (compound-p (car x)))) x)
       ((and (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (remove-a-e (cadr x)));if have all or exist  
       ((listp (car x)) (remove-a-e (car x)))
       ((equal (car x) '~) x);if ~
       ((or (equal (car x) '=>) (equal (car x) '<=>) (equal (car x) '=)) (append (list (car x)) (list (remove-a-e (cadr x))) (list (remove-a-e (caddr x)))))
       ((or (equal (car x) '||) (equal (car x) '&))(append (list (car x)) (list (remove-a-e (cadr x))) (list (remove-a-e (caddr x)))))
       ((atom (car x)) (car x))
       (T 'FAIL)
 )
)

;process each '~
(defun process-minus-each (x)
 (cond ((compound-p x) (list '~ x))
       ((and (listp x) (equal (car x) '~)) (cadr x))
       ((and (listp x) (listp (car x)) (equal (caar x) 'all) (equal (length x) 2)) (list (list 'exist (cadar x)) (list '~ (cadr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'exist) (equal (length x) 2)) (list (list 'all (cadar x)) (list '~ (cadr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'all)) (list (list 'exist (cadar x)) (list '~ (cdr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'exist)) (list (list 'all (cadar x)) (list '~ (cdr x))))
       ((and (listp x) (equal (car x) '&)) (list '|| (list '~ (cadr x)) (list '~ (caddr x))))
       ((and (listp x) (equal (car x) '||)) (list '& (list '~ (cadr x)) (list '~ (caddr x))))
       (T (list '~ x))))

;process '~ once
(defun process-minus-once (x)
 (cond ((compound-p x) x)
       ((and (listp x) (equal (car x) '~) (not (compound-p (cadr x))) (not (atom (cadr x)))) (process-minus-once (process-minus-each (cadr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'all)) (list (list 'all (cadar x)) (process-minus-once (cdr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'exist)) (list (list 'exist (cadar x)) (process-minus-once (cdr x))))
       ((and (listp x) (or (equal (car x) '&) (equal (car x) '||) (equal (car x) '=))) (list (car x) (process-minus-once (cadr x)) (process-minus-once (caddr x))))
      
       ((and (listp x) (equal (length x) 1)) (process-minus-once (car x)))
       (T x)))

;get cnf
(defun cnf (x &optional (c-list NIL) (flag NIL))
 (cond ((compound-p x) (append c-list (list x)))
       ((and (listp x) (equal '~ (car x))) (list (append c-list x)))
       ((and (listp x) (equal '|| (car x)) (equal flag T)) (append c-list (list (append (cnf (cadr x)) (cnf (caddr x))))))
       ((and (listp x) (equal '|| (car x)) (equal flag NIL)) (append c-list (cnf (cadr x)) (cnf (caddr x))))
       ((and (listp x) (equal (length x) 1)) (cnf (car x) c-list flag))
       ((and (listp x) (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (cnf (cdr x) (append c-list (list (car x))) T))
       (T c-list)
 )
)


;make sub list
(defun sub-list (x &optional (sub-set NIL) (occur-all NIL))
 (cond ((and (listp (car x)) (equal (caar x) 'all)) 
                                (sub-list (cdr x) (append sub-set (list (cons (cadar x) (next-var)))) (next-var)))

       ((and (listp (car x)) (equal (caar x) 'exist) occur-all) 
                        (sub-list (cdr x) (append sub-set (list (cons (cadar x) (make-compound :op (next-f) :args (list occur-all))))) occur-all))
       
       ((and (listp (car x)) (equal (caar x) 'exist) (not occur-all)) 
                        (sub-list (cdr x) (append sub-set (list (cons (cadar x) (next-c)))) NIL))
       
       (T sub-set)))

;next-var
(defun next-var()
 (setq *var-to-assign* (1+ *var-to-assign*))
 ;because the var will be use twice
 (nth *var-to-assign* '(*o *o *p *p *q *q *r *r *s *s *t *t *u *u *v *v *w *w *x *x *y *y *z *z)))

;next-var
(defun next-c()
 (setq *constant-to-assign* (1+ *constant-to-assign*))
 (nth *constant-to-assign* '(k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15)))

;next-var
(defun next-f()
 (setq *function-to-assign* (1+ *function-to-assign*))
 (nth *function-to-assign* '(f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)))

;drop quantifier
(defun drop-q (x sub-set)
 (cond ((and (listp x) (listp (car x)) (or (equal 'all (caar x)) (equal 'exist (caar x)))) (drop-q (cdr x) sub-set))
       ((and (listp x) (listp (car x)) (equal (length x) 1)) (drop-q (car x) sub-set))
       (T (apply-unify sub-set x))))

(defun snf (fol)
 (setq *var-to-assign* -1)
 (setq *constant-to-assign* -1)
 (setq *function-to-assign* -1)
 (setq result (process-im fol))
 (setq result (process-minus-once result))
 (setq result (a-e result))
 (setq result (cnf result))
 (setq sub-set (sub-list result))
 (setq result (drop-q result sub-set)))




























;;(m-struc (cdr x))
;;(append nil (recurs (cdadar x)))
        ;;(list (list '~ (make-functn :proposition (caadar x) :parameters (append '(car (cdadar x)) (recurs (cdr (cdadar x)))))) (m-struc (cdr x)))
      ;;(list (make-functn :proposition (caar x) :parameters (cadar x))  (m-struc (cdr x)))

;;extra credit
;next var characters to assign
(defvar *var-to-assign* -1)

;next constant characters to assign
(defvar *constant-to-assign* -1)

;next function characters to assign
(defvar *function-to-assign* -1)

;process =>
(defun remove-im (x)
  (cond ((and (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (append (list (car x)) (remove-im (cdr x))))
        ((and (listp (car x)) (equal (length x) 1)) (list (remove-im (car x))))
        ((and (listp (car x)) (not (equal (length x) 1))) x)
        ((equal (car x) '=>) (list '|| (list '~ (cadr x)) (caddr x)))
        (T x)
  )
)

;process im=>
(defun process-im (x)
 (cond ((equal x NIL) x)
       ((functn-p x) x)
       ((and (listp (remove-im x)) (listp (car x))) (append (list (process-im (car (remove-im x)))) (process-im (cdr (remove-im x)))))
       ((and (listp (remove-im x)) (not (listp (car x)))) (append (list (car x)) (process-im (cdr (remove-im x)))))
       (T x) 
 )
)


;process 'all and 'exist
(defun a-e (x)
 (setq a-e-binding (extract-a-e x))
 (append a-e-binding (list (remove-a-e x))) 
)

;extract 'all and 'exist
(defun extract-a-e (x &optional (bind NIL))
 (cond ((or (functn-p x) (and (listp x) (functn-p (car x)))) bind)
       ((and (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (extract-a-e (cadr x) (append bind (list (car x)))));if have all or exist
       ((listp (car x)) (extract-a-e (car x) bind))
       ((equal (car x) '~) (extract-a-e (cdr x) bind));if ~
       ((or (equal (car x) '=>) (equal (car x) '<=>) (equal (car x) '=)) (append bind (extract-a-e (cadr x)) (extract-a-e (caddr x))))
       ((or (equal (car x) '||) (equal (car x) '&)) (append bind (extract-a-e (cadr x)) (extract-a-e (caddr x))))
       ((atom (car x)) bind)
       (T 'FAIL)
 )
)


;remove a-e elements
(defun remove-a-e (x)
 (cond ((or (functn-p x) (and (listp x) (functn-p (car x)))) x)
       ((and (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (remove-a-e (cadr x)));if have all or exist  
       ((listp (car x)) (remove-a-e (car x)))
       ((equal (car x) '~) x);if ~
       ((or (equal (car x) '=>) (equal (car x) '<=>) (equal (car x) '=)) (append (list (car x)) (list (remove-a-e (cadr x))) (list (remove-a-e (caddr x)))))
       ((or (equal (car x) '||) (equal (car x) '&))(append (list (car x)) (list (remove-a-e (cadr x))) (list (remove-a-e (caddr x)))))
       ((atom (car x)) (car x))
       (T 'FAIL)
 )
)

;process each '~
(defun process-minus-each (x)
 (cond ((functn-p x) (list '~ x))
       ((and (listp x) (equal (car x) '~)) (cadr x))
       ((and (listp x) (listp (car x)) (equal (caar x) 'all) (equal (length x) 2)) (list (list 'exist (cadar x)) (list '~ (cadr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'exist) (equal (length x) 2)) (list (list 'all (cadar x)) (list '~ (cadr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'all)) (list (list 'exist (cadar x)) (list '~ (cdr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'exist)) (list (list 'all (cadar x)) (list '~ (cdr x))))
       ((and (listp x) (equal (car x) '&)) (list '|| (list '~ (cadr x)) (list '~ (caddr x))))
       ((and (listp x) (equal (car x) '||)) (list '& (list '~ (cadr x)) (list '~ (caddr x))))
       (T (list '~ x))))

;process '~ once
(defun process-minus-once (x)
 (cond ((functn-p x) x)
       ((and (listp x) (equal (car x) '~) (not (functn-p (cadr x))) (not (atom (cadr x)))) (process-minus-once (process-minus-each (cadr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'all)) (list (list 'all (cadar x)) (process-minus-once (cdr x))))
       ((and (listp x) (listp (car x)) (equal (caar x) 'exist)) (list (list 'exist (cadar x)) (process-minus-once (cdr x))))
       ((and (listp x) (or (equal (car x) '&) (equal (car x) '||) (equal (car x) '=))) (list (car x) (process-minus-once (cadr x)) (process-minus-once (caddr x))))
      
       ((and (listp x) (equal (length x) 1)) (process-minus-once (car x)))
       (T x)))

;get cnf
(defun cnf (x &optional (c-list NIL) (flag NIL))
 (cond ((functn-p x) (append c-list (list x)))
       ((and (listp x) (equal '~ (car x))) (list (append c-list x)))
       ((and (listp x) (equal '|| (car x)) (equal flag T)) (append c-list (list (append (cnf (cadr x)) (cnf (caddr x))))))
       ((and (listp x) (equal '|| (car x)) (equal flag NIL)) (append c-list (cnf (cadr x)) (cnf (caddr x))))
       ((and (listp x) (equal (length x) 1)) (cnf (car x) c-list flag))
       ((and (listp x) (listp (car x)) (or (equal (caar x) 'all) (equal (caar x) 'exist))) (cnf (cdr x) (append c-list (list (car x))) T))
       (T c-list)
 )
)


;make sub list
(defun sub-list (x &optional (sub-set NIL) (occur-all NIL))
 (cond ((and (listp (car x)) (equal (caar x) 'all)) 
                                (sub-list (cdr x) (append sub-set (list (cons (cadar x) (next-var)))) (next-var)))

       ((and (listp (car x)) (equal (caar x) 'exist) occur-all) 
                        (sub-list (cdr x) (append sub-set (list (cons (cadar x) (make-functn :proposition (next-f) :parameters (list occur-all))))) occur-all))
       
       ((and (listp (car x)) (equal (caar x) 'exist) (not occur-all)) 
                        (sub-list (cdr x) (append sub-set (list (cons (cadar x) (next-c)))) NIL))
       
       (T sub-set)))

;next-var
(defun next-var()
 (setq *var-to-assign* (1+ *var-to-assign*))
 ;because the var will be use twice
 (nth *var-to-assign* '(*o *o *p *p *q *q *r *r *s *s *t *t *u *u *v *v *w *w *x *x *y *y *z *z)))

;next-var
(defun next-c()
 (setq *constant-to-assign* (1+ *constant-to-assign*))
 (nth *constant-to-assign* '(k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15)))

;next-var
(defun next-f()
 (setq *function-to-assign* (1+ *function-to-assign*))
 (nth *function-to-assign* '(f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)))

;drop quantifier
(defun drop-q (x sub-set)
 (cond ((and (listp x) (listp (car x)) (or (equal 'all (caar x)) (equal 'exist (caar x)))) (drop-q (cdr x) sub-set))
       ((and (listp x) (listp (car x)) (equal (length x) 1)) (drop-q (car x) sub-set))
       (T (apply-unify sub-set x))))

(defun snf (fol)
 (setq *var-to-assign* -1)
 (setq *constant-to-assign* -1)
 (setq *function-to-assign* -1)
 (setq result (process-im fol))
 (setq result (process-minus-once result))
 (setq result (a-e result))
 (setq result (cnf result))
 (setq sub-set (sub-list result))
 (setq result (drop-q result sub-set)))




;;(defparameter a-e-binding NIL)
;;(defparameter sub-set NIL)

;;(defvar *global-theta* NIL)


;;(append '(car (cdadar x)) (recurs (cdr (cdadar x))))
;;'(car (cdadar x))
;;(recurs (cdadar x))
;;(cdadar x)
;;(cdar x)



;test case 1
(setq test1-1 (list (list '~ (make-functn :clause 'hasfinal :parameters '(?x))) (list '~ (make-functn :clause 'take :parameters '(?y ?x))) (list '~ (make-functn :clause 'ishappy :parameters '(?y)))))
(setq test1-2 (list (list '~ (make-functn :clause 'iseasy :parameters '(?x))) (make-functn :clause 'take :parameters (list (make-functn :clause 'f :parameters '(?x)) '?x))))
(setq test1-3 (list (make-functn :clause 'ishappy :parameters (list (make-functn :clause 'f :parameters '(?x))))))
(setq test-kb1 (list test1-1 test1-2 test1-3))
(setq test-nq1 (list (make-functn :clause 'iseasy :parameters '(?w)) (make-functn :clause 'hasfinal :parameters '(?w))))


;test case 2
(setq test2-1 (list (make-functn :clause 's :parameters '(*x)) (make-functn :clause 'm :parameters '(*x))))
(setq test2-2 (list (list '~ (make-functn :clause 'm :parameters '(*y))) (list '~ (make-functn :clause 'l :parameters '(*x2 rain)))))
(setq test2-3 (list (list '~ (make-functn :clause 's :parameters '(*z))) (make-functn :clause 'l :parameters '(*z snow))))
(setq test2-4 (list (list '~ (make-functn :clause 'l :parameters '(ellen *u))) (list '~ (make-functn :clause 'l :parameters '(tony *u)))))
(setq test2-5 (list (make-functn :clause 'l :parameters '(tony snow))))
(setq test-kb2 (list test2-1 test2-2 test2-3 test2-4 test2-5))
(setq test-nq2 (list (list '~ (make-functn :clause 'm :parameters '(ellen))) (make-functn :clause 's :parameters '(ellen))))


;test case 3
(setq test3-1 (list (make-functn :clause 'animal :parameters '(?w)) (make-functn :clause 'loves :parameters '(?v ?x))))
(setq test3-2 (list (list '~ (make-functn :clause 'loves :parameters '(?x ?u))) (make-functn :clause 'loves :parameters '(?v ?x))))
(setq test3-3 (list (list '~ (make-functn :clause 'loves :parameters '(?y ?x))) (list '~ (make-functn :clause 'animal :parameters '(?z))) (list '~ (make-functn :clause 'kills :parameters '(?x ?z)))))
(setq test3-4 (list (list '~ (make-functn :clause 'animal :parameters '(?x))) (make-functn :clause 'loves :parameters '(jack ?x))))
(setq test3-5 (list (make-functn :clause 'kills :parameters '(jack tuna)) (make-functn :clause 'kills :parameters '(curiosity tuna))))
(setq test3-6 (list (make-functn :clause 'cat :parameters '(tuna))))
(setq test3-7 (list (list '~ (make-functn :clause 'cat :parameters '(?x))) (make-functn :clause 'animal :parameters '(?x))))
(setq test-kb3 (list test3-1 test3-2 test3-3 test3-4 test3-5 test3-6 test3-7))
(setq test-nq3 (list (list '~ (make-functn :clause 'kills :parameters '(curiosity tuna)))))



;test case 4 
(setq test4-1 (list (make-functn :clause 'ws :parameters '(*x)) (make-functn :clause 'sd :parameters '(*x))))
(setq test4-2 (list (list '~ (make-functn :clause 'ws :parameters '(*z))) (make-functn :clause 'likes :parameters '(*z Warm))))
(setq test4-3 (list (list '~ (make-functn :clause 'likes :parameters '(Laura *w))) (list '~ (make-functn :clause 'likes :parameters '(Jacob *w)))))
(setq test4-4 (list (make-functn :clause 'likes :parameters '(Jacob Warm))))
(setq test-kb4 (list test4-1 test4-2 test4-3 test4-4))
(setq test-nq4 (list (list '~ (make-functn :clause 'sd :parameters '(*v))) (make-functn :clause 'ws :parameters '(*v))))

;test case 5
(setq test5-1 (list (list '~ (make-functn :clause 'e :parameters '(*x))) (make-functn :clause 'v :parameters '(*x)) (make-functn :clause 's :parameters (list '*x (make-functn :clause 'f :parameters '(*x))))))
(setq test5-2 (list (list '~ (make-functn :clause 'e :parameters '(*x))) (make-functn :clause 'v :parameters '(*x)) (make-functn :clause 'c :parameters (list (make-functn :clause 'f :parameters '(*x))))))
(setq test5-3 (list (make-functn :clause 'p :parameters '(c))))
(setq test5-4 (list (make-functn :clause 'e :parameters '(c))))
(setq test5-5 (list (list '~ (make-functn :clause 's :parameters '(c *y))) (make-functn :clause 'p :parameters '(*y))))
(setq test5-6 (list (list '~ (make-functn :clause 'p :parameters '(*z))) (list '~ (make-functn :clause 'v :parameters '(*z)))))
(setq test-kb5 (list test5-1 test5-2 test5-3 test5-4 test5-5 test5-6))
(setq test-nq5 (list (list '~ (make-functn :clause 'p :parameters '(*w))) (list '~ (make-functn :clause 'c :parameters '(*w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;test cases for snf;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;test case 1
(setq test-snf1 (list '(exist *x) (list '|| (make-functn :clause 'p :parameters '(*x)) (make-functn :clause 'q :parameters '(*x)))))

;test case 2
(setq test-snf2 (list '(all *x) (list '=> (make-functn :clause 'p :parameters '(*x)) (list '~ (make-functn :clause 'q :parameters '(*x))))))

;test case 3
(setq test-snf3 (list '(all *x) '(exist *y) (list '=> (list '& (make-functn :clause 'p :parameters '(*x)) (make-functn :clause 'q :parameters '(*y))) (list '~ (make-functn :clause 'r :parameters '(*x))))))

;test case 4
(setq test-snf4 (list '(exist *x) '(all *y) (list '=> (make-functn :clause 'p :parameters '(*x)) (list '|| (make-functn :clause 'q :parameters '(*x)) (list '~ (make-functn :clause 'r :parameters '(*x *y)))))))

;test case 5
(setq test-snf5 (list '(all *x) (list '=> (make-functn :clause 'p :parameters '(*x)) (list '(exist *y) (list (make-functn :clause 'r :parameters '(*y)))))))

