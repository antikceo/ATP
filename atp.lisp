
(defparameter kb1
'(_AND
  (_OR (WS ?x) (SD ?x))
  (_OR (_NOT (SD ?y)) (_NOT (Like ?y Waves)))
  (_OR (_NOT (WS ?z)) (Like ?z Warm))   
  (_OR (_NOT (Like Laura ?w)) (_NOT (Like Jacob ?w)))
  (_OR (Like Jacob ?w) (Like Laura ?w))
  (_OR (Like Jacob Warm))
  (_OR (Like Jacob Waves))
))

(defparameter nq1
'(_AND
  (_OR (_NOT (SD ?v)) (WS ?v)))
)

(defparameter kb2
'(_AND
  (_OR (WS ?x) (SD ?x))
  (_OR (_NOT (WS Jacob)))
))

(defparameter nq2
'(_AND
  (_OR  (_NOT (SD Jacob))))
)


(defparameter kb3
'(_AND
  (_OR (SD ?x) (MD ?x))
  (_OR (_NOT (MD ?y)) (_NOT (LD ?x2 Sun)))
  (_OR (_NOT (SD ?z)) (LD ?z Stars))   
  (_OR (_NOT (LD Lauren ?u)) (_NOT (LD Jermaine ?u)))
  (_OR (LD Jermaine Stars))
))

(defparameter nq3
'(_AND
  (_OR (_NOT (MD Lauren)) (SD Lauren)))
)

(defparameter kb4
'(_AND
  (_OR (_NOT (HD ?x)) (_NOT (TD ?y ?x)) (_NOT (ID ?y)))
  (_OR (_NOT (ED ?x)) (TD John ?x))   
  (_OR (ID John))
))

(defparameter nq4
'(_AND
  (_OR (ED ?w) (HD ?w)))
)

(defparameter kb5 
 '(_AND
   (_OR (_NOT (Columbia ?x)) (isCourse (@sf ?x)))
   (_OR (_NOT (Columbia ?x)) (TakeCourse ?x (@sf ?x)))
   (_OR (Columbia John))))

(defparameter nq5
'(_AND
 (_OR (_NOT (isCourse ?y)) (_NOT (TakeCourse John ?y))
 )))



;;params used throughout the program
(defparameter new-kb NIL)
(defparameter new-nq NIL)
(defparameter root NIL)
(defparameter fringe NIL)
(defparameter state-temp NIL)
(defparameter action-temp NIL)
(defparameter cost-temp NIL)
(defparameter result NIL)
(defparameter holder1 NIL)
(defparameter holder2 NIL)
(defparameter answer NIL)
(defvar *nodes-expanded* 0)

(defstruct (functn (:conc-name nil)) clause parameters)

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
  (= (length (q-elements q)) 0))       

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))             

(defun q-remove (q)
  "Removes the element from the front of the queue and returns it."
  (if (listp (q-elements q))
      (pop (q-elements q))             
    (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  (funcall (q-enqueue q) q items)
  q)

;;;; The Three Enqueing Functions
(defun enqueue-LIFO (q items)
  "Adds a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q))) 
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
			    )))
	(when (> (heap-val heap i key) (heap-val heap smaller-child key))
	  (rotatef (elt heap i) (elt heap smaller-child))  
	  (heapify heap smaller-child key))))
    ))

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))       
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)      
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
(defun search-heuristic (kb nq successorf goalp samep heur-f &optional (enqueuef #'append))
  ;;new node = root
  (setq root (make-node :state (list NIL NIL nq) :action kb))
  ;;new queue = fringe
  (setq fringe (make-q :enqueue #'enqueue-priority :key heur-f))
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
(defun action-sequence (node &optional (temp nil))
  (if (node-parent node)
      ;;add all parent nodes to our list stored in 'temp'
    (action-sequence (node-parent node) 
                     (cons (outp (node-state node)) (cons '=====> temp)))
                     ;;(cons (node-state node) (cons '=====> temp)))
    (format t "~{~a~^~% ~}" temp)
    ))

(defun outp (state &optional (temp nil))
  (setq holder1 (car state))
  ;(format t "list: ~S~% list2: ~S~% temp: ~S~% other: ~S~%"
   ;       holder1  state temp (cdr state))
  
  (if (not holder1) temp
    (if (listp (car holder1)) (outp (cdr state) (append temp (neg holder1)))
      (outp (cdr state) (append temp (pos holder1))))
    ))

(defun checker1 (temp)
  (if (listp (car temp)) (neg temp)
    (pos temp))
  )

(defun pos (state)
  ;(format t "Test pos")
  (setq holder2  (list "@" (clause (car state))  (parameters (car state))))
  (if (not (cdr state)) holder2
    (cons holder2 (checker1 (cdr state)))))
      
(defun neg (state)
  ;(format t "Test neg")
  (setq holder2 (list "_NOT @" (clause (cadar state)) (parameters (cadar state))))
  (if (not (cdr state)) holder2
    (cons holder2 (checker1 (cdr state)))))
     

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
                       (setq state-temp (list actions (caddr (node-state node)) (resolution actions (caddr (node-state node)))))
		       (setq action-temp (node-action node))
		       (setq cost-temp 1)

                       ;;if resolution fails, ignore node
                       (cond ((equal (caddr state-temp) 'fail) NIL) 
			     ((equal (length (caddr state-temp)) (+ (length actions) (length (caddr (node-state node))))) NIL)
			     (T (list action-temp state-temp cost-temp))
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

;;heuristic function
(defun heur (node)
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
(defun resolution (x y &optional (theta NIL) (temp_x x))
  ;; 
  (if (not y) (unify-res x theta temp_x)
    (if (not x) (resolution y NIL theta temp_x)
      (resolution (each-res (car y) x theta) (cdr y) (append theta (nth-value 1 (each-res (car y) x theta))) temp_x))))

;process resolution results
(defun unify-res (x theta temp_x)
  (if (equalp x temp_x) temp_x 
    (do-unification theta x) 
 ))

;use each predicate to resolute a list
(defun each-res (y x theta &optional (result NIL) (old-x x))
  (if (not x) (values-list (list (unify-reso result y old-x) theta))
     (if (can-resolve y (car x) theta) 
        (each-res y (cdr x) (append theta (unify (rem-neg y) (rem-neg (car x)))) result old-x)
      (if (resolvable y (car x) theta) 
          (each-res y (cdr x) (append theta (unify (rem-neg y) (rem-neg (car x)))) (append result (list y)) old-x)
        (each-res y (cdr x) theta (append result (list (car x))) old-x)
        ))
))

;process each-res results
(defun unify-reso (result y old-x)
 (if (and (equalp result old-x) (not (member y result)))
  (append result (list y))
  result
 ))

;judge whether two predicates can be resolved
(defun can-resolve (x y &optional (theta NIL))
  (if (equal x NIL) NIL  
    ;;else check for equality of predicates
    ;;then check for whether they both have opposite signs
    ;;check that their unification does not fail 
    (if (and (equal (get-pn x) (get-pn y)) 
             (not (equal (is-pos x) (is-pos y))) 
             (not (equal 'fail (unify (rem-neg x) (rem-neg y) theta)))
             ) T
      NIL
      )))

;get predicates name
(defun get-pn (x)
  (if (listp x)
      (clause (cadr x))
    (clause x)))

;;if 
(defun is-pos (x)
  (if (listp x)
      NIL
    T
    ))

(defun is-neg (x)
  (if (listp x)
      NIL
    T
    ))

;elminate the '_NOT'
(defun rem-neg (x)
  (if (is-pos x)
      x
    (cadr x)
    ))

;check whether same term to be resolved
(defun resolvable (x y &optional (theta NIL))
 (if (equal x NIL)
  NIL
  (if (and (equal (get-pn x) (get-pn y))
           (equal (is-pos x) (is-pos y))
           (not (equal 'fail (unify (rem-neg x) (rem-neg y) theta))) 
      )
   T
   NIL
  )))

;apply unification
(defun do-unification (theta x)
  ;;
  (if (not theta) (clear-duplicates x)
    (do-unification (cdr theta) (indiv-unification (car theta) x))
    ))

;remove same term in the clauses
(defun clear-duplicates (x)
  (remove-duplicates x :test #'equalp))

;apply unifications in theta individually
(defun indiv-unification (theta-item x &optional (result NIL))
 (cond ((not x) result)
       ;if it is negative
       ((listp (car x)) (indiv-unification theta-item (cdr x) 
                   (append result (list(list '_NOT (make-functn :clause (clause (cadar x)) :parameters (substitute (cdr theta-item) (car theta-item) (parameters (cadar x)))))))))
                   ;;(append result (list (cadar x)))))
                   ;;NIL))
       ;if it is positive
       ((functn-p (car x)) (indiv-unification theta-item (cdr x) 
                   (append result (list (make-functn :clause (clause (car x)) :parameters (substitute (cdr theta-item) (car theta-item) (parameters (car x))))))))))
                   ;;(append result (list (car x)))))))
                   ;;NIL))))

(defun unify (x y &optional (theta nil))
   (cond ((eql theta 'fail) 'fail)
         ((eql x y) theta)
         ((varp x) (unify-var x y theta))
         ((varp y) (unify-var y x theta))
         ((and (functn-p x) (functn-p y)) 
          (unify (parameters x) (parameters y) (unify (clause x) (clause y) theta)))
         ((and (listp x) (listp y)) 
          ;;
          (unify (cdr x) (cdr y) (unify (car x) (car y) theta)))
         (t 'fail)))

(defun unify-var (var x theta)
   (let ((varbinding (assoc var theta))
         (xbinding (assoc x theta))
         (temp-x (replacer theta x)))
        (cond (varbinding (unify (cdr varbinding) x theta))
              (xbinding (unify var (cdr xbinding) theta)) 
              ((occurs-p var temp-x) 'fail)
              (t (cons (cons var temp-x) theta))
        )))


(defun occurs-p (var temp-x)
 (cond ((and (functn-p temp-x) (member var (parameters temp-x))) T)
       (T NIL)
 ))

;;checks for whether symbol is variable
;;vars start with '?' 
;;
(defun varp (var) 
 (if (and (atom var) (equal (subseq (prin1-to-string var) 0 1) "?"))
  T
  NIL
 ))

;replace variables with current substitute pair
(defun replace-var (parameters var theta)
 (if (eq var NIL)
  parameters
  (if (equal (assoc (car var) theta) NIL)
   (replace-var parameters (cdr var) theta)
   (replace-var (substitute (cdr (assoc (car var) theta)) (car var) parameters) (cdr var) theta))))

;do replacements
(defun replacer (theta x)
 (if (functn-p x)
  (make-functn :clause (clause x) :parameters (replace-var (parameters x) (parameters x) theta))   
  x ))

(defun convert-nq (nq)
  (if (string= (car nq) "_AND") (convert2 (cdr nq))           
    nq)
  )

(defun convert2 (rest-kb &optional (result nil))
  ;;if nq is empty
  (m-struc (cdar rest-kb))
  )

(defun convert-input (kb)
  (if (string= (car kb) "_AND") (convert1 (cdr kb))           
    kb)
)

(defun convert1 (rest-kb &optional (result nil))
  ;;if kb is empty
  (if (not rest-kb) result
    (convert1 (cdr rest-kb) (append result (list (m-struc (cdar rest-kb))))))
    ;;(list  (m-struc (cdar kb)) (convert1 (cdr kb) )))
  )

(defun m-struc (x  &optional (result nil))
  (if (not x) result
    ;;if it is negative
    (if (string= (caar x) "_NOT") 
         (m-struc (cdr x) (append result  (list (list '_NOT (make-functn :clause (caadar x) :parameters (cdadar x) )))))
       (m-struc (cdr x) (append result (list (make-functn :clause (caar x) :parameters (cdar x)))))
       )))

(defun recurs (x &optional (result nil))
  (if (not x) result
    (list (car x) (recurs (cdr x) (append result (car x))))
  ))


;;main function
(defun atp (kb nq)
  (setq new-kb  (copy-list kb))
  (setq new-nq  (copy-list nq))
  (setq *nodes-expanded* 1)  
  (setq new-kb  (convert-input new-kb))
  (setq new-nq  (convert-nq new-nq))
  (setq answer (search-heuristic new-kb new-nq #'successor #'goalp #'samep #'heur #'enqueue-priority))
  ;;(setq answer (search-heuristic kb nq #'successor #'goalp #'samep #'heur #'enqueue-priority))
  answer
)







