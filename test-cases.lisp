
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

