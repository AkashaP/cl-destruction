(in-package :cl-destruction)

;; A very cheaty library that's all to do with avoiding new consing.

;; have to write ucopy first
;; (defmethod nsync ((source sequence) (target sequence) &optional working-space)
;;   (if (working-space)
;;       (let (()))))

;; (defmethod ucopy-seq ((thing list)))

;; (defmethod ucopy-seq ((thing sequence))
;;   (copy-seq ))

;; (defmethod ucopy ((thing sequence) working-space)
;;   (let ((copied (copy-seq thing)))
;;     (loop for i from 0
;;           do (handler-case
;;                  (let ((item (elt copied i)))
;;                    )
;;                (type-error () (return))))))

(defun make-working-space ()
  (make-hash-table :test #'eq :size 100000000))

;; This one is like the one below it but also conses/trims to match the legnth
(defmethod nsync ((source list) (target list) &optional (proc #'identity)) 
  "Sets the contents of TARGET so all cars of TARGET are the cars of SOURCE
(but not the cons cells themselves)"
  (loop for sc on source
        for tc on target
        with repeat = t
        while (and repeat sc tc)
        if (and (null (cdr sc)) (cdr tc)) do
          (setf (cdr tc) nil repeat nil) else do
            ;; nsync? or...
            (setf (car tc) (funcall proc (car sc)))
        if (and (cdr sc) (null (cdr tc))) do
          (nconc tc (cons (ucopy (funcall proc (cadr sc))) nil))))

;; Old version. Does not nconc new cells, but *appears* to work.
;; (defmethod nsync ((source list) (target list) &optional (proc #'identity)) 
;;   "Sets the contents of TARGET so all cars of TARGET are the cars of SOURCE
;; (but not the cons cells themselves)"
;;   (if (and (null target) (not (null source)))
;;       (push (funcall proc (car source)) target))
;;   (loop
;;     with sc = source
;;     with tc = target 
;;     do (if (atomp (car sc)) 
;;            (setf (car tc) (car sc))
;;            (nsync (funcall proc (car sc)) (car tc))) 
;;        (when (null (cdr sc))
;;          (setf (cdr tc) nil)
;;          (return target))
;;        (when (null (cdr tc))
;;          (setf (cdr tc) (cons (funcall proc (car sc)) nil)))
;;        (setf sc (cdr sc)
;;              tc (cdr tc))))

;; Old version
;; If the cars are also lists then this could entangle the sublists from source onto target, bad!
;; (defmethod nsync ((source list) (target list) &optional (proc #'identity)) 
;;   "Sets the contents of TARGET so all cars of TARGET are the cars of SOURCE
;; (but not the cons cells themselves)"
;;   (if (and (null target) (not (null source)))
;;       (push (funcall proc (car source)) target))
;;   (loop
;;     with sc = source
;;     with tc = target 
;;     do (setf (car tc) (car sc)) 
;;        (when (null (cdr sc))
;;          (setf (cdr tc) nil)
;;          (return target))
;;        (when (null (cdr tc))
;;          (setf (cdr tc) (cons (funcall proc (car sc)) nil)))
;;        (setf sc (cdr sc)
;;              tc (cdr tc))))

(defmethod nsync ((source vector) (target vector) &optional (proc #'identity))
  (if (and (adjustable-array-p target)
           (< (length target) (length source)))
      (loop for i from (length source) below (length target)
            do (vector-push-extend (funcall proc (aref source i)) target))
      (dotimes (x (- (length source) (length target)))
        (vector-pop target)))
  (loop
    for i from 0 below (min (length source) (length target))
    do (if (atomp (aref source i))
           (setf (aref target i) (funcall proc (aref source i)))
           (if (aref target i) 
               (let ((a (funcall proc (aref source i)))
                     (b (aref target i)))
                 ;; (print a)
                 ;; (print b)
                 (nsync (funcall proc (aref source i)) (aref target i)))
               (setf (aref target i) (ucopy (funcall proc (aref source i))))))))

(defmethod nsync ((source hash-table) (target hash-table) &optional (proc #'identity))
  
  ;; Delete things in target not part of source
  (loop
    with last = nil
    with lastk = nil
    for tk being the hash-keys of target
    if last
      do (remhash lastk target)
         (setf last nil lastk nil)
    if (eq nil (nth-value 1 (gethash tk source)))
      do (setf last t lastk tk)
    finally (if last
                (remhash lastk target)))
  ;; synchronise target
  (loop for sk being the hash-keys of source
          using (hash-value dat)
        do (if (atomp dat)
               (setf (gethash sk target) (funcall proc dat))
               ;; TODO this fails if target is nil
               (let ((a (funcall proc dat))
                     (b (gethash sk target)))
                 ;; (print a)
                 ;; (print b)
                 (if (gethash sk target) ; this is like, catching on nil though
                     (nsync (funcall proc dat) (gethash sk target))
                     (setf (gethash sk target) (ucopy (funcall proc dat))))))))

(defmethod nsync ((source structure-object) (target structure-object) &optional (proc #'identity))
  "Copy a structure recursively. Might not work in all implementations."
  (let ((slots (closer-mop:class-direct-slots (class-of source))))
    (dolist (slot slots)
      (let* ((slot-name (closer-mop:slot-definition-name slot))
             (item (slot-value source slot-name)))
        (if (atomp item)
            (setf (slot-value target slot-name)
                  (funcall proc item))
            (if (slot-value target slot-name) 
                (nsync (funcall proc (slot-value source slot-name)) (slot-value target slot-name))
                (setf (slot-value target slot-name)
                      (ucopy (funcall proc (slot-value source slot-name))))))))))

(defun atomp (item)
  (or
   (eq nil item) 
   (not
    (or
     (hash-table-p item)
     (typep item 'sequence) 
     (typep (class-of item) 'structure-class)
     (typep (class-of item) 'standard-class)))))


;; (defmethod nrotate-left ((l list) n &key (from 0 frp) (to (length l) top))
;;   "Destructively modify l in-place (use setq still), rotating it n places left"
;;   (let ((x (nthcdr n l))
;;         (y (nbutlast l (- to n))))
;;     (setq l (nconc x y)))
;;   l)

;; (defmethod nrotate-right ((l list) n)
;;   "Destructively modify l in-place (use setq still), rotating it n places right"
;;   (nrotate-left l (- (length l) n)))

(defmethod nrotate-left ((l sequence) n &key (from 0) (to (- (length l) 1))) 
  "Destructively modify l in-place, rotating it n places right"
  (let ((k (- to from -1)))
    (dotimes (i (gcd k n))
      (let* ((j (+ from (mod i k)))
             (temp (elt l j)))
        (loop for d = (+ from (mod (+ j n (- from)) k))
              repeat (- to from)
              do (setf
                  (elt l j) (elt l d)
                  j d))
        (setf (elt l j) temp))))
  l)

(defmethod nrotate-right ((l sequence) n &key (from 0) (to (- (length l) 1))) 
  "Destructively modify l in-place, rotating it n places left"
  (nrotate-left l (- n) :from from :to to)
  l)

(defun nupdate (source target) 
  "Recursively sets the contents of TARGET to match SOURCE, but keeping them separate."
  (nsync target source
         (lambda (sc tc)
           (if (sequencep (car sc))
               (nupdate sc tc)
               sc))))

(defun bubblesync (pred &rest lists)
  "A version of bubblesort, that syncs (cdr lists) in the same sorted way as (car lists), destructively.

E.g: (synced-bubblesort #'< (list 5 4 3 2 1) (list :e :d :c :b :a) (list 'potatoes :tomatoes 'salads :pastas :pizzas)) =>
((1 2 3 4 5) (:a :b :c :d :e) (list :pizzas :pastas 'salads :tomatoes 'potatoes))

Useful for sorting parallel lists without breaking the index link between them from sorting."
  (declare (dynamic-extent pred lists))
  (loop for swaps = nil
        do (loop 
             with h1 = (car lists)
             with h2 = (cdr lists)
             for c1 on h1
             while (cdr c1)
             do (unless (funcall pred (first c1) (second c1))
                  (setq swaps t)
                  (let ((tmp (first c1)))
                    (setf (first c1) (second c1)
                          (second c1) tmp))
                  (loop for c2 in h2
                        do (let ((tmp (first c2)))
                             (setf (first c2) (second c2)
                                   (second c2) tmp))))
                (setq h2 (mapcar #'cdr h2)))
        while swaps))

(defun bubblesort (pred list)
  (car (bubblesync pred list)))

;; (defsetf (setf find) (item sequence &rest args &key from-end (start 0) end key test test-not))

(defun get-prop (indicator place &key (default nil) (identity #'cadr) (test #'eq) from-end (start 0) end) 
  (declare (type list place)
           (type function identity test)
           (type integer start))
  (if from-end (setq place (nreverse place)
                     start (- (length place) end)
                     end (- (length place) start))) 
  (let ((result
          (loop for p on place by #'cddr
                for i from 0
                ;; if (atom (cdr p))
                ;;   do (error 'simple-type-error
                ;;             :format-control "malformed property list: ~S."
                ;;             :format-arguments (list place)
                ;;             :datum (cdr p)
                ;;             :expected-type 'cons) 
                ;; else
                if (and (< (- start 1) i (if end end (1+ i)))
                        (funcall test indicator (car p)))
                  do (return (values (funcall identity p) t))
                finally (return (values default nil)))))
    (if from-end (setq place (nreverse place)))
    result))

(defun delete-prop (indicator place &key (default nil) (test #'eq) from-end (start 0) end) 
  (declare (type list place)
           (type function test)
           (type integer start))
  (if from-end (setq place (nreverse place)
                     start (- (length place) end)
                     end (- (length place) start))) 
  (let ((result
          (loop for p on place by #'cddr
                for i from 0
                with s = 0
                ;; if (atom (cdr p))
                ;; do (error 'simple-type-error
                ;;           :format-control "malformed property list: ~S."
                ;;           :format-arguments (list place)
                ;;           :datum (cdr p)
                ;;           :expected-type 'cons) 
                ;; else
                if (and (< (- start 1) i (if end end (1+ i)))
                        (funcall test indicator (car p)))
                  do (incf s)
                     (setf (car p) (caddr p)
                           (cadr p) (cadddr p))
                finally (setf place (nbutlast place (* 2 s))))))
    (if from-end (setq place (nreverse place)))
    ;; (nbutlast result s)
    ))

;; Setf variant is not possible to write
;; if defsetf is used, then place will be impossible to change if it is (list)/nil

(defun set-prop (indicator value place &key (test #'eq) from-end (start 0) end)
  (multiple-value-bind
        (place success)
      (get-prop indicator place :test test :from-end from-end :start start :end end :identity #'identity)
    (if success
        (setf (car place) value)
        (progn
          (push value place)
          (push indicator place)))))

;; (defun get-prop (place indicator &key (default nil) (test #'eq) from-end (start 0) end)
;;   "Search the property list stored in PLACE for the first INDICATOR.
;;   If one is found, return the corresponding value, else return DEFAULT."
;;   (if from-end (setq place (nreverse place)
;;                      start (- (length place) end)
;;                      end (- (length place) start)))
;;   (let ((res (do ((plist place (cddr plist))
;;                   (i 0 (1+ i)))
;;                  ((or (null plist)
;;                       (if end (<= end i))) default)
;;                (cond
;;                  ((not (< start i end)))
;;                  ((atom (cdr plist))
;;                   (error 'simple-type-error
;;                          :format-control "malformed property list: ~S."
;;                          :format-arguments (list place)
;;                          :datum (cdr plist)
;;                          :expected-type 'cons))
;;                  ((funcall test (car plist) indicator)
;;                   (return (cadr plist)))))))
;;     (if from-end (setq place (nreverse place))) 
;;     res))

;; (defsetf get-prop (indicator place &optional &key default (test #'eq) from-end (start 0) (end nil)) (value)
;;   (let ((p (gensym)))
;;     `(if (null ,place)
;;          (setf (getf ,place ,indicator) (push ,place ,value))
;;          (loop
;;            initially (if ,from-end (setq ,place (nreverse ,place)))
;;            for ,p on ,place by #'cddr if (funcall ,test (car ,p) ,indicator)
;;            do (setf (cadr ,p) ,value)
;;               (if ,from-end (setq ,place (nreverse place)))
;;               (return ,place)
;;            finally (if ,from-end (setq ,place (nreverse place)))
;;                    (setf (cdr ,place) (cons ,indicator (cdr ,place))
;;                          (cdr ,place) (cons ,value (cdr ,place)))
;;                    (if (cddr ,place)
;;                        (setf (third ,place) (first ,place)
;;                              (first ,place) ,indicator))
;;                    (return ,place)))))

;; works.... except when the f***n place is blank. who designed this dumb facility?
;; (defsetf get-prop (indicator place &optional &key default (test #'eq) from-end (start 0) (end nil)) (value)
;;   (let ((p (gensym)))
;;     `(progn
;;        (setf (getf ,place 2) (cons 1 nil))
;;        (push ,indicator ,place)
;;        ,place
;;        ;; (if (null ,place)
;;        ;;     (setf (getf ,place ,indicator) (push ,place ,value))
;;        ;;     (loop
;;        ;;       initially (if ,from-end (setq ,place (nreverse ,place)))
;;        ;;       for ,p on ,place by #'cddr if (funcall ,test (car ,p) ,indicator)
;;        ;;       do (setf (cadr ,p) ,value)
;;        ;;          (if ,from-end (setq ,place (nreverse place)))
;;        ;;          (return ,place)
;;        ;;       finally (if ,from-end (setq ,place (nreverse place)))
;;        ;;               (setf (cdr ,place) (cons ,indicator (cdr ,place))
;;        ;;                     (cdr ,place) (cons ,value (cdr ,place)))
;;        ;;               (if (cddr ,place)
;;        ;;                   (setf (third ,place) (first ,place)
;;        ;;                         (first ,place) ,indicator))
;;        ;;               (return ,place)))
;;        )))

;; (declaim (inline get-prop))
;; (defsetf get-prop (indicator place &optional &key default (test #'eq) from-end (start 0) (end nil)) (value)
;;   (let ((p (gensym)))
;;     ;; (do ((pl (symbol-plist place) (cddr pl)))
;;     ;;     ((endp pl)

;;     ;;      value)
;;     ;;   (cond ((endp (cdr pl))
;;     ;;          (error "~S has an odd number of items in its property list."
;;     ;;                 symbol))
;;     ;;         ((eq (car pl) indicator)
;;     ;;          (rplaca (cdr pl) value)
;;     ;;          (return value)))) 
;;     `(progn
;;        ;; (setf (getf ,place 2) (cons 1 nil))
;;        (setf ,place
;;              ;; (cons 1 (cons 2 (cons ,place)))
;;              (list* 1 2 a) 
;;              ;; (setf (symbol-plist ,place)
;;              ;;       (cons ,indicator (cons ,value (symbol-plist ,place))))
;;              )
;;        ;; (do ((pl (symbol-plist ,place) (cddr pl)))
;;        ;;     ((endp pl)
;;        ;;      (setf (place-plist ,place)
;;        ;;            (list* ,indicator ,value (symbol-plist ,place)))
;;        ;;      value)
;;        ;;   (cond ((endp (cdr pl))
;;        ;;          (error "~S has an odd number of items in its property list."
;;        ;;                 ,place))
;;        ;;         ((eq (car pl) ,indicator)
;;        ;;          (rplaca (cdr pl) ,value)
;;        ;;          (return ,value)))) 
;;        ;; (push ,indicator ,place)
;;        ;; ,place
;;        ;; (if (null ,place)
;;        ;;     (setf (getf ,place ,indicator) (push ,place ,value))
;;        ;;     (loop
;;        ;;       initially (if ,from-end (setq ,place (nreverse ,place)))
;;        ;;       for ,p on ,place by #'cddr if (funcall ,test (car ,p) ,indicator)
;;        ;;       do (setf (cadr ,p) ,value)
;;        ;;          (if ,from-end (setq ,place (nreverse place)))
;;        ;;          (return ,place)
;;        ;;       finally (if ,from-end (setq ,place (nreverse place)))
;;        ;;               (setf (cdr ,place) (cons ,indicator (cdr ,place))
;;        ;;                     (cdr ,place) (cons ,value (cdr ,place)))
;;        ;;               (if (cddr ,place)
;;        ;;                   (setf (third ,place) (first ,place)
;;        ;;                         (first ,place) ,indicator))
;;        ;;               (return ,place)))
;;        )))
;; ;; (in-package "SB-IMPL")
;; ;; (define-setf-expander getf1 (place prop &optional default &environment env)
;; ;;   (multiple-value-bind (place-tempvars place-tempvals stores set get)
;; ;;       (get-setf-expansion place env)
;; ;;     (multiple-value-bind (call-tempvars call-tempvals call-args bitmask)
;; ;;         (collect-setf-temps (list prop default) env '(indicator default))
;; ;;       (let* ((newval (gensym "NEW")))
;; ;;         (values `(,@place-tempvars ,@call-tempvars)
;; ;;                 `(,@place-tempvals ,@call-tempvals) `(,newval)
;; ;;                 `(let ((,(car stores) (%putf ,get ,(first call-args) ,newval))
;; ;;                        ,@(cdr stores))
;; ;;                    ,@(when (logbitp 1 bitmask) (last call-tempvars))
;; ;;                    ,set
;; ;;                    ,newval)
;; ;;                 `(getf ,get ,@call-args))))))

;; (in-package "SB-IMPL")
;; (define-setf-expander getf1 (prop place &optional default &environment env)
;;   (multiple-value-bind (place-tempvars place-tempvals stores set get)
;;       (get-setf-expansion place env)
;;     (multiple-value-bind (call-tempvars call-tempvals call-args bitmask)
;;         (collect-setf-temps (list prop default) env '(indicator default))
;;       (let* ((newval (gensym "NEW")))
;;         (values `(,@place-tempvars ,@call-tempvars)
;;                 `(,@place-tempvals ,@call-tempvals) `(,newval)
;;                 `(let ((,(car stores) (%putf ,get ,(first call-args) ,newval))
;;                        ,@(cdr stores))
;;                    ,@(when (logbitp 1 bitmask) (last call-tempvars))
;;                    ,set
;;                    ,newval)
;;                 `(getf ,get ,@call-args))))))

;; Thanks to https://codereview.stackexchange.com/q/156392 davypough

(defmethod ucopy ((sym symbol))
  "Simply return the symbol."
  sym)

(defmethod ucopy ((num number))
  "Simply return the number."
  num)

(defmethod ucopy ((char character))
  "Simply return the character."
  char)

(defmethod ucopy ((fn function))
  "Simply return the function."
  fn)

(defmethod ucopy ((path pathname))
  "Simply return the path."
  path)

(defmethod ucopy ((seq sequence))
  "Copy a sequence recursively."
  (map (type-of seq) #'ucopy seq))

(defmethod ucopy ((ht hash-table))
  "Copy a hash table recursively."
  (loop with new-ht = (make-hash-table
                       :test (hash-table-test ht)
                       :size (hash-table-size ht)
                       :rehash-size (hash-table-rehash-size ht)
                       :rehash-threshold (hash-table-rehash-threshold ht))
        for key being the hash-key in ht using (hash-value value)
        do (setf (gethash (ucopy key) new-ht) (ucopy value))
        finally (return new-ht)))

(defmethod ucopy ((arr array))
  "Copy an array recursively."
  (let ((new-arr (make-array (array-dimensions arr)
                             :element-type (array-element-type arr)
                             :adjustable (adjustable-array-p arr))))
    (dotimes (i (array-total-size arr))
      (setf (row-major-aref new-arr i)
            (ucopy (row-major-aref arr i))))
    new-arr))

(defmethod ucopy ((struct structure-object))
  "Copy a structure recursively. Might not work in all implementations."
  (let ((new-struct (copy-structure struct))
        (slots (closer-mop:class-direct-slots (class-of struct))))
    (dolist (slot slots)
      (let ((slot-name (closer-mop:slot-definition-name slot)))
        (setf (slot-value new-struct slot-name)
              (ucopy (slot-value struct slot-name)))))
    new-struct))

(defmethod ucopy ((inst standard-object))
  "Copy an instance of a class recursively. Might not work in all implementations."
  (let ((new-inst (allocate-instance (class-of inst)))
        (slots (closer-mop:class-direct-slots (class-of inst))))
    (dolist (slot slots)
      (let ((slot-name (closer-mop:slot-definition-name slot)))
        (when (slot-boundp inst slot-name)
          (setf (slot-value new-inst slot-name)
                (ucopy (slot-value inst slot-name))))))
    new-inst))
