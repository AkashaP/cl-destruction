(in-package :cl-destruction)

;; A very cheaty library that's all to do with avoiding new consing.

(defun make-working-space ()
  (make-hash-table :test #'eq :size 100000000))

(defmethod nrotate-left ((l sequence) n &key (from 0) (to (- (length l) 1))) 
  (declare (type integer n from to))
  "Destructively modify l in-place, rotating it n places left"
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
  (declare (type integer n from to)) 
  "Destructively modify l in-place, rotating it n places right"
  (nrotate-left l (- n) :from from :to to)
  l)

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
                        if (cadr c2)
                          do (let ((tmp (first c2)))
                               (setf (first c2) (second c2)
                                     (second c2) tmp))))
                (setq h2 (mapcar #'cdr h2)))
        while swaps)
  (apply #'values lists))

(defun bubblesort (pred list)
  (car (bubblesync pred list)))

(defmethod get-prop (indicator (place sequence) &key default (test #'eq) from-end (start 0) (end (length place)))
  (loop for i from 0 by 2 repeat (/ (- end start) 2)
        for idx = (if from-end (- end i 1) (+ start i)) 
        for el = (elt place idx)
        if (funcall test indicator el)
          do (return (values (elt place (1+ idx)) t))
        finally (return (values default nil))))

(defmethod set-prop (indicator value (place list) &key default (test #'eq) from-end (start 0) (end (length place)))
  (loop for i from 0 by 2 repeat (/ (- end start) 2)
        for idx = (if from-end (- end i 1) (+ start i)) 
        for el = (elt place idx)
        if (funcall test indicator el)
          do (setf (elt place (1+ idx)) value)
             (return (values place t))
        finally
           (push value place)
           (push indicator place)
           (return (values place nil))))

;; Thanks to https://codereview.stackexchange.com/q/156392 davypough for the base idea

(defvar *working-space* (make-hash-table))
(defvar *not-found* (gensym "not-found"))
(defvar %set-instance (lambda (k v) (declare (ignore k v))))

(defmacro ucopy (thing working-space)
  (let ((k (gensym))
        (v (gensym)))
    `(let ((*working-space* ,working-space)
           (%set-instance (lambda (,k ,v)
                            (setf (gethash ,k *working-space*) ,v))))
       (declare (special *working-space* %set-instance)
                (dynamic-extent *working-space* %set-instance))
       (%ucopy ,thing))))


(defmethod %ucopy ((sym symbol))
  "Simply return the symbol."
  sym)

(defmethod %ucopy ((num number))
  "Simply return the number."
  num)

(defmethod %ucopy ((char character))
  "Simply return the character."
  char)

(defmethod %ucopy ((fn function))
  "Simply return the function.
   Note closures are not supported as they are naturally opaque."
  fn)

(defmethod %ucopy ((path pathname))
  "Simply return the path."
  path)

(defmethod %ucopy ((seq sequence))
  "Copy a sequence recursively."
  (let ((res (gethash seq *working-space* *not-found*)))
    (if (eq res *not-found*)
        (let ((new-seq (map (type-of seq) #'%ucopy seq)))
          (funcall %set-instance seq new-seq)
          new-seq) 
        res)))

(defmethod %ucopy ((seq list))
  "Copy a list recursively."
  (let ((res (gethash seq *working-space* *not-found*)))
    (if (eq res *not-found*)
        (let ((new-seq
                ;; this handles improper lists too
                (if (listp (cdr seq))
                    (loop with s = seq
                          while s
                          if (endp s)
                            do (loop-finish)
                          else if (not (listp (cdr s)))
                                 collect (cons (%ucopy (car s))
                                               (%ucopy (cdr s)))
                                 and do (loop-finish)
                          else collect (%ucopy (car s))
                               and do (setf s (cdr s)))
                    (cons (%ucopy (car seq))
                          (%ucopy (cdr seq))))))
          (funcall %set-instance seq new-seq)
          new-seq) 
        res)))

(defmethod %ucopy ((ht hash-table))
  "Copy a hash table recursively."
  (let ((res (gethash ht *working-space* *not-found*)))
    (if (eq res *not-found*)
        (loop with new-ht = (make-hash-table
                             :test (hash-table-test ht)
                             :size (hash-table-size ht)
                             :rehash-size (hash-table-rehash-size ht)
                             :rehash-threshold (hash-table-rehash-threshold ht))
              for key being the hash-key in ht using (hash-value value)
              do (setf (gethash (%ucopy key) new-ht) (%ucopy value))
              finally (funcall %set-instance ht new-ht)
                      (return new-ht))
        res)))

(defmethod %ucopy ((arr array))
  "Copy an array recursively."
  (let ((res (gethash arr *working-space* *not-found*)))
    (if (eq res *not-found*)
        (let ((new-arr (make-array (array-dimensions arr)
                                   :element-type (array-element-type arr)
                                   :adjustable (adjustable-array-p arr))))
          (dotimes (i (array-total-size arr))
            (setf (row-major-aref new-arr i)
                  (%ucopy (row-major-aref arr i))))
          (funcall %set-instance arr new-arr)
          new-arr) 
        res)))

(defmethod %ucopy ((struct structure-object))
  "Copy a structure recursively. Might not work in all implementations."
  (let ((res (gethash struct *working-space* *not-found*)))
    (if (eq res *not-found*)
        (let ((new-struct (copy-structure struct))
              (slots (closer-mop:class-direct-slots (class-of struct))))
          (dolist (slot slots)
            (let ((slot-name (closer-mop:slot-definition-name slot)))
              (setf (slot-value new-struct slot-name)
                    (%ucopy (slot-value struct slot-name)))))
          (funcall %set-instance struct new-struct)          
          new-struct) 
        res)))

(defmethod %ucopy ((inst standard-object))
  "Copy an instance of a class recursively. Might not work in all implementations."
  (let ((res (gethash inst *working-space* *not-found*)))
    (if (eq res *not-found*)
        (let ((new-inst (allocate-instance (class-of inst)))
              (slots (closer-mop:class-direct-slots (class-of inst))))
          (dolist (slot slots)
            (let ((slot-name (closer-mop:slot-definition-name slot)))
              (when (slot-boundp inst slot-name)
                (setf (slot-value new-inst slot-name)
                      (%ucopy (slot-value inst slot-name))))))
          (funcall %set-instance inst new-inst) 
          new-inst) 
        res)))
