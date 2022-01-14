(in-package :cl-destruction)

(defmethod nrotate-left ((l list) n)
  "Destructively modify l in-place (use setq still), rotating it n places left"
  (let ((x (nthcdr n l))
        (y (nbutlast l (- (length l) n))))
    (setq l (nconc x y)))
  l)

(defmethod nrotate-right ((l list) n)
  "Destructively modify l in-place (use setq still), rotating it n places right"
  (nrotate-left (- (length l) n) l))

(defmethod nrotate-right ((l sequence) n) 
  "Destructively modify l in-place, rotating it n places right"
  (let ((k (length l)))
    (dotimes (i (gcd k n))
      (let* ((temp (elt l i))
             (j i))
        (loop for d = (mod (+ j n) k)
              while (not (eq d i))
              do (setf
                  (elt l j) (elt l d)
                  j d))
        (setf (elt l j) temp))))
  l)

(defmethod nrotate-left ((l sequence) n) 
  "Destructively modify l in-place, rotating it n places left"
  (nrotate-right l (- (length l) n))
  l)

(defmethod nsync ((source list) (target list) &optional (proc #'identity)) 
  "Sets the contents of TARGET so all cars of TARGET are the cars of SOURCE
(but not the cons cells themselves)"
  (if (and (null target) (not (null source)))
      (push (funcall proc (car source)) target))
  (loop
    with sc = source
    with tc = target 
    do (setf (car tc) (car sc)) 
       (when (null (cdr sc))
         (setf (cdr tc) nil)
         (return target))
       (when (null (cdr tc))
         (setf (cdr tc) (cons (funcall proc (car sc)) nil)))
       (setf sc (cdr sc)
             tc (cdr tc))))

(defmethod nsync ((source vector) (target vector) &optional (proc #'identity))
  (if (and (adjustable-array-p target)
           (< (length target) (length source)))
      (loop for i from (length source) below (length target)
            do (vector-push-extend (funcall proc-item (aref source i)) target))
      (dotimes (x (- (length source) (length target)))
        (vector-pop target)))
  (loop
    for i from 0 below (min (length source) (length target))
    do (setf (aref target i) (funcall proc (aref source i)))))

(defmethod nsync ((source hash-table) (target hash-table) &optional (proc #'identity))
  (let ((last nil)
        (lastk nil))
    (loop
      for tk being the hash-keys of target
      if last
        do (remhash lastk target)
           (setf last nil lastk nil)
      if (eq nil (nth-value 1 (gethash tk source)))
        do (setf last t lastk tk)
      finally (if last
                  (remhash lastk target))))

  ;; What if dat is a sequence or something though? unsure.
  (loop for sk being the hash-keys of source
          using (hash-value dat)
        do (setf (gethash sk target)
                 (funcall proc dat))))

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
