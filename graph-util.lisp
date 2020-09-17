(defparameter *max-label-length* 30)

(defun dot-name (expression)
  (substitute-if #\_
                 (complement #'alphanumericp)
                 (prin1-to-string expression)))

(defun dot-label (expression)
  (if expression
      (let ((s (write-to-string expression :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc
   (lambda (node)
     (mapc
      (lambda (edge)
        (fresh-line)
        (princ (dot-name (car node)))
        (princ "->")
        (princ (dot-name (car edge)))
        (princ "[label=\"")
        (princ (dot-label (cdr edge)))
        (princ "\"];"))
      (cdr node)))
   edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun graph->png (filename nodes edges)
  (dot->png filename
            (lambda ()
              (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist
   (lambda (list)
     (mapc
      (lambda (edge)
        (unless (assoc (car edge) (cdr list))
          (fresh-line)
          (princ (dot-name (caar list)))
          (princ "--")
          (princ (dot-name (car edge)))
          (princ "[label=\"")
          (princ (dot-label (cdr edge)))
          (princ "\"];")))
      (cdar list)))
   edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (filename nodes edges)
  (dot->png filename
            (lambda ()
              (ugraph->dot nodes edges))))

(defun dot->png (filename thunk)
  (with-open-file (*standard-output*
                   filename
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " filename)))
