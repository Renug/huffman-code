(defclass huffman-binary-tree-node ()
  ((data :accessor data
         :initform 'null
         :initarg :data)
   (left-node :accessor left-node
              :initform 'null
              :initarg :left-node)
   (right-node :accessor right-node
               :initform 'null
               :initarg :right-node)
   (power-value :accessor power-value
                :initform 0
                :initarg :power-value)))

(defun construct-huffman-tree (lst)
  (let ((power-value (make-hash-table))
        (huffman-nodes (list)))
    (flet ((count-power-value (item)
             (multiple-value-bind (value is-avalie) (gethash item power-value)
               (setf (gethash item power-value) (if is-avalie (1+ value) 1))))
           (new-huffman-binary-tree-node (key val)
                   (pushnew (make-instance 'huffman-binary-tree-node 
                              :data key 
                              :power-value val) huffman-nodes)))
      (mapcar #'count-power-value lst)
      (maphash #'new-huffman-binary-tree-node power-value)
      (setf huffman-nodes (sort huffman-nodes #'< :key #'power-value)))
    (loop while (< 1 (length huffman-nodes))
      do (let* ((left (if (first huffman-nodes) (first huffman-nodes) 'null))
                (right (if (second huffman-nodes) (second huffman-nodes) 'null))
                (new-node (make-instance 'huffman-binary-tree-node 
                            :power-value (+ (power-value left) (power-value right))
                            :left-node left 
                            :right-node right)))
           (setf huffman-nodes (cddr huffman-nodes))
           (pushnew new-node huffman-nodes)
           (setf huffman-nodes (sort huffman-nodes #'< :key #'power-value))))
    (first huffman-nodes)))

(defmethod huffman-dict ((huffman-node huffman-binary-tree-node))
  (let ((retval (make-hash-table)))
    (labels ((gen-huffman-code (huffman-node-local &optional (code ""))
               (unless (eql (left-node huffman-node-local) 'null)
                   (gen-huffman-code (left-node huffman-node-local) (concatenate 'string code "0")))
               (unless (eql (right-node huffman-node-local) 'null)
                   (gen-huffman-code (right-node huffman-node-local) (concatenate 'string code "1")))
               (unless (eql (data huffman-node-local) 'null)
                 (setf (gethash (data huffman-node-local) retval) code))))
      (gen-huffman-code huffman-node))
    retval))
                 
                
(maphash #'(lambda (key val) (format t "key ~a,val ~a~%" key val )) (huffman-dict (construct-huffman-tree (list #\a
														#\b
														#\c
														#\d
														#\e
														#\f))))