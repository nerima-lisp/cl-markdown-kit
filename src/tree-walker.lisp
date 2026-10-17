(in-package #:cl-markdown-kit)

(defstruct (%node-tree-cps-frame (:constructor %make-node-tree-cps-frame ()))
  node
  parent
  grandparent
  previous-sibling
  next-sibling
  (child-index 0 :type (integer 0 *))
  (last-child-p nil :type boolean)
  remaining-children
  (next-child-index 0 :type (integer 0 *))
  previous-child
  (entered-p nil :type boolean))

(defun walk-node-tree-cps (root callback)
  (check-type root node)
  (check-type callback function)
  (let ((frames (make-array 16 :adjustable t :initial-element nil))
        (depth 0))
    (setf (aref frames 0) (%make-node-tree-cps-frame))
    (setf (%node-tree-cps-frame-node (aref frames 0)) root
          (%node-tree-cps-frame-child-index (aref frames 0)) 0
          (%node-tree-cps-frame-last-child-p (aref frames 0)) t)
    (loop for frame = (aref frames depth)
          do (cond
               ((not (%node-tree-cps-frame-entered-p frame))
                 (setf (%node-tree-cps-frame-entered-p frame) t)
                 (let ((action
                        (funcall callback
                                 (%node-tree-cps-frame-node frame)
                                 :enter
                                 (%node-tree-cps-frame-parent frame)
                                 (%node-tree-cps-frame-grandparent frame)
                                 (%node-tree-cps-frame-previous-sibling frame)
                                 (%node-tree-cps-frame-next-sibling frame)
                                 (%node-tree-cps-frame-child-index frame)
                                 (%node-tree-cps-frame-last-child-p frame))))
                   (cond
                     ((eq action :stop)
                      (return-from walk-node-tree-cps
                        root))
                     ((eq action :skip)
                      (setf (%node-tree-cps-frame-remaining-children frame) nil))
                     (t
                      (setf (%node-tree-cps-frame-remaining-children frame) (node-children
                                                                             (%node-tree-cps-frame-node
                                                                              frame)))))))
               ((%node-tree-cps-frame-remaining-children frame)
                (let* ((siblings
                        (%node-tree-cps-frame-remaining-children frame))
                       (child (car siblings))
                       (child-index
                        (%node-tree-cps-frame-next-child-index frame))
                       (child-depth (1+ depth))
                       (previous-sibling
                        (%node-tree-cps-frame-previous-child frame))
                       (next-sibling (cadr siblings))
                       (last-child-p (null (cdr siblings))))
                  (setf (%node-tree-cps-frame-remaining-children frame) (cdr
                                                                         siblings)
                        (%node-tree-cps-frame-next-child-index frame) (1+
                                                                       child-index)
                        (%node-tree-cps-frame-previous-child frame) child)
                  (check-type child node)
                  (when (= child-depth (length frames))
                    (setf frames (adjust-array frames
                                               (* 2 (length frames))
                                               :initial-element
                                               nil)))
                  (unless (aref frames child-depth)
                    (setf (aref frames child-depth) (%make-node-tree-cps-frame)))
                  (incf depth)
                  (let ((child-frame (aref frames depth)))
                    (setf (%node-tree-cps-frame-node child-frame) child
                          (%node-tree-cps-frame-parent child-frame) (%node-tree-cps-frame-node
                                                                     frame)
                          (%node-tree-cps-frame-grandparent child-frame) (%node-tree-cps-frame-parent
                                                                          frame)
                          (%node-tree-cps-frame-previous-sibling child-frame) previous-sibling
                          (%node-tree-cps-frame-next-sibling child-frame) next-sibling
                          (%node-tree-cps-frame-child-index child-frame) child-index
                          (%node-tree-cps-frame-last-child-p child-frame) last-child-p
                          (%node-tree-cps-frame-remaining-children child-frame) nil
                          (%node-tree-cps-frame-next-child-index child-frame) 0
                          (%node-tree-cps-frame-previous-child child-frame) nil
                          (%node-tree-cps-frame-entered-p child-frame) nil))))
               (t
                 (funcall callback
                          (%node-tree-cps-frame-node frame)
                          :leave
                          (%node-tree-cps-frame-parent frame)
                          (%node-tree-cps-frame-grandparent frame)
                          (%node-tree-cps-frame-previous-sibling frame)
                          (%node-tree-cps-frame-next-sibling frame)
                          (%node-tree-cps-frame-child-index frame)
                          (%node-tree-cps-frame-last-child-p frame))
                 (if (zerop depth)
                     (return)
                     (decf depth)))))
    root))
