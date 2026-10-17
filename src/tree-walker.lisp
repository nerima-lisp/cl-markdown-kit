(in-package #:cl-markdown-kit)

(defstruct
    (node-tree-frame (:constructor %make-node-tree-frame ()) (:predicate nil))
  node
  phase
  parent
  grandparent
  previous-sibling
  next-sibling
  remaining-siblings
  (child-index 0 :type (integer 0 *))
  (last-child-p nil :type boolean)
  result
  child-results
  remaining-children
  (next-child-index 0 :type (integer 0 *))
  previous-child
  (entered-p nil :type boolean))

(defun %walker-check-action (action phase)
    (unless (or (null action)
                (eq action :descend)
                (eq action :skip)
                (eq action :stop))
      (error "Walker callback returned invalid action ~S during ~S."
             action phase))
    action)

  (defun %walker-root-frame (root)
    (let ((frame (%make-node-tree-frame)))
      (setf (node-tree-frame-node frame) root
            (node-tree-frame-child-index frame) 0
            (node-tree-frame-last-child-p frame) t)
      frame))

  (defun %walker-enter-frame (frame callback)
    (setf (node-tree-frame-entered-p frame) t
          (node-tree-frame-phase frame) :enter
          (node-tree-frame-result frame) nil
          (node-tree-frame-child-results frame) nil)
    (let ((action
            (%walker-check-action
             (funcall callback frame)
             (node-tree-frame-phase frame))))
      (unless (eq action :stop)
        (setf (node-tree-frame-remaining-children frame)
              (if (eq action :skip)
                  nil
                  (node-children (node-tree-frame-node frame)))))
      action))

  (defun %walker-leave-frame (frame callback)
    (setf (node-tree-frame-phase frame) :leave
          (node-tree-frame-child-results frame)
          (nreverse (node-tree-frame-child-results frame)))
    (%walker-check-action
     (funcall callback frame)
     (node-tree-frame-phase frame)))

  (defun %walker-ensure-frame (frames index)
    (when (= index (length frames))
      (setf frames
            (adjust-array frames
                          (* 2 (length frames))
                          :initial-element nil)))
    (unless (aref frames index)
      (setf (aref frames index) (%make-node-tree-frame)))
    (values frames (aref frames index)))

  (defun %walker-initialize-child-frame
      (frame child parent-frame previous-sibling next-sibling
       remaining-siblings child-index last-child-p)
    (setf (node-tree-frame-node frame) child
          (node-tree-frame-phase frame) nil
          (node-tree-frame-parent frame)
          (node-tree-frame-node parent-frame)
          (node-tree-frame-grandparent frame)
          (node-tree-frame-parent parent-frame)
          (node-tree-frame-previous-sibling frame) previous-sibling
          (node-tree-frame-next-sibling frame) next-sibling
          (node-tree-frame-remaining-siblings frame) remaining-siblings
          (node-tree-frame-child-index frame) child-index
          (node-tree-frame-last-child-p frame) last-child-p
          (node-tree-frame-result frame) nil
          (node-tree-frame-child-results frame) nil
          (node-tree-frame-remaining-children frame) nil
          (node-tree-frame-next-child-index frame) 0
          (node-tree-frame-previous-child frame) nil
          (node-tree-frame-entered-p frame) nil)
    frame)

  (defun %walker-descend (frames depth parent-frame)
    (let* ((siblings (node-tree-frame-remaining-children parent-frame))
           (child (car siblings))
           (child-index (node-tree-frame-next-child-index parent-frame))
           (child-depth (1+ depth))
           (previous-sibling (node-tree-frame-previous-child parent-frame))
           (next-sibling (cadr siblings))
           (remaining-siblings (cdr siblings))
           (last-child-p (null remaining-siblings)))
      (setf (node-tree-frame-remaining-children parent-frame)
            remaining-siblings
            (node-tree-frame-next-child-index parent-frame)
            (1+ child-index)
            (node-tree-frame-previous-child parent-frame)
            child)
      (check-type child node)
      (multiple-value-bind (expanded-frames child-frame)
          (%walker-ensure-frame frames child-depth)
        (%walker-initialize-child-frame
         child-frame child parent-frame previous-sibling next-sibling
         remaining-siblings child-index last-child-p)
        (values expanded-frames child-depth))))

  (defun %walker-fold-child-result (frames depth frame)
    (let ((parent-frame (aref frames (1- depth))))
      (push (node-tree-frame-result frame)
            (node-tree-frame-child-results parent-frame))))

  (defun walk-node-tree (root callback)
    (check-type root node)
    (check-type callback function)
    (let ((frames (make-array 16 :adjustable t :initial-element nil))
          (depth 0))
      (setf (aref frames 0) (%walker-root-frame root))
      (loop for frame = (aref frames depth)
            do (cond
                 ((not (node-tree-frame-entered-p frame))
                  (when (eq (%walker-enter-frame frame callback) :stop)
                    (return-from walk-node-tree root)))
                 ((node-tree-frame-remaining-children frame)
                  (multiple-value-setq (frames depth)
                    (%walker-descend frames depth frame)))
                 (t
                  (%walker-leave-frame frame callback)
                  (if (zerop depth)
                      (return)
                      (progn
                        (%walker-fold-child-result frames depth frame)
                        (decf depth))))))
      root))
