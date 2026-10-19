(in-package #:cl-markdown-kit)

(defun %hw-code-info-token (info)
  (when (and info (plusp (length info)))
    (let* ((start (position-if-not (lambda (char)
                                     (find char '(#\Space #\Tab #\Newline
                                                  #\Return #\Page)))
                                   info))
           (end (and start
                     (position-if (lambda (char)
                                    (find char '(#\Space #\Tab #\Newline
                                                 #\Return #\Page)))
                                  info :start start))))
      (when start
        (subseq info start end)))))

(defun %hw-write-code-block (context node)
  (%hw-newline context)
  (let ((info (%hw-code-info-token (code-block-info-string node))))
    (if info
        (progn
          (%hw-write context "<pre><code class=\"language-")
          (%hw-escaped-attribute context info)
          (%hw-write context "\">")
          (%hw-escaped-text context (code-block-body node))
          (%hw-write context "</code></pre>"))
        (progn
          (%hw-write context "<pre><code>")
          (%hw-escaped-text context (code-block-body node))
          (%hw-write context "</code></pre>")))
    (%hw-write-char context #\Newline)))

(defun %hw-tight-list-paragraph-p (context)
  (let ((parent (%html-writer-context-tree-parent context))
        (grandparent (%html-writer-context-tree-grandparent context)))
    (and (list-item-p parent)
         (list-node-p grandparent)
         (list-node-tight-p grandparent))))

(defun %hw-write-list-item-open (context node)
  (%hw-newline context)
  (%hw-write context "<li>")
  (when (task-list-item-p node)
    (%hw-write context
               (if (task-list-item-checked-p node)
                   "<input type=\"checkbox\" checked=\"\" disabled=\"\" /> "
                   "<input type=\"checkbox\" disabled=\"\" /> "))))

(defun %hw-write-list-item-close (context)
  (%hw-write context "</li>")
  (%hw-write-char context #\Newline))

(defun %hw-write-list-open (context node)
  (%hw-newline context)
  (if (eq (list-node-kind node) :ordered)
      (if (= (list-node-start-number node) 1)
          (progn
            (%hw-write context "<ol>")
            (%hw-write-char context #\Newline))
          (%hw-write context
                     (format nil "<ol start=\"~D\">~%"
                             (list-node-start-number node))))
      (progn
        (%hw-write context "<ul>")
        (%hw-write-char context #\Newline))))

(defun %hw-write-list-close (context node)
  (%hw-write context (if (eq (list-node-kind node) :ordered) "</ol>" "</ul>"))
  (%hw-write-char context #\Newline))

(defun %hw-write-table-open (context)
  (%hw-newline context)
  (%hw-write context "<table>")
  (setf (%html-writer-context-table-body-open-p context) nil))

(defun %hw-write-table-close (context)
  (when (%html-writer-context-table-body-open-p context)
    (%hw-newline context)
    (%hw-write context "</tbody>")
    (setf (%html-writer-context-table-body-open-p context) nil))
  (%hw-newline context)
  (%hw-write context "</table>")
  (%hw-write-char context #\Newline))

(defun %hw-write-table-row-open (context node)
  (if (table-row-header-row-p node)
      (progn
        (%hw-newline context)
        (%hw-write context "<thead>")
        (%hw-newline context))
      (unless (%html-writer-context-table-body-open-p context)
        (%hw-newline context)
        (%hw-write context "<tbody>")
        (%hw-write-char context #\Newline)
        (setf (%html-writer-context-table-body-open-p context) t)))
  (%hw-newline context)
  (%hw-write context "<tr>"))

(defun %hw-write-table-row-close (context node)
  (%hw-newline context)
  (%hw-write context "</tr>")
  (when (table-row-header-row-p node)
    (%hw-newline context)
    (%hw-write context "</thead>")))

(defun %hw-write-table-cell-open (context node)
  (let* ((row (%html-writer-context-tree-parent context))
         (table (%html-writer-context-tree-grandparent context))
         (alignment (or (table-cell-alignment node)
                        (nth (%html-writer-context-tree-child-index context)
                             (table-alignments table)))))
    (%hw-newline context)
    (%hw-write context (if (table-row-header-row-p row) "<th" "<td"))
    (when alignment
      (%hw-write context " align=\"")
      (%hw-escaped-attribute context (string-downcase (string alignment)))
      (%hw-write-char context #\"))
    (%hw-write-char context #\>)))

(defun %hw-write-table-cell-close (context)
  (%hw-write context
             (if (table-row-header-row-p
                  (%html-writer-context-tree-parent context))
                 "</th>"
                 "</td>")))
