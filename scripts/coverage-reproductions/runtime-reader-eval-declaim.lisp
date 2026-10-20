(declaim #.(progn (if flag :covered :uncovered) '(optimize (speed 3))))
#.(progn (setf cl-user::*coverage-reader-eval-marker* t) nil)
