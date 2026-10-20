(defstruct preceding-multiline
  first-slot
  second-slot)
(defconstant +after-multiline+ 17)
(defstruct uses-after-multiline-constant
  (value +after-multiline+))
