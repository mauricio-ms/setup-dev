(defun nth-elt (element xs)
  "Return zero-indexed position of ELEMENT in list XS, or nil if absent."
  (let ((idx  0))
    (catch 'nth-elt
      (dolist (x  xs)
        (when (equal element x) (throw 'nth-elt idx))
        (setq idx  (1+ idx)))
      nil)))
