;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (require-final-newline . t)
  (indent-tabs-mode)
  (checkdoc-arguments-in-order-flag . nil)
  (checkdoc-force-docstrings-flag . nil))
 (emacs-lisp-mode
  (sentence-end-double-space . nil)
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))
