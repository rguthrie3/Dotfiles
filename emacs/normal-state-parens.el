(require 'smartparens)

(defun normal-state-parens--on-close ()
  (let ((c (char-after (point))))
    (seq-contains-p '( ?\) ?\] ?\} ) c)))

(defmacro normal-state-parens--wrap-command (new-name command)
  `(defun ,new-name ()
     (interactive)
     (when (normal-state-parens--on-close)
       (forward-char))
     (,command)))

(normal-state-parens--wrap-command normal-state-parens-sp-forward
                                   sp-forward-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward
                                   sp-backward-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-forward-slurp
                                   sp-forward-slurp-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-forward-barf
                                   sp-forward-barf-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward-slurp
                                   sp-backward-slurp-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward-barf
                                   sp-backward-barf-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-up
                                   sp-up-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-down
                                   sp-down-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward-up
                                   sp-backward-up-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward-down
                                   sp-backward-down-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-beginning
                                   sp-beginning-of-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-end
                                   sp-end-of-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-kill
                                   sp-kill-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward-kill
                                   sp-backward-kill-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-yank
                                   sp-copy-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-backward-yank
                                   sp-backward-copy-sexp)

(normal-state-parens--wrap-command normal-state-parens-sp-transpose
                                   sp-transpose-sexp)

(provide 'normal-state-parens)
