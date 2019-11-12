;; ==============================================================================
;; Prefix documentation:
;;
;; C-x Usual Emacs commands
;; C-p Projectile
;; ,   Vim bindings
;; C-w Window bindings
;; C-a Tmux prefix
;; C-c CIDER
;; C-e Paredit
;;
;; Available:
;; C-s
;; C-q
;; C-t 
;; C-y
;; C-z
;;
;; "Prefixes" that are used for just a single action
;; C-g       Quit command
;; C-f       Find file
;; C-v       Visual block
;; C-r       Redo
;; C-b       Find buffer
;; C-u       Scroll up
;; C-d       Scroll down
;; C-h,j,k,l Move window
;; C-o,i     Jump List
;; ==============================================================================

;; =======================================================================
;; PACKAGE INITIALIZATION
;; =======================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(setq package-enable-at-startup t)
(require 'use-package)

;; =======================================================================
;; SCRIPTING CMDS
;; =======================================================================
(defun split-horizontally-and-move-to-window ()
  "Split a window and also move to it"
  (interactive)
  (split-window-vertically)
  (windmove-down))

(defun split-vertically-and-move-to-window ()
  "Split a window and also move to it"
  (interactive)
  (split-window-horizontally)
  (windmove-right))

(defun dired-in-new-tab ()
  (interactive)
  (require 'projectile)
  (let ((root (or (projectile-project-root) default-directory)))
    (evil-ex-call-command nil "tabnew" nil)
    (dired root)))

(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; =======================================================================
;; EVIL
;; =======================================================================
(use-package evil
  :ensure t
  :config (evil-mode)
  (use-package evil-tabs
    :ensure t
    :config
    (evil-tabs-mode)
    (evil-define-key 'motion 'global (kbd "g t") 'evil-tabs-goto-tab)
    (evil-define-key 'motion 'global (kbd "g T") 'elscreen-previous))
  (defun evil-tab-sensitive-quit ()
    (interactive)
    (require 'evil)
    (if (> (length (elscreen-get-screen-list)) 1)
        (if (> (count-windows) 1)
            (evil-quit)
          (elscreen-kill))
      (evil-quit)))

  ;; Window movement
  (evil-define-key '(normal motion) 'global "\C-l" 'windmove-right)
  (evil-define-key '(normal motion) 'global "\C-k" 'windmove-up)
  (evil-define-key '(normal motion) 'global "\C-j" 'windmove-down)
  (evil-define-key '(normal motion) 'global "\C-h" 'windmove-left)
  (evil-define-key '(normal motion insert) 'global (kbd "C-x w") 'delete-other-windows)
  (evil-define-key '(normal motion) 'global (kbd "C-w") nil)

  (evil-define-key '(normal motion) 'global "\C-d" 'evil-scroll-down)
  (evil-define-key '(normal motion) 'global "\C-u" 'evil-scroll-up)
  (evil-define-key '(normal motion) 'global (kbd "0") 'evil-first-non-blank)
  (evil-define-key 'insert 'global "\C-w" 'evil-delete-backward-word)

  ;; Old vim prefix
  (evil-define-key '(normal motion) 'global (kbd ",") nil)
  (evil-define-key '(normal motion) 'global (kbd ",q") 'evil-tab-sensitive-quit)
  (evil-define-key '(normal motion) 'global (kbd ",t") 'dired-in-new-tab)
  (evil-define-key '(normal motion) 'global (kbd ",r") 'evil-tabs-current-buffer-to-tab)
  (evil-define-key 'normal 'global (kbd ",w") 'save-buffer)
  (evil-define-key 'normal 'global (kbd ",s") 'split-horizontally-and-move-to-window)
  (evil-define-key 'normal 'global (kbd ",v") 'split-vertically-and-move-to-window)
  (evil-define-key 'normal 'global (kbd ",c") 'comment-or-uncomment-region)
  (evil-define-key 'normal 'global (kbd ",m") 'compile))

;; =======================================================================
;; HYDRA
;; =======================================================================
(use-package hydra
  :ensure t)

(defhydra hydra-window (evil-normal-state-map "C-w")
  "modify windows"
  ("k" (evil-window-increase-height 5) "increase-height")
  ("j" (evil-window-decrease-height 5) "decrease-height")
  ("h" (evil-window-decrease-width 5) "decrease-width")
  ("l" (evil-window-increase-width 5) "increase-width"))

(defhydra hydra-tabs (global-map "C-x n")
  "switch tabs"
  ("l" evil-tabs-goto-tab "next-tab")
  ("h" elscreen-previous "prev-tab"))

;; =======================================================================
;; CLEAN UP STANDARD EMACS CONF
;; =======================================================================
;; Unbind things that we want to use elsewhere
(evil-define-key '(normal motion insert) 'global (kbd "C-z") nil)
(evil-define-key '(normal motion insert) 'global (kbd "C-e") nil)
(evil-define-key '(normal motion insert) 'global (kbd "C-y") nil)
(evil-define-key '(normal motion insert) 'global (kbd "C-t") nil)
(evil-define-key '(normal motion insert) 'global (kbd "C-c") nil)

;; Don't need emacs inc search
(define-key global-map (kbd "C-s") nil)
(evil-define-key '(normal motion insert) 'global (kbd "C-s") nil)

;; Prevent accident
(define-key global-map (kbd "C-x f") nil)
(define-key global-map (kbd "C-x a") nil)

;; Rebind describe key somewhere else
(define-key global-map (kbd "C-x k") 'describe-key)

;; Better switch to buffer binding
(evil-define-key '(normal motion) 'global (kbd "C-b") 'switch-to-buffer)

;; =======================================================================
;; STANDARD EMACS CONFIGS
;; =======================================================================
;; Load colorscheme
(load-theme 'darcula t)

;; Disable annoying bell
(setq ring-bell-function 'ignore)

;; Highlight matching paren when cursor is over it
(show-paren-mode t)

;; Don't create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Revert buffer automatically if file changes externally
(global-auto-revert-mode t)

;; Show line numbers in left margin and mode line
(global-linum-mode)
(column-number-mode)

;; Highlight the line the cursor is on
(global-hl-line-mode 1)

;; In emacs bindings, make C-w delete the word before point
(define-key global-map (kbd "C-w") 'backward-kill-word)

;; Don't jump the screen when scrolling up or down
;; and have 7 lines at the top or bottom when scrolling
(setq-default scroll-conservatively 10000)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-margin 10)
(setq auto-window-vscroll nil)

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Indent 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

;; Make it so that hitting Shift-Backspace doesn't open help menu
(define-key global-map "\C-h" 'delete-backward-char)

;; Write backups in .emacs.d/backups instead of all over fs
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Ctags
(setq tags-table-list '()) ; Put tags file here

;; Compilation
(require 'compile)
(define-key global-map (kbd "C-x m") 'compile)
(define-key compilation-mode-map (kbd "g") nil)
(define-key compilation-mode-map (kbd "r") 'recompile)

;; Always display certain buffers in a new window
(setq display-buffer-alist '(("\\*cider-error\\*"
                              (display-buffer-reuse-window display-buffer-pop-up-window)
                              ())))
(setq split-width-threshold 1)
(setq split-height-threshold 1)

;; =======================================================================
;; DIRED
;; =======================================================================
(use-package dired
  :config

  ;; Dired sort directories together at the top
  ;; TODO: This doesn't work on OSX
                                        ;(setq dired-listing-switches "-la --group-directories-first")

  ;; Always refresh Dired from the fs state
  (setq dired-auto-revert-buffer t)

  ;; These are required because Evil unbinds all of its keys by default in Dired mode
  ;; (it isn't that dired-mode-map overrides Evil... a bit confusing)
  (define-key dired-mode-map (kbd "n") 'evil-search-next)
  (define-key dired-mode-map (kbd "N") 'evil-search-previous)
  (define-key dired-mode-map (kbd "?") 'evil-search-backward)
  (define-key dired-mode-map (kbd "G") 'revert-buffer)

  (use-package dired-subtree
    :ensure t
    :config
    (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)))

;; =======================================================================
;; Python
;; =======================================================================
(defun python-init-stuff ()
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))
(add-hook 'python-mode-hook 'python-init-stuff)

;; =======================================================================
;; HTML/CSS
;; =======================================================================
(defun my-css-stuff ()
  (add-to-list 'company-backends 'company-css))

(add-hook 'css-mode-hook 'my-css-stuff)

;; =======================================================================
;; C++
;; =======================================================================
(use-package cc-mode
  :config

  ;; Make _ not a word boundary
  (defun cpp-init-stuff ()
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table))
  (add-hook 'c++-mode-hook 'cpp-init-stuff)

  ;; C++ indentation
  ;; Don't indent within a namespace
  (setq my-cc-style
        '("cc-mode"
          (c-offsets-alist . ((innamespace . [0])))))
  (c-add-style "my-cc-style" my-cc-style)
  (add-hook 'c++-mode-hook (lambda () (c-set-style "my-cc-style")))

  ;; Fix all problems with modern C++ syntax highlighting
  (use-package modern-cpp-font-lock :ensure t)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.H\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))

;; =======================================================================
;; CLOJURE
;; =======================================================================
;; Useful paredit hydra that puts everything under one prefix
(defhydra hydra-paredit (global-map "C-e")
  "paredit"
  ("l" paredit-forward-slurp-sexp "slurp-forward")
  ("L" paredit-forward-barf-sexp "barf-forward")
  ("h" paredit-backward-slurp-sexp "slurp-backward")
  ("H" paredit-backward-barf-sexp "barf-backward")
  ("9" paredit-wrap-round "wrap-round")
  ("0" paredit-wrap-round "wrap-round")
  ("{" paredit-wrap-curly "wrap-curly")
  ("[" paredit-wrap-square "wrap-square")
  ("f" paredit-forward "forward")
  ("F j" paredit-forward-down "forward-down")
  ("F k" paredit-forward-up "forward-up")
  ("b" paredit-backward "backward")
  ("B j" paredit-backward-down "backward-down")
  ("B k" paredit-backward-up "backward-up")
  ("s" paredit-splice-sexp "splice")
  ("t" transpose-sexps "transpose")
  ("j" paredit-join-sexps "join")
  ("r" raise-sexp "join")
  ("k" kill-sexp "kill"))

(use-package rainbow-delimiters :ensure t)

;; Makes normal mode operations preserve paren balance
(use-package paredit
  :config
  (use-package evil-paredit :ensure t)

  ;; Paredit in emacs lisp
  (add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode) (paredit-mode) (evil-paredit-mode))))

(use-package aggressive-indent :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)

  (use-package clj-refactor :ensure t)
  (use-package cljr-helm :ensure t)

  (use-package cider
    :ensure t
    :config
    (setq cider-repl-display-in-current-window t)
    ;; Unbind certain things that we don't use (so which-key is more useful)
    (define-key clojure-mode-map (kbd "C-c M-c") nil) ; connect
    (define-key clojure-mode-map (kbd "C-c M-C") nil) ; connect
    (define-key clojure-mode-map (kbd "C-c M-j") nil) ; jack in
    (define-key clojure-mode-map (kbd "C-c M-J") nil) ; jack in
    (define-key clojure-mode-map (kbd "C-c M-x") nil) ; restart
    (define-key clojure-mode-map (kbd "C-c M-z") nil) ; restart
    (define-key clojure-mode-map (kbd "C-c C-x") nil) ; cider start
    (define-key clojure-mode-map (kbd "C-c M-t") nil) ; trace stuff
    (define-key cider-mode-map (kbd "C-c C-.") nil) ; find ns
    (define-key cider-mode-map (kbd "C-c C-:") nil) ; find kw
    (define-key cider-mode-map (kbd "C-c M-.") nil) ; find resource
    (define-key cider-mode-map (kbd "C-c M-p") nil)
    (define-key cider-mode-map (kbd "C-c M-d") nil)
    (define-key cider-mode-map (kbd "C-c M-e") nil) ; eval
    (define-key cider-mode-map (kbd "C-c M-:") nil) ; eval
    (define-key cider-mode-map (kbd "C-c M-m") nil) ; macroexpand
    (define-key cider-mode-map (kbd "C-c M-r") nil) ; restart

    ;; Cider control
    (define-key cider-mode-map (kbd "C-c q") 'cider-quit)
    (define-key cider-mode-map (kbd "C-c j") 'cider-jack-in)
    (define-key cider-mode-map (kbd "C-c J") 'cider-jack-in-cljs)

    ;; Find thing: C-c g
    (let (my-cider-find-map)
      (define-prefix-command 'my-cider-find-map)
      (define-key my-cider-find-map (kbd "d") 'cider-find-var)
      (define-key my-cider-find-map (kbd "n") 'cider-find-ns)
      (define-key my-cider-find-map (kbd "r") 'cider-find-resource)
      (define-key my-cider-find-map (kbd "k") 'cider-find-keyword)
      (define-key cider-mode-map (kbd "C-c g") 'my-cider-find-map))

    ;; Expression evaluation: C-c e
    (let (my-cider-eval-map)
      (define-prefix-command 'my-cider-eval-map)
      (define-key my-cider-eval-map (kbd "l") 'cider-eval-last-sexp)
      (define-key my-cider-eval-map (kbd "r") 'cider-eval-last-sexp-to-repl)
      (define-key my-cider-eval-map (kbd "p") 'cider-insert-last-sexp-in-repl)
      (define-key my-cider-eval-map (kbd "t") 'cider-eval-defun-at-point)
      (define-key my-cider-eval-map (kbd "a") 'cider-eval-sexp-at-point)
      (define-key my-cider-eval-map (kbd "d") 'cider-debug-defun-at-point)
      (define-key cider-mode-map (kbd "C-c e") 'my-cider-eval-map))

    ;; Macros: C-c m
    (let (my-cider-macro-map)
      (define-prefix-command 'my-cider-macro-map)
      (define-key my-cider-macro-map (kbd "1") 'cider-macroexpand-1)
      (define-key my-cider-macro-map (kbd "a") 'cider-macroexpand-all)
      (define-key cider-mode-map (kbd "C-c m") 'my-cider-macro-map))

    ;; Formatting: C-c f
    (let (my-cider-format-map)
      (define-prefix-command 'my-cider-format-map)
      (define-key my-cider-format-map (kbd "r") 'cider-format-region)
      (define-key my-cider-format-map (kbd "f") 'cider-format-defun)
      (define-key cider-mode-map (kbd "C-c f") 'my-cider-format-map))

    ;; Documentation: C-c C-d (by default), C-c d
    (define-key cider-doc-map (kbd "c") 'clojure-view-cheatsheet)
    (define-key cider-mode-map (kbd "C-c d") 'cider-doc-map)

    ;; Namespacing
    (define-key cider-mode-map (kbd "C-c C-n") 'cider-repl-set-ns)

    ;; Fix up REPL usage in Normal mode
    (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-backward-input)
    (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-forward-input)
    ;; We define RET -> cider-repl-return in cider-repl-mode-hooks for evil reasons

    ;; Refactoring: C-c r
    (define-key clojure-mode-map (kbd "C-c C-r") 'cljr-helm)
    (let ((my-cljr-refactor-map)
          (my-cljr-refactor-rename-map)
          (my-cljr-refactor-let-map)
          (my-cljr-refactor-cycle-map))
      (define-prefix-command 'my-cljr-refactor-map)
      (define-prefix-command 'my-cljr-refactor-rename-map)
      (define-prefix-command 'my-cljr-refactor-let-map)
      (define-prefix-command 'my-cljr-refactor-cycle-map)
      ;; Renaming refactorings C-c r r
      (define-key my-cljr-refactor-rename-map (kbd "f") 'cljr-rename-file-or-dir)
      (define-key my-cljr-refactor-rename-map (kbd "s") 'cljr-rename-symbol)
      (define-key my-cljr-refactor-map (kbd "r") 'my-cljr-refactor-rename-map)
      ;; Let refactorings C-c r l
      (define-key my-cljr-refactor-let-map (kbd "i") 'clojure-introduce-let)
      (define-key my-cljr-refactor-let-map (kbd "m") 'clojure-move-to-let)
      (define-key my-cljr-refactor-let-map (kbd "r") 'cljr-remove-let)
      (define-key my-cljr-refactor-map (kbd "l") 'my-cljr-refactor-let-map)
      ;; Inline symbol C-c r i
      (define-key my-cljr-refactor-map (kbd "i") 'cljr-inline-symbol)
      ;; Cycle C-c r c
      (define-key my-cljr-refactor-cycle-map (kbd "p") 'clojure-cycle-privacy)
      (define-key my-cljr-refactor-cycle-map (kbd "n") 'clojure-cycle-not)
      (define-key my-cljr-refactor-cycle-map (kbd "i") 'clojure-cycle-if)
      (define-key my-cljr-refactor-map (kbd "c") 'my-cljr-refactor-cycle-map)

      (define-key clojure-mode-map (kbd "C-c r") 'my-cljr-refactor-map)))

  (defun my-cider-repl-mode-stuff ()
    (evil-define-key '(normal motion) 'local (kbd "<up>") 'cider-repl-backward-input)
    (evil-define-key '(normal motion) 'local (kbd "<down>") 'cider-repl-forward-input)
    (evil-define-key '(normal motion) 'local (kbd "RET") 'cider-repl-return))

  (defun my-clojure-stuff ()
    (rainbow-delimiters-mode t) ; Highlight matching parens
    (cider-mode t)
    (paredit-mode t)
    (clj-refactor-mode t)
    (yas-minor-mode t)
    (aggressive-indent-mode t)
    (evil-define-key 'insert 'local (kbd "(") 'paredit-open-round)
    (evil-define-key 'insert 'local (kbd "{") 'paredit-open-curly)
    (evil-define-key 'insert 'local (kbd "[") 'paredit-open-square)
    (evil-define-key 'insert 'local (kbd ")") 'paredit-close-round)
    (evil-define-key 'insert 'local (kbd "}") 'paredit-close-curly)
    (evil-define-key 'insert 'local (kbd "]") 'paredit-close-square))

  (add-hook 'clojure-mode-hook 'my-clojure-stuff)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-stuff))

;; =======================================================================
;; CLANG FORMAT
;; =======================================================================
(defun clang-format-region-at-point ()
  (interactive)
  (defvar-local bounds (bounds-of-thing-at-point 'paragraph))
  (clang-format-region (car bounds) (cdr bounds)))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "file")
  (evil-define-key 'visual 'c++-mode-map (kbd "SPC ff") 'clang-format-region)
  (evil-define-key 'normal 'c++-mode-map (kbd "SPC ff") 'flang-format-region-at-point))

;; =======================================================================
;; HELM
;; =======================================================================
(use-package helm
  :ensure t
  :config (helm-mode)
  (evil-define-key 'normal 'global (kbd "C-x C-f") 'helm-find-files)
  (evil-define-key 'normal 'global (kbd "C-f") 'helm-find-files)
  (evil-define-key '(normal motion) 'global (kbd "C-x b") 'helm-apropos)
  (evil-define-key '(insert normal motion) 'global "\M-x" 'helm-M-x)
  ;; C-w to kill word when doing a helm search
  (define-key helm-map (kbd "C-w") 'backward-kill-word))


(use-package helm-swoop
  :ensure t
  :config
  (evil-define-key 'normal 'global (kbd "C-x /") 'helm-swoop)
  (define-key helm-swoop-map (kbd "C-n") 'helm-next-line)
  (define-key helm-swoop-map (kbd "C-p") 'helm-previous-line)
  (setq helm-swoop-split-with-mutiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-use-fuzzy-match t))
  
;; =======================================================================
;; OTHER PKGS
;; =======================================================================
(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

(use-package yasnippet
  :ensure t
  :config (use-package yasnippet-snippets :ensure t)
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-c &") nil)
  (evil-define-key 'insert 'global (kbd "C-x y y") 'yas-expand)
  (evil-define-key 'insert 'global (kbd "C-x y c") 'company-yasnippet)
  (evil-define-key 'insert 'global (kbd "C-x y n") 'yas-next-field)
  (evil-define-key 'insert 'global (kbd "C-x y p") 'yas-prev-field))

(use-package evil-magit
  :ensure t
  :config
  (evil-magit-init)
  (global-set-key (kbd "C-x g") 'magit-status))

;; =======================================================================
;; IRONY
;; =======================================================================
;; (use-package irony
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-json irony-cdb-clang-complete))
;;   :config
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
;;    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;;    (add-hook 'irony-mode-hook #'irony-eldoc))
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<backtab>") 'company-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word)
  (add-to-list 'company-backends 'company-yasnippet)
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 0
	company-show-numbers nil
	company-tooltip-limit 20
	company-dabbrev-downcase nil))
  ;; (use-package company-irony
  ;;   :ensure t
  ;;   :defer t
  ;;   :config
  ;;   (setq company-irony-ignore-case 'smart)
  ;;   (add-to-list 'company-backends 'company-irony)
  ;;   (use-package company-c-headers
  ;;     :ensure t
  ;;     :functions irony--extract-user-search-paths company-c-headers
  ;;     :preface
  ;;     (defun company-c-headers-path-user-irony ()
  ;;   "Return the user include paths for the current buffer."
  ;;   (when irony-mode
  ;;     (irony--extract-user-search-paths irony--compile-options irony--working-directory)))
  ;;     :config
  ;;     (setq company-c-headers-path-user #'company-c-headers-path-user-irony)
  ;;     (add-to-list 'company-backends #'company-c-headers)))

;; =======================================================================
;; PROJECTILE
;; =======================================================================
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)

  ;; Set projectile prefix
  (evil-define-key 'normal 'global (kbd "C-p") nil)
  (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

  ;; Open dired at root by just typing D
  (evil-define-key 'normal 'global (kbd "D") 'projectile-dired))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))


;; =======================================================================
;; Generated!
;; =======================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (aggressive-indent clj-refactor hydra cider rainbow-delimiters evil-paredit clojure-mode helm-ag ag csv-mode evil-magit clang-format yasnippet modern-cpp-font-lock irony helm use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
