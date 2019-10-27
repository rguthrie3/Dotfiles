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
;; C-n
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
;; HYDRA
;; =======================================================================
(use-package hydra
  :ensure t)

;; =======================================================================
;; EVIL
;; =======================================================================
(defun split-horizontally-and-move-to-window ()
  (interactive)
  (split-window-vertically)
  (windmove-down))

(defun split-vertically-and-move-to-window ()
  (interactive)
  (split-window-horizontally)
  (windmove-right))

(use-package evil
  :ensure t
  :config (evil-mode))

(use-package evil-tabs
  :ensure t
  :config (evil-tabs-mode))

(evil-define-command evil-tab-sensitive-quit (&optional bang)
  :repeat nil
  (interactive "<!>")
  (if (> (length (elscreen-get-screen-list)) 1)
      (if (> (count-windows) 1)
          (evil-quit)
        (elscreen-kill))
    (evil-quit)))

(defun dired-in-new-tab ()
  (interactive)
  (require 'projectile)
  (let ((root (or (projectile-project-root) default-directory)))
    (evil-ex-call-command nil "tabnew" nil)
    (dired root)))

(evil-define-key '(normal motion) 'global "\C-l" 'windmove-right)
(evil-define-key '(normal motion) 'global "\C-k" 'windmove-up)
(evil-define-key '(normal motion) 'global "\C-j" 'windmove-down)
(evil-define-key '(normal motion) 'global "\C-h" 'windmove-left)
(evil-define-key '(normal motion) 'global "\C-d" 'evil-scroll-down)
(evil-define-key '(normal motion) 'global "\C-u" 'evil-scroll-up)
(evil-define-key '(normal motion insert) 'global (kbd "C-x w") 'delete-other-windows)
(evil-define-key '(normal motion) 'global (kbd "0") 'evil-first-non-blank)
(evil-define-key 'insert 'global "\C-w" 'evil-delete-backward-word)

(evil-define-key '(normal motion) 'global (kbd ",") nil)
(evil-define-key '(normal motion) 'global (kbd ",q") 'evil-tab-sensitive-quit)
(evil-define-key '(normal motion) 'global (kbd ",t") 'dired-in-new-tab)
(evil-define-key '(normal motion) 'global (kbd ",r") 'evil-tabs-current-buffer-to-tab)
(evil-define-key 'normal 'global (kbd ",w") 'save-buffer)
(evil-define-key 'normal 'global (kbd ",s") 'split-horizontally-and-move-to-window)
(evil-define-key 'normal 'global (kbd ",v") 'split-vertically-and-move-to-window)
(evil-define-key 'normal 'global (kbd ",c") 'comment-or-uncomment-region)
(evil-define-key 'normal 'global (kbd ",m") 'compile)

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

;; =======================================================================
;; STANDARD EMACS CONFIGS
;; =======================================================================
;; Load colorscheme
(load-theme 'darcula t)

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

;; =======================================================================
;; DIRED
;; =======================================================================
(require 'dired)

;; Dired sort directories together at the top
(setq dired-listing-switches "-la --group-directories-first")

;; Always refresh Dired from the fs state
(setq dired-auto-revert-buffer t)

(define-key dired-mode-map (kbd "n") 'evil-search-next)
(define-key dired-mode-map (kbd "N") 'evil-search-previous)
(define-key dired-mode-map (kbd "?") 'evil-search-backward)
(define-key dired-mode-map (kbd "G") 'revert-buffer)

(use-package dired-subtree
  :ensure t
  :config
  (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle))

;; =======================================================================
;; Python
;; =======================================================================
(defun python-init-stuff ()
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))
(add-hook 'python-mode-hook 'python-init-stuff)

;; =======================================================================
;; C++
;; =======================================================================
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
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Ctags
(setq tags-table-list '()) ; Put tags file here

;; =======================================================================
;; CLOJURE
;; =======================================================================
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
  ("b" paredit-backward "backward")
  ("s" paredit-splice-sexp "splice")
  ("j" paredit-join-sexps "join")
  ("k" kill-sexp "kill"))

(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode) (evil-paredit-mode)))

(defun my-clojure-stuff ()
  (rainbow-delimiters-mode t)
  (cider-mode t)
  (paredit-mode t)
  (clj-refactor-mode t)
  (yas-minor-mode t)
  (evil-define-key 'insert 'local (kbd "(") 'paredit-open-round)
  (evil-define-key 'insert 'local (kbd "{") 'paredit-open-curly)
  (evil-define-key 'insert 'local (kbd "[") 'paredit-open-square)
  (evil-define-key 'insert 'local (kbd ")") 'paredit-close-round)
  (evil-define-key 'insert 'local (kbd "}") 'paredit-close-curly)
  (evil-define-key 'insert 'local (kbd "]") 'paredit-close-square))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-in-current-window t))

(use-package clj-refactor
  :ensure t)

(add-hook 'clojure-mode-hook 'my-clojure-stuff)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; Cider control
(define-key cider-mode-map (kbd "C-c q") 'cider-quit)
(define-key cider-mode-map (kbd "C-c j") 'cider-jack-in)

;; Definition lookup
(define-key cider-mode-map (kbd "C-c g d") 'cider-find-var)

;; Expression evaluation
(define-key cider-mode-map (kbd "C-c C-e") nil)
(define-key cider-mode-map (kbd "C-c C-e l") 'cider-eval-last-sexp)
(define-key cider-mode-map (kbd "C-c C-e r") 'cider-eval-last-sexp-to-repl)
(define-key cider-mode-map (kbd "C-c C-e p") 'cider-insert-last-sexp-in-repl)
(define-key cider-mode-map (kbd "C-c C-e t") 'cider-eval-defun-at-point)
(define-key cider-mode-map (kbd "C-c C-e a") 'cider-eval-sexp-at-point)
(define-key cider-mode-map (kbd "C-c C-e d") 'cider-debug-defun-at-point)

;; Formatting
(define-key cider-mode-map (kbd "C-c f r") 'cider-format-region)
(define-key cider-mode-map (kbd "C-c f f") 'cider-format-defun)

;; Documentation
(define-key cider-mode-map (kbd "C-c C-d c") 'clojure-view-cheatsheet)

;; Namespacing
(define-key cider-mode-map (kbd "C-c C-n") 'cider-repl-set-ns)

;; Renaming refactorings
(define-key clj-refactor-map (kbd "C-c r r f") 'cljr-rename-file-or-dir)
(define-key clj-refactor-map (kbd "C-c r r s") 'cljr-rename-symbol)
;; Let refactorings
(define-key clojure-mode-map (kbd "C-c r l i") 'clojure-introduce-let)
(define-key clojure-mode-map (kbd "C-c r l m") 'clojure-move-to-let)
(define-key clj-refactor-map (kbd "C-c r l r") 'cljr-remove-let)
;; Inline symbol
(define-key clj-refactor-map (kbd "C-c r i") 'cljr-inline-symbol)


;; =======================================================================
;; CLANG FORMAT
;; =======================================================================
(defun clang-format-region-at-point ()
  (interactive)
  (defvar-local bounds (bounds-of-thing-at-point 'paragraph))
  (clang-format-region (car bounds) (cdr bounds)))

(defun my-clang-format-set-config ()
  (setq clang-format-style-option "file")
  (evil-define-key 'visual 'c++-mode-map (kbd "SPC ff") 'clang-format-region)
  (evil-define-key 'normal 'c++-mode-map (kbd "SPC ff") 'flang-format-region-at-point))

(use-package clang-format
  :ensure t
  :config (my-clang-format-set-config))

;; =======================================================================
;; HELM
;; =======================================================================
(use-package helm
  :ensure t
  :config (helm-mode))
(evil-define-key 'normal 'global (kbd "C-x C-f") 'helm-find-files)
(evil-define-key 'normal 'global (kbd "C-f") 'helm-find-files)
(evil-define-key '(normal motion) 'global (kbd "C-x b") 'helm-apropos)
(evil-define-key '(insert normal motion) 'global "\M-x" 'helm-M-x)

(evil-define-key '(normal motion) 'global (kbd "C-b") 'switch-to-buffer)

(define-key helm-map (kbd "C-w") 'backward-kill-word)

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
;; IRONY
;; =======================================================================
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-json irony-cdb-clang-complete))
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
   (add-hook 'irony-mode-hook #'irony-eldoc))
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
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-show-numbers nil
	company-tooltip-limit 20
	company-dabbrev-downcase nil)
  (use-package company-irony
    :ensure t
    :defer t
    :config
    (setq company-irony-ignore-case 'smart)
    (add-to-list 'company-backends 'company-irony)
    (use-package company-c-headers
      :ensure t
      :functions irony--extract-user-search-paths company-c-headers
      :preface
      (defun company-c-headers-path-user-irony ()
	"Return the user include paths for the current buffer."
	(when irony-mode
	  (irony--extract-user-search-paths irony--compile-options irony--working-directory)))
      :config
      (setq company-c-headers-path-user #'company-c-headers-path-user-irony)
      (add-to-list 'company-backends #'company-c-headers)))
)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.H\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))

;; =======================================================================
;; OTHER PKGS
;; =======================================================================
(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package evil-easymotion
  :ensure t)
(evilem-default-keybindings "SPC")

(use-package yasnippet
  :ensure t
  :config (use-package yasnippet-snippets :ensure t)
  (yas-global-mode 1)
  (evil-define-key 'insert 'global (kbd "C-x y y") 'yas-expand)
  (evil-define-key 'insert 'global (kbd "C-x y c") 'company-yasnippet)
  (evil-define-key 'insert 'global (kbd "C-x y n") 'yas-next-field)
  (evil-define-key 'insert 'global (kbd "C-x y p") 'yas-prev-field))

(use-package evil-magit
  :ensure t
  :config (evil-magit-init))
(global-set-key (kbd "C-x g") 'magit-status)

;; =======================================================================
;; PROJECTILE
;; =======================================================================
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map))
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm))

;; Open dired at root by just typing D
(evil-define-key 'normal 'global (kbd "D") 'projectile-dired)

(evil-define-key 'normal 'global (kbd "C-p") nil)

;; =======================================================================
;; PROJECT EXPLORER
;; =======================================================================
(use-package project-explorer
  :ensure t
  :config
  (evil-define-key 'normal 'global (kbd ",nn") 'project-explorer-open)
  (evil-define-key 'normal 'global (kbd ",nf") 'project-explorer-open)

  (setq pe/filenotify-enabled 1)
  (setq pe/width 32))

(add-hook 'project-explorer-mode-hook (lambda ()
  (evil-define-key '(normal motion) 'local (kbd "u") 'pe/up-element)
  (evil-define-key '(normal motion) 'local (kbd "TAB") 'pe/tab)
  (evil-define-key '(normal motion) 'local (kbd "a") 'pe/goto-top)
  (evil-define-key '(normal motion) 'local (kbd "+") 'pe/create-file)
  (evil-define-key '(normal motion) 'local (kbd "-") 'pe/delete-file)
  (evil-define-key '(normal motion) 'local (kbd "r") 'pe/rename-file)
  (evil-define-key '(normal motion) 'local (kbd "RET") 'pe/return)
  (evil-define-key '(normal motion) 'local (kbd "q") 'pe/quit)
))

;; =======================================================================
;; CUSTOM COMMANDS
;; =======================================================================
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


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
