;; ==============================================================================
;; Prefix documentation:
;;
;; Available:
;; C-q
;; C-t 
;; C-y
;; C-n
;; C-All non letter except ;
;; Most M-
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
(defun robert-split-horizontally-and-move-to-window ()
  "Split a window and also move to it"
  (interactive)
  (split-window-vertically)
  (windmove-down))

(defun robert-split-vertically-and-move-to-window ()
  "Split a window and also move to it"
  (interactive)
  (split-window-horizontally)
  (windmove-right))

(defun robert-dired-in-new-tab ()
  (interactive)
  (require 'projectile)
  (let ((root (or (projectile-project-root) default-directory)))
    (evil-ex-call-command nil "tabnew" nil)
    (dired root)))

(defun robert-reload-emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun robert-kill-this-buffer-and-close-window ()
  (interactive)
  (kill-this-buffer)
  (robert-evil-tab-sensitive-quit))

(defun robert-vterm-horizontally-in-new-window ()
  (interactive)
  (robert-split-horizontally-and-move-to-window)
  (multi-vterm))

(defun robert-vterm-move-to-start-of-prompt ()
  (interactive)
  (evil-first-non-blank)
  (evil-find-char 1 ?$)
  (evil-forward-char))

(defun robert-accept-search-and-center-screen ()
  (interactive)
  (evil-scroll-line-to-center (line-number-at-pos))
  (isearch-exit))

(defun robert-evil-tab-sensitive-quit ()
  (interactive)
  (require 'evil)
  (if (> (length (elscreen-get-screen-list)) 1)
      (if (> (count-windows) 1)
          (evil-quit)
        (elscreen-kill))
    (evil-quit)))

;; =======================================================================
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; STANDARD EMACS CONFIGS
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; =======================================================================
;; Load colorscheme
(load-theme 'darcula t)

;; Disable to tool bar in gui mode (useless and takes tons of space)
(tool-bar-mode -1)

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

;; y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't confirm killing proc buffers
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

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

;; Write backups in .emacs.d/backups instead of all over fs
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Ctags
(setq tags-table-list '()) ; Put tags file here

;; Compilation
(require 'compile)

;; Always display certain buffers in a new window
(setq display-buffer-alist '(("\\*cider-error\\*"
                              (display-buffer-reuse-window display-buffer-pop-up-window)
                              ())))
(setq split-width-threshold 1)
(setq split-height-threshold 1)

;; Grep ignore directories
(require 'grep)
(add-to-list 'grep-find-ignored-directories "doxygen")

;; Make it so that hitting Shift-Backspace doesn't open help menu
(define-key global-map "\C-h" 'delete-backward-char)

;; Winner
(setq winner-dont-bind-my-keys t)
(require 'winner)
(winner-mode 1)

;; =======================================================================
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; PACKAGES
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; =======================================================================
;; =======================================================================
;; EVIL
;; Copied from evil-core.el, since this is helpful for debugging bindings
;;
;; Evil is defined as a globalized minor mode, enabled with the toggle
;; function `evil-mode'.  This in turn enables `evil-local-mode' in
;; every buffer, which sets up the buffer's state.
;;
;; Each state has its own keymaps, and these keymaps have status as
;; "emulation keymaps" with priority over regular keymaps.  Emacs
;; maintains the following keymap hierarchy (highest priority first):
;;
;;     * Overriding keymaps/overlay keymaps...
;;     * Emulation mode keymaps...
;;       - Evil keymaps...
;;     * Minor mode keymaps...
;;     * Local keymap (`local-set-key')
;;     * Global keymap (`global-set-key')
;;
;; Within this hierarchy, Evil arranges the keymaps for the current
;; state as shown below:
;;
;;     * Intercept keymaps...
;;     * Local state keymap
;;     * Minor-mode keymaps...
;;     * Auxiliary keymaps...
;;     * Overriding keymaps...
;;     * Global state keymap
;;     * Keymaps for other states...
;;
;; These keymaps are listed in `evil-mode-map-alist', which is listed
;; in `emulation-mode-map-alist'.
;; =======================================================================
(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(use-package evil
  :ensure t
  :config

  ;; Evil mode everywhere
  (evil-mode)

  ;; Collection of keybindings for additional modes. Good common starting place
  ;; for supporting evil
  (use-package evil-collection
    :after evil
    :ensure t
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init 'vterm)
    (evil-collection-init 'xref)
    (evil-collection-init 'info)
    (evil-collection-init 'compile)
    (evil-collection-init 'dired)
    (evil-collection-init 'debug))

  ;; Vim-like tabs support
  (use-package evil-tabs
    :ensure t
    :config
    (evil-tabs-mode)
    ;; Don't want elscreen default prefix
    (define-key global-map (kbd "C-z") nil))

  ;; Normal mode can go to EOL
  (setq evil-move-beyond-eol t)

  ;;Change how evil states are displayed in the mode line
  (setq evil-normal-state-tag "NORMAL")
  (setq evil-motion-state-tag "MOTION")
  (setq evil-insert-state-tag "INSERT")
  (setq evil-visual-state-tag "VISUAL")
  (setq evil-emacs-state-tag "EMACS"))

(evil-define-key '(normal motion) 'global (kbd "SPC") nil)
(evil-define-key '(normal motion) 'global (kbd ",") nil)

;; =======================================================================
;; HELM
;; =======================================================================
(use-package helm
  :ensure t
  :config
  (helm-mode)

  ;; When doing a helm search want C-w to delete a word
  (define-key helm-map (kbd "C-w") 'backward-kill-word)

  ;; Set up locate db
  ;; (setq locate-db-command
  ;;       (with-temp-buffer
  ;;         (insert-file-contents "PATH") (buffer-string)))
  ;; (setq helm-locate-command locate-db-command)

  ;; Make find-file behave more like terminal bindings
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-find-files-map (kbd "C-<return>") 'helm-ff-run-switch-other-window)
  (define-key helm-find-files-map (kbd "M-<return>") 'helm-ff-run-switch-other-frame)
  (define-key helm-buffer-map (kbd "C-<return>") 'helm-buffer-switch-other-window)
  (define-key helm-buffer-map (kbd "M-<return>") 'helm-buffer-switch-other-frame)
  (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)

  ;; Dont start the helm search at the file under point
  (setq helm-find-files-ignore-thing-at-point t)

  ;; Helm for xref
  (use-package helm-xref
    :ensure t)

  ;; Helm search in a file
  (use-package helm-swoop
    :ensure t
    :config
    (define-key helm-swoop-map (kbd "C-n") 'helm-next-line)
    (define-key helm-swoop-map (kbd "C-p") 'helm-previous-line)

    (setq helm-swoop-split-with-mutiple-windows t)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-use-fuzzy-match t)))

;; =======================================================================
;; VTerm
;; Needs --with-native-compilation in emacs build
;; =======================================================================
(use-package vterm
  :ensure t
  :config
  (use-package multi-vterm :ensure t)
  (defun kill-vterm-buffers ()
    (interactive)
    (kill-some-buffers (seq-filter (lambda (b)
                                     (string-match-p "^\\*vterm" (buffer-name b)))
                                   (buffer-list))))
  (evil-define-key 'insert vterm-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'insert vterm-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key '(normal insert motion visual) vterm-mode-map (kbd "C-x ESC") 'vterm-send-escape)
  (evil-define-key '(normal motion) vterm-mode-map (kbd "0") 'robert-vterm-move-to-start-of-prompt))

;; =======================================================================
;; DIRED
;; =======================================================================
(use-package dired
  :config

  ;; Dired sort directories together at the top
  ;; TODO: This doesn't work on OSX
  ;; (setq dired-listing-switches "-la --group-directories-first")

  ;; Always refresh Dired from the fs state
  (setq dired-auto-revert-buffer t)

  ;; These are required because Evil unbinds all of its keys by default in Dired mode
  (evil-define-key '(normal motion) dired-mode-map (kbd "gg") 'evil-goto-first-line)
  (evil-define-key '(normal motion) dired-mode-map (kbd "G") 'revert-buffer)
  (evil-define-key '(normal motion) dired-mode-map (kbd "?") 'evil-search-backward)
  (evil-define-key '(normal motion) dired-mode-map (kbd "n") 'evil-search-next)
  (evil-define-key '(normal motion) dired-mode-map (kbd "N") 'evil-search-previous)

  ;; Unbind so SPC prefix commands work
  (evil-define-key '(normal motion) dired-mode-map (kbd "SPC") nil)

  ;; Allow viewing subtrees in the same buffer
  (use-package dired-subtree
    :ensure t
    :config
    (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)))

;; =======================================================================
;; COMPILE
;; =======================================================================
(use-package compile
  :config
  ;; next-error skip warnings
  (setq compilation-skip-threshold 2)

  (define-key compilation-mode-map (kbd "g") nil)
  (define-key compilation-mode-map (kbd "r") 'recompile)
  (define-key compilation-mode-map (kbd "n") 'evil-search-next)
  (define-key compilation-mode-map (kbd "N") 'evil-search-previous)

  (evil-define-key '(normal motion) compilation-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key '(normal motion) compilation-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key '(normal motion) compilation-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key '(normal motion) compilation-mode-map (kbd "C-l") 'windmove-right))

;; =======================================================================
;; PROCESS MENU MODE
;; =======================================================================
(defun robert-process-menu-bindings ()
  (evil-define-key '(normal motion) 'local (kbd "x") 'process-menu-delete-process))
(add-hook 'process-menu-mode-hook #'robert-process-menu-bindings)

;; =======================================================================
;; INFO/WOMAN MODE
;; =======================================================================
;; Don't want my window bindings overriden
(evil-define-key '(normal motion) Info-mode-map (kbd "C-j") 'windmove-down)
(evil-define-key '(normal motion) Info-mode-map (kbd "C-k") 'windmove-up)
(evil-define-key '(normal motion) Info-mode-map (kbd "C-h") 'windmove-left)
(evil-define-key '(normal motion) Info-mode-map (kbd "C-l") 'windmove-right)

;; Node movement
(evil-define-key '(normal motion) Info-mode-map (kbd "M-j") 'Info-next)
(evil-define-key '(normal motion) Info-mode-map (kbd "M-k") 'Info-prev)
(evil-define-key '(normal motion) Info-mode-map (kbd "M-h") 'elscreen-previous)
(evil-define-key '(normal motion) Info-mode-map (kbd "M-l") 'evil-tabs-goto-tab)

(setq woman-fill-frame 1)
(setq woman-fill-column 80)

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
  ("l" (evil-window-increase-width 5) "increase-width")
  ("q" (delete-other-windows) "delete-other-windows" :exit t)
  ("x" (elscreen-kill) "kill-tab" :exit t)
  ("b" (balance-windows-area) "balance-area" :exit t))

;; =======================================================================
;; WHICH KEY
;; =======================================================================
(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; =======================================================================
;; ACE JUMP
;; =======================================================================
(use-package ace-jump-mode
  :ensure t
  :config
  (evil-define-key 'normal 'global (kbd "SPC SPC") 'ace-jump-mode))

;; =======================================================================
;; YASNIPPET
;; =======================================================================
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-keymap (kbd "C-f") 'yas-next-field)
  (define-key yas-keymap (kbd "C-b") 'yas-prev-field)
  (evil-define-key 'insert 'global (kbd "C-SPC") 'yas-expand)
  (evil-define-key 'insert 'global (kbd "M-y") 'company-yasnippet))

;; =======================================================================
;; MAGIT
;; =======================================================================
(use-package evil-magit
  :ensure t
  :config
  (evil-magit-init))

;; =======================================================================
;; RECENTF
;; =======================================================================
(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 50))

;; =======================================================================
;; TELEPHONE LINE
;; =======================================================================
(set-terminal-coding-system 'utf-8)

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (nil .  (telephone-line-buffer-segment
                   telephone-line-minor-mode-segment))))
  (setq telephone-line-primary-left-separator 'telephone-line-abs-left)
  (setq telephone-line-secondary-left-separator 'telephone-line-abs-hollow-left)
  (setq telephone-line-primary-right-separator 'telephone-line-abs-right)
  (setq telephone-line-secondary-right-separator 'telephone-line-abs-hollow-right)
  (setq telephone-line-rhs
        '((nil . (telephone-line-misc-info-segment))
          (evil . (telephone-line-airline-position-segment))))
  (telephone-line-mode 1))

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

  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)))

;; =======================================================================
;; COMPANY
;; =======================================================================
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; Unbind some completion stuff (apparently these sometimes overwrite company-complete stuff set below)
  (evil-define-key 'insert 'global (kbd "C-n") nil)
  (evil-define-key 'insert 'global (kbd "C-p") nil)

  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<backtab>") 'company-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word)
  (setq company-idle-delay 0.2
	    company-minimum-prefix-length 1
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
;; -----------------------------------------------------------------------
;; PROGRAMMING LANGUAGE SUPPORT
;; -----------------------------------------------------------------------
;; =======================================================================
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
;; CLANG FORMAT
;; =======================================================================
(defun clang-format-region-at-point ()
  (interactive)
  (defvar-local bounds (bounds-of-thing-at-point 'paragraph))
  (clang-format-region (car bounds) (cdr bounds)))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style-option "file"))

;; =======================================================================
;; CLOJURE / LISP
;; =======================================================================
(use-package smartparens
  :ensure t
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "(" nil :unless '(sp-in-string-p))
  (sp-pair "\\(" nil :unless '(sp-in-string-p))

  (defhydra hydra-smartparens nil
    "smartparens"
    ("f" sp-forward-slurp-sexp "slurp-forward")
    ("F" sp-forward-barf-sexp "barf-forward")
    ("b" sp-backward-slurp-sexp "slurp-backward")
    ("B" sp-backward-barf-sexp "barf-backward")
    ("l" sp-forward-sexp "forward")
    ("k" sp-up-sexp "forward-up")
    ("j" sp-down-sexp "forward-down")
    ("h" sp-backward-sexp "backward")
    ("K" sp-backward-up-sexp "backward-up")
    ("J" sp-backward-down-sexp "backward-down")
    ("L" sp-next-sexp "next")
    ("H" sp-previous-sexp "previous")
    ("0" sp-beginning-of-sexp "beginning")
    ("$" sp-end-of-sexp "end")
    ("x" sp-kill-sexp "forward-kill")
    ("X" sp-backward-kill-sexp "backward-kill")
    ("y" sp-copy-sexp "copy-forward")
    ("Y" sp-backward-copy-sexp "copy-backward")
    ("s" sp-splice-sexp "splice")
    ("t" sp-transpose-sexp "transpose"))

  (use-package evil-smartparens
    :ensure t
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(use-package rainbow-delimiters :ensure t)
(use-package aggressive-indent :ensure t)

(defun my-elisp-stuff ()
  (smartparens-strict-mode t)
  (aggressive-indent-mode t))
(add-hook 'emacs-lisp-mode-hook #'my-elisp-stuff)

(use-package clojure-mode
  :ensure t
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)

  (use-package cider
    :ensure t
    :config
    (use-package clj-refactor :ensure t)
    (use-package cljr-helm :ensure t)
    (use-package helm-cider :ensure t :config (helm-cider-mode t))

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
    (define-key cider-mode-map (kbd "C-c RET") nil) ; macroexpand
    (define-key cider-mode-map (kbd "C-c ,") nil) ; test (use C-c C-t)
    (define-key cider-mode-map (kbd "C-c C-?") nil) ; xref
    (define-key cider-mode-map (kbd "C-c C-d") nil) ; Doc (use C-c d)
    (define-key cider-mode-map (kbd "C-c M-i") nil) ; Inspect
    (define-key cider-mode-map (kbd "C-c M-s") nil) ; Selector
    (define-key cider-mode-map (kbd "C-c M-;") nil)
    (define-key cider-mode-map (kbd "C-c M-t") nil) ; Cider trace
    (define-key cider-mode-map (kbd "C-c C-M-l") nil) ; Load all files
    (define-key cider-mode-map (kbd "C-c M-z") nil) ; Load all files

    ;; Cider control
    (define-key cider-mode-map (kbd "C-c C-q") 'cider-quit)
    (define-key cider-mode-map (kbd "C-c j") 'cider-jack-in)
    (define-key cider-mode-map (kbd "C-c J") 'cider-jack-in-cljs)

    ;; Find thing: C-c g
    (let (find-thing-cider-robert-map)
      (define-prefix-command 'find-thing-cider-robert-map)
      (define-key find-thing-cider-robert-map (kbd "d") 'cider-find-var)
      (define-key find-thing-cider-robert-map (kbd "n") 'cider-find-ns)
      (define-key find-thing-cider-robert-map (kbd "r") 'cider-find-resource)
      (define-key find-thing-cider-robert-map (kbd "k") 'cider-find-keyword)
      (define-key cider-mode-map (kbd "C-c g") 'find-thing-cider-robert-map))

    ;; Expression evaluation: C-c e
    (let (eval-cider-robert-map)
      (define-prefix-command 'eval-cider-robert-map)
      (define-key eval-cider-robert-map (kbd "l") 'cider-eval-last-sexp)
      (define-key eval-cider-robert-map (kbd "r") 'cider-eval-last-sexp-to-repl)
      (define-key eval-cider-robert-map (kbd "p") 'cider-insert-last-sexp-in-repl)
      (define-key eval-cider-robert-map (kbd "t") 'cider-eval-defun-at-point)
      (define-key eval-cider-robert-map (kbd "a") 'cider-eval-sexp-at-point)
      (define-key eval-cider-robert-map (kbd "d") 'cider-debug-defun-at-point)
      (define-key cider-mode-map (kbd "C-c e") 'eval-cider-robert-map))

    ;; Macros: C-c m
    (let (macro-cider-robert-map)
      (define-prefix-command 'macro-cider-robert-map)
      (define-key macro-cider-robert-map (kbd "1") 'cider-macroexpand-1)
      (define-key macro-cider-robert-map (kbd "a") 'cider-macroexpand-all)
      (define-key cider-mode-map (kbd "C-c m") 'macro-cider-robert-map))

    ;; Formatting: C-c f
    (let (format-cider-robert-map)
      (define-prefix-command 'format-cider-robert-map)
      (define-key format-cider-robert-map (kbd "r") 'cider-format-region)
      (define-key format-cider-robert-map (kbd "f") 'cider-format-defun)
      (define-key cider-mode-map (kbd "C-c f") 'format-cider-robert-map))

    ;; Namespace C-c n
    (let (ns-cider-robert-map)
      (define-prefix-command 'ns-cider-robert-map)
      (define-key ns-cider-robert-map (kbd "b") 'cider-browse-ns)
      (define-key ns-cider-robert-map (kbd "n") 'cider-repl-set-ns)
      (define-key ns-cider-robert-map (kbd "f") 'cider-ns-refresh)
      (define-key cider-mode-map (kbd "C-c n") 'ns-cider-robert-map))

    ;; Documentation: C-c C-d (by default), C-c d
    (define-key cider-doc-map (kbd "c") 'cider-cheatsheet)
    (define-key cider-doc-map (kbd "C") 'helm-cider-cheatsheet)
    (define-key cider-mode-map (kbd "C-c d") 'cider-doc-map)

    ;; Namespacing
    (define-key cider-mode-map (kbd "C-c C-n") 'cider-repl-set-ns)

    ;; Fix up REPL usage in Normal mode
    (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-backward-input)
    (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-forward-input)
    ;; We define RET -> cider-repl-return in cider-repl-mode-hooks for evil reasons

    ;; Refactoring: C-c r
    (define-key clojure-mode-map (kbd "C-c C-r") 'cljr-helm)
    (let ((refactor-cljr-robert-map)
          (rename-refactor-cljr-robert-map)
          (let-refactor-cljr-robert-map)
          (cycle-refactor-cljr-robert-map)
          (add-refactor-cljr-robert-map))
      (define-prefix-command 'refactor-cljr-robert-map)
      (define-prefix-command 'rename-refactor-cljr-robert-map)
      (define-prefix-command 'let-refactor-cljr-robert-map)
      (define-prefix-command 'cycle-refactor-cljr-robert-map)
      (define-prefix-command 'add-refactor-cljr-robert-map)
      ;; Renaming refactorings C-c r r
      (define-key rename-refactor-cljr-robert-map (kbd "f") 'cljr-rename-file-or-dir)
      (define-key rename-refactor-cljr-robert-map (kbd "s") 'cljr-rename-symbol)
      (define-key refactor-cljr-robert-map (kbd "r") 'rename-refactor-cljr-robert-map)
      ;; Let refactorings C-c r l
      (define-key let-refactor-cljr-robert-map (kbd "i") 'clojure-introduce-let)
      (define-key let-refactor-cljr-robert-map (kbd "m") 'clojure-move-to-let)
      (define-key let-refactor-cljr-robert-map (kbd "r") 'cljr-remove-let)
      (define-key let-refactor-cljr-robert-map (kbd "e") 'cljr-expand-let)
      (define-key refactor-cljr-robert-map (kbd "l") 'let-refactor-cljr-robert-map)
      ;; Inline symbol C-c r i
      (define-key refactor-cljr-robert-map (kbd "i") 'cljr-inline-symbol)
      ;; Cycle C-c r c
      (define-key cycle-refactor-cljr-robert-map (kbd "p") 'clojure-cycle-privacy)
      (define-key cycle-refactor-cljr-robert-map (kbd "n") 'clojure-cycle-not)
      (define-key cycle-refactor-cljr-robert-map (kbd "i") 'clojure-cycle-if)
      (define-key refactor-cljr-robert-map (kbd "c") 'cycle-refactor-cljr-robert-map)
      ;; Add C-c r a
      (define-key add-refactor-cljr-robert-map (kbd "r") 'cljr-add-require-to-ns)
      (define-key add-refactor-cljr-robert-map (kbd "d") 'cljr-add-project-dependency)
      (define-key add-refactor-cljr-robert-map (kbd "l") 'cljr-add-declaration)
      (define-key add-refactor-cljr-robert-map (kbd "a") 'cljr-add-missing-libspec)
      (define-key refactor-cljr-robert-map (kbd "a") 'add-refactor-cljr-robert-map)

      (define-key clojure-mode-map (kbd "C-c r") 'refactor-cljr-robert-map)))

  (defun my-cider-repl-mode-stuff ()
    (rainbow-delimiters-mode t)
    (smartparens-strict-mode t)
    (evil-define-key '(normal motion) 'local (kbd "<up>") 'cider-repl-backward-input)
    (evil-define-key '(normal motion) 'local (kbd "<down>") 'cider-repl-forward-input)
    (evil-define-key '(normal motion) 'local (kbd "RET") 'cider-repl-return))

  (defun my-clojure-stuff ()
    (rainbow-delimiters-mode t) ; Highlight matching parens
    (cider-mode t)
    (smartparens-strict-mode t)
    (clj-refactor-mode t)
    (yas-minor-mode t)
    (aggressive-indent-mode t))

  (add-hook 'clojure-mode-hook 'my-clojure-stuff)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-stuff))

;; =======================================================================
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; KEYBINDINGS
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; =======================================================================
;; Filesystem movement
(evil-define-key '(normal motion) 'global (kbd "C-f") 'helm-find-files)
(evil-define-key '(normal motion) 'global (kbd "C-b") 'helm-mini)
(evil-define-key 'normal 'global (kbd "SPC d") 'helm-projectile-find-file)
(evil-define-key '(normal motion) 'global (kbd "C-x l") 'helm-locate)
(evil-define-key '(normal motion) 'global (kbd "C-x f") 'find-name-dired)

;; Frames
(evil-define-key '(normal motion) 'global (kbd "SPC f f") 'make-frame)
(evil-define-key '(normal motion) 'global (kbd "SPC f x") 'delete-frame)

;; Tabs
(evil-define-key '(normal motion insert) 'global (kbd "M-h") 'elscreen-previous)
(evil-define-key '(normal motion insert) 'global (kbd "M-l") 'evil-tabs-goto-tab)
(evil-define-key '(normal motion) 'global (kbd ",t") 'robert-dired-in-new-tab)

;; Windows
(evil-define-key '(normal motion) 'global (kbd "C-j") 'windmove-down)
(evil-define-key '(normal motion) 'global (kbd "C-k") 'windmove-up)
(evil-define-key '(normal motion) 'global (kbd "C-h") 'windmove-left)
(evil-define-key '(normal motion) 'global (kbd "C-l") 'windmove-right)
(evil-define-key '(normal motion) 'global (kbd ",q") 'robert-evil-tab-sensitive-quit)
(evil-define-key '(normal motion) vterm-mode-map (kbd ",k") 'robert-kill-this-buffer-and-close-window)
(evil-define-key '(normal motion) 'global (kbd "C-x C-k") 'robert-kill-this-buffer-and-close-window)
(evil-define-key '(normal motion) 'global (kbd ",s") 'robert-split-horizontally-and-move-to-window)
(evil-define-key '(normal motion) 'global (kbd ",v") 'robert-split-vertically-and-move-to-window)
(evil-define-key '(normal motion) 'global (kbd "C-w") 'hydra-window/body)

;; Scrolling
(evil-define-key '(normal motion) 'global "\C-d" 'evil-scroll-down)
(evil-define-key '(normal motion) 'global "\C-u" 'evil-scroll-up)

;; Text movement/editing
(evil-define-key '(normal motion) 'global (kbd "0") 'evil-first-non-blank)
(evil-define-key 'insert 'global "\C-w" 'evil-delete-backward-word)
(evil-define-key '(normal motion) 'global (kbd "'") 'evil-repeat-find-char-reverse)
(evil-define-key 'normal 'global (kbd ",w") 'save-buffer)
(evil-define-key 'normal 'global (kbd ",c") 'comment-or-uncomment-region)

;; Text search
(evil-define-key '(normal motion) 'global (kbd "SPC s") 'helm-do-grep-ag)
(evil-define-key '(normal motion) 'global (kbd "SPC S") 'helm-projectile-ag)
(evil-define-key 'normal 'global (kbd "SPC /") 'helm-swoop)

;; Ctags
(evil-define-key '(normal motion) 'global (kbd "SPC t") 'projectile-find-tag)
(evil-define-key '(normal motion) 'global (kbd "SPC x d") 'xref-find-definitions)
(evil-define-key '(normal motion) 'global (kbd "SPC x r") 'xref-find-references)

;; Clang format
(evil-define-key 'visual 'c++-mode-map (kbd "SPC c") 'clang-format-region)
(evil-define-key 'normal 'c++-mode-map (kbd "SPC c") 'flang-format-region-at-point)

;; Helm emacs
(evil-define-key '(insert normal motion) 'global (kbd "M-x") 'helm-M-x)
(evil-define-key '(normal motion) 'global (kbd "C-x b") 'helm-apropos)

;; Other helm bindings
(evil-define-key '(normal motion) 'global (kbd "C-x k") 'helm-show-kill-ring)
(evil-define-key '(normal motion) 'global (kbd "SPC r") 'helm-resume)
(evil-define-key '(normal motion) 'global (kbd "SPC i") 'helm-imenu)

;; Vterm
(evil-define-key '(normal motion) 'global (kbd "SPC v") 'multi-vterm)
(evil-define-key '(normal motion) 'global (kbd "SPC C-v") 'robert-vterm-horizontally-in-new-window)
(evil-define-key '(normal motion) 'global (kbd "SPC ,") 'multi-vterm-rename-buffer)

;; Processes
(evil-define-key '(normal motion) 'global (kbd "C-x p") 'list-processes)

;; Smartparens
(evil-define-key  '(normal motion visual insert)  'global (kbd "C-e") 'hydra-smartparens/body)

;; Compilation
(evil-define-key '(normal motion) 'global (kbd ",m") 'compile)
(evil-define-key '(normal motion) 'global (kbd "C-;") 'recompile)

;; Documentation
(evil-define-key '(normal motion) 'global (kbd "SPC m") 'woman)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Rebind universal arg
(define-key global-map (kbd "M-u") 'universal-argument)

;; Map C-s to escape
(define-key key-translation-map (kbd "C-s") (kbd "<escape>"))

;; Help prefix
(define-key global-map (kbd "C-x h") help-map)

;; Generically do this in places without evil bindings
(define-key global-map (kbd "C-w") 'backward-kill-word)

;; Recenter screen after search
;; Note: Since evil uses isearch and isearch uses overriding-terminal-local-map
;; we should set this keybinding without evil-define-key to make it work.
;; It will show up in the overriding-terminal-local-map this way.
(define-key isearch-mode-map (kbd "<return>") 'robert-accept-search-and-center-screen)

;; Undefines
(define-key global-map (kbd "C-x a") nil)
(define-key global-map (kbd "C-x m") nil)
(define-key global-map (kbd "C-4") nil)
(define-key global-map (kbd "C-c M-g") nil)

;; =======================================================================
;; Generated!
;; =======================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-collection-setup-minibuffer t)
 '(package-selected-packages
   '(multi-vterm vterm cider ace-jump helm-cider-history helm-cider aggressive-indent hydra rainbow-delimiters evil-paredit clojure-mode helm-ag ag csv-mode evil-magit clang-format yasnippet modern-cpp-font-lock irony helm use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
