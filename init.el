;; ==============================================================================
;; Prefix documentation:
;;
;; Leaders:
;; , inherited from old vim config
;; SPC most general purpose
;; C-x closely tied to emacs stuff
;; C-c major mode specific
;;
;; Todo
;; - Tree-sitter: the default c++ font-lock is awful: is this any better?
;; - Org mode: learn bindings or add new ones, figure out a proper workflow
;; - lsp-ui: integrate docs, peek, imenu into workflow, keep what works
;; - cleverparens: explore what movement ops can be integrated
;; - rust mode / prolog mode usage and tooling
;; ==============================================================================
;; =======================================================================
;; PACKAGE INITIALIZATION
;; =======================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(setq package-enable-at-startup t)
(require 'use-package)

(use-package general :ensure t)

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
  (require 'elscreen)
  (if (> (length (elscreen-get-screen-list)) 1)
      (if (> (count-windows) 1)
          (evil-quit)
        (elscreen-kill))
    (evil-quit)))

(defun robert-open-header-in-new-window ()
  (interactive)
  (robert-split-horizontally-and-move-to-window)
  (ff-find-other-file))

;; =======================================================================
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; STANDARD EMACS CONFIGS
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; =======================================================================
;; Load colorscheme
(load-theme 'doom-pine t)

;; Disable to tool bar in gui mode (useless and takes tons of space)
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

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
(global-display-line-numbers-mode 1)
(column-number-mode 1)

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
                              ())
                             ("\\*compilation\\*"
                              (display-buffer-reuse-window display-buffer-pop-up-window)
                              ())
                             ("\\*rustfmt\\*"
                              (display-buffer-reuse-window display-buffer-in-side-window)
                              ('side . 'bottom)
                              ('window-height . 0.3))))
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

;; Rebind universal arg
(define-key global-map (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

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

(set-terminal-coding-system 'utf-8)

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

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :config
  ;; Evil mode everywhere
  (evil-mode 1)

  ;; Evil undo
  (evil-set-undo-system 'undo-redo)

  ;; Normal mode can go to EOL
  (setq evil-move-beyond-eol t)

  ;; Unbind
  (evil-define-key '(normal motion) 'global (kbd "SPC") nil)
  (evil-define-key '(normal motion) 'global (kbd ",") nil)

  ;; Change how evil states are displayed in the mode line
  (setq evil-normal-state-tag "NORMAL"
        evil-motion-state-tag "MOTION"
        evil-insert-state-tag "INSERT"
        evil-visual-state-tag "VISUAL"
        evil-emacs-state-tag "EMACS"))

;; Collection of keybindings for additional modes. Good common starting place
;; for supporting evil
(use-package evil-collection
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  ;; These set up autoloads for their respective packages
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
  (evil-tabs-mode 1)
  :general
  ;; Don't want elscreen default prefix
  ("C-z" nil)
  (:states '(normal motion)
   ",q" 'robert-evil-tab-sensitive-quit
   ",t" 'robert-dired-in-new-tab)
  (:states '(normal motion insert visual emacs)
   :keymaps 'override
   "M-h" 'elscreen-previous
   "M-l" 'evil-tabs-goto-tab))


;; =======================================================================
;; HELM
;; =======================================================================
(use-package helm
  :ensure t
  :general
  ;; When doing a helm search want C-w to delete a word
  ("M-x" 'helm-M-x
   "C-x l" 'helm-locate
   "C-x b" 'helm-apropos
   "C-x k" 'helm-show-kill-ring)
  (:states '(normal motion)
   "C-f" 'helm-find-files
   "C-b" 'helm-mini
   "SPC i" 'helm-imenu
   "SPC r" 'helm-resume
   "SPC s" 'helm-do-grep-ag
   "SPC m" 'helm-man-woman)
  (:keymaps 'helm-map
   "C-w" 'backward-kill-word
   "C-z" 'helm-select-action)
  (:keymaps 'helm-find-files-map
   "TAB" 'helm-execute-persistent-action
   "C-<return>" 'helm-ff-run-switch-other-window
   "M-<return>" 'helm-ff-run-switch-other-frame
   "C-w" 'helm-find-files-up-one-level)
  (:keymaps 'helm-buffer-map
   "C-<return>" 'helm-buffer-switch-other-window
   "M-<return>" 'helm-buffer-switch-other-frame)

  :config
  (helm-mode 1)

  ;; Set up locate db
  ;; (setq locate-db-command
  ;;       (with-temp-buffer
  ;;         (insert-file-contents "PATH") (buffer-string)))
  ;; (setq helm-locate-command locate-db-command)

  ;; Dont start the helm search at the file under point
  (setq helm-find-files-ignore-thing-at-point t))

(use-package helm-xref
  :ensure t)

;; Helm search in a file
(use-package helm-swoop
  :ensure t
  :general
  (:states '(normal motion)
   "SPC /" 'helm-swoop)
  (:keymaps 'helm-swoop-map
   "C-n" 'helm-next-line
   "C-p" 'helm-previous-line)
  :config
  (setq helm-swoop-split-with-mutiple-windows t
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-use-fuzzy-match t))

;; =======================================================================
;; VTerm
;; Needs --with-native-compilation in emacs build
;; =======================================================================
(use-package vterm
  :ensure t
  :general
  (:keymaps 'vterm-mode-map
   "C-x ESC" 'vterm-send-escape)
  (:states 'normal :keymaps 'vterm-mode-map
   "0" 'robert-vterm-move-to-start-of-prompt)
  :config
  (defun kill-vterm-buffers ()
    (interactive)
    (kill-some-buffers (seq-filter (lambda (b)
                                     (string-match-p "^\\*vterm" (buffer-name b)))
                                   (buffer-list)))))

(use-package multi-vterm
  :ensure t
  :after vterm
  :general
  (:states '(normal motion)
   "SPC v" 'multi-vterm
   "SPC C-v" 'robert-vterm-horizontally-in-new-window
   "SPC ," 'multi-vterm-rename-buffer))

;; =======================================================================
;; COMPANY
;; =======================================================================
(use-package company
  :ensure t
  :demand t
  :general
  (:keymaps 'company-active-map
   "RET" nil
   "<return>" nil
   "<backtab>" 'company-complete-common
   "<tab>" 'company-complete-selection
   "TAB" 'company-complete-selection
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "C-w" 'backward-kill-word)
  :config
  (global-company-mode 1)
  ;; Unbind some completion stuff (apparently these sometimes overwrite company-complete stuff set below)
  (evil-define-key 'insert 'global (kbd "C-n") nil)
  (evil-define-key 'insert 'global (kbd "C-p") nil)
  
  (setq company-idle-delay 0.2
	    company-minimum-prefix-length 1
	    company-show-numbers nil
	    company-tooltip-limit 20
	    company-dabbrev-downcase nil))

;; =======================================================================
;; LSP
;; =======================================================================
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c++-mode . lsp-deferred)
   (rust-mode . lsp-deferred))
  :general
  (:states 'normal
   "SPC l" '(:keymap lsp-command-map))
  :commands lsp
  :config
  (setq lsp-clangd-binary-path "/usr/bin/clangd"
        lsp-clients-clangd-args '("--header-insertion=never")
        lsp-enable-on-type-formatting nil)
  (use-package helm-lsp :ensure t)
  (use-package lsp-ui
    :ensure t
    :general
    (:states '(normal motion)
     "SPC e" 'lsp-ui-imenu)
    (:states '(normal motion)
     :prefix "SPC x"
     :prefix-command 'xref-robert-prefix
     "h" 'helm-lsp-workspace-symbol
     "p" 'lsp-ui-peek-find-definitions
     "e" 'lsp-ui-peek-find-references
     "f" 'lsp-find-definition
     "r" 'lsp-find-references
     "i" 'lsp-find-implementation
     "t" 'lsp-find-type-definition)
    (:states '(normal motion)
     :prefix "SPC d"
     :prefix-command 'doc-robert-prefix
     "g" 'lsp-ui-doc-glance
     "d" 'lsp-ui-doc-show
     "q" 'lsp-ui-doc-hide
     "f" 'lsp-ui-doc-focus-frame)
    :config
    (setq lsp-ui-peek-enable t
          lsp-ui-peek-always-show t)
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-position 'at-point))
  (use-package flycheck
    :ensure t)
  (use-package flycheck-rust
    :ensure t)
  ;; SPC d prefix above is preferred, but this at least allows it to
  ;; be used in insert mode if desired
  (general-def 'lsp-command-map
    "h d" 'lsp-ui-doc-show
    "h f" 'lsp-ui-doc-focus-frame
    "h q" 'lsp-ui-doc-hide
    "a h" 'helm-lsp-code-actions))

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
;; YASNIPPET
;; =======================================================================
(use-package yasnippet
  :ensure t
  :general
  (:keymaps 'yas-keymap
   "C-f" 'yas-next-field
   "C-b" 'yas-prev-field)
  (:states 'insert
   "C-SPC" 'yas-expand
   "M-y" 'company-yasnippet)
  :config
  (yas-global-mode 1))

;; =======================================================================
;; MAGIT
;; =======================================================================
(use-package magit
  :ensure t
  :general
  ("C-x g" 'magit-status))

;; =======================================================================
;; PROJECTILE
;; =======================================================================
(use-package projectile
  :ensure t
  :general
  (:states '(normal insert motion)
   "C-p" 'projectile-command-map)
  :config
  (projectile-mode t)

  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)))

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
  (define-key compilation-mode-map (kbd "N") 'evil-search-previous))

;; =======================================================================
;; INFO/WOMAN MODE
;; =======================================================================
;; Node movement
(evil-define-key '(normal motion) Info-mode-map (kbd "M-j") 'Info-next)
(evil-define-key '(normal motion) Info-mode-map (kbd "M-k") 'Info-prev)
(evil-define-key '(normal motion) Info-mode-map (kbd "<backspace>") 'Info-last)

(setq woman-fill-frame 1)
(setq woman-fill-column 80)

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
  :general
  (:states '(normal motion)
   "SPC SPC" 'ace-jump-mode))

;; =======================================================================
;; TELEPHONE LINE
;; =======================================================================
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

;; (setq major-mode-remap-alist
;;       '((rust-mode . rust-ts-mode)))

;; (setq treesit-language-source-alist
;;       '((rust "https://github.com/tree-sitter/tree-sitter-rust")))

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
;; C++
;; =======================================================================
(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.H\\'" . c++-mode)
         ("\\.C\\'" . c++-mode))
  :general
  (:states 'visual
   :keymaps 'c++-mode-map
   "SPC c" 'clang-format-region)
  :config
  ;; Make _ not a word boundary
  (defun cpp-init-stuff ()
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table))
  (add-hook 'c++-mode-hook 'cpp-init-stuff)

  ;; C++ indentation
  ;; Don't indent within a namespace
  (setq my-cc-style
        '("cc-mode"
          (c-offsets-alist (innamespace . [0])
                           (case-label . +)
                           (arglist-close . 0))))
  (c-add-style "my-cc-style" my-cc-style)
  (add-hook 'c++-mode-hook (lambda () (c-set-style "my-cc-style")))

  ;; Fix all problems with modern C++ syntax highlighting
  (use-package modern-cpp-font-lock
    :ensure t)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;; =======================================================================
;; CLOJURE / LISP
;; =======================================================================
(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (clojure-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode))
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
    ("S" sp-split-sexp "split")
    ("t" sp-transpose-sexp "transpose")
    ("<" evil-cp-drag-backward "drag-back")
    (">" evil-cp-drag-forward "drag-fwd")
    ("M-a" evil-cp-insert-at-end-of-form "insert-end" :exit t)
    ("M-i" evil-cp-insert-at-beginning-of-form "insert-beginning" :exit t)
    ("M-o" evil-cp-open-below-form "open-below" :exit t)
    ("M-O" evil-cp-open-above-form "open-above" :exit t)
    ("M-k" evil-cp-beginning-of-defun "defun-begin")
    ("M-j" evil-cp-end-of-defun "defun-end"))

  (add-hook 'smartparens-enabled-hook (lambda ()
                                        (evil-define-key '(normal insert motion visual) 'local (kbd "C-e") 'hydra-smartparens/body)))

  (use-package evil-smartparens
    :ensure t
    :hook ((smartparens-enabled . evil-smartparens-mode))))

(use-package evil-cleverparens
  :ensure t
  :hook ((smartparens-enabled . evil-cleverparens-mode))
  :init (setq evil-cleverparens-use-additional-movement-keys nil
              evil-cleverparens-use-additional-bindings nil)
  :general
  (:keymaps 'evil-cleverparens-mode-map
   "M-a" 'evil-cp-insert-at-end-of-form
   "M-i" 'evil-cp-insert-at-beginning-of-form))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (cider-repl-mode . aggressive-indent-mode)))

(general-def 'emacs-lisp-mode-map
  "C-c e" 'eval-last-sexp
  "C-c d" 'eval-defun
  "C-c f" 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.edn\\'")
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (use-package cljr-helm :ensure t))

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode)
  :config
  (use-package helm-cider :ensure t :config (helm-cider-mode t))

  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-in-current-window t)

  ;; Find thing: C-c g
  (general-def cider-mode-map
    :prefix "C-c g"
    :prefix-command 'find-thing-cider-robert
    "d" 'cider-find-var
    "n" 'cider-find-ns
    "r" 'cider-find-resource
    "k" 'cider-find-keyword)

  ;; Eval C-c e
  (general-def cider-mode-map
    :prefix "C-c e"
    :prefix-command 'eval-thing-cider-robert
    "l" 'cider-eval-last-sexp
    "r" 'cider-eval-last-sexp-to-repl
    "p" 'cider-insert-last-sexp-in-repl
    "t" 'cider-eval-defun-at-point
    "a" 'cider-eval-sexp-at-point
    "d" 'cider-debug-defun-at-point)

  ;; Macros
  (general-def cider-mode-map
    :prefix "C-c m"
    :prefix-command 'macro-cider-robert
    "1" 'cider-macroexpand-1
    "a" 'cider-macroexpand-all)

  ;; Formatting
  (general-def cider-mode-map
    :prefix "C-c f"
    :prefix-command 'format-cider-robert
    "r" 'cider-format-region
    "f" 'cider-format-defun)

  ;; Namespace
  (general-def cider-mode-map
    :prefix "C-c n"
    "b" 'cider-browse-ns
    "n" 'cider-repl-set-ns
    "f" 'cider-ns-refresh)
  (define-key cider-mode-map (kbd "C-c C-n") 'cider-repl-set-ns) ;; Nice to not have to release Ctrl

  ;; Docs
  (general-def cider-doc-map
    "c" 'helm-cider-cheatsheet)
  (define-key cider-mode-map (kbd "C-c d") 'cider-doc-map)

  ;; Fix up REPL usage in Normal mode
  (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-backward-input)
  (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-forward-input)
  ;; We define RET -> cider-repl-return in cider-repl-mode-hooks for evil reasons

  ;; Refactoring
  (define-key clojure-mode-map (kbd "C-c C-r") 'cljr-helm)

  ;; Repl hooks
  (defun my-cider-repl-mode-stuff ()
    (smartparens-strict-mode t)
    (evil-define-key '(normal motion) 'local (kbd "<up>") 'cider-repl-backward-input)
    (evil-define-key '(normal motion) 'local (kbd "<down>") 'cider-repl-forward-input)
    (evil-define-key '(normal motion) 'local (kbd "RET") 'cider-repl-return))

  (add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-stuff))

;; =======================================================================
;; RUST
;; =======================================================================
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; =======================================================================
;; PROLOG
;; =======================================================================
(use-package prolog
  :ensure t
  :mode ("\\.pl\\'" . prolog-mode))

;; =======================================================================
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; KEYBINDINGS
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; =======================================================================
;; Misc builtin stuff
(general-def '(normal motion)
  "C-x f" 'find-name-dired
  "C-x p" 'list-processes
  "SPC x d" 'xref-find-definitions
  "SPC x r" 'xref-find-references
  ",m" 'compile
  "C-;" 'recompile)

;; Frames
(general-def '(normal motion)
  :prefix "SPC f"
  :prefix-command 'frames-robert-prefix
  "f" 'make-frame
  "x" 'delete-frame)

;; Windows
(general-def '(normal insert motion visual emacs) 'override
  "C-j" 'windmove-down
  "C-k" 'windmove-up
  "C-h" 'windmove-left
  "C-l" 'windmove-right)
(general-def '(normal motion)
  ",s" 'robert-split-horizontally-and-move-to-window
  ",v" 'robert-split-vertically-and-move-to-window
  "C-w" 'hydra-window/body
  "C-x C-k" 'robert-kill-this-buffer-and-close-window)

;; Scrolling
(general-def '(normal motion)
  "C-d" 'evil-scroll-down
  "C-u" 'evil-scroll-up)

;; Text movement/editing
(general-def '(normal motion)
  "0" 'evil-first-non-blank
  "'" 'evil-repeat-find-char-reverse
  ",w" 'save-buffer
  ",c" 'comment-or-uncomment-region
  ",h" 'robert-open-header-in-new-window
  "SPC h" 'ff-find-other-file)
(general-def 'insert
  "C-w" 'evil-delete-backward-word)

;; Text search
;; (evil-define-key 'normal 'global (kbd "SPC /") 'helm-swoop)

;; Ctags
;; (evil-define-key '(normal motion) 'global (kbd "SPC x d") 'xref-find-definitions)
;; (evil-define-key '(normal motion) 'global (kbd "SPC x r") 'xref-find-references)

;; Clang format
;; (evil-define-key 'visual 'c++-mode-map (kbd "SPC c") 'clang-format-region)
;; (evil-define-key 'normal 'c++-mode-map (kbd "SPC c") 'clang-format-region-at-point)

;; Other helm bindings
;; (evil-define-key '(normal motion) 'global (kbd "C-x k") 'helm-show-kill-ring)
;; (evil-define-key '(normal motion) 'global (kbd "SPC r") 'helm-resume)
;; (evil-define-key '(normal motion) 'global (kbd "SPC i") 'helm-imenu)

;; Vterm
;; (evil-define-key '(normal motion) 'global (kbd "SPC v") 'multi-vterm)
;; (evil-define-key '(normal motion) 'global (kbd "SPC C-v") 'robert-vterm-horizontally-in-new-window)
;; (evil-define-key '(normal motion) 'global (kbd "SPC ,") 'multi-vterm-rename-buffer)

;; Processes
;; (evil-define-key '(normal motion) 'global (kbd "C-x p") 'list-processes)

;; Smartparens
;; (evil-define-key  '(normal motion visual insert)  'global (kbd "C-e") 'hydra-smartparens/body)

;; Compilation
;; (evil-define-key '(normal motion) 'global (kbd ",m") 'compile)
;; (evil-define-key '(normal motion) 'global (kbd "C-;") 'recompile)

;; Documentation
;; (evil-define-key '(normal motion) 'global (kbd "SPC m") 'helm-man-woman)

;; Source Code Movement
;; (evil-define-key '(normal motion) 'global (kbd ",h") 'robert-open-header-in-new-window)
;; (evil-define-key '(normal motion) 'global (kbd "SPC h") 'ff-find-other-file)

;; =======================================================================
;; Generated!
;; =======================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" default))
 '(package-selected-packages
   '(helm-lsp flycheck-rust flycheck cargo evil-cleverparens general doom-themes nord-theme lsp-ui lsp-mode undo-tree gruvbox-theme darcula-theme multi-vterm vterm cider ace-jump helm-cider-history helm-cider aggressive-indent hydra rainbow-delimiters evil-paredit clojure-mode helm-ag ag csv-mode evil-magit clang-format yasnippet modern-cpp-font-lock irony helm use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))
