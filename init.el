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
;; C-s Tab hydra (can be replaced)
;;
;; Available:
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

(defun kill-this-buffer-and-close-window ()
  (interactive)
  (kill-this-buffer)
  (evil-tab-sensitive-quit))

(defun vterm-horizontally-in-new-window ()
  (interactive)
  (split-horizontally-and-move-to-window)
  (multi-vterm))

(defun vterm-move-to-start-of-prompt ()
  (interactive)
  (evil-first-non-blank)
  (evil-find-char 1 ?$)
  (evil-forward-char))

(defun accept-search-and-center-screen ()
  (interactive)
  (evil-scroll-line-to-center (line-number-at-pos))
  (isearch-exit))

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
    ;;(evil-collection-init 'vterm)
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
    (define-key global-map (kbd "C-z") nil)

    ;; Tab movement with alt-{h,l}
    (evil-define-key '(normal motion insert) 'global (kbd "M-h") 'elscreen-previous)
    (evil-define-key '(normal motion insert) 'global (kbd "M-l") 'evil-tabs-goto-tab))

  ;;Change how evil states are displayed in the mode line
  (setq evil-normal-state-tag "NORMAL")
  (setq evil-motion-state-tag "MOTION")
  (setq evil-insert-state-tag "INSERT")
  (setq evil-visual-state-tag "VISUAL")
  (setq evil-emacs-state-tag "EMACS")

  (defun evil-tab-sensitive-quit ()
    (interactive)
    (require 'evil)
    (if (> (length (elscreen-get-screen-list)) 1)
        (if (> (count-windows) 1)
            (evil-quit)
          (elscreen-kill))
      (evil-quit)))

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
;; WINDOW / TEXT MOVEMENT
;; =======================================================================
;; Window movement
(evil-define-key '(normal motion) 'global "\C-l" 'windmove-right)
(evil-define-key '(normal motion) 'global "\C-k" 'windmove-up)
(evil-define-key '(normal motion) 'global "\C-j" 'windmove-down)
(evil-define-key '(normal motion) 'global "\C-h" 'windmove-left)
(evil-define-key '(normal motion insert) 'global (kbd "C-x w") 'delete-other-windows)

;; Scrolling
(evil-define-key '(normal motion) 'global "\C-d" 'evil-scroll-down)
(evil-define-key '(normal motion) 'global "\C-u" 'evil-scroll-up)
(evil-define-key '(normal motion) 'global (kbd "0") 'evil-first-non-blank)
(evil-define-key 'insert 'global "\C-w" 'evil-delete-backward-word)

;; Text movement/editing
(evil-define-key '(normal motion) 'global (kbd "0") 'evil-first-non-blank)
(evil-define-key 'insert 'global (kbd "C-w") 'evil-delete-backward-word)
(evil-define-key '(normal motion) 'global (kbd "'") 'evil-repeat-find-char-reverse)

;; =======================================================================
;; MISC KEYBINDINGS
;; =======================================================================
;; We use SPC as a prefix for commonly used commands. Set to nil
;; here so we can bind it as a prefix key
(evil-define-key '(normal motion) 'global (kbd "SPC") nil)

;; Rebind describe key
(define-key global-map (kbd "C-x k") 'describe-key)

;; Rebind universal arg
(define-key global-map (kbd "M-u") 'universal-argument)

;; Kill buffer with processes open
(define-key global-map (kbd "C-x C-k") 'kill-this-buffer-and-close-window)

;; Find in directory
(define-key global-map (kbd "C-x f") 'find-name-dired)

;; Make/delete frames
(evil-define-key '(normal motion) 'global (kbd "SPC f f") 'make-frame)
(evil-define-key '(normal motion) 'global (kbd "SPC f x") 'delete-frame)

;; Xref
(evil-define-key '(normal motion) 'global (kbd "C-t") 'xref-find-references)

;; Recenter screen after search
;; Note: Since evil uses isearch and isearch uses overriding-terminal-local-map
;; we should set this keybinding without evil-define-key to make it work.
;; It will show up in the overriding-terminal-local-map this way.
(define-key isearch-mode-map (kbd "<return>") 'accept-search-and-center-screen)

;; Make it so that hitting Shift-Backspace doesn't open help menu
(define-key global-map "\C-h" 'delete-backward-char)

;; =======================================================================
;; STANDARD EMACS CONFIGS
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
(define-key global-map (kbd "C-x m") 'compile)
(define-key compilation-mode-map (kbd "g") nil)
(define-key compilation-mode-map (kbd "r") 'recompile)
(define-key compilation-mode-map (kbd "n") 'evil-search-next)
(define-key compilation-mode-map (kbd "N") 'evil-search-previous)

;; Always display certain buffers in a new window
(setq display-buffer-alist '(("\\*cider-error\\*"
                              (display-buffer-reuse-window display-buffer-pop-up-window)
                              ())))
(setq split-width-threshold 1)
(setq split-height-threshold 1)

;; Grep ignore directories
(require 'grep)
(add-to-list 'grep-find-ignored-directories "doxygen")

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

  ;; Helm find file
  (evil-define-key '(normal motion) 'global (kbd "C-f") 'helm-find-files)
  (evil-define-key '(normal motion) 'global (kbd "C-x l") 'helm-locate)
  (evil-define-key '(normal motion) 'global (kbd "C-b") 'helm-mini)

  ;; Make find-file behave more like terminal bindings
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)

  ;; Dont start the helm search at the file under point
  (setq helm-find-files-ignore-thing-at-point t)

  ;; Other helm bindings
  (evil-define-key '(normal motion) 'global (kbd "C-x b") 'helm-apropos)
  (evil-define-key '(insert normal motion) 'global "\M-x" 'helm-M-x)
  (evil-define-key '(normal motion) 'global (kbd "C-x C-r") 'helm-resume)
  (evil-define-key '(normal motion) 'global (kbd "C-x a") 'helm-do-grep-ag)
  ;;(evil-define-key '(normal motion insert) 'global ("C-x c") 'helm-show-kill-ring)

  ;; Helm for xref
  (use-package helm-xref
    :ensure t)

  ;; Helm search in a file
  (use-package helm-swoop
    :ensure t
    :config
    (evil-define-key 'normal 'global (kbd "SPC /") 'helm-swoop)

    (define-key helm-swoop-map (kbd "C-n") 'helm-next-line)
    (define-key helm-swoop-map (kbd "C-p") 'helm-previous-line)

    (setq helm-swoop-split-with-mutiple-windows t)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-use-fuzzy-match t)))

;; =======================================================================
;; VTerm
;; Needs --with-native-compilation in emacs build
;; =======================================================================
;; (use-package vterm
;;   :ensure t
;;   :config
;;   (defun kill-vterm-buffers ()
;;     (interactive)
;;     (kill-some-buffers (seq-filter (lambda (b)
;;                                      (string-match-p "^\\*vterm" (buffer-name b)))
;;                                    (buffer-list))))
;;   (evil-define-key '(normal motion) vterm-mode-map (kbd "0") 'vterm-move-to-start-of-prompt)
;;   (evil-define-key '(normal motion) 'global (kbd "SPC v") 'vterm-other-window)
;;   (evil-define-key '(normal motion) 'global (kbd "SPC M") 'multi-vterm)
;;   (evil-define-key '(normal motion) 'global (kbd "SPC m") 'vterm-horizontally-in-new-window)
;;   (evil-define-key '(normal motion) 'global (kbd "SPC ,") 'multi-vterm-rename-buffer))

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

;; =======================================================================
;; WHICH KEY
;; =======================================================================
(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; =======================================================================
;; ACE JUMP
;; =======================================================================
;; (use-package ace-jump-mode
;;   :ensure t
;;   :config
;;   (evil-define-key 'normal 'global (kbd "SPC SPC") 'ace-jump-mode)
;;   (evil-define-key 'normal 'global (kbd "SPC w") 'ace-jump-word-mode)
;;   (evil-define-key 'normal 'global (kbd "SPC c") 'ace-jump-char-mode)
;;   (evil-define-key 'normal 'global (kbd "SPC l") 'ace-jump-line-mode))

;; =======================================================================
;; YASNIPPET
;; =======================================================================
(use-package yasnippet
  :ensure t
  :config (use-package yasnippet-snippets :ensure t)
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-c &") nil)
  (evil-define-key 'insert 'global (kbd "M-y y") 'yas-expand)
  (evil-define-key 'insert 'global (kbd "M-y c") 'company-yasnippet)
  (evil-define-key 'insert 'global (kbd "M-y n") 'yas-next-field)
  (evil-define-key 'insert 'global (kbd "M-y p") 'yas-prev-field))

;; =======================================================================
;; MAGIT
;; =======================================================================
(use-package evil-magit
  :ensure t
  :config
  (evil-magit-init)
  (global-set-key (kbd "C-x g") 'magit-status))

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

  ;; Open dired at root by just typing D
  (evil-define-key 'normal 'global (kbd "D") 'projectile-dired)

  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)
    (setq projectile-completion-system 'helm))

  (evil-define-key 'normal 'global (kbd "SPC s") 'helm-projectile-ag)
  (evil-define-key 'normal 'global (kbd "SPC d") 'helm-projectile-find-file)
  (evil-define-key 'normal 'global (kbd "SPC t") 'projectile-find-tag))

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
  (setq clang-format-style-option "file")
  (evil-define-key 'visual 'c++-mode-map (kbd "SPC ff") 'clang-format-region)
  (evil-define-key 'normal 'c++-mode-map (kbd "SPC ff") 'flang-format-region-at-point))

;; =======================================================================
;; CLOJURE / LISP
;; =======================================================================
(use-package smartparens
  :ensure t
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "(" nil :unless '(sp-in-string-p))
  (sp-pair "\\(" nil :unless '(sp-in-string-p))

  (defhydra hydra-smartparens (smartparens-mode-map "C-e")
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
  (evil-define-key '(normal insert visual motion) 'global (kbd "C-e") 'hydra-smartparens/body)

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
    (let ((my-cljr-refactor-map)
          (my-cljr-refactor-rename-map)
          (my-cljr-refactor-let-map)
          (my-cljr-refactor-cycle-map)
          (my-cljr-refactor-add-map))
      (define-prefix-command 'my-cljr-refactor-map)
      (define-prefix-command 'my-cljr-refactor-rename-map)
      (define-prefix-command 'my-cljr-refactor-let-map)
      (define-prefix-command 'my-cljr-refactor-cycle-map)
      (define-prefix-command 'my-cljr-refactor-add-map)
      ;; Renaming refactorings C-c r r
      (define-key my-cljr-refactor-rename-map (kbd "f") 'cljr-rename-file-or-dir)
      (define-key my-cljr-refactor-rename-map (kbd "s") 'cljr-rename-symbol)
      (define-key my-cljr-refactor-map (kbd "r") 'my-cljr-refactor-rename-map)
      ;; Let refactorings C-c r l
      (define-key my-cljr-refactor-let-map (kbd "i") 'clojure-introduce-let)
      (define-key my-cljr-refactor-let-map (kbd "m") 'clojure-move-to-let)
      (define-key my-cljr-refactor-let-map (kbd "r") 'cljr-remove-let)
      (define-key my-cljr-refactor-let-map (kbd "e") 'cljr-expand-let)
      (define-key my-cljr-refactor-map (kbd "l") 'my-cljr-refactor-let-map)
      ;; Inline symbol C-c r i
      (define-key my-cljr-refactor-map (kbd "i") 'cljr-inline-symbol)
      ;; Cycle C-c r c
      (define-key my-cljr-refactor-cycle-map (kbd "p") 'clojure-cycle-privacy)
      (define-key my-cljr-refactor-cycle-map (kbd "n") 'clojure-cycle-not)
      (define-key my-cljr-refactor-cycle-map (kbd "i") 'clojure-cycle-if)
      (define-key my-cljr-refactor-map (kbd "c") 'my-cljr-refactor-cycle-map)
      ;; Add C-c r a
      (define-key my-cljr-refactor-add-map (kbd "r") 'cljr-add-require-to-ns)
      (define-key my-cljr-refactor-add-map (kbd "d") 'cljr-add-project-dependency)
      (define-key my-cljr-refactor-add-map (kbd "l") 'cljr-add-declaration)
      (define-key my-cljr-refactor-add-map (kbd "a") 'cljr-add-missing-libspec)
      (define-key my-cljr-refactor-map (kbd "a") 'my-cljr-refactor-add-map)

      (define-key clojure-mode-map (kbd "C-c r") 'my-cljr-refactor-map)))

  (defun my-cider-repl-mode-stuff ()
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
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-stuff))

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
   '(cider ace-jump helm-cider-history helm-cider aggressive-indent hydra rainbow-delimiters evil-paredit clojure-mode helm-ag ag csv-mode evil-magit clang-format yasnippet modern-cpp-font-lock irony helm use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
