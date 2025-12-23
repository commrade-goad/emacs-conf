;; NOTE: Might install corfu but setup it so slow it didnt bother the perf
;;       Might install multiple-cursor and disable evil-mode somewhere in the future
;;       install: https://github.com/renzmann/treesit-auto
;;       BUILTIN way to get grammar: M-x treesit-install-language-grammar

;; Major mode remap
(add-to-list 'major-mode-remap-alist '(html-mode . mhtml-mode))

;; set ansi color support on the compile command buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Bootstrap straight.el
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file t))

;; Use straight.el for package management
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Disable menu, tool, and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq make-backup-files nil) ;; ~
(setq auto-save-default nil) ;; #
(setq create-lockfiles nil) ;; #

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

;; Line numbers and font
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(add-to-list 'default-frame-alist `(font . "IosevkaTerm Nerd Font Mono 14"))

(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t))

;; install vterm for better terminal
(use-package vterm
  :defer t
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 5000))

(add-to-list 'load-path "~/.config/emacs/simpc-mode")
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; editorconfig suppor pretty nice to have
;; (use-package editorconfig
;;   :ensure t
;;   :config
;;   (editorconfig-mode 1))

;; markdown mode
(use-package markdown-mode
  :defer t
  :straight t)

(add-hook 'go-ts-mode-hook (lambda ()
  (setq go-ts-mode-indent-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
))

;; LSP
(setq eglot-send-changes-idle-time 1)
(setq eglot-ignored-server-capabilities '(:inlayHintProvider
                                          :hoverProvider
                                          :signatureHelpProvider
                                          :documentHighlightProvider
                                          :codeLensProvider
                                          :documentFormattingProvider
                                          :documentRangeFormattingProvider
                                          :documentOnTypeFormattingProvider
                                          :executeCommandProvider))

(use-package eglot
  :straight (:type built-in)
  :config
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  )

;; the autocomplete
(setq cape-dabbrev-min-length 3)
(setq cape-dabbrev-limit 10)
(setq dabbrev-other-buffers t)
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point
                                #'cape-dabbrev
                                #'cape-file)))))

;; setting some fancy stuff here
(setq ido-everywhere t)
(setq ido-show-dot-for-dired t)
(ido-mode 1)

(use-package smex
  :defer t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; Magit
(use-package magit
  :defer t
  :straight t
  :init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Evil mode and Evil Collection with vanilla emacs on insert mode  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-disable-insert-state-bindings t)
(setq evil-want-C-u-scroll t)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq evil-shift-width 4)
(evil-set-leader 'normal (kbd "SPC"))

(evil-define-key 'visual 'prog-mode-map (kbd "g c") 'comment-line)
(evil-define-key 'normal 'prog-mode-map (kbd "g c") 'comment-line)
(evil-define-key 'normal 'global (kbd "<leader> c C") 'compile)
(evil-define-key 'normal 'global (kbd "<leader> c c") 'project-compile)
(evil-define-key 'normal 'global (kbd "<leader> b s") 'list-buffers)
(evil-define-key 'normal 'global (kbd "<leader> f f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader> f F") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader> f s") 'project-find-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Default vanilla emacs experience not bad but its f up my vim motion muscle memory  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package god-mode
;;   :straight t
;;   :config
;;   (global-set-key (kbd "C-c g") #'god-mode-all)
;;   (define-key god-local-mode-map "?" #'isearch-backward)
;;   (define-key god-local-mode-map "/" #'isearch-forward)
;;   )
;; (global-set-key (kbd "C-n") #'backward-char)
;; (global-set-key (kbd "C-e") #'next-line)
;; (global-set-key (kbd "C-i") #'previous-line)
;; (global-set-key (kbd "C-o") #'forward-char)
;; (global-set-key (kbd "C-;") #'move-end-of-line)

;; split something
(setq split-width-threshold nil)

;; Temp buffer (really usefull)
(defun create-temp-buffer ()
  (interactive)
  (switch-to-buffer "*temp*"))

(custom-set-variables
 '(custom-safe-themes
   '("2edc6777b0076ed4d6c7197e3cbaacc738ca7d6b5538e502ebe512365bcf54e2"
     "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "5cf12a54172956d44e1e44495cea9705468489e8b569a1d1ad301c2bca8a5503"
     default))
 '(inhibit-startup-screen t))
