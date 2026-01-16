;; -*- lexical-binding: t; -*-

;; change to use meow!
;; multiple-cursor and disable evil-mode somewhere in the future and add undo redo plugin
;; install: https://github.com/renzmann/treesit-auto
;; BUILTIN way to get grammar: M-x treesit-install-language-grammar

;; Garbage settings
(setq gc-cons-threshold most-positive-fixnum)
;; (setq gc-cons-threshold (* 1024 1024 1024)) ;; before value 'most-positive-fixnum'
(setq gc-cons-percentage 0.6)
(run-with-idle-timer
 8 t
 (lambda ()
   (garbage-collect)))

;; Major mode remap
(add-to-list 'major-mode-remap-alist '(html-mode . mhtml-mode))

;; disable bell
(setq ring-bell-function 'ignore)

;; disable char hl on diff
(setq diff-refine nil)

;; stuff to improve performance
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq-default display-line-numbers-width 2)
(setq treesit-font-lock-level 2)

;; set ansi color support on the compile command buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Setup package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
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
;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(add-to-list 'default-frame-alist `(font . "Maple Mono NL 13"))
;; (add-to-list 'default-frame-alist `(font . "IosevkaTerm Nerd Font Mono 14"))

;; == THEME ==

(use-package gruber-darker-theme)
(load-theme 'gruber-darker t)

;; (use-package naysayer-theme)
;; (load-theme 'naysayer t)

;; (add-to-list 'custom-theme-load-path (expand-file-name "fuuted/" user-emacs-directory))
;; (load-theme 'fuuted t)

;; ===========

;; install vterm for better terminal
(setq vterm-max-scrollback 5000)
(use-package vterm
  :defer t
  :ensure t)

;; (add-to-list 'load-path (expand-file-name "simpc-mode/" user-emacs-directory))
;; (require 'simpc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . c-or-c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go" . go-ts-mode))

;; markdown mode
(use-package markdown-mode
  :defer t)

(add-hook 'go-ts-mode-hook (lambda ()
  (setq go-ts-mode-indent-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
))

;; LSP
(setq read-process-output-max (* 1024 1024))
(fset #'jsonrpc--log-event #'ignore)
(setq eglot-events-buffer-config '(:size 0 :format short))

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
  :defer t)
(add-hook 'go-ts-mode-hook 'eglot-ensure)

;; the autocomplete
(setq cape-dabbrev-min-length 2) ;; 3
(setq cape-dabbrev-limit 5)
(setq dabbrev-other-buffers t)
(use-package cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point
                                #'cape-dabbrev
                                #'cape-file)))))

;; company for the autocompletion ui
(use-package company
  :defer t
  :init
  (global-company-mode 1)
  :config
  (setq company-minimum-prefix-length 2 ;; 3
        company-idle-delay 0.2
        company-tooltip-limit 5
        )
  )

(with-eval-after-load 'company
  (setq company-backends (delete 'company-clang company-backends)))

;; setting some fancy stuff here
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
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
  :init)

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
(evil-define-key 'normal 'global (kbd "<leader> f s") 'project-find-regexp)

;; split something
(setq split-width-threshold nil)

;; Temp buffer (really usefull)
(defun create-temp-buffer ()
  (interactive)
  (switch-to-buffer "*temp*"))

;; some dired nice config
(setq dired-dwim-target t)
;; (setq dired-kill-when-opening-new-dired-buffer t)
(setq wdired-allow-to-change-permissions t)

(use-package nov
  :defer t)

;; (defun my/reload-fuuted-theme ()
;;   "Fully reload the fuuted theme from disk."
;;   (interactive)
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (when (featurep 'fuuted-theme)
;;     (unload-feature 'fuuted-theme t))

;;   (add-to-list 'custom-theme-load-path (expand-file-name "fuuted/" user-emacs-directory))
;;   (load-theme 'fuuted t))

(keymap-set global-map "C-c f" #'find-file)
(keymap-set global-map "C-c b" #'switch-to-buffer)
(keymap-set global-map "C-c c" #'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BELOW AUTO-GENERATED BY EMACS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(custom-safe-themes
   '("2edc6777b0076ed4d6c7197e3cbaacc738ca7d6b5538e502ebe512365bcf54e2"
     "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "5cf12a54172956d44e1e44495cea9705468489e8b569a1d1ad301c2bca8a5503"
     default))
 '(inhibit-startup-screen t)
 '(package-selected-packages nil))
