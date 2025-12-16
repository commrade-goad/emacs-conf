;; ENV
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setq exec-path (append exec-path '("/usr/bin")))

;; Set gc
(setq gc-cons-threshold 100000000)

;; issue on x11 window resize fix
(setq frame-resize-pixelwise t)    ;; Prevent rounding issues
(setq frame-inhibit-implied-resize t)  ;; STOP automatic frame resizing

;; HTML will always use the new mhtml
(fset 'html-mode 'mhtml-mode)

;; set ansi color support on the compile command buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; for the whole project thing
(require 'project)

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
(ido-mode 1)
(ido-everywhere 1)
(indent-tabs-mode 0)

(setq make-backup-files nil) ;; ~
(setq auto-save-default nil) ;; #
(setq create-lockfiles nil) ;; #

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

;; Line numbers and font
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(set-frame-font "IosevkaTerm Nerd Font Mono 14" nil t)
;; (add-to-list 'default-frame-alist `(font . "Iosevka Nerd Font Mono-14"))

;; Use Smex stuff
(use-package smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; install the theme
(use-package naysayer-theme
  :straight t
  :config
  (load-theme 'naysayer t))

;; (use-package gruber-darker-theme
;;   :straight t
;;   :config
;;   (load-theme 'gruber-darker t))

;; install vterm for better terminal
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 5000))

;; install the simpc mode because the buildin mode is heavy for no reason at all
(add-to-list 'load-path "/home/goad/.config/emacs/simpc-mode")
;; Importing simpc-mode
(require 'simpc-mode)
;; Automatically enabling simpc-mode on files with extensions like .h, .c, .cpp, .hpp
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))


;; editorconfig suppor pretty nice to have
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; markdown mode
(use-package markdown-mode
  :straight t
  :init
  )

;; LSP
(setq c-default-style "stroustrup")
(setq c-basic-indent 4)
(setq c-basic-offset 4)

(setq eglot-send-changes-idle-time 0.5)
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
  ;; :config
  ;; (add-hook 'c-mode-hook 'eglot-ensure)
  ;; (add-hook 'c-ts-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  )

;; Autocompletion UI
(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :config
  ;; Enable auto-popup
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1)

  ;; Show documentation/tooltips
  (corfu-popupinfo-mode))

;; for the open buffer autocompletion
(use-package dabbrev)

;; Magit
(use-package magit :straight t :init)

;; Install drag-stuff package
(use-package drag-stuff
  :straight t
  :config
  (drag-stuff-global-mode 1))

;; Evil mode and Evil Collection
(setq evil-want-C-u-scroll t)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)
(setq evil-normal-state-cursor 'box
      evil-insert-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-replace-state-cursor 'box
      evil-operator-state-cursor 'box
      evil-motion-state-cursor 'box
      evil-emacs-state-cursor 'box)


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

(setq evil-undo-system 'undo-redo)
(setq evil-shift-width 4)
(evil-set-leader 'normal (kbd "SPC"))

(evil-define-key 'visual 'global (kbd "g c") 'comment-line)
(evil-define-key 'normal 'global (kbd "g c") 'comment-line)
(evil-define-key 'normal 'global (kbd "<leader> c C") 'compile)
(evil-define-key 'normal 'global (kbd "<leader> c c") 'project-compile)
(evil-define-key 'normal 'global (kbd "<leader> b s") 'list-buffers)
(evil-define-key 'normal 'global (kbd "<leader> d l") 'duplicate-line)
(evil-define-key 'normal 'global (kbd "<leader> f f") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader> f F") 'project-find-file)
(evil-define-key 'normal 'global (kbd "<leader> f s") 'project-find-regexp)

(evil-define-key 'normal 'global (kbd "C-j") 'drag-stuff-down)
(evil-define-key 'normal 'global (kbd "C-k") 'drag-stuff-up)
(evil-define-key 'visual 'global (kbd "C-j") 'drag-stuff-down)
(evil-define-key 'visual 'global (kbd "C-k") 'drag-stuff-up)
(evil-define-key 'insert 'global (kbd "C-n") 'dabbrev-completion)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; split something
(setq split-width-threshold nil)

;; Automatically clean trailing whitespace when saving files
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Custom set variables and faces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "5cf12a54172956d44e1e44495cea9705468489e8b569a1d1ad301c2bca8a5503"
     default))
 '(inhibit-startup-screen t)
 '(package-selected-packages '(evil evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
