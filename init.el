(setq diff-refine nil)                  ;; disable char hl on diff
(setq column-number-mode t)             ;; for the line number and stuff
(setq treesit-font-lock-level 2)        ;; less syntax hl
(setq make-backup-files nil)            ;; ~
(setq auto-save-default nil)            ;; #
(setq create-lockfiles nil)             ;; #
(setq split-width-threshold nil)
(setq dired-dwim-target t)
;; (setq display-line-numbers-type 'relative)
(setq frame-resize-pixelwise t)
(setq isearch-lazy-count t)                    ;; to enable countin on isearch
(setq editorconfig-get-properties-function
      #'editorconfig-core-get-properties-hash) ;; use hash table for the editorconfig properties
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)
(savehist-mode 1)

(setq-default c-ts-mode-indent-offset 4
              c-ts-mode-indent-style 'gnu
              tab-width 4
              indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

(keymap-set global-map "C-c f" #'find-file)
(keymap-set global-map "C-c b" #'switch-to-buffer)
(keymap-set global-map "C-c c" #'compile)

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (when compilation-filter-start
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun create-temp-buffer ()
 (interactive)
  (switch-to-buffer "*temp*"))

(defun my-ts-indent-4 ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)

  ;; language-specific overrides
  (when (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (setq-local c-ts-mode-indent-offset 4)))

(add-hook 'treesit-major-mode-hook #'my-ts-indent-4)

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local bidi-display-reordering nil
                        bidi-inhibit-bpa t
                        show-trailing-whitespace t)
            ;; (display-line-numbers-mode 1)
            (editorconfig-mode 1)))

;; (add-to-list 'default-frame-alist `(font . "Iosevka Nerd Font 14"))
(add-to-list 'default-frame-alist `(font . "Monospace 13"))

(add-to-list 'custom-theme-load-path (expand-file-name "fuuted/" user-emacs-directory))
(load-theme 'fuuted t)

(defun my/reload-fuuted-theme ()
  "Fully reload the fuuted theme from disk."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (when (featurep 'fuuted-theme)
    (unload-feature 'fuuted-theme t))
  (load-theme 'fuuted t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. PACKAGE MANAGER SETUP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;
;; 3. THE PACKAGE ;;
;;;;;;;;;;;;;;;;;;;;


(use-package xclip
  :init
  (xclip-mode 1)
  (setq x-select-enable-clipboard t))

(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-cycle t
        vertico-sort-function #'vertico-sort-history-alpha))

(setq vterm-max-scrollback 5000)
(use-package vterm
  :defer t
  :ensure t)

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (html-mode . mhtml-mode)
        ))
(add-to-list 'auto-mode-alist '("\\.go" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-ts-mode))

(add-hook 'go-ts-mode-hook (lambda ()
                             (setq tab-width 4)
                             (setq go-ts-mode-indent-offset 4)
                             (setq indent-tabs-mode t)))

(use-package markdown-mode
  :defer t)

(setq read-process-output-max (* 1024 1024))
(fset #'jsonrpc--log-event #'ignore)
(setq eglot-events-buffer-config '(:size 0 :format short))
(setq eglot-sync-connect nil)
(remove-hook 'jsonrpc-event-hook 'jsonrpc--log-event)

(setq eglot-ignored-server-capabilities '(:inlayHintProvider
                                          ;; :hoverProvider
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
(add-hook 'ts-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename))

(defun my/eglot-capf-augment ()
  (setq-local completion-at-point-functions
              (cons #'eglot-completion-at-point
                    completion-at-point-functions)))
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf-augment)

(setq cape-dabbrev-min-length 3)
(setq cape-dabbrev-limit 5)
(setq dabbrev-other-buffers t)

(use-package cape
  :defer t)

(defun my/base-capf-setup ()
  (setq-local completion-at-point-functions
              (append completion-at-point-functions
                      (list #'cape-dabbrev
                            #'cape-file))))
(add-hook 'prog-mode-hook #'my/base-capf-setup)
(add-hook 'text-mode-hook #'my/base-capf-setup)

(use-package corfu
  :defer t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'first)
  :hook
  ((prog-mode . corfu-mode)
   (text-mode . corfu-mode))
  )

(use-package magit
  :defer t
  :init)

(setq evil-default-cursor t)
(setq evil-normal-state-cursor 'box
      evil-insert-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-replace-state-cursor 'box
      evil-operator-state-cursor 'box)

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
  :init
  (setq evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))


(setq evil-shift-width 4)
(evil-set-leader 'normal (kbd "SPC"))

(evil-define-key 'normal 'global (kbd "g c") 'comment-line)
(evil-define-key 'visual 'global (kbd "g c") 'comment-line)
(evil-define-key 'normal 'global (kbd "[ d") #'flymake-goto-prev-error)
(evil-define-key 'normal 'global (kbd "] d") #'flymake-goto-next-error)
(evil-define-key 'normal 'global (kbd "g l") #'flymake-show-buffer-diagnostics)

;; removed:  ido, smex
;; added:    vertico
;; not-used: consult(modern smex), oderless kinda not helpfull at all (maybe little helpful for buffer switch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BELOW AUTO-GENERATED BY EMACS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
