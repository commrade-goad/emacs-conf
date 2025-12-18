(deftheme fuuted
  "A heavily muted, low-saturation version of Gruber Darker covering all standard components.")

(let ((bg0      "#161a19") ; Primary dark gray-teal
      (bg-1     "#0e1110") ; Deeper background
      (bg+1     "#2d3331") ; Mid-dark
      (bg+2     "#434d4a") ; Lighter background (popups)
      (bg+3     "#484848") ; Secondary selection
      (bg+4     "#52494e") ; Tooltip/Match
      (fg       "#b8aaa7") ; Muted off-white
      (fg+1     "#d1c1be") ; Lighter muted pinkish-white

      ;; Muted Accent Palette
      (grey     "#737d90") ; Dusty blue-grey
      (sage     "#5a6853") ; Muted green
      (teal     "#637c73") ; Muted teal
      (rose     "#b994a1") ; Dusty rose
      (lavender "#9983a5") ; Muted purple
      (clay     "#726a5b") ; Muted brown/clay
      (gold     "#887c6c") ; Dusty gold

      ;; Muted Semantic Colors
      (muted-red    "#a3686a")
      (muted-orange "#b38d6b")
      (dark-green   "#3e4d39")
      )

  (custom-theme-set-variables
   'fuuted
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'fuuted

   ;; --- Basic UI ---
   `(default ((t (:foreground ,fg :background ,bg0))))
   `(cursor ((t (:background ,gold))))
   `(region ((t (:background ,bg+1 :foreground nil))))
   `(secondary-selection ((t (:background ,bg+3))))
   `(mode-line ((t (:background ,bg+1 :foreground ,fg+1))))
   `(mode-line-inactive ((t (:background ,bg0 :foreground ,grey))))
   `(fringe ((t (:background ,bg0 :foreground ,bg+1))))
   `(vertical-border ((t (:foreground ,bg+1))))
   `(minibuffer-prompt ((t (:foreground ,teal :weight bold))))
   `(link ((t (:foreground ,grey :underline t))))
   `(link-visited ((t (:foreground ,lavender :underline t))))
   `(tooltip ((t (:background ,bg+4 :foreground ,fg+1))))
   `(shadow ((t (:foreground ,bg+2))))

   ;; --- Syntax Highlighting (Font Lock) ---
   `(font-lock-builtin-face ((t (:foreground ,muted-orange))))
   `(font-lock-comment-face ((t (:foreground ,clay :italic t))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,clay))))
   `(font-lock-constant-face ((t (:foreground ,lavender))))
   `(font-lock-doc-face ((t (:foreground ,dark-green))))
   `(font-lock-doc-string-face ((t (:foreground ,dark-green))))
   `(font-lock-function-name-face ((t (:foreground ,grey))))
   `(font-lock-keyword-face ((t (:foreground ,gold :bold t))))
   `(font-lock-string-face ((t (:foreground ,sage))))
   `(font-lock-type-face ((t (:foreground ,teal))))
   `(font-lock-variable-name-face ((t (:foreground ,fg+1))))
   `(font-lock-warning-face ((t (:foreground ,muted-red :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,clay))))

   ;; --- Whitespace ---
   `(whitespace-trailing ((t (:background ,bg+1 :foreground ,bg+2))))
   `(whitespace-space ((t (:background ,bg0 :foreground ,bg+1))))
   `(whitespace-tab ((t (:background ,bg0 :foreground ,bg+1))))
   `(whitespace-newline ((t (:background ,bg0 :foreground ,bg+1))))
   `(whitespace-line ((t (:background ,bg+1 :foreground ,muted-red))))
   `(whitespace-empty ((t (:background ,bg+1 :foreground ,bg+1))))
   `(whitespace-indentation ((t (:background ,bg0 :foreground ,bg+1))))
   `(whitespace-space-after-tab ((t (:background ,bg0 :foreground ,bg+1))))
   `(whitespace-space-before-tab ((t (:background ,bg0 :foreground ,bg+1))))
   `(trailing-whitespace ((t (:background ,bg+1))))

   ;; --- Parenthesis ---
   `(show-paren-match ((t (:background ,bg+2 :foreground ,gold :weight bold))))
   `(show-paren-mismatch ((t (:background ,muted-red :foreground ,fg+1))))

   ;; --- Corfu / Company (Completion) ---
   `(corfu-default ((t (:background ,bg+1 :foreground ,fg))))
   `(corfu-current ((t (:background ,bg+2 :foreground ,fg+1 :weight bold))))
   `(corfu-bar ((t (:background ,grey))))
   `(corfu-border ((t (:background ,bg+2))))
   `(company-tooltip ((t (:background ,bg+1 :foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg+2 :foreground ,fg+1))))
   `(company-tooltip-common ((t (:foreground ,gold))))

   ;; --- Magit ---
   `(magit-branch ((t (:foreground ,grey))))
   `(magit-diff-hunk-header ((t (:background ,bg+1))))
   `(magit-diff-file-header ((t (:background ,bg+2))))
   `(magit-log-sha1 ((t (:foreground ,muted-red))))
   `(magit-log-author ((t (:foreground ,clay))))
   `(magit-item-highlight ((t (:background ,bg+1))))

   ;; --- Org Mode ---
   `(org-level-1 ((t (:foreground ,grey :weight bold))))
   `(org-level-2 ((t (:foreground ,teal :weight bold))))
   `(org-level-3 ((t (:foreground ,lavender :weight bold))))
   `(org-todo ((t (:foreground ,muted-red :weight bold))))
   `(org-done ((t (:foreground ,sage :weight bold))))
   `(org-column ((t (:background ,bg-1))))

   ;; --- Compilation ---
   `(compilation-info ((t (:foreground ,sage))))
   `(compilation-warning ((t (:foreground ,gold :weight bold))))
   `(compilation-error ((t (:foreground ,muted-red))))

   ;; --- Agda2 & AUCTeX ---
   `(agda2-highlight-datatype-face ((t (:foreground ,teal))))
   `(agda2-highlight-function-face ((t (:foreground ,grey))))
   `(agda2-highlight-keyword-face ((t (:foreground ,gold :bold t))))
   `(font-latex-bold-face ((t (:foreground ,fg :bold t))))
   `(font-latex-italic-face ((t (:foreground ,fg :italic t))))
   `(font-latex-string-face ((t (:foreground ,sage))))

   ;; --- Search ---
   `(isearch ((t (:foreground ,bg0 :background ,gold))))
   `(isearch-fail ((t (:foreground ,fg :background ,muted-red))))
   `(lazy-highlight ((t (:background ,bg+2))))
   `(match ((t (:background ,bg+4))))

   ;; --- Terminal (vterm/ansi) ---
   `(term-color-black   ((t (:foreground ,bg+2 :background ,bg+3))))
   `(term-color-red     ((t (:foreground ,muted-red :background ,muted-red))))
   `(term-color-green   ((t (:foreground ,sage :background ,sage))))
   `(term-color-yellow  ((t (:foreground ,gold :background ,gold))))
   `(term-color-blue    ((t (:foreground ,grey :background ,grey))))
   `(term-color-magenta ((t (:foreground ,lavender :background ,lavender))))
   `(term-color-cyan    ((t (:foreground ,teal :background ,teal))))
   `(term-color-white   ((t (:foreground ,fg :background ,fg+1))))

   ;; --- Misc ---
   `(line-number ((t (:inherit default :foreground ,bg+2 :background ,bg0 :weight normal :slant normal :underline nil :strike-through nil))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,gold :background ,bg0 :weight bold))))
   `(header-line ((t (:background ,bg+1 :foreground ,fg))))
   `(tab-bar ((t (:background ,bg-1 :foreground ,grey))))
   `(tab-bar-tab ((t (:background ,bg0 :foreground ,gold :weight bold))))
   ))

(provide-theme 'fuuted)
