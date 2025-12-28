;; mac: switch meta/hyper key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
;; mac: keep right alt outside of emacs, to type ü ä ...
(setq mac-right-option-modifier nil)

;; recent files mode
(recentf-mode 1)

;; bootstrap-install of straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; packages - all installed automatically on first loading
(straight-use-package 'avy)
(straight-use-package 'bm)
(straight-use-package 'cmake-mode)
(straight-use-package 'corfu)
(straight-use-package 'dape)
(straight-use-package 'direx)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'dumb-jump)
(straight-use-package 'expand-region)
(straight-use-package 'fill-column-indicator)
(straight-use-package 'go-mode)
(straight-use-package 'groovy-mode)
(straight-use-package 'haskell-mode)
(straight-use-package 'hi2)
(straight-use-package 'highlight-symbol)
(straight-use-package 'json-mode)
(straight-use-package 'julia-mode)
(straight-use-package 'julia-repl)
(straight-use-package 'julia-vterm)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-pyright)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'mc-extras)
(straight-use-package 'multiple-cursors)
(straight-use-package 'treemacs)
(straight-use-package 'nix-mode)
(straight-use-package 'ollama-buddy)
(straight-use-package 'paredit)
(straight-use-package 'projectile)
(straight-use-package 'protobuf-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'rg)
(straight-use-package 'rustic)
(straight-use-package 'sbt-mode)
(straight-use-package 'scala-mode)
(straight-use-package 'shader-mode)
(straight-use-package 'smex)
(straight-use-package 'switch-window)
(straight-use-package 'typescript-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'vue-mode)
(straight-use-package 'web-mode)
(straight-use-package 'wgrep)
(straight-use-package 'yaml-mode)
(straight-use-package 'yasnippet)

(straight-use-package
 '(copilot :type git
           :host github
           :repo "copilot-emacs/copilot.el"))

;;   - color themes
(straight-use-package 'ample-theme)
(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'doom-themes)
(straight-use-package 'flatland-theme)
(straight-use-package 'material-theme)



;; add ~/bin
(add-to-list 'load-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(add-to-list 'exec-path (expand-file-name "~/miniconda3/bin"))

;; enable corfu globally for autocompletion
(global-corfu-mode)

(defun lsp-or-dumb-jump ()
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-find-definition)
    (dumb-jump-go)))

(defun magit-diff-origin-master ()
  "Show a magit diff between the current HEAD and origin/master."
  (interactive)
  (magit-diff-range "origin/master...HEAD"))

;; VISUAL ASPECT
(setq inhibit-splash-screen t)                                  ;; no splash screen
(tool-bar-mode -1)                                              ;; no toolbar
(scroll-bar-mode -1)                                            ;; no scroll bar
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)                               ;; disable bell
(set-fringe-mode 0)                                             ;; (also here)
(show-paren-mode 1)                                             ;; show matching parents
(add-hook 'before-save-hook 'delete-trailing-whitespace)        ;; delete trailing whitespaces on save
(column-number-mode)                                            ;; display column number in command buf.
(add-hook 'prog-mode-hook 'subword-mode)                        ;; camel-case kill-word
(display-time-mode 1)
;;(wrap-region-global-mode)
(global-display-line-numbers-mode t)                                           ;; line numbering on
(setq linum-format "%d ")                                       ;; adjust line number column size
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'prog-mode-hook #'hs-minor-mode)                      ;; hs-minor-mode for all programming modes
(setq fill-column 100)                                          ;; for M-q (instead of default 70)

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-x C-j") 'projectile-find-file)          ;; find file in project
(setq projectile-indexing-method 'hybrid )
(setq projectile-globally-ignored-file-suffixes '("~" "pyc") )


(global-set-key (kbd "C-M-/")   'toggle-comment-region)         ;; comment code - custom function below
(global-set-key (kbd "C-M-j")   'treemacs)                      ;; tree view of project directory

(global-set-key (kbd "C-m")     'indent-new-comment-line)       ;; continue w/ comments
(global-set-key (kbd "M-k")     'kill-current-buffer)           ;; kill current buffer
(global-set-key (kbd "C-;")     'scroll-down-line)
(global-set-key (kbd "C-'")     'scroll-up-line)
(global-set-key (kbd "C-x o")   'switch-window)                 ;; for when there are more than 2 windows
(global-set-key (kbd "C-M-o")   'other-window)                  ;; the default cyclical switch, quicker
(global-set-key (kbd "C-=")     'er/expand-region)              ;; expand region
(global-set-key (kbd "C-.")     'lsp-or-dumb-jump)              ;; lsp jump to definition, or dumb jump
(global-set-key (kbd "C-,")     'lsp-find-references)
(global-set-key (kbd "C-c C-.") 'lsp-describe-thing-at-point)   ;; lsp show type of variable under cursor
(global-set-key (kbd "H-i")     'lsp-treemacs-symbols)          ;; using lsp, tree of all function defs etc. in buffer

(global-set-key (kbd "M-j")     'avy-goto-word-1)               ;; avy-jump (on word)
(global-set-key (kbd "C-M-y")   'toggle-truncate-lines)         ;; switch on-off word wrap
(global-set-key (kbd "C-M-t")   'hs-toggle-hiding)              ;; hide/show block
(global-set-key (kbd "M-x")     'smex)                          ;; nicer M-x
(global-set-key (kbd "C-z")     'nop)                           ;; do nothing (prevent minimize to dock)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)            ;; open recent file
(global-set-key (kbd "M-s")     'rg)                            ;; ripgrep search

(global-set-key (kbd "C-c C-b") 'rustic-cargo-build)            ;; cargo build
(global-set-key (kbd "C-c C-f") 'rustic-cargo-fmt)              ;; cargo format

(global-set-key (kbd "H-o")     'ollama-buddy-menu)             ;; ollama menu

;; bookmarks shortcuts
(global-set-key (kbd "H-b")     'bm-toggle)                     ;; toggle bookmark on current line/buffer
(global-set-key (kbd "H-n")     'bm-next)                       ;; jump to next bookmark
(global-set-key (kbd "H-p")     'bm-previous)                   ;; jump to previous bookmark
(setq bm-cycle-all-buffers t)  ;; allow jumping to bookmarks in other buffers

;; multiple cursors key bindings
(require 'cl)   ;; fix missing equalp symbol
(global-set-key (kbd "C->")             'mc/mark-next-like-this)
(global-set-key (kbd "C-<")             'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-l")           'mc/edit-lines)
(global-set-key (kbd "C-c C-<")         'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<mouse-1>")   'mc/add-cursor-on-click)
(global-set-key (kbd "C-c C-l")         'recompile)

(defun get-symbol-under-cursor ()
  "Select and return the symbol (e.g., variable name) under the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (let ((symbol (buffer-substring-no-properties (car bounds) (cdr bounds))))
          ;; Visually select it
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t)
          ;; Return the symbol
          (message "Symbol: %s" symbol)
          symbol)
      (message "No symbol under cursor.")
      nil)))

(defun insert-string-below (str)
  "Insert STR on a new line below the current line."
  (interactive "sInsert string below: ")
  (end-of-line)
  (newline-and-indent)
  (insert str))

(defun python-print-debugging-statement-for-symbol-under-cursor ()
  (interactive)
  (insert-string-below (format "print(f\"{%s=}\")" (get-symbol-under-cursor))))

(global-set-key (kbd "C-C p")     'python-print-debugging-statement-for-symbol-under-cursor) ;; print var debugging statement

;; custom comment toggle function
(defun toggle-comment-region ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; graphic-only keybindings
(if (display-graphic-p)
    (progn
      ;; themes
      (setq pretty-themes '(doom-palenight
			    doom-bluloco-dark
			    doom-henna
			    material
			    doom-nord
			    doom-opera
			    misterioso
			    flatland
			    whiteboard
			    doom-ayu-mirage
			    ;; doom-spacegrey
			    ;; doom-peacock
			    ;; doom-vibrant
			    ;; sanityinc-tomorrow-eighties
			    ;; ample
			    ))
      (setq all-themes (custom-available-themes))
      (defun cycle-pretty-themes ()
	(interactive)
	(load-theme (car pretty-themes) t)
	(setq linum-format "%3d ") ;; force line number column size
	(setq pretty-themes (append (cdr pretty-themes) (list (car pretty-themes)))))
      (defun cycle-all-themes ()
	(interactive)
	(load-theme (car all-themes) t)
	(message "loaded theme %s" (car all-themes))
	(setq linum-format "%3d ") ;; force line number column size
	(setq all-themes (append (cdr all-themes) (list (car all-themes)))))
      (defadvice load-theme
	  (before theme-dont-propagate activate)
	(progn
	  (mapc #'disable-theme custom-enabled-themes)))
      ;; default win size
      (add-to-list 'default-frame-alist '(height . 58))
      (add-to-list 'default-frame-alist '(width . 120))
      ;; key-bindings for window resizing/navigation
      (global-set-key (kbd "H-<return>")        'toggle-fullscreen)             ;; toggle full screen
      (global-set-key (kbd "H-M-<left>")        'shrink-window-horizontally)    ;; shrink window horizontally
      (global-set-key (kbd "H-M-<right>")       'enlarge-window-horizontally)   ;; enlarge window horizontally
      (global-set-key (kbd "H-M-<down>")        'shrink-window)                 ;; shrink window vertically
      (global-set-key (kbd "H-M-<up>")          'enlarge-window)                ;; enlarge window vertically
      (global-set-key [H-left]                  'windmove-left)                 ;; move to left windnow
      (global-set-key [H-right]                 'windmove-right)                ;; move to right window
      (global-set-key [H-up]                    'windmove-up)                   ;; move to upper window
      (global-set-key [H-down]                  'windmove-down)                 ;; move to lower window
      ;; other key bindings
      (global-set-key (kbd "H-M-s")     'magit-status)          ;; git status

      (global-set-key (kbd "H-M-t")     'hs-toggle-hiding)      ;; toggle show/hide block
      (global-set-key (kbd "H-M-b")     'browse-url-at-point)   ;; open url under cursor in chrome
      (global-set-key (kbd "H-M-l")     'cycle-pretty-themes)
      (global-set-key (kbd "H-M-k")     'cycle-all-themes)

      (global-set-key (kbd "H-M-i")     'copilot-accept-completion)

      ;; other
      (custom-set-faces
       '(default ((t (:inherit nil :weight medium :height 130 :width normal :family "source code pro")))))
      ;; (powerline-default-theme)
      (cycle-pretty-themes)))

;; global-behavior
(global-auto-revert-mode 1)                             ;; automatically reload files when changed
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)                                            ;; IDO
(define-key global-map (kbd "RET") 'newline-and-indent) ;; auto indentation

;; yasnippet helper functions
(setq
 yas-already-loaded nil)
(defun yas-reload-if-necessary ()
  (interactive)
  (progn
    (yas-minor-mode)
    (when (not yas-already-loaded)
      (progn
        (yas-reload-all)
        (setq yas-already-loaded 1)))))
(yas-reload-if-necessary)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; C/C++
(setq c-default-style "linux" c-basic-offset 2)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(defun override-cc-cl ()
       (local-set-key (kbd "C-c C-l") 'recompile))
(add-hook 'c-mode-common-hook 'override-cc-cl)

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; freecad macros (python scripts)
(add-to-list 'auto-mode-alist '("\\.FCMacro\\'" . python-mode))

;; haskell
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(defun haskellfmt ()
  (interactive)
  (unless (use-region-p) (mark-whole-buffer))
  (shell-command-on-region (mark) (point) "brittany" (current-buffer) t))

;; rust -- remember to have rust-analyzer binary in $PATH
(setq lsp-rust-server 'rust-analyzer)
(add-hook 'rust-mode-hook 'yas-reload-if-necessary)

;; LISPs
(add-hook 'emacs-lisp-mode-hook                   'paredit-mode)
(add-hook 'clojure-mode-hook                      'paredit-mode)

;; javascript
(setq js-indent-level 2)

;; json pretty print
(defun jj ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "jq ." (current-buffer) t)))

;; disable background color in vue-mode
(add-hook
 'mmm-mode-hook
 (lambda ()
   (set-face-background 'mmm-default-submode-face nil)))

;; avro schemas (json syntax)
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))

;; ipython
(add-hook 'python-mode-hook 'yas-reload-if-necessary)

;; web-mode
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ngt?\\'" . web-mode))

;; scala formatter
(defun scalafmt ()
  (interactive)
  (unless (use-region-p) (mark-whole-buffer))
  (shell-command-on-region (mark) (point) "scalafmt --stdin" (current-buffer) t))

;; golang
(add-hook
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook 'gofmt-before-save)
   (setq tab-width 2)
   (setq indent-tabs-mode 1)
   (lsp)))

;; csharp
(defun csharp-stuff ()
  (electric-pair-local-mode 1) ;; Emacs 25
  (setq c-basic-offset 4)
  (setq-default indent-tabs-mode nil) ;; no tabs
  ;; (setq tab-width 4)
  )
(add-hook 'csharp-mode-hook 'csharp-stuff)

;; julia
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
;; (add-hook 'julia-mode-hook #'julia-vterm-mode)  ;; testing out new minor mode

;; xml indent w/ 4 spaces
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; miscellanea
(setq tramp-use-ssh-controlmaster-options nil)                                                  ;; fix tramp without breaking ghc-mod
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "~/tmp/tramp")))       ;; tramp path
(put 'erase-buffer 'disabled nil)

(setq fci-rule-column 80)
(setq magit-last-seen-setup-instructions "1.4.0")

(defun camelcase ()
  (interactive)
  (query-replace-regexp
   "\\(\\w\\)_\\(\\w\\)"
   "\\1\\,(upcase \\2)"))

;; lilypond
(setq load-path (append (list "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp") load-path))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))


;; show byte offset at point
(defun show-byte-offset-at-point ()
  "Report the byte offset (0-indexed) in the file
corresponding to the position of point."
  (interactive)
  (message "byte offset: %d" (1- (position-bytes (point)))))


;; markdown preview -- NB needs pandoc installed
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#323334" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#eceff4"])
 '(custom-safe-themes
   '("0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" default))
 '(exwm-floating-border-color "#181818")
 '(fci-rule-color "#525252")
 '(highlight-tail-colors ((("#3d413c") . 0) (("#3a4143") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#3f3f3f"))
 '(objed-cursor-color "#C16069")
 '(pdf-view-midnight-colors (cons "#eceff4" "#323334"))
 '(rustic-ansi-faces
   ["#323334" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#eceff4"])
 '(vc-annotate-background "#323334")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A2BF8A")
    (cons 40 "#bac389")
    (cons 60 "#d3c788")
    (cons 80 "#ECCC87")
    (cons 100 "#e3b57e")
    (cons 120 "#da9e75")
    (cons 140 "#D2876D")
    (cons 160 "#c88982")
    (cons 180 "#be8b98")
    (cons 200 "#B58DAE")
    (cons 220 "#b97e97")
    (cons 240 "#bd6f80")
    (cons 260 "#C16069")
    (cons 280 "#a0575e")
    (cons 300 "#804f54")
    (cons 320 "#5f4749")
    (cons 340 "#525252")
    (cons 360 "#525252")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((comp))))
