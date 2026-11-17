;;; init.el --- main config -*- lexical-binding: t -*-
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
(straight-use-package 'dockerfile-mode)
(straight-use-package 'dired-sidebar)
(straight-use-package 'dired-subtree)
(straight-use-package 'dumb-jump)
(straight-use-package 'expand-region)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'go-mode)
(straight-use-package 'hotfuzz)
(straight-use-package 'json-mode)
(straight-use-package 'julia-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-pyright)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'marginalia)
(straight-use-package 'multiple-cursors)
(straight-use-package 'nix-mode)
(straight-use-package 'orderless)
(straight-use-package 'paredit)
(straight-use-package 'protobuf-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'rg)
(straight-use-package 'rustic)
(straight-use-package 'shader-mode)
(straight-use-package 'switch-window)
(straight-use-package 'typescript-mode)
(straight-use-package 'vertico)
(straight-use-package 'toml-mode)
(straight-use-package 'vterm)
(straight-use-package 'vue-mode)
(straight-use-package 'web-mode)
(straight-use-package 'wgrep)
(straight-use-package 'yaml-mode)
(straight-use-package 'yasnippet)

;;   - color themes
(straight-use-package 'doom-themes)
(straight-use-package 'flatland-theme)
(straight-use-package 'material-theme)



;; use the login shell's PATH (nvm npm, cargo, etc.) -- GUI Emacs launched from
;; the Dock otherwise only sees the minimal macOS PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; add ~/bin
(add-to-list 'load-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

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
;; toolbar / scrollbar / fringes are disabled in early-init.el (avoids a frame redraw)
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)                               ;; disable bell
(add-hook 'before-save-hook 'delete-trailing-whitespace)        ;; delete trailing whitespaces on save
(column-number-mode)                                            ;; display column number in command buf.
(add-hook 'prog-mode-hook 'subword-mode)                        ;; camel-case kill-word
;;(wrap-region-global-mode)
(global-display-line-numbers-mode t)                                           ;; line numbering on
(setq use-short-answers t)                                      ;; y/n instead of yes/no
(add-hook 'prog-mode-hook #'hs-minor-mode)                      ;; hs-minor-mode for all programming modes
(setq-default fill-column 100)                                        ;; for M-q (instead of default 70)

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-x C-j") 'project-find-file)             ;; find file in project (built-in project.el)


(global-set-key (kbd "C-M-/")   'toggle-comment-region)         ;; comment code - custom function below
(global-set-key (kbd "C-M-j")   'dired-sidebar-toggle-sidebar)  ;; sidebar tree of project directory

(global-set-key (kbd "M-k")     'kill-current-buffer)           ;; kill current buffer
(global-set-key (kbd "C-;")     'scroll-down-line)
(global-set-key (kbd "C-'")     'scroll-up-line)
(global-set-key (kbd "C-x o")   'switch-window)                 ;; for when there are more than 2 windows
(global-set-key (kbd "C-M-o")   'other-window)                  ;; the default cyclical switch, quicker
(global-set-key (kbd "C-=")     'er/expand-region)              ;; expand region
(global-set-key (kbd "C-.")     'lsp-or-dumb-jump)              ;; lsp jump to definition, or dumb jump
(global-set-key (kbd "C-,")     'lsp-find-references)
(global-set-key (kbd "C-c C-.") 'lsp-describe-thing-at-point)   ;; lsp show type of variable under cursor
(global-set-key (kbd "C-c i") 'imenu)                         ;; jump to function defs etc. in buffer (lsp-backed)

(global-set-key (kbd "M-j")     'avy-goto-word-1)               ;; avy-jump (on word)
(global-set-key (kbd "C-M-y")   'toggle-truncate-lines)         ;; switch on-off word wrap
(global-set-key (kbd "C-M-t")   'hs-toggle-hiding)              ;; hide/show block
(global-set-key (kbd "C-x C-r") 'recentf-open-files)            ;; open recent file
(global-set-key (kbd "M-s")     'rg)                            ;; ripgrep search

(global-set-key (kbd "C-c C-b") 'rustic-cargo-build)            ;; cargo build
(global-set-key (kbd "C-c C-f") 'rustic-cargo-fmt)              ;; cargo format


;; bookmarks shortcuts
(global-set-key (kbd "C-c b") 'bm-toggle)                     ;; toggle bookmark on current line/buffer
(global-set-key (kbd "C-c ]") 'bm-next)                       ;; jump to next bookmark
(global-set-key (kbd "C-c [") 'bm-previous)                   ;; jump to previous bookmark
(setq bm-cycle-all-buffers t)  ;; allow jumping to bookmarks in other buffers

;; date insertion shortcuts
(defun day-of-month-ordinal-suffix (day)
  "Return the English ordinal suffix (st/nd/rd/th) for DAY (1-31)."
  (if (memq (mod day 100) '(11 12 13))
      "th"
    (or (cdr (assq (mod day 10) '((1 . "st") (2 . "nd") (3 . "rd"))))
        "th")))

(defun insert-date-short ()
  "Insert the current date as dd.mm.yyyy at point."
  (interactive)
  (insert (format-time-string "%d.%m.%Y")))

(defun insert-date-long ()
  "Insert the current date like: Wednesday, October 3rd, 2024 at point."
  (interactive)
  (let ((day (string-to-number (format-time-string "%e"))))
    (insert (format-time-string (concat "%A, %B " (number-to-string day)
                                        (day-of-month-ordinal-suffix day) ", %Y")))))

(global-set-key (kbd "C-c d") 'insert-date-short)             ;; insert date dd.mm.yyyy
(global-set-key (kbd "C-c D") 'insert-date-long)              ;; insert date Wednesday, October 3rd, 2024

;; multiple cursors key bindings
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
		     ))
;; theme cycling (works in GUI and terminal)
(setq all-themes nil)
(defun cycle-pretty-themes ()
  (interactive)
  (load-theme (car pretty-themes) t)
  (setq pretty-themes (append (cdr pretty-themes) (list (car pretty-themes)))))
(defun cycle-all-themes ()
  (interactive)
  (unless all-themes (setq all-themes (custom-available-themes)))
  (load-theme (car all-themes) t)
  (message "loaded theme %s" (car all-themes))
  (setq all-themes (append (cdr all-themes) (list (car all-themes)))))
(advice-add 'load-theme :before
            (lambda (&rest _) (mapc #'disable-theme custom-enabled-themes)))
(global-set-key (kbd "C-c l")     'cycle-pretty-themes)     ;; next pretty theme
(global-set-key (kbd "C-c L")     'cycle-all-themes)        ;; next theme (all installed)
(global-set-key (kbd "C-c u")     'browse-url-at-point)     ;; open url under cursor in browser
(global-set-key (kbd "C-x g")     'magit-status)            ;; git status
;; window resizing: built-in C-x { / C-x } (width), C-x ^ (taller), C-x - (shrink to fit)

;; graphic-only settings
(when (display-graphic-p)
  (global-set-key (kbd "H-<return>") 'toggle-frame-fullscreen)    ;; toggle full screen
  (custom-set-faces
   '(default ((t (:inherit nil :weight medium :height 130 :width normal :family "source code pro"))))))

;; load the first theme in the list (and rotate it to the back)
(cycle-pretty-themes)
;; minibuffer completion: vertical list (vertico), match space-separated pieces
;; in any order (orderless), annotations next to candidates (marginalia)
(vertico-mode 1)
(savehist-mode 1)                                       ;; remember minibuffer history (recent M-x commands first)
(marginalia-mode 1)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))
                                        (project-file (styles hotfuzz)))) ;; C-x C-j: fuzzy + ranked
(define-key global-map (kbd "RET") 'newline-and-indent) ;; auto indentation

(defun random-theme ()
  "Activate a random color theme from available themes."
  (interactive)
  (let* ((themes (custom-available-themes))
	 (theme (nth (random (length themes)) themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (message "Loaded theme: %s" theme)))

(defun vterm-new ()
  "Create a new vterm buffer (do not reuse the current one)."
  (interactive)
  (vterm t))
(global-set-key (kbd "C-c t") #'vterm-new)

;; shortcuts for opening claude or gemini in a terminal
(defun vterm-run-command-silent (command)
  "Open a new vterm named after COMMAND and execute it."
  (let ((buffer-name (format "*vterm: %s*" command)))
    (vterm buffer-name)
    (vterm-send-string command)
    (vterm-send-return)))
(defun vterm-claude ()
  "Open a vterm and run claude."
  (interactive)
  (vterm-run-command-silent "claude"))

(global-set-key (kbd "C-c c") #'vterm-claude)

;; yasnippet helper functions
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
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

;; C/C++
(setq c-default-style "linux" c-basic-offset 2)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(defun override-cc-cl ()
       (local-set-key (kbd "C-c C-l") 'recompile))
(add-hook 'c-mode-common-hook 'override-cc-cl)


;; freecad macros (python scripts)
(add-to-list 'auto-mode-alist '("\\.FCMacro\\'" . python-mode))

;; rust -- remember to have rust-analyzer binary in $PATH
(setq lsp-rust-server 'rust-analyzer)
(add-hook 'rust-mode-hook 'yas-reload-if-necessary)

;; LISPs
(add-hook 'emacs-lisp-mode-hook                   'paredit-mode)

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
;; completion is handled by corfu through completion-at-point, not company
(setq lsp-completion-provider :none)
;; lsp-mode's lsp-ts-query client crashes on load (breaks every lsp start); skip it
(with-eval-after-load 'lsp-mode
  (setq lsp-client-packages (delq 'lsp-ts-query lsp-client-packages)))
;; bootstrap: install the pyright language server (via npm, into lsp-mode's cache) if missing
(with-eval-after-load 'lsp-pyright
  (unless (or (executable-find "pyright-langserver") (ignore-errors (lsp-package-path 'pyright)))
    (if (executable-find "npm")
        (lsp-install-server nil 'pyright)
      (message "pyright not installed and npm not found; install Node, then M-x lsp-install-server RET pyright"))))
(add-hook 'python-mode-hook #'lsp-deferred)

;; web-mode
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ngt?\\'" . web-mode))

;; golang
(add-hook
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook #'gofmt-before-save nil t) ;; buffer-local
   (setq tab-width 2)
   (setq indent-tabs-mode 1)
   (lsp-deferred)))

;; csharp
(defun csharp-stuff ()
  (electric-pair-local-mode 1) ;; Emacs 25
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil) ;; no tabs (this buffer only)
  ;; (setq tab-width 4)
  )
(add-hook 'csharp-mode-hook 'csharp-stuff)

;; xml indent w/ 4 spaces
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; miscellanea
(put 'erase-buffer 'disabled nil)

(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode 1)

(defun camelcase ()
  (interactive)
  (query-replace-regexp
   "\\(\\w\\)_\\(\\w\\)"
   "\\1\\,(upcase \\2)"))

;; lilypond
;; uses whichever ~/local/lilypond-* install exists (lilypond-mode ships with LilyPond)
(dolist (dir (file-expand-wildcards "~/local/lilypond-*/share/emacs/site-lisp"))
  (add-to-list 'load-path dir))
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
;; keep Customize output out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)


;; non-essential global modes: enable right after the first frame is up
(run-with-idle-timer
 0.5 nil
 (lambda ()
   (global-auto-revert-mode 1)                          ;; automatically reload files when changed
   (display-time-mode 1)))

;; macOS: apps launched from the shell (e.g. `emacs &`) open without
;; activating (focus-stealing prevention), so the window appears behind
;; everything.  Raise ourselves to the front once the frame is on-screen,
;; and retry until it actually sticks (raising too early doesn't stick).
(when (eq system-type 'darwin)
  (defun my/activate-emacs ()
    "Bring the Emacs process to the front (macOS only).
`x-focus-frame' calls -[NSApp activateIgnoringOtherApps:] directly, so it
needs no AppleScript / System Events automation permission."
    (condition-case err
        (x-focus-frame (selected-frame))
      (error (message "my/activate-emacs: %s" (error-message-string err)))))
  (defun my/emacs-frontmost-p ()
    "Return t if the selected Emacs frame has input focus."
    (eq (frame-focus-state (selected-frame)) t))
  (defvar my/activate-attempts 0
    "How many times we've tried to raise Emacs at startup.")
  (defun my/activate-emacs-retry (&rest _)
    (when (and (display-graphic-p) (not (my/emacs-frontmost-p)))
      (when (< (setq my/activate-attempts (1+ my/activate-attempts)) 5)
        (my/activate-emacs)
        (run-at-time 0.4 nil #'my/activate-emacs-retry))))
  ;; Direct launch: the initial frame exists while init runs, but it only
  ;; becomes visible at the very end of startup -- defer with a short timer.
  ;; With a daemon/client, wait for the first frame instead.
  (if (daemonp)
      (progn
        (defun my/activate-emacs-once (frame)
          (when (display-graphic-p frame)
            (remove-hook 'after-make-frame-functions #'my/activate-emacs-once)
            (run-at-time 0.3 nil #'my/activate-emacs-retry)))
        (add-hook 'after-make-frame-functions #'my/activate-emacs-once))
    (run-with-timer 0.4 nil #'my/activate-emacs-retry)))
