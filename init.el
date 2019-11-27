;; mac: switch meta/hyper key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(package-initialize)

;; recent files mode
(recentf-mode 1)

;; add ~/bin
(add-to-list 'load-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(add-to-list 'exec-path (expand-file-name "~/miniconda3/bin"))

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
(global-linum-mode t)                                           ;; line numbering on
(setq linum-format "%d ")                                       ;; adjust line number column size
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'prog-mode-hook #'hs-minor-mode)                      ;; hs-minor-mode for all programming modes

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-M-/")   'toggle-comment-region)         ;; comment code - custom function below
(global-set-key (kbd "C-M-j")   'direx:jump-to-directory)       ;; tree view of project directory

(global-set-key (kbd "C-m")     'indent-new-comment-line)       ;; continue w/ comments
(global-set-key (kbd "M-k")     'kill-this-buffer)              ;; kill current buffer
(global-set-key (kbd "C-M-i")   'company-complete)              ;; autocomplete w/ company mode
(global-set-key (kbd "C-;")     'scroll-down-line)
(global-set-key (kbd "C-'")     'scroll-up-line)
(global-set-key (kbd "C-x o")   'switch-window)                 ;; for when there are more than 2 windows
(global-set-key (kbd "C-M-o")   'other-window)                  ;; the default cyclical switch, quicker
(global-set-key (kbd "C-=")     'er/expand-region)              ;; expand region
(global-set-key (kbd "C-.")     'dumb-jump-go)                  ;; dumb jump to definition

(global-set-key (kbd "M-j")     'avy-goto-word-1)               ;; avy-jump (on word)
(global-set-key (kbd "M-[")     'avy-goto-char)                 ;; avy-jump (on any char)
(global-set-key (kbd "M-]")     'avy-goto-char-2)               ;; avy-jump (on any two chars)
(global-set-key (kbd "C-M-y")   'toggle-truncate-lines)         ;; switch on-off word wrap
(global-set-key (kbd "C-M-t")   'hs-toggle-hiding)              ;; hide/show block
(global-set-key (kbd "M-x")     'smex)                          ;; nicer M-x
(global-set-key (kbd "C-z")     'nop)                           ;; do nothing (prevent minimize to dock)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)            ;; open recent file
(global-set-key (kbd "M-s")     'rg)                            ;; ripgrep search
(global-set-key (kbd "C-c C-i") 'string-inflection-toggle)      ;; cycle through camelcase etc.


;; multiple cursors key bindings
(require 'cl)   ;; fix missing equalp symbol
(global-set-key (kbd "C->")             'mc/mark-next-like-this)
(global-set-key (kbd "C-<")             'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-l")           'mc/edit-lines)
(global-set-key (kbd "C-c C-<")         'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<mouse-1>")   'mc/add-cursor-on-click)
(global-set-key (kbd "C-c C-l")         'recompile)

;; custom comment toggle function
(defun toggle-comment-region ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; full screen mode
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


;; code for using `sk` to open files (do a cargo install skim beforehand)
(defvar sk/executable "sk")
(defun sk/after-term-handle-exit (process-name msg)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string text "\n" t "\s.*\s"))
         (target (car (last (butlast lines 1))))
         (file (expand-file-name target)))
    (kill-buffer "*sk*")
    (jump-to-register :sk-windows)
    (when (file-exists-p file)
      (find-file file)))
  (advice-remove 'term-handle-exit #'sk/after-term-handle-exit))
(defun sk/start ()
  (interactive)
  (require 'term)
  (window-configuration-to-register :sk-windows)
  (advice-add 'term-handle-exit :after #'sk/after-term-handle-exit)
  (let ((buf (get-buffer-create "*sk*"))
        (window-height (- 10)))  ;; win height
    (if (fboundp #'projectile-project-root)
	(progn
	  (setq default-directory
		(condition-case err
		    (projectile-project-root)
		  (error
		   default-directory)))))
    (split-window-vertically window-height)
    (other-window 1) ;; go to the bottom???
    (make-term "sk" sk/executable)
    (switch-to-buffer buf)
    (linum-mode 0)
    (set-window-margins nil 1)
    ;; disable various settings known to cause artifacts
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (face-remap-add-relative 'mode-line '(:box nil))
    (term-char-mode)))



;; graphic-only keybindings
(if (display-graphic-p)
    (progn
      ;; themes
      (setq pretty-themes '(doom-spacegrey
			    sanityinc-tomorrow-eighties
			    misterioso
			    flatland whiteboard))
      (defun cycle-pretty-themes ()
	(interactive)
	(load-theme (car pretty-themes) t)
	(setq linum-format "%3d ") ;; force line number column size
	(setq pretty-themes (append (cdr pretty-themes) (list (car pretty-themes)))))
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
      ;; (global-set-key (kbd "H-M-o")     'fzf)                   ;; open file w/ fzf
      (global-set-key (kbd "H-M-p")     'projectile-mode)       ;; toggle projectile-mode
      (global-set-key (kbd "H-M-o")     'sk/start)              ;; find files w/ https://github.com/lotabout/skim
      (global-set-key (kbd "H-M-t")     'hs-toggle-hiding)      ;; toggle show/hide block
      (global-set-key (kbd "H-M-b")     'browse-url-at-point)   ;; open url under cursor in chrome
      (global-set-key (kbd "H-M-l")     'cycle-pretty-themes)
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
  (progn
    (yas-minor-mode)
    (when (not yas-already-loaded)
      (progn
        (yas-reload-all)
        (setq yas-already-loaded 1)))))

;; C/C++
(setq c-default-style "linux" c-basic-offset 2)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'company-mode)
(defun override-cc-cl ()
       (local-set-key (kbd "C-c C-l") 'recompile))
(add-hook 'c-mode-common-hook 'override-cc-cl)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(setq clang-format-executable "/usr/local/opt/llvm/bin/clang-format")

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; octave/matlab
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; haskell
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'company-mode)

(defun haskellfmt ()
  (interactive)
  (unless (use-region-p) (mark-whole-buffer))
  (shell-command-on-region (mark) (point) "brittany" (current-buffer) t))

;; rust
;; (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
;; (setq racer-rust-src-path
;;       (expand-file-name
;;        "~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-rust-rls-command '("rustup" "run" ;; "nightly"
;; 			       "rls"))
;;   (require 'lsp-rust))

;; LISPs
(add-hook 'emacs-lisp-mode-hook                   'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook  'paredit-mode)
(add-hook 'scheme-mode-hook                       'paredit-mode)
(add-hook 'clojure-mode-hook                      'paredit-mode)
(add-hook 'clojure-mode-hook                      'company-mode)
(add-hook 'scheme-mode-hook                      'hs-minor-mode)
(setq scheme-program-name "/usr/local/bin/guile")

;; javascript
(setq js-indent-level 2)
(add-hook 'js-mode-hook 'company-mode)

;; json pretty print
(defun jj ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "jq ." (current-buffer) t)))

;; avro schemas (json syntax)
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))

;; ipython
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter (expand-file-name "~/miniconda3/bin/ipython")
      python-shell-interpreter-args "-i --pylab --simple-prompt")
(add-hook 'python-mode-hook 'yas-reload-if-necessary)
(add-hook 'python-mode-hook 'company-mode)
;; reformat code using yapf
(defun yapf ()
  (interactive)
  (unless (use-region-p) (mark-whole-buffer))
  (shell-command-on-region (mark) (point) (executable-find "yapf") (current-buffer) t))
(global-set-key (kbd "C-c C-y") 'yapf)                          ;; run yapf python formatter


;; web-mode
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ngt?\\'" . web-mode))

;; scala
(add-hook 'scala-mode-hook 'company-mode)
;; formatter
(defun scalafmt ()
  (interactive)
  (unless (use-region-p) (mark-whole-buffer))
  (shell-command-on-region (mark) (point) "scalafmt --stdin" (current-buffer) t))
;; scalafmt --stdin

;; golang
(add-hook
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook 'gofmt-before-save)
   (setq tab-width 2)
   (setq indent-tabs-mode 1)
   (company-mode)
   (lsp)))

;; julia
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

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

(require 'wgrep)

;; lilypond
(setq load-path (append (list "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp") load-path))
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))


;; latex

;; (require 'browse-url) ; part of gnu emacs
(defun lookup-thesaurus ()
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "https://www.thesaurus.com/browse/" word))))
(global-set-key (kbd "<f8>")     'lookup-thesaurus)



;;;;;;;;;; test

(defun block-to-python ()
  (interactive)
  (push-mark)
  (search-backward "###---###")
  (set-mark-command nil)
  (search-forward "###---###" nil nil 2)
  ;; TODO if not found, do this ...
  ;; (goto-char (point-max))
  (python-shell-send-region (mark) (point)))

(global-set-key (kbd "C-c C-e") 'block-to-python)


;; (setq sml/no-confirm-load-theme t)
;; (sml/setup)
;; (setq sml/theme 'atom-one-dark)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(beacon-color "#f2777a")
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (lsp-ui company-lsp lsp-mode doom-themes vlf color-theme-sanityinc-tomorrow yasnippet yaml-mode wrap-region window-number wgrep-ag web-mode typescript-mode toml-mode switch-window sublime-themes smex smart-mode-line-atom-one-dark-theme scala-mode sbt-mode rjsx-mode rg racket-mode racer protobuf-mode projectile powerline paredit pallet omnibox oceanic-theme mc-extras material-theme markdown-mode magit kaolin-themes julia-repl julia-mode json-mode highlight-symbol hi2 groovy-mode go-mode geiser flatland-theme fill-column-indicator expand-region dumb-jump dockerfile-mode direx danneskjold-theme company-irony company-ghc cmake-mode clang-format cider blacken avy atomic-chrome atom-one-dark-theme atom-dark-theme)))
 '(safe-local-variable-values (quote ((firestarter . stem-sep-upload-current-file))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight medium :height 130 :width normal :family "source code pro")))))
