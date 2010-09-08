;; -*- emacs-lisp -*-
;; emacs configuration file. Copyleft 2009, 2010, Ji Han
;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;; adjust look-and-feel under windowing system
(unless (string= window-system nil)
  (progn
    ;; (menu-bar-mode nil)
    (tool-bar-mode nil)
    (scroll-bar-mode 'right)
    (setq scroll-conservatively 300)
    (setq scroll-preserve-screen-position 1)
    (set-frame-font "Consolas 10")
    (setq x-select-enable-clipboard t)
    (add-hook 'comint-mode-hook
      '(lambda ()
        (setq comint-process-echoes t)))))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(setq-default fill-column 80)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list nil)
(setq c-hungry-delete-key t)

(setq track-eol t)
(setq next-line-add-newlines nil)
(fset 'yes-or-no-p 'y-or-n-p)

(global-font-lock-mode t)
(transient-mark-mode t)
(show-paren-mode t)

;; parenthesis matching
(defun match-paren (arg)
  (interactive "p")
  (cond ((looking-at "(")(progn (forward-sexp 1)(backward-char 1)))
        ((looking-at ")")(progn (forward-char 1)(backward-sexp 1)))
        (t nil)))

;; dynamic cursor
(add-hook 'post-command-hook
  '(lambda ()
    (cond (buffer-read-only (setq cursor-type 'hbar))
          (overwrite-mode (setq cursor-type 'box))
          (t (setq cursor-type 'bar)))))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; AucTex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; cmake
(require 'cmake-mode)
(setq auto-mode-alist
  (append 
    '(("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    auto-mode-alist))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; htmlize
(require 'htmlize)
(setq htmlize-output-type "inline-css")

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; w3m (a poor man's chrome/firefox/opera/safari)
(if (string= window-system nil)
  (progn
    (setq browse-url-browser-function 'w3m-browse-url)
    (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
    (global-set-key "\C-xm" 'browse-url-at-point)
    (setq w3m-use-cookies t)))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; cedet
(require 'cedet)

;;; ecb
(require 'ecb)

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;;; Haskell mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Tuareg mode (Linux only--O'Caml crashes randomly under Vista/Windows 7)
(unless (string= window-system "w32")
  (progn
    (load "append-tuareg")
    (load "custom-tuareg")))

;;; SLIME (Clozure CL for Window x86/Linux amd_64)
(set-language-environment "utf-8")
(setq inferior-lisp-program
  (cond ((string= window-system "w32") "/opt/ccl/wx86cl -K utf-8")
        (t "/opt/ccl/lx86cl64 -K utf-8")))
(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;; C++ indentation style
(add-hook 'c++-mode-hook
 '(lambda()
    (c-set-style "stroustrup")))

;; compile single file via g++ if there's no makefile
(add-hook 'c++-mode-hook
  '(lambda ()
    (unless (or (file-exists-p "makefile") (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
        (let ((file (file-name-nondirectory buffer-file-name)))
          (concat "g++ -g -o " (file-name-sans-extension file) " " file))))))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;; map user-defined key bindings
(if (file-exists-p "~/.emacs.d/user-key-bindings.el")
  (load "~/.emacs.d/user-key-bindings"))

;; keep byte-compiled dotemacs up to date
(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
    (when (equal buffer-file-name user-init-file)
      (add-hook 'after-save-hook
        '(lambda ()
          (let ((dotemacs (file-name-sans-extension user-init-file)))
            (byte-compile-file ;;user-init-file *sometimes* ends with ".elc"!
              (concat dotemacs (if (file-exists-p dotemacs) "" ".el")))))))))

;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;; ;;;;;;;;

;; -*- end of file -*-
