(require 'package)
(package-initialize)

(setq c-default-style "linux"
      c-basic-offset 2)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/elisp")

(if (file-exists-p (concat "~/.emacs.d/elisp/" "llvm-mode.el"))
    (require 'llvm-mode)
  )

(if (file-exists-p (concat "~/.emacs.d/elisp/" "org-s5.el"))
    (require 'org-s5)
  )

(show-paren-mode 1)

(setq exec-path (append exec-path '("~/bin" "/usr/local/bin")))

(global-flycheck-mode)

;; (flycheck-add-next-checker 'python-flake8 'python-mypy)

(defun flycheck-list-errors-only-when-errors ()
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-when-let (buffer (get-buffer flycheck-error-list-buffer))
      (dolist (window (get-buffer-window-list buffer))
        (quit-window nil window)))))


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)

(defun lunaryorn-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))

;; (icy-mode 1)

;; Mac/homebrew only?
(setq-default ispell-program-name "/usr/local/bin/aspell")

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(add-hook 'shell-mode 'ansi-color-for-commit-mode-on)

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))

;; Point it straight at homebrew-installed Exuberant ctags
(setq path-to-ctags "/usr/local/bin/ctags")

(setq temporary-file-directory "~/.emacs.d/tmp")

(defun create-tags (dir-name)
  "Create tags file"
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name
           (directory-file-name dir-name)))
  )

;; (setq column-enforce-column 79)

;; minor modes used in *any* major mode
;; (add-hook 'after-change-major-mode-hook
;; 	  (lambda ()
;; 	    (column-enforce-mode)
;; 	    ))

(defun find-root-file (name dir)
  (car
   (or
    (mapcar (lambda (file)
              (expand-file-name file dir))
            (directory-files dir nil name))
    (mapcar (lambda (upper)
              (find-root-file name upper))
            (delq dir (list (expand-file-name ".." dir)))
            )
    )
   )
  )

(defun insert-markdown-slide ()
  "Inserts a markdown slide; optionally around the current active region"
  (interactive)
  (if (use-region-p)
      (progn
        (save-excursion
          (goto-char (region-beginning))
          (insert "{% slide %}\n\n"))
        (save-excursion
          (goto-char (region-end))
          (insert "\n{% endslide %}")))
    (progn
        (insert "{% slide %}\n\n{% endslide %}")
        (forward-line -1))))

(defun insert-markdown-slide-fragment ()
  "Inserts a markdown slide fragment; optionally around the current active region"
  (interactive)
  (if (use-region-p)
      (progn
        (save-excursion
          (goto-char (region-beginning))
          (insert "{% frag %}\n\n"))
        (save-excursion
          (goto-char (region-end))
          (insert "\n{% endfrag %}")))
    (progn
        (insert "{% frag %}\n\n{% endfrag %}")
        (forward-line -1))))

(defun insert-markdown-note ()
  "Inserts a markdown note; optionally around the current active region"
  (interactive)
  (if (use-region-p)
      (progn
        (save-excursion
          (goto-char (region-beginning))
          (insert "<aside class=\"notes\" markdown=\"1\">\n\n"))
        (save-excursion
          (goto-char (region-end))
          (insert "\n</aside>")))
    (progn
        (insert "<aside class=\"notes\" markdown=\"1\">\n\n</aside>")
        (forward-line -1))))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.[Mm]arkdown" . markdown-mode)
	    (cons '("\\.md" . markdown-mode)
		  auto-mode-alist
		  )
	    )
      )

(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (auto-fill-mode)
	     (flyspell-mode)
	     (local-set-key (kbd "C-c s") `insert-markdown-slide)
	     (local-set-key (kbd "C-c f") `insert-markdown-slide-fragment)
	     (local-set-key (kbd "C-c n") `insert-markdown-note)
	     )
	  )

(add-hook 'scss-mode-hook
	  '(lambda ()
	     (set-variable css-indent-offset 2)
	     (set-variable scss-compile-at-save nil)
	     )
	  )

(add-hook 'html-mode-hook
	  '(lambda ()
	     (set-variable sgml-basic-offset 2)
	     )
	  )

;; Enable flymake minor mode, with custom keybindings
(defun enable-flymake ()
  (flymake-mode)
  (local-set-key (kbd "M-n") `flymake-goto-next-error)
  (local-set-key (kbd "M-p") `flymake-goto-prev-error))

;; Useful Python stuff
(fset 'mock-patch
   "@mock.patch(\"\", autospec=True)\C-[b\C-[b\C-b\C-b\C-b")

(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-<") 'python-indent-shift-left)
	     (local-set-key (kbd "C->") 'python-indent-shift-right)
	     ;; (enable-flymake)
	     (setq column-enforce-column 99)
	     (local-set-key (kbd "C-c p") `mock-patch)
	     ))

;; requires puppet-mode
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; requires coffee-mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
          (lambda () (set 'tab-width 2)))

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Requires js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq js2-basic-offset 2)
      (setq column-enforce-column 99)
      (set-variable 'indent-tabs-mode nil)
      ;; (enable-flymake)
      ))

(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.react.js$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (lunaryorn-use-js-executables-from-node-modules)))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.react.js$")))

(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-attr-indent-offset 2)

(add-hook 'web-mode-hook
          (lambda ()
            	     (setq column-enforce-column 99)
                   (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
                   (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
                   ;; (enable-flymake)
                   ))

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-clojure-indent
              (async 1))))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (or (equal web-mode-content-type "jsx")
          (equal web-mode-content-type ".react.js"))
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-flake8rc ".flake8")
 '(ispell-local-dictionary "en")
 '(package-selected-packages
   (quote
    (go-mode web-mode typescript-mode terraform-mode vcl-mode rjsx-mode kotlin-mode dart-mode flycheck graphviz-dot-mode groovy-mode dockerfile-mode nginx-mode yaml-mode web-beautify scss-mode puppet-mode markdown-mode+ less-css-mode js2-mode icicles flymake-easy flymake-cursor ess column-enforce-mode coffee-mode clojurescript-mode clojure-mode actionscript-mode))))


(defun replace-random (to-replace)
  (interactive "MTo Replace: ")
  (while (re-search-forward to-replace nil t)
    (replace-match
     (concat
      "\\1"
      (mapconcat
       (lambda (x)
	 (apply #'string
		(list (aref "abcdefghijklmnopqrstuvwxyz" (random 26)))))
       (number-sequence 1 8)
       ""
       )
      "\\2"
      )
     )
    )
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
