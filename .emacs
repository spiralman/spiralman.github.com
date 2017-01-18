(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'load-path "~/.emacs.d/elisp")

(if (file-exists-p (concat "~/.emacs.d/elisp/" "llvm-mode.el"))
    (require 'llvm-mode)
  )

(if (file-exists-p (concat "~/.emacs.d/elisp/" "org-s5.el"))
    (require 'org-s5)
  )

(show-paren-mode 1)

(setq exec-path (append exec-path '("~/bin" "/usr/local/bin")))

;; (icy-mode 1)

;; Mac/homebrew only?
(setq-default ispell-program-name "/usr/local/bin/aspell")

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

(setq column-enforce-column 79)

;; minor modes used in *any* major mode
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (column-enforce-mode)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	    ))

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

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(when (load "flymake" t)
  (defun flymake-pycheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		      'flymake-create-temp-intemp))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pycheck-init))

  (require 'flymake-eslint)
  (let ((local-eslint (file-truename "./node_modules/.bin/eslint")))
        (if (file-executable-p local-eslint)
            (set-variable 'flymake-eslint-executable local-eslint)))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-eslint-load))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.html\\'" flymake-eslint-load))
  )

(fset 'insert-markdown-slide
   "{% slide %}\C-m\C-m{% endslide %}\C-m\C-p\C-p\C-m\C-m\C-p")

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
	     (enable-flymake)
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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq js2-basic-offset 2)
      (setq column-enforce-column 99)
      (set-variable 'indent-tabs-mode nil)
      (enable-flymake)))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.react.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

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
                   (enable-flymake)))

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
 '(ispell-local-dictionary "en"))

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
