(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(show-paren-mode 1)

(setq exec-path (append exec-path '("~/bin" "/usr/local/bin")))

;; (icy-mode 1)

;; Mac/homebrew only?
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; Point it straight at homebrew-installed Exuberant ctags
(setq path-to-ctags "/usr/local/bin/ctags")

(setq temporary-file-directory "~/.emacs.d/tmp")

(fset 'markdown-slide
   "<section>\C-m<div markdown=\"1\">\C-m\C-m</div>\C-m</section>\C-[OA\C-[OA\C-m\C-m\C-[OA")

(defun create-tags (dir-name)
  "Create tags file"
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name
           (directory-file-name dir-name)))
  )

(setq-default highlight-80+-columns 79)

;; minor modes used in *any* major mode
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (highlight-80+-mode)
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

  (defun flymake-jsl-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-intemp))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "jsl" (list "--nologo" "--nosummary" "--nofilelisting" "--conf"
                        (find-root-file "jsl.conf" (file-name-directory buffer-file-name))
                        local-file )
            )
      )
    )

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pycheck-init))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.js\\'" flymake-jsl-init))
  )

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

(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-<") 'python-indent-shift-left)
	     (local-set-key (kbd "C->") 'python-indent-shift-right)
	     (flymake-mode)
	     (make-local-variable 'highlight-80+-columns)
	     (setq highlight-80+-columns 99)
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
      (set-variable 'indent-tabs-mode nil)
      (flymake-mode)))

(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
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
