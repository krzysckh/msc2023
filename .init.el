(progn
  (setq *default-directory* "~/programowanko/science-cup-2023")
  (defun conf/run-in-default-dir (s async)
    (cd *default-directory*)
    (if async
        (async-shell-command s)
      (shell-command s)))

  (defun conf/prepare ()
    (setq default-directory *default-directory*)
    (mapcar #'find-file-noselect (file-expand-wildcards "rc.scm"))
    (mapcar #'find-file-noselect (file-expand-wildcards "scm/*.scm"))
    (mapcar #'find-file-noselect (file-expand-wildcards "src/*.[ch]"))

    (setq buffer-menu #'switch-to-buffer) ; TODO: przeniose to do .emacs
    (setq list-buffers #'switch-to-buffer) ; TODO: przeniose to do .emacs

    (setq conf/make "make")
    (if (string= (system-name) "openchad") (setq conf/make "gmake"))

    (global-set-key
     (kbd "C-c C-t")
     (lambda ()
       (interactive)
       (conf/run-in-default-dir "etags scm/*.scm src/*.[ch] tinyscheme/*.[ch]" nil)
       (setq tags-file-name (concat default-directory "/TAGS"))))

    (global-set-key
     (kbd "C-c C-c C-c")
     (lambda ()
       (interactive)
       (conf/run-in-default-dir (concat conf/make " clean") nil)))

    (global-set-key
     (kbd "C-c C-p")
     (lambda ()
       (interactive)
       (conf/run-in-default-dir (concat conf/make " pubcpy") t)))

    (setq compile-command
          (concat
           "cd ~/programowanko/science-cup-2023/ ; "
           conf/make " && ./main")))

  (conf/prepare))
