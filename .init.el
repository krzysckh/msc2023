(progn
  (defun conf/prepare ()
    (setq *default-directory* "~/programowanko/science-cup-2023")
    (setq default-directory "~/programowanko/science-cup-2023")
    (mapcar #'find-file-noselect (file-expand-wildcards "rc.scm"))
    (mapcar #'find-file-noselect (file-expand-wildcards "scm/*.scm"))
    (mapcar #'find-file-noselect (file-expand-wildcards "src/*.[ch]"))

    (setq buffer-menu #'switch-to-buffer) ; TODO: przeniose to do .emacs
    (setq list-buffers #'switch-to-buffer) ; TODO: przeniose to do .emacs

    (global-set-key (kbd "C-c C-t") (lambda ()
                                      (interactive)
                                      (cd *default-directory*)
                                      (shell-command "etags scm/*.scm src/*.[ch] tinyscheme/*.[ch]")
                                      (setq tags-file-name (concat default-directory "/TAGS"))))

    (setq conf/make "make")
    (if (string= (system-name) "openchad") (setq conf/make "gmake"))

    (setq compile-command
          (concat
           "cd ~/programowanko/science-cup-2023/ ; "
           conf/make " && ./main")))

  (conf/prepare))
