(progn
  (defun conf/prepare ()
    (setq conf/make "make")
    (if (string= (system-name) "openchad") (setq conf/make "gmake"))

    (setq compile-command
          (concat
           "cd ~/programowanko/science-cup-2023/ ; "
           conf/make " && ./main")))

  (conf/prepare))
