(defun tablist-get-id ()
  (interactive)
  (message (tabulated-list-get-id)))


(defun tablist-get-emtry ()
  (interactive)
  (message "%s" (tabulated-list-get-entry)))


(defun tablist-print (&optional format sort-key entries)
  (let ((buffer-name "*tablist*"))
    (with-output-to-temp-buffer buffer-name
      (progn
        (pop-to-buffer buffer-name) ; Selects the buffer, necessary for proper printing
        (setq tabulated-list-format (or format [("Col1" 18 t)
                                                ("Col2" 12 nil)
                                                ("Col3"  10 t)
                                                ("Col4" 0 nil)]))
        (setq tabulated-list-padding 2)
        (setq tabulated-list-sort-key
              (if sort-key `(,sort-key) (list (nth 0 (aref tabulated-list-format 2)))))
        (tabulated-list-init-header)
        (setq tabulated-list-entries (or entries '(("numbers" ["1" "2" "c" "4"])
                                                   ("letters" ["a" "b" "3" "d"]))))
        (tabulated-list-print t))
      )))


(defun tablist-print-alist (alist)
  (let* ((alist (cl-map 'list
                        #'(lambda (x) `(,(format "%s" (car x)) [,(format "%s" (car x)) ,(format "%s" (cdr x))]))
                        alist)))
    (tablist-print [("Key" 10 t) ("Value" 0 t)] "Key" alist)))


(defun tablist-print-alist-example ()
  (tablist-print-alist '((:foo . bar) (:baz . 123) (:qux . t) (:norf . nil) (:worble . '(blah)))))


(defun tablist-print-plist (plist)
  (let* ((plist (cl-loop for (key value) on plist by 'cddr
                         collect `(,(format "%s" key) [,(format "%s" key) ,(format "%s" value)]))))
    (tablist-print [("Key" 10 t) ("Value" 0 t)] "Key" plist)))


(defun tablist-print-plist-example ()
  (tablist-print-plist '(:foo bar :baz 123 :qux t :norf nil :worble '(blah))))


(defun tablist-print-hash-table (ht)
  (setq formatted-ht '())
  (maphash (lambda (k v)
             (push `(,(format "%s" k) [,(format "%s" k) ,(format "%s" v)]) formatted-ht))
           ht)
  (tablist-print [("Key" 10 t) ("Value" 0 t)] "Key" formatted-ht)
  formatted-ht)


(defun tablist-print-hash-table-example ()
  (tablist-print-hash-table #s(hash-table
                               size 30
                               test equal
                               data (:foo bar :baz 123 :qux t :norf nil :worble '(blah)))))


(defun tablist-interactive-print (format sort-key entries)
  ;; Note: Provide entries as an unquoted list
  (interactive "xTabulated list format: \nsTabulated list sort key: \nxTabulated list entries: ")
  (tablist-print format sort-key entries))
