(mapcar #'ql:quickload '(:cl-ppcre :rutils))

(proclaim '(inline last1 str))

(defun last1 (lst)
  (car (last lst)))

(defun str (obj)
  (format nil "~A" obj))


(defun test-regexp-in-file-lines (f-path regexp &key (line-print-p T)
                                                     (start-at 0)
                                                     (external-format :utf-8))
  "Test the application of regexp in file lines"
  (with-open-file (stream f-path
                          :external-format external-format)
    (do ((line (read-line stream nil)
               (read-line stream nil))
         (i 0))
        ((null line))
      (rutils:when-it (and (>= i start-at)
                           (ppcre:register-groups-bind (finding) 
                               (regexp line)
                             finding))
        (when line-print-p
          (print line))
        (print rutils:it)
        (unless (y-or-n-p "Continue?")
          (return))))))


(defun transfer-in-lines-to-out (in-path out-path &key (in-external-format  :utf-8)
                                                       (out-external-format :utf-8)
                                                       (regexp "(.*)")
                                                       (quotep nil)
                                                       (fn-validate #'(lambda (it) it)))
  "Read in-path text file content and transfer to out-path
  by applying regexp filter and fn-validate function.
  Example:
  (transfer-in-lines-to-out '/tmp/in.txt'
                            '/tmp/out.txt'
                            :in-external-format :iso-8859-1
                            :regexp '<s> (.* [\.|\\?|!]) </s>'
                            :fn-validate #'(lambda (it)
                                             (> (length (ppcre:split '\\s+' it)) 3)))"
  (with-open-file (ostream out-path
                           :direction :output
                           :if-exists :supersede
                           :external-format o-external-format)
    (with-open-file (istream in-path
                             :external-format i-external-format)
      (do ((line (read-line istream nil)
                 (read-line istream nil)))
          ((null line))
        (rutils:if-it (ppcre:register-groups-bind (finding)
                                                  (regexp line)
                                                  finding)
                      (when (funcall fn-validate rutils:it)
                        (cond (quotep
                               (format ostream "~S~%" rutils:it))
                              (t (format ostream "~A~%" rutils:it)))))))))


(defun partition-in-file-to-outs (in-path out-path-template &key (number-lines 10000)
                                                                 (file-n-pattern ":i")
                                                                 (in-external-format  :utf-8)
                                                                 (out-external-format :utf-8)
                                                                 (quotep nil))
  "Partition of in-path into out-path-template files.
  Divides the input file lines between the files generated with 
  an upper limit of the number of lines (:number-lines).
  Example:
  (partition-in-file-to-outs '/tmp/in.txt'
                             '/tmp/out-:i.txt'
                             :number-lines 500"
  (let ((i 0))
    (with-open-file (istream in-path
                             :external-format in-external-format)
      (loop
        do (let ((ostream (open (ppcre:regex-replace file-n-pattern
                                                     out-path-template
                                                     (str i))
                                :direction :output
                                :if-exists :supersede
                                :external-format out-external-format)))
             (dotimes (i number-lines) 
               (rutils:if-it (read-line istream nil)                
                             (cond (quotep
                                    (format ostream "~S~%" rutils:it))
                                   (t (format ostream "~A~%" rutils:it)))
                             (progn
                               (close ostream)
                               (return-from partition-in-file-to-outs))))
             (close ostream)
             (incf i))))))
