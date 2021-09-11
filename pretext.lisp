(mapcar #'ql:quickload '(:cl-ppcre :rutils))

(proclaim '(inline last1 str))

(defun last1 (lst)
  (car (last lst)))

(defun str (obj)
  (format nil "~A" obj))


(defun test-regexp-in-file-lines (f-path regexp &key (line-printp T)
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
        (when line-printp
          (print line))
        (print rutils:it)
        (unless (y-or-n-p "Continue?")
          (return))))))




(defun transfer-in-lines-to-out (in-path out-path &key (i-external-format :utf-8)
                                                       (o-external-format :utf-8)
                                                       (regexp "(.*)")
                                                       (fn-validate #'(lambda (it) it)))
  "Read i-path text file content and transfer to o-path
  by applying regexp filter and fn-validate function.
  Example:
  (transfer-in-lines-to-out '/tmp/in.txt'
                            '/tmp/out.txt'
                            :i-external-format :iso-8859-1
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
                       (write-sequence (format nil "~s~%" rutils:it)
                                       ostream)))))))


(defun partition-in-file-to-outs (in-path template-out-path &key (number-line 10000)
                                                                 (replacement ":i")
                                                                 (i-external-format :utf-8)
                                                                 (o-external-format :utf-8))
  "Partition of in-path into template-out-path files.
  Divides the input file lines between the files generated with 
  an upper limit of the :number-of-line"
  (let ((i 0))
    (with-open-file (stream in-path
                            :external-format i-external-format)
      (loop
         do (let ((out (open (ppcre:regex-replace replacement
                                                  template-out-path
                                                  (str i))
                             :direction :output
                             :if-exists :supersede
                             :external-format o-external-format)))
              (dotimes (i number-line) 
                (rutils:if-it (read-line stream nil)                
                              (write-line rutils:it out)
                              (progn
                                (close out)
                                (return-from partition-in-file-to-outs))))
              (close out)
              (incf i))))))
