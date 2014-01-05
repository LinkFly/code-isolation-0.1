(asdf:defsystem :code-isolation  
  :depends-on (:lift)
  :components ((:file "code-isolation")))

(defmethod perform ((op test-op) (c (eql (find-system :code-isolation))))
  (funcall (find-symbol "DESCRIBE" :code-isolation)
           (funcall (find-symbol "RUN-TESTS" :code-isolation) 
                    :suite (find-symbol "CODE-ISOLATION-TESTS" :code-isolation))))

