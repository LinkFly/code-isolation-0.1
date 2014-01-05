(defpackage :code-isolation
  (:use :cl :lift)
  (:export #:def-code-interaction #:call-isolation-code #:with-code-isolation #:*code-interaction-functions*))

(in-package :code-isolation)

(eval-when (:compile-toplevel)
  (unintern 'code-isolation-tests)) ;;Для корректной работе тестов после перекомпиляции
(deftestsuite code-isolation-tests () ()) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *code-interaction-functions* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;For tests
  (defparameter *__tmp-code-interaction-func* nil)
  (intern "__TMP-code-INTERACTION-FUNC__"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sym-to-specvar (sym &aux sym-new-name (package (symbol-package sym)))
    (setf sym-new-name (concatenate 'string "*" (symbol-name sym) "*"))
    (if package 
        (intern sym-new-name package)
      (make-symbol sym-new-name)))) 
(addtest sym-to-specvar-test 
  ;;Усовершенствование теста: сделать пакет для создаваемых символов __tmp-code-interaction-func и 
  ;;*__tmp-code-interaction-func* (эта переменная должна быть динамической) и потом удалить его
  (ensure-same (symbol-name (sym-to-specvar (make-symbol "SOME_SYMBOL")))
               "*SOME_SYMBOL*"))
               
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keywordize (sym)
    (intern (symbol-name sym) :keyword)))
(addtest keywordize-test
    (ensure-same (symbol-name (keywordize '|KEYWORDIZE|))
                 (symbol-name (find-symbol "KEYWORDIZE" :keyword))))

(defmethod call-isolation-code (specvar-sym parameters)
  (apply specvar-sym parameters))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-code-interaction (code-fun-name (&rest params) &body body &aux sym-for-specvar)
    (setf sym-for-specvar (sym-to-specvar code-fun-name))
    `(progn
       (defparameter ,sym-for-specvar ;;if code-fun-name eq my-fun then *my-fun*
         (lambda (&key ,@params)
           ,@body))
       (pushnew ',sym-for-specvar *code-interaction-functions*)
       (defmethod ,code-fun-name (&key ,@params)
         (call-isolation-code ,sym-for-specvar (list ,@(loop :for param :in params
                                                             :collect (keywordize param)
                                                             :collect param)))))))

;(unintern (find-symbol (symbol-name '#:__tmp-code-interaction-func)))
;(unintern (find-symbol (symbol-name '#:__tmp-code-interaction-func)))
;(intern (symbol-name '#:__tmp-code-interaction-func))
;(intern (symbol-name '#:*__tmp-code-interaction-func*))
(addtest def-code-interaction-test ;;Усовершенствование теста: сделать пакет для создаваемых символов __tmp-code-interaction-func и *__tmp-code-interaction-func* и потом удалить его
  (ensure-same 
   (let ((*code-interaction-functions* nil))
     (def-code-interaction __tmp-code-interaction-func (article)
       (declare (ignore article))
       (list "some-code-for-database-interaction"))
     (list (__tmp-code-interaction-func :article "'188296'")
           *code-interaction-functions*
           (let ((*__tmp-code-interaction-func* (lambda (&key article)
                                                (when (string= "'188296'" article)
                                                  (values '((186817809 "ВТ.Защитная книга для пожилых людей. Советы и рецепты")
                                                            (186832218 "ВТ.Защитная книга для пожилых людей. Советы и рецепты"))
                                                          #("ID" "SHORT_NAME"))))))
             (multiple-value-list
              (__tmp-code-interaction-func :article "'188296'")))))
   '(("some-code-for-database-interaction")
     (*__tmp-code-interaction-func*)
     (((186817809 "ВТ.Защитная книга для пожилых людей. Советы и рецепты") (186832218 "ВТ.Защитная книга для пожилых людей. Советы и рецепты")) 
      #("ID" "SHORT_NAME")))
   :test #'equalp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition code-into-with-code-isolation (error)
    ((fn-spec-sym :initarg :fn-spec-sym :accessor code-into-with-code-isolation-fn-spec-sym))
    (:report (lambda (condition stream)
               (format stream "Произошло обращение к функциональному значению связанному с символом ~S внутри макроса with-code-isolation (или к ф-ии без обрамляющих знаков \"*\").
Для выполнения кода обращающегося к базе данных, внутри макроса with-code-isolation, необходимо этот код заменить \"заглушкой\", пример:
(let ((*<символ-code-функции>* (lambda (...) <код_загушки>* ...)))
  <вызов_тестируемой_ф-ии>*)"
                       (code-into-with-code-isolation-fn-spec-sym condition)))))

  (defmacro with-code-isolation (vars-lambda-stubs &body body)
    `(progv *code-interaction-functions* 
         (loop :for fn-spec-sym :in *code-interaction-functions*
               :collect (let ((fn-spec-sym fn-spec-sym))
                          (lambda (&rest args)
                            (declare (ignore args))
                            (error 'code-into-with-code-isolation :fn-spec-sym fn-spec-sym))))
       (let (,@vars-lambda-stubs)
         ,@body))))
(addtest with-code-isolation-test ;;Усовершенствование теста: сделать пакет для создаваемых символов __tmp-code-interaction-func и *__tmp-code-interaction-func* и потом удалить его
  (ensure-same (let ((*code-interaction-functions* nil))
                 (def-code-interaction __tmp-code-interaction-func (article)
                   (declare (ignore article))
                   (list "some-code-for-database-interaction"))
                 (def-code-interaction __tmp-code-interaction-func2 (article)
                   (declare (ignore article))
                   (list "some-code-for-database-interaction2"))
                 (list 
                  (string= "ok"
                           (block isolation-err-block
                             (handler-bind ((code-into-with-code-isolation #'(lambda (condition)
                                                                             (declare (ignore condition))
                                                                             (return-from isolation-err-block "ok"))))
                               (with-code-isolation ()
                                 (__tmp-code-interaction-func :article "188296")))))
                  (eq '*__tmp-code-interaction-func2*
                           (block isolation-err-block
                             (handler-bind ((code-into-with-code-isolation #'(lambda (condition)
                                                                             (return-from isolation-err-block 
                                                                               (code-into-with-code-isolation-fn-spec-sym condition)
                                                                               ))))
                               (with-code-isolation ()
                                 (__tmp-code-interaction-func2 :article "188296")))))
                  (with-code-isolation ((*__tmp-code-interaction-func* (lambda (&rest args) 
                                                                     (declare (ignore args))
                                                                     "ok"))
                                      (*__tmp-code-interaction-func2* (lambda (&rest args) 
                                                                     (declare (ignore args))
                                                                     "ok2"))
                                      )
                    (list (string= "ok" (__tmp-code-interaction-func :article "188296"))
                          (string= "ok2" (__tmp-code-interaction-func2 :article "188296"))))))
               '(t t (t t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
