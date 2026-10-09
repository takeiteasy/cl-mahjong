(in-package #:cl-mahjong)

(define-condition definition-error (error) ())

(define-condition unknown-definition (definition-error)
  ((kind :initarg :kind :reader error-kind)
   (name :initarg :name :reader error-name))
  (:report (lambda (c stream)
             (format stream "Unknown ~(~a~) ~(~a~)" (error-kind c) (error-name c)))))

(define-condition circular-extends (definition-error)
  ((chain :initarg :chain :reader error-chain))
  (:report (lambda (c stream)
             (format stream "Circular :extends: ~{~(~a~)~^ -> ~}" (error-chain c)))))

(define-condition invalid-definition (definition-error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c stream) (write-string (error-message c) stream))))

(defun invalid (control &rest args)
  (error 'invalid-definition :message (apply #'format nil control args)))

(defun keyword-of (designator)
  (intern (string designator) :keyword))

(defstruct (definition (:constructor make-definition (kind name parent clauses)))
  kind name parent clauses (compiled nil))

(defvar *definitions* (make-hash-table :test 'equal))

(defgeneric compile-definition (kind parent clauses)
  (:documentation "Compile CLAUSES of a definition of KIND into an immutable value.
PARENT is the compiled value of the :extends definition, or NIL."))

(defun definition-key (kind name)
  (cons kind (keyword-of name)))

(defun ancestors (definition)
  "Names of DEFINITION's ancestors; stops at a cycle or an unregistered parent."
  (let ((kind (definition-kind definition))
        (names '())
        (parent (definition-parent definition)))
    (loop while (and parent (not (member parent names)))
          do (push parent names)
             (let ((next (gethash (definition-key kind parent) *definitions*)))
               (setf parent (and next (definition-parent next)))))
    names))

(defun register-definition (kind name parent clauses)
  "Store a definition and discard the compiled value of everything that extends it."
  (let ((key (definition-key kind name))
        (parent (and parent (keyword-of parent))))
    (setf (gethash key *definitions*) (make-definition kind (cdr key) parent clauses))
    (loop for definition being the hash-values of *definitions*
          when (and (eq (definition-kind definition) kind)
                    (member (cdr key) (ancestors definition)))
            do (setf (definition-compiled definition) nil))
    (cdr key)))

(defun find-definition (kind name &optional chain)
  (let* ((key (definition-key kind name))
         (definition (or (gethash key *definitions*)
                         (error 'unknown-definition :kind kind :name (cdr key)))))
    (when (member key chain :test #'equal)
      (error 'circular-extends :chain (reverse (mapcar #'cdr (cons key chain)))))
    (or (definition-compiled definition)
        (setf (definition-compiled definition)
              (compile-definition
               kind
               (and (definition-parent definition)
                    (find-definition kind (definition-parent definition) (cons key chain)))
               (definition-clauses definition))))))

(defun parse-definition-body (body)
  "Split a def* body into (values parent clauses)."
  (if (eq (first body) :extends)
      (values (second body) (cddr body))
      (values nil body)))
