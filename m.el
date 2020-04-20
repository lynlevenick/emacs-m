;;; m --- Memoization wrappers around functions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-macs))

(defmacro m--with-symbols (key names &rest body)
  "Bind NAMES to interned symbols then eval BODY.

Each element within NAMES is a SYMBOL.

Each SYMBOL specifies that the variable named by SYMBOL should
be bound to a symbol constructed using ‘intern’ and keyed with
the value of KEY and each SYMBOL for uniqueness. Since symbols
are interned they are accessible normally."
  (declare (indent 1))

  (let ((key-name (gensym "key-name")))
    `(let ((,key-name (symbol-name ,key)))
       (let ,(cl-loop for name in names
                      collect `(,name (intern (concat "m--"
                                                      ,key-name "--"
                                                      (symbol-name ',name)))))
         ,@body))))

(defconst m--sentinel (make-symbol "m--sentinel")
  "Sentinel value for ‘m-defun’ signaling an uncalled method.")

(defun m--latest (func arglist body props)
  "Transform BODY to memoize the previous invocation.

Return a list (GLOBAL BODY) which specifies global-scoped
declarations and the transformed body.

FUNC is the name of the function. ARGLIST is the arguments that the
function will receive. PROPS has the same meaning as in ‘m-defun’."

  ;; TODO: let over defun may work better here when not buffer-local
  ;; TODO: Abstract over N arguments instead of special casing 0-1-many?
  ;; TODO: Can any architecture from here be generalized?

  (m--with-symbols func (prev-args prev-value after-change current-args)
    (let ((def-fn (if (plist-get props :buffer-local) #'defvar-local #'defvar))
          (arity (length arglist)))
      `((,@(when (> arity 0)
             `((,def-fn ,prev-args m--sentinel)))
         (,def-fn ,prev-value ,(if (> arity 0) 'nil 'm--sentinel))
         ,@(pcase (plist-get props :clear-on)
            ('nil nil)
            ('edit
             `((defun ,after-change (&rest _)
                 (setf ,(if (> arity 0) prev-args prev-value) m--sentinel))
               (add-hook 'after-change-functions #',after-change)))
            (clear-on (user-error "Unknown clear-on: %s" clear-on))))
        (,(pcase arity
           (0 `(if (eq ,prev-value m--sentinel)
                   (setf ,prev-value (progn ,@body))
                 ,prev-value))
           (1 `(if (equal ,prev-args ,@arglist)
                   ,prev-value
                 (prog1
                     (setf ,prev-value (progn ,@body))
                   (setf ,prev-args ,@arglist))))
           (_ `(let ((,current-args (list ,@arglist)))
                   (if (equal ,prev-args ,current-args)
                       ,prev-value
                     (prog1
                         (setf ,prev-value (progn ,@body))
                       (setf ,prev-args ,current-args)))))))))))

(defun m--hash (func arglist body props)
  "Transform BODY to memoize previous invocations into a hash.

Return a list (GLOBAL BODY) which specifies global-scoped
declarations and the transformed body.

FUNC is the name of the function. ARGLIST is the arguments that the
function will receive. PROPS has the same meaning as in ‘m-defun’."

  ;; TODO: See ‘m--latest’ TODOs

  (m--with-symbols func (prev-calls after-change cached current-args)
    (let ((def-fn (if (plist-get props :buffer-local) #'defvar-local #'defvar))
          (arity (length arglist)))
      (when (= arity 0)
        (user-error "Memoization into hash by zero arguments"))
      `(((,def-fn ,prev-calls (make-hash-table :test 'equal :weakness t))
         ,@(pcase (plist-get props :clear-on)
             ('nil nil)
             ('edit
              `((defun ,after-change (&rest _)
                  (clrhash ,prev-calls))
                (add-hook 'after-change-functions #',after-change)))
             (clear-on (user-error "Unknown clear-on: %s" clear-on))))
        (,(pcase arity
            (1 `(let ((,cached (gethash ,@arglist ,prev-calls m--sentinel)))
                  (if (eq ,cached m--sentinel)
                      (puthash ,@arglist (progn ,@body) ,prev-calls)
                    ,cached)))
            (_ `(let* ((,current-args (list ,@arglist))
                       (,cached (gethash ,current-args ,prev-calls m--sentinel)))
                  (if (eq ,cached m--sentinel)
                      (puthash ,current-args (progn ,@body) ,prev-calls)
                    ,cached)))))))))

;;;###autoload
(defmacro m-defun (name arglist &rest body)
  "Define NAME as a memoized function.

NAME, ARGLIST, DOCSTRING, DECL, and BODY have the same meaning as in ‘defun’.
Optional PROPS are a group of configuration options for the memoization.

:buffer-local  Whether storage is buffer-local.
               May be nil or t. Default is nil.
:clear-on      When storage is invalidated.
               May be nil or the symbol ‘edit’. Default is nil.
:storage       Storage to be used during memoization.
               May be the symbol ‘latest’ or the symbol ‘hash’.
               Default is ‘latest’.

\(fn NAME ARGLIST &optional DOCSTRING DECL PROPS... &rest BODY)"
  (declare (debug defun) (doc-string 3) (indent 2))

  (let ((body-prefix nil)
        (props nil))
    ;; Take docstring and decl
    (when (eq (type-of (car body)) 'string)
      (push (pop body) body-prefix))
    (when (eq (car-safe (car body)) 'declare)
      (push (pop body) body-prefix))
    (cl-callf reverse body-prefix)
    ;; Take props
    (while (keywordp (car body))
      (push (pop body) props)
      (push (pop body) props))
    (cl-callf reverse props)
    ;; Construct function
    (pcase-let* ((storage (or (plist-get props :storage) 'latest))
                 (memo-fn (intern (concat "m--" (symbol-name storage))))
                 (`(,global ,transformed-body)
                  (funcall memo-fn name
                           (cl-loop for arg in arglist
                                    unless (or (eq arg '&optional)
                                               (eq arg '&rest))
                                    collect arg)
                           body props)))
      `(progn
         ,@global
         (defun ,name ,arglist
           ,@body-prefix
           ,@transformed-body)))))

(provide 'm)
;;; m.el ends here
