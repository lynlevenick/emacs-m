;;; m.el --- Memoization wrappers around functions -*- lexical-binding: t -*-

;; Author: Lyn Levenick
;; Package-Requires: ((emacs "26.3"))
;; Package-Version: 2.0.0
;; URL: https://github.com/lynlevenick/emacs-memo

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute and/or
;; modify it under the terms of the Apache License Version 2.

;;; Commentary:

;; Provides a macro, ‘m-defun’, which memoizes function
;; definitions directly. By memoizing function bodies
;; directly, improved byte-compiler output over generic
;; solutions can be produced in some situations. The type
;; of memoization and when it is cleared may be configured.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defconst m--sentinel (make-symbol "m--sentinel")
  "Sentinel value for ‘m-defun’ signaling no value present.

Not interned; the value bound to ‘m--sentinel’ is tested at
call time.")

(defvar m--variables nil
  "Variables to be bound for current expansion in ‘m-defun’.

Binding takes place around the entire declaration, used to
share definitions between expansions.")

(defun m--form-or-progn (forms)
  "Return a length 1 form of FORMS."

  (if (> (length forms) 1)
      `(progn ,@forms)
    (car forms)))

(defun m--register-variables (&rest decls)
  "Register DECLS with ‘m--variables’."

  (cl-loop for decl in decls
           if (alist-get (car decl) m--variables)
           do (user-error "Symbol %s already bound" (car decl))
           else
           do (push decl m--variables)))

(defmacro m--with-symbols (key names &rest body)
  "Bind NAMES to interned symbols then eval BODY.

Each element within NAMES is a SYMBOL.

Each SYMBOL specifies that the variable named by SYMBOL should
be bound to a symbol constructed using ‘intern’ and keyed with
the value of KEY and each SYMBOL for uniqueness. Byte-compiles
are consistent when using interned symbols, but they allow any
code to access them."
  (declare (indent 2))

  `(let ,(cl-loop for name in names
                  collect `(,name (intern (concat "m--"
                                                  (symbol-name ,key) "--"
                                                  ,(symbol-name name)))))
     ,@body))

(defmacro memo (&rest _SPECS)
  "Do not evaluate any arguments, and return nil.

If a ‘memo’ form appears before a ‘m-defun’ body, SPECS
specifies memoization configuration for the function,
modifying the function declaration."

  nil)

;; list of functions that define variables for memoization??
;;   could be skipped if a final pass does definitions, but
;;   how would it know about them?
;; list of functions that transform function body
;; list of functions that wrap function body
;; latter two insert un-evaluated placeholders (somehow??)
;; which are replaced in a final pass, erroring for undefined

;;; :storage

;; latest

(defun m--storage-latest (name arglist _props body)
  "See ‘m--transform-body-list’.

NAME, ARGLIST, PROPS, and BODY have the same meaning as in
‘m--transform-body-list’."

  (m--with-symbols name (prev-args prev-value current-args)
    (pcase (length arglist)
      (0 (m--register-variables `(,prev-value m--sentinel))
         `((if (eq ,prev-value m--sentinel)
               (setf ,prev-value ,body)
             ,prev-value)))
      (1 (m--register-variables `(,prev-args m--sentinel)
                                   `(,prev-value nil))
         `((if (equal ,prev-args ,@arglist)
               ,prev-value
             (prog1 ; body may fail, doesn't set prev-args if so
                 (setf ,prev-value ,body)
               (setf ,prev-args ,@arglist)))))
      (_ (m--register-variables `(,prev-args m--sentinel)
                                   `(,prev-value nil))
         `((let ((,current-args (list ,@arglist)))
             (if (equal ,prev-args ,current-args)
                 ,prev-value
               (prog1 ; body may fail, doesn't set prev-args if so
                   (setf ,prev-value ,body)
                 (setf ,prev-args ,current-args)))))))))

(defun m--storage-latest-invalidate (name arglist _props)
  "See ‘m--invalidate-method-list’.

NAME, ARGLIST, and PROPS have the same meaning as in
‘m--invalidate-method-list’."

  (m--with-symbols name (prev-args prev-value)
    `(setf ,(if (> (length arglist) 0) prev-args prev-value)
           m--sentinel)))

;; hash

(defun m--storage-hash (name arglist _props body)
  "See ‘m--transform-body-list’.

NAME, ARGLIST, PROPS, and BODY have the same meaning as in
‘m--transform-body-list’."

  (m--with-symbols name (prev-calls cached current-args)
    (m--register-variables
     `(,prev-calls (make-hash-table :test 'equal :weakness t)))
    (pcase (length arglist)
      (0 (user-error "Memoization into hash by zero arguments"))
      (1 `((let ((,cached (gethash ,@arglist ,prev-calls m--sentinel)))
             (if (eq ,cached m--sentinel)
                 (puthash ,@arglist ,body ,prev-calls)
               ,cached))))
      (_ `((let* ((,current-args (list ,@arglist))
                  (,cached (gethash ,current-args ,prev-calls
                                    m--sentinel)))
             (if (eq ,cached m--sentinel)
                 (puthash ,current-args ,body ,prev-calls)
               ,cached)))))))

(defun m--storage-hash-invalidate (name _arglist _props)
  "See ‘m--invalidate-method-list’.

NAME, ARGLIST, and PROPS have the same meaning as in
‘m--invalidate-method-list’."

  (m--with-symbols name (prev-calls)
    `(clrhash ,prev-calls)))

;;; :invalidate-on

(defvar m--invalidate-method-list
  `((latest . ,#'m--storage-latest-invalidate)
    (hash . ,#'m--storage-hash-invalidate))
  "List associating storage type to function invalidating that storage.

Each element takes the form (STORAGE . FN) where STORAGE is
the type of storage and FN is its macro expansion.

FN is called as (FN NAME ARGLIST PROPS) and returns a form
invalidating storage for the function NAME.")

;; edit

(defun m--invalidate-on-edit (name arglist props decl)
  "See ‘m--transform-decl-list’.

NAME, ARGLIST, PROPS, and DECL have the same meaning as in
‘m--transform-decl-list’."

  (m--with-symbols name (after-change)
    `((defun ,after-change (&rest _)
        ,(let* ((storage (or (plist-get props :storage) 'latest))
                (invalidation (alist-get storage m--invalidate-method-list)))
           (if invalidation
               (funcall invalidation name arglist props)
             (user-error "Unknown storage: %s" storage))))
      (add-hook 'after-change-functions #',after-change)
      ,@decl)))

;;; meta pass - define variables

(defun m--define-variables (_name _arglist props decl)
  "See ‘m--transform-decl-list’.

NAME, ARGLIST, PROPS, and DECL have the same meaning as in
‘m--transform-decl-list’."

  (prog1
      (let ((buffer-local (plist-get props :buffer-local)))
        (if (and lexical-binding (not buffer-local))
            ;; lexical binding and global, can let-over-defun
            `((let ,m--variables
                ,@decl))
          ;; buffer local or dynamic binding, can't let-over-defun
          (let ((defvar-fn (if buffer-local #'defvar-local #'defvar)))
            `(,@(cl-loop for decl in m--variables
                         collect `(,defvar-fn ,@decl))
              ,@decl))))
    (setf m--variables nil)))

;;; body and decl transformations

(defvar m--transform-body-list
  `((:storage
     (:default . ,#'m--storage-latest)
     (latest . ,#'m--storage-latest)
     (hash . ,#'m--storage-hash)))
  "List of memoization properties and their expansion.

Each element takes the form (PROP HANDLERS...) where HANDLERS
is a list of the form (VALUE . FN) where VALUE is the value of
PROP in PROPS and FN is its macro expansion.
Keyword :default may be used instead of VALUE to specify
transformation to take place if no PROP matches or is specified
in PROPS.

Evaluated in order against PROPS.

FN is called as (FN NAME ARGLIST PROPS BODY) and returns a new
function body. NAME, ARGLIST, PROPS, and BODY have the same
meaning as in ‘m-defun’.")

(defvar m--transform-decl-list
  `((:invalidate-on
     (edit . ,#'m--invalidate-on-edit))
    (:m--define-variables
     (:default . ,#'m--define-variables)))
  "See ‘m--transform-body-list’.

FN is instead called as (FN NAME ARGLIST PROPS DECL) and
returns a new declaration.")

;; would like to have:
;; :key-on  Additional key to memoization.
;;          May be nil or an expression to evaluate. Default is nil.
;;          FIXME: implement this - it would have to essentially
;;          extend the arglist for all _inner_ passes?? eval
;;          outside to inside? lmao

;;;###autoload
(defmacro m-defun (name arglist &rest body)
  "Define NAME as a memoized function.

NAME, ARGLIST, DOCSTRING, DECL, and BODY have the same meaning
as in ‘defun’.

MEMO is an declaration, optional, of the form (memo PROPS...)
where PROPS is a property list, interpreted as follows:

:buffer-local   Whether cache is buffer-local.
                May be nil or t. Default is nil.
:invalidate-on  When cache is invalidated.
                May be nil or the symbol ‘edit’. Default is nil.
:storage        Backing storage for the memoization.
                May be one of the symbols ‘latest’ or ‘hash’.
                Default is ‘latest’.

\(fn NAME ARGLIST &optional DOCSTRING DECL MEMO &rest BODY)"
  (declare (debug defun) (doc-string 3) (indent 2))

  (let ((narrow-arglist ; excludes keywords
         (cl-loop for arg in arglist
                  unless (or (eq arg '&optional)
                             (eq arg '&rest))
                  collect arg))
        (body-prefix) (props) (decl))

    ;; Take docstring and decl
    (when (eq (type-of (car body)) 'string)
      (push (pop body) body-prefix))
    (when (eq (car-safe (car body)) 'declare)
      (push (pop body) body-prefix))
    (cl-callf nreverse body-prefix)

    ;; Take props
    (when (eq (car-safe (car body)) 'memo)
      (setf props (cdr (pop body))))

    ;; Construct body
    (cl-callf m--form-or-progn body)
    (cl-loop for (prop . handlers) in m--transform-body-list
             do (when-let ((prop-val (or (plist-get props prop) :default))
                           (handler (alist-get prop-val handlers)))
                  (setf body (funcall handler name narrow-arglist props body))))

    ;; Construct decl
    (setf decl `((defun ,name ,arglist ,@body-prefix ,@body)))
    (cl-loop for (prop . handlers) in m--transform-decl-list
             do (when-let ((prop-val (or (plist-get props prop) :default))
                           (handler (alist-get prop-val handlers)))
                  (setf decl (funcall handler name narrow-arglist props decl))))

    ;; Return a single form
    (m--form-or-progn decl)))

(provide 'm)
;;; m.el ends here
