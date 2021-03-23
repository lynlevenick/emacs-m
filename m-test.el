;;; Tests for m.el -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'ert)
(require 'm)

(defmacro m-test-valid (arity &rest keywords)
  "Define a test for a function definition of ARITY with KEYWORDS.

Used to ensure valid runnable code is generated for a given set of keywords."
  (let* ((base-name (concat "test-valid_"
                            (cl-loop for keyword in (sort (cl-loop for keyword in keywords
                                                                   if (keywordp keyword)
                                                                   collect keyword)
                                                          #'string<)
                                     concat (substring (symbol-name keyword) 1)
                                     concat "="
                                     concat (symbol-name (plist-get keywords keyword))
                                     concat "_")
                            "arity="
                            (number-to-string arity)))
         (test-name (intern base-name))
         (var-name (intern (concat base-name "_value")))
         (fn-name (intern (concat base-name "_fn"))))
    `(progn
       (declare-function ,fn-name ,buffer-file-name)
       (ert-deftest ,test-name ()
         (defvar ,var-name 0)
         (m-defun ,fn-name
             ,(cl-loop for i from 1 to arity
                       collect (intern (concat "arg" (number-to-string i))))
           (memo ,@keywords)
           (cl-incf ,var-name))

         (should (= (,fn-name ,@(cl-loop for i from 1 to arity collect i))
                    (,fn-name ,@(cl-loop for i from 1 to arity collect i))))))))

(m-test-valid 0)
(m-test-valid 1)
(m-test-valid 2)
(m-test-valid 0 :invalidate-on edit)
(m-test-valid 1 :invalidate-on edit)
(m-test-valid 2 :invalidate-on edit)
(m-test-valid 0 :buffer-local t)
(m-test-valid 1 :buffer-local t)
(m-test-valid 2 :buffer-local t)
(m-test-valid 0 :invalidate-on edit :buffer-local t)
(m-test-valid 1 :invalidate-on edit :buffer-local t)
(m-test-valid 2 :invalidate-on edit :buffer-local t)
(m-test-valid 1 :storage hash)
(m-test-valid 2 :storage hash)
(m-test-valid 1 :storage hash :invalidate-on edit)
(m-test-valid 2 :storage hash :invalidate-on edit)
(m-test-valid 1 :storage hash :buffer-local t)
(m-test-valid 2 :storage hash :buffer-local t)
(m-test-valid 1 :storage hash :invalidate-on edit :buffer-local t)
(m-test-valid 2 :storage hash :invalidate-on edit :buffer-local t)

;; latest; 1 argument

(defvar m-test-latest-1-value 0)
(m-defun m-test-latest-1 (_)
  (cl-incf m-test-latest-1-value))

(ert-deftest test-latest-1 ()
  (let* ((result-0 (m-test-latest-1 'a))
         (result-1 (m-test-latest-1 'a))
         (result-2 (m-test-latest-1 'b))
         (result-3 (m-test-latest-1 'b)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 (m-test-latest-1 'a)))))

;; latest; 2+ arguments

(defvar m-test-latest-many-value 0)
(m-defun m-test-latest-many (_0 _1)
  (cl-incf m-test-latest-many-value))

(ert-deftest test-latest-many ()
  (let* ((result-0 (m-test-latest-many 'a 'b))
         (result-1 (m-test-latest-many 'a 'b))
         (result-2 (m-test-latest-many 'c 'b))
         (result-3 (m-test-latest-many 'c 'b))
         (result-4 (m-test-latest-many 'c 'd)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 result-4))
    (should (/= result-4 (m-test-latest-many 'c 'e)))))

;; latest; 1 keyword argument

(defvar m-test-latest-keyword-value 0)
(m-defun m-test-latest-keyword (&optional _0)
  (cl-incf m-test-latest-keyword-value))

(ert-deftest test-latest-keyword ()
  (let* ((result-0 (m-test-latest-keyword 'a))
         (result-1 (m-test-latest-keyword 'a))
         (result-2 (m-test-latest-keyword))
         (result-3 (m-test-latest-keyword)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 (m-test-latest-keyword 'a)))))

;; latest; 0 arguments; buffer-local

(defvar m-test-latest-buffer-local-value 0)
(m-defun m-test-latest-buffer-local ()
  (memo :buffer-local t)

  (cl-incf m-test-latest-buffer-local-value))

(ert-deftest test-latest-buffer-local ()
  (let* ((result-0 nil)
         (result-1 nil)
         (result-2 nil)
         (result-3 nil)
         (result-4 nil))
    (with-temp-buffer
      (setf result-0 (m-test-latest-buffer-local)
            result-1 (m-test-latest-buffer-local)))
    (with-temp-buffer
      (setf result-2 (m-test-latest-buffer-local)
            result-3 (m-test-latest-buffer-local)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))))

;; latest; 0 arguments; on edit

(defvar m-test-latest-invalidate-value 0)
(m-defun m-test-latest-invalidate ()
  (memo :invalidate-on edit)

  (cl-incf m-test-latest-invalidate-value))

(ert-deftest test-latest-invalidate ()
  (let* ((result-0 nil)
         (result-1 nil)
         (result-2 nil)
         (result-3 nil))
    (with-temp-buffer
      (setf result-0 (m-test-latest-invalidate)
            result-1 (m-test-latest-invalidate))
      (insert "characters")
      (setf result-2 (m-test-latest-invalidate)
            result-3 (m-test-latest-invalidate)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))))

;; hash; 0 arguments

(ert-deftest test-hash-0 ()
  (should-error
   (m-defun m-test-hash-0 ()
     (memo :storage hash)

     0)))

;; hash; 1 argument

(defvar m-test-hash-1-value 0)
(m-defun m-test-hash-1 (_)
  (memo :storage hash)

  (cl-incf m-test-hash-1-value))

;; Hash tests assume nothing will cause garbage collection of the
;; keys or values during execution because hash storage is weak
;; TODO: let-bind GC variables to enforce that?
(ert-deftest test-hash-1 ()
  (let* ((result-0 (m-test-hash-1 'a))
         (result-1 (m-test-hash-1 'a))
         (result-2 (m-test-hash-1 'b))
         (result-3 (m-test-hash-1 'b))
         (result-4 (m-test-hash-1 'a)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 result-4))
    (should (= result-4 result-0))))

;; hash; 2+ arguments

(defvar m-test-hash-many-value 0)
(m-defun m-test-hash-many (_0 _1)
  (memo :storage hash)

  (cl-incf m-test-hash-many-value))

(ert-deftest test-hash-many ()
  (setf m-test-hash-many-value 0)
  (let* ((result-0 (m-test-hash-many 'a 'b))
         (result-1 (m-test-hash-many 'a 'b))
         (result-2 (m-test-hash-many 'c 'b))
         (result-3 (m-test-hash-many 'c 'b))
         (result-4 (m-test-hash-many 'c 'd))
         (result-5 (m-test-hash-many 'c 'b))
         (result-6 (m-test-hash-many 'a 'b)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 result-4))
    (should (/= result-4 result-5))
    (should (= result-5 result-3))
    (should (= result-6 result-0))))

;; ensure documentation is preserved

(m-defun m-test-docs ()
  "Return zero."
  (memo :storage latest)

  0)

(ert-deftest test-docs ()
  (should (string-equal "Return zero." (documentation #'m-test-docs))))
