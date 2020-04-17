;;; Tests for m.el -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'ert)
(require 'm)

(defvar m-test-latest-0-value 0)
(m-defun m-test-latest-0 ()
  (cl-incf m-test-latest-0-value))

(ert-deftest m-latest-0 ()
  (setf m-test-latest-0-value 0)
  (should (= (m-test-latest-0) (m-test-latest-0))))

(defvar m-test-latest-1-value 0)
(m-defun m-test-latest-1 (_)
  (cl-incf m-test-latest-1-value))

(ert-deftest m-latest-1 ()
  (setf m-test-latest-1-value 0)
  (let* ((result-0 (m-test-latest-1 'a))
         (result-1 (m-test-latest-1 'a))
         (result-2 (m-test-latest-1 'b))
         (result-3 (m-test-latest-1 'b)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 (m-test-latest-1 'a)))))

(defvar m-test-latest-many-value 0)
(m-defun m-test-latest-many (_0 _1)
  (cl-incf m-test-latest-many-value))

(ert-deftest m-latest-many ()
  (setf m-test-latest-many-value 0)
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

(defvar m-test-latest-keyword-value 0)
(m-defun m-test-latest-keyword (&optional _0)
  (cl-incf m-test-latest-keyword-value))

(ert-deftest m-latest-keyword ()
  (setf m-test-latest-keyword-value 0)
  (let* ((result-0 (m-test-latest-keyword 'a))
         (result-1 (m-test-latest-keyword 'a))
         (result-2 (m-test-latest-keyword))
         (result-3 (m-test-latest-keyword)))
    (should (= result-0 result-1))
    (should (/= result-1 result-2))
    (should (= result-2 result-3))
    (should (/= result-3 (m-test-latest-keyword 'a)))))

(defvar m-test-latest-buffer-local-value 0)
(m-defun m-test-latest-buffer-local ()
  :buffer-local t

  (cl-incf m-test-latest-buffer-local-value))

(ert-deftest m-latest-buffer-local ()
  (setf m-test-latest-buffer-local-value 0)
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
    (should (= result-2 result-3))
    (should (/= result-3 (m-test-latest-buffer-local)))))
