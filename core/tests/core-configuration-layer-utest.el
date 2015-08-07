;;; core-configuration-layer-utest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-used-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-used-layers--result-order-is-not-reversed ()
  (mocker-let ((configuration-layer/make-layer
                (x)
                ((:input '(layer3) :output 'layer3)
                 (:input '(layer2) :output 'layer2)
                 (:input '(layer1) :output 'layer1))))
    (let* ((input '(layer1 layer2 layer3))
           (result (configuration-layer//declare-used-layers input)))
      (should (equal result input)))))

(ert-deftest test-declare-used-layers--ignore-not-found-layer ()
  (mocker-let ((configuration-layer/make-layer
                (x)
                ((:input '(layer3) :output 'layer3)
                 (:input '(layer2-not-found) :output nil)
                 (:input '(layer1) :output 'layer1))))
              (let* ((input '(layer1 layer2-not-found layer3))
                     (expected '(layer1 layer3))
                     (result (configuration-layer//declare-used-layers input)))
                (should (equal result expected)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-layer
;; ---------------------------------------------------------------------------

(ert-deftest test-make-layer--input-is-a-symbol ()
  (let ((input 'testlayer)
        (expected (cfgl-layer "testlayer"
                              :name 'testlayer
                              :dir "/a/dummy/path/testlayer/"
                              :ext-dir "/a/dummy/path/testlayer/extensions/")))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,input) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer/make-layer input)))
        (should (equal result expected))))))

(ert-deftest test-make-layer--input-is-a-list ()
  (let ((input '(testlayer :variables var1 t var2 nil))
        (expected (cfgl-layer "testlayer"
                              :name 'testlayer
                              :dir "/a/dummy/path/testlayer/"
                              :ext-dir "/a/dummy/path/testlayer/extensions/"
                              :variables '(var1 t var2 nil))))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,(car input)) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer/make-layer input)))
        (should (equal result expected))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//set-layers-variables
;; ---------------------------------------------------------------------------

(ert-deftest test-set-layers-variables--none ()
  (let ((input `(,(cfgl-layer "testlayer"
                              :name 'testlayer
                              :dir "/a/path/"
                              :ext-dir "/a/path/extensions/")))
        (var 'foo))
    (configuration-layer//set-layers-variables input)
    (should (eq var 'foo))))

(ert-deftest test-set-layers-variables--one-value ()
  (let ((input `(,(cfgl-layer "testlayer"
                              :name 'testlayer
                              :dir "/a/path/"
                              :ext-dir "/a/path/extensions/"
                              :variables '(var1 'bar)))))
    (setq var1 'foo)
    (configuration-layer//set-layers-variables input)
    (should (eq var1 'bar))))

(ert-deftest test-set-layers-variables--multiple-values ()
  (let ((input `(,(cfgl-layer "testlayer"
                              :name 'testlayer
                              :dir "/a/path/"
                              :ext-dir "/a/path/extensions/"
                              :variables '(var1 'bar1 var2 'bar2 var3 'bar3)))))
    (setq var1 'foo)
    (setq var2 'foo)
    (setq var3 'foo)
    (configuration-layer//set-layers-variables input)
    (should (eq var1 'bar1))
    (should (eq var2 'bar2))
    (should (eq var3 'bar3))))

(ert-deftest test-set-layers-variables--odd-number-of-values ()
  (let ((input `(,(cfgl-layer "testlayer"
                              :name 'testlayer
                              :dir "/a/path/"
                              :ext-dir "/a/path/extensions/"
                              :variables '(var1 'bar var2)))))
    (mocker-let
     ((spacemacs-buffer/warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (setq var1 'foo)
     (setq var2 'foo)
     (configuration-layer//set-layers-variables input)
     (should (eq var1 'bar))
     (should (eq var2 'foo)))))

;; ---------------------------------------------------------------------------
;; configuration-layer/make-package
;; ---------------------------------------------------------------------------

(ert-deftest test-make-package--input-is-a-symbol ()
  (let* ((input 'testpkg)
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :location 'elpa
                                 :step nil))
         (result (configuration-layer/make-package input)))
    (should (equal result expected))))

(ert-deftest test-make-package--input-is-a-list ()
  (let* ((input '(testpkg :location local :step pre))
         (expected (cfgl-package "testpkg"
                                 :name 'testpkg
                                 :location 'local
                                 :step 'pre))
         (result (configuration-layer/make-package input)))
    (should (equal result expected))))

;; ---------------------------------------------------------------------------
;; configuration-layer/get-packages
;; ---------------------------------------------------------------------------

(ert-deftest test-get-packages--symbols-only ()
  (let* ((layer1 (cfgl-layer "testlayer1" :name 'testlayer1 :dir "/path"))
         (layers (list layer1))
         (testlayer1-packages '(pkg1 pkg2 pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer1/init-pkg1 nil)
    (defun testlayer1/init-pkg2 nil)
    (defun testlayer1/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal '([object cfgl-package "pkg3" pkg3 testlayer1 nil nil elpa nil]
                                 [object cfgl-package "pkg2" pkg2 testlayer1 nil nil elpa nil]
                                 [object cfgl-package "pkg1" pkg1 testlayer1 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--lists-only ()
  (let* ((layer1 (cfgl-layer "testlayer1" :name 'testlayer1 :dir "/path"))
         (layers (list layer1))
         (testlayer1-packages '((pkg1 :location elpa)
                                (pkg2 :location recipe)
                                (pkg3 :location local :step pre)))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer1/init-pkg1 nil)
    (defun testlayer1/init-pkg2 nil)
    (defun testlayer1/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal '([object cfgl-package "pkg3" pkg3 testlayer1 nil nil local pre]
                                 [object cfgl-package "pkg2" pkg2 testlayer1 nil nil recipe nil]
                                 [object cfgl-package "pkg1" pkg1 testlayer1 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--symbols-and-lists ()
  (let* ((layer1 (cfgl-layer "testlayer1" :name 'testlayer1 :dir "/path"))
         (layers (list layer1))
         (testlayer1-packages '(pkg1
                                (pkg2 :location recipe)
                                (pkg3 :location local :step pre)
                                pkg4))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer1/init-pkg1 nil)
    (defun testlayer1/init-pkg2 nil)
    (defun testlayer1/init-pkg3 nil)
    (defun testlayer1/init-pkg4 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal '([object cfgl-package "pkg4" pkg4 testlayer1 nil nil elpa nil]
                                 [object cfgl-package "pkg3" pkg3 testlayer1 nil nil local pre]
                                 [object cfgl-package "pkg2" pkg2 testlayer1 nil nil recipe nil]
                                 [object cfgl-package "pkg1" pkg1 testlayer1 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pkg2-has-no-owner-because-no-init-function ()
  (let* ((layer2 (cfgl-layer "testlayer2" :name 'testlayer2 :dir "/path"))
         (layers (list layer2))
         (testlayer2-packages '(pkg1 pkg2 pkg3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer2/init-pkg1 nil)
    (defun testlayer2/init-pkg3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 1))))
                (should (equal '([object cfgl-package "pkg3" pkg3 testlayer2 nil nil elpa nil]
                                 [object cfgl-package "pkg2" pkg2 nil nil nil elpa nil]
                                 [object cfgl-package "pkg1" pkg1 testlayer2 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pre-init-function ()
  (let* ((layer3 (cfgl-layer "testlayer3" :name 'testlayer3 :dir "/path"))
         (layer4 (cfgl-layer "testlayer4" :name 'testlayer4 :dir "/path"))
         (layers (list layer3 layer4))
         (testlayer3-packages '(pkg1))
         (testlayer4-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer3/init-pkg1 nil)
    (defun testlayer4/pre-init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "pkg1" pkg1 testlayer3 (testlayer4) nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--post-init-function ()
  (let* ((layer3 (cfgl-layer "testlayer3" :name 'testlayer3 :dir "/path"))
         (layer5 (cfgl-layer "testlayer5" :name 'testlayer5 :dir "/path"))
         (layers (list layer3 layer5))
         (testlayer3-packages '(pkg1))
         (testlayer5-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer3/init-pkg1 nil)
    (defun testlayer5/post-init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "pkg1" pkg1 testlayer3 nil (testlayer5) elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pre-and-post-init-functions ()
  (let* ((layer3 (cfgl-layer "testlayer3" :name 'testlayer3 :dir "/path"))
         (layer6 (cfgl-layer "testlayer6" :name 'testlayer6 :dir "/path"))
         (layers (list layer3 layer6))
         (testlayer3-packages '(pkg1))
         (testlayer6-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer3/init-pkg1 nil)
    (defun testlayer6/pre-init-pkg1 nil)
    (defun testlayer6/post-init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "pkg1" pkg1 testlayer3 (testlayer6) (testlayer6) elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--several-init-functions-last-one-is-the-owner ()
  (let* ((layer7 (cfgl-layer "testlayer7" :name 'testlayer7 :dir "/path"))
         (layer8 (cfgl-layer "testlayer8" :name 'testlayer8 :dir "/path"))
         (layers (list layer7 layer8))
         (testlayer7-packages '(pkg1))
         (testlayer8-packages '(pkg1))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer7/init-pkg1 nil)
    (defun testlayer8/init-pkg1 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 1)
                                     (:output nil :occur 1)
                                     (:output t :occur 1)
                                     (:output nil :occur 1)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "pkg1" pkg1 testlayer8 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--pre-extensions-backward-compatibility ()
  (let* ((layer1 (cfgl-layer "testlayer1" :name 'testlayer1 :dir "/path"))
         (layers (list layer1))
         (testlayer1-packages '(pkg1))
         (testlayer1-pre-extensions '(ext1 ext2 ext3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer1/init-pkg1 nil)
    (defun testlayer1/init-ext1 nil)
    (defun testlayer1/init-ext2 nil)
    (defun testlayer1/init-ext3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 2)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "ext3" ext3 testlayer1 nil nil local pre]
                                 [object cfgl-package "ext2" ext2 testlayer1 nil nil local pre]
                                 [object cfgl-package "ext1" ext1 testlayer1 nil nil local pre]
                                 [object cfgl-package "pkg1" pkg1 testlayer1 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--post-extensions-backward-compatibility ()
  (let* ((layer1 (cfgl-layer "testlayer1" :name 'testlayer1 :dir "/path"))
         (layers (list layer1))
         (testlayer1-packages '(pkg1))
         (testlayer1-post-extensions '(ext1 ext2 ext3))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer1/init-pkg1 nil)
    (defun testlayer1/init-ext1 nil)
    (defun testlayer1/init-ext2 nil)
    (defun testlayer1/init-ext3 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 2)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "ext3" ext3 testlayer1 nil nil local post]
                                 [object cfgl-package "ext2" ext2 testlayer1 nil nil local post]
                                 [object cfgl-package "ext1" ext1 testlayer1 nil nil local post]
                                 [object cfgl-package "pkg1" pkg1 testlayer1 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

(ert-deftest test-get-packages--extensions-backward-compatibility ()
  (let* ((layer1 (cfgl-layer "testlayer1" :name 'testlayer1 :dir "/path"))
         (layers (list layer1))
         (testlayer1-packages '(pkg1))
         (testlayer1-pre-extensions '(ext1 ext2 ext3))
         (testlayer1-post-extensions '(ext4 ext5 ext6))
         (mocker-mock-default-record-cls 'mocker-stub-record))
    (defun testlayer1/init-pkg1 nil)
    (defun testlayer1/init-ext1 nil)
    (defun testlayer1/init-ext2 nil)
    (defun testlayer1/init-ext3 nil)
    (defun testlayer1/init-ext4 nil)
    (defun testlayer1/init-ext5 nil)
    (defun testlayer1/init-ext6 nil)
    (mocker-let ((file-exists-p (f) ((:output t :occur 2)))
                 (configuration-layer/layer-usedp (l) ((:output t :occur 2))))
                (should (equal '([object cfgl-package "ext6" ext6 testlayer1 nil nil local post]
                                 [object cfgl-package "ext5" ext5 testlayer1 nil nil local post]
                                 [object cfgl-package "ext4" ext4 testlayer1 nil nil local post]
                                 [object cfgl-package "ext3" ext3 testlayer1 nil nil local pre]
                                 [object cfgl-package "ext2" ext2 testlayer1 nil nil local pre]
                                 [object cfgl-package "ext1" ext1 testlayer1 nil nil local pre]
                                 [object cfgl-package "pkg1" pkg1 testlayer1 nil nil elpa nil])
                               (configuration-layer/get-packages layers))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//directory-type
;; ---------------------------------------------------------------------------

(ert-deftest test-directory-type--input-is-a-file ()
  (let ((input "/a/path/to/a/layer/file"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--category ()
  (let ((input (concat configuration-layer-contrib-directory "!vim/")))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (eq 'category (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--input-is-an-empty-directory ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--input-is-directory-and-not-a-layer ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("toto.el" "tata.el") :occur 1))))
     (should (null (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-packages.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("packages.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-extensions.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("extensions.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-config.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("config.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-keybindings.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("keybindings.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

(ert-deftest test-directory-type--layer-with-funcs.el ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1)))
      (directory-files
       (directory &optional full match nosort)
       ((:record-cls 'mocker-stub-record :output '("funcs.el") :occur 1))))
     (should (eq 'layer (configuration-layer//directory-type input))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//get-category-from-path
;; ---------------------------------------------------------------------------

(ert-deftest test-get-category-from-path--input-is-a-file ()
  (let ((input "/a/path/to/a/file"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (should (null (configuration-layer//get-category-from-path input))))))

(ert-deftest test-get-category-from-path--input-is-a-regular-directory ()
  (let ((input "/a/path/to/a/layer/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (null (configuration-layer//get-category-from-path input))))))

(ert-deftest test-get-category-from-path--return-category ()
  (let ((input "/a/path/to/a/!cat/"))
    (mocker-let
     ((file-directory-p (f)
                        ((:record-cls 'mocker-stub-record :output t :occur 1))))
     (should (eq 'cat (configuration-layer//get-category-from-path input))))))
