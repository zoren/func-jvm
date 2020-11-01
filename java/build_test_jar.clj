#!/usr/bin/env bb

(ns build-test-java
  (:require
   [babashka.process :refer [$ check]]))

(check ($ javac -d . PublicInstanceField.java))
(check ($ jar cf experimentation.jar experimentation))
