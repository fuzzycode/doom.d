;;; private/json/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "jq")
  (warn! "jq not found. jq packages will not function"))
