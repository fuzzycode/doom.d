;;; private/spelling/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "aspell")
    (warn! "Couldn't find aspell executable; spell checker will not work"))
