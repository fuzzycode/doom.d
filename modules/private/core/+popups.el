;;; private/core/+popups.el -*- lexical-binding: t; -*-

(set-popup-rule! "^\\*Shell Command Output\\*$" :quit 'other :side 'bottom :height 40 :select nil :actions '(+bl/special-mode-action-fn))
