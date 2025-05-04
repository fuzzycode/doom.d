;;; tools/ai/doctor.el -*- lexical-binding: t; -*-


(unless (executable-find "ollama")
  (warn! "ollama executable not found. Local llms with ollama will not work"))
