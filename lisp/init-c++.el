(defun c-header-protection ()
  "Header protection."
  (interactive)
  (let ((name buffer-file-name))
    (when name
      (setq name (upcase (replace-regexp-in-string "[^a-zA-Z]+" "_" (file-name-nondirectory name)))))
    (save-excursion
      (beginning-of-buffer)
      (insert "#ifndef _" name "\n#define _" name "\n\n")
      (end-of-buffer)
      (insert "#endif\n"))))

(eval-after-load 'autoinsert
  '(define-auto-insert
     (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
     (lambda()
       (save-excursion
         (tempo-template-c++-class)
         (goto-char (point-max))
         (c-header-protection)
         (insert "\n\n")
         (goto-char (point-min))
         ;;      (doxymacs-insert-file-comment)
         ))))


(eval-after-load 'autoinsert
  '(define-auto-insert
     (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C / C++ implementation")
     (lambda()
       ;;       (doxymacs-insert-file-comment)
       (let ((class-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
         (insert "#include \"" class-name ".h\"\n\n")
         ;;      (insert class-name "::" class-name "()\n{}\n\n")
         ;;      (insert class-name "::~" class-name "()\n{}\n\n")
         ))))

(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(add-hook 'c++-mode-hook '(lambda ()
                            (tempo-use-tag-list 'c++-tempo-tags)
                            ))

;;; Preprocessor Templates (appended to c-tempo-tags)

(tempo-define-template "c-include"
                       '("#include <" r ">" > n
                         )
                       "cincl"
                       "Insert a #include <> statement"
                       'c++-tempo-tags)

(tempo-define-template "c-ifdef"
                       '("#ifdef " (p "ifdef-clause:" clause) > n> p n
                         "#else /* !(" (s clause) ") */" n> p n
                         "#endif /* " (s clause)" */" n>
                         )
                       "cifdef"
                       "Insert a #ifdef #else #endif statement"
                       'c++-tempo-tags)

(tempo-define-template "c-ifndef"
                       '("#ifndef " (p "ifndef-clause:" clause) > n
                         "#define " (s clause) n> p n
                         "#endif /* " (s clause)" */" n>
                         )
                       "cifndef"
                       "Insert a #ifndef #define #endif statement"
                       'c++-tempo-tags)

(tempo-define-template "c-if"
                       '(> "if (" (p "if-clause:" clause) ")"  n>
                           "{" > n> r n
                           "}" > n>
                           )
                       "cif"
                       "Insert a C if statement"
                       'c++-tempo-tags)

(tempo-define-template "c-else"
                       '(> "else" n>
                           "{" > n> r n
                           "}" > n>
                           )
                       "celse"
                       "Insert a C else statement"
                       'c++-tempo-tags)

(tempo-define-template "c-if-else"
                       '(> "if (" (p "if-clause:" clause) ")"  n>
                           "{" > n> r n
                           "}" > n>
                           > "else" n>
                           "{" > n> r n
                           "}" > n>
                           )
                       "cifelse"
                       "Insert a C if else statement"
                       'c++-tempo-tags)

(tempo-define-template "c-while"
                       '(> "while (" (p "while-clause:" clause) ")" >  n>
                           "{" > n> r n
                           "}" > n>
                           )
                       "cwhile"
                       "Insert a C while statement"
                       'c++-tempo-tags)

(tempo-define-template "c-for"
                       '(> "for (" (p "variable type:" type) " " (p "variable:" var) " = " (p "lower bound:" lb) "; " (s var)
                           " < "(p "upper bound:" ub)"; ++" (s var) ")" >  n>
                           "{" > n> r n
                           "}" > n>
                           )
                       "cfor"
                       "Insert a C for loop:for(x = 0; x < ..; x++)"
                       'c++-tempo-tags)

(tempo-define-template "c-foreach"
                       '(> "foreach (" (p "variable type:" type) " " (p "variable:" var) ", " (p "iterable:" it) ")" >  n>
                           "{" > n> r n
                           "}" > n>
                           )
                       "cforeach"
                       "Insert a Qt foreach loop:foreach(T x, list<T>)"
                       'c++-tempo-tags)

(tempo-define-template "c-switch"
                       '(> "switch (" (p "switch-condition:" clause) ")" >  n>
                           "{" > n
                           "case " (p "first value:") ":" > n> "{" n>
                           "break;" > n> "}" n
                           "}" > n>
                           )
                       "cswitch"
                       "Insert a C switch statement"
                       'c++-tempo-tags)

(tempo-define-template "c-case"
                       '(n "case " (p "value:") ":" > n> "{" n>
                           "break;" > n> "}"
                           )
                       "ccase"
                       "Insert a C case statement"
                       'c++-tempo-tags)

(tempo-define-template "c++-class"
                       '("class " (p "classname:" class) p n "{" n "public:" n>
                         (s class) "();" n n>
                         "~" (s class) "();" n n>
                         "};" n>
                         )
                       "cclass"
                       "Insert a class skeleton"
                       'c++-tempo-tags)

(tempo-define-template "c++-copy-constructor"
                       '(
                         (p "classname:" class) "(const " (s class) " & copy);" n> p
                         )
                       "copyctr"
                       "Insert a copy constructor"
                       'c++-tempo-tags)

(tempo-define-template "c++-const-get"
                       '(
                         "const " (p "attr type:" type) "& get_" (s attrname) "() const { return _" (s attrname) "; }" n>
                         )
                       "cconstget"
                       "Insert an attribute's const getter."
                       'c++-tempo-tags)

(tempo-define-template "c++-get"
                       '(
                         (p "attr type:" type) "& get_" (s attrname) "() { return _" (s attrname) "; }" n>
                         )
                       "cget"
                       "Insert an attribute's getter."
                       'c++-tempo-tags)

(tempo-define-template "c++-set"
                       '(
                         "void set_" (s attrname) "(const " (p "attribute's type:" type) "& val) { _" (s attrname) " = val; }" n>
                         )
                       "cset"
                       "Insert an attribute's setter."
                       'c++-tempo-tags)

(tempo-define-template "c-main"
                       '(> "int main(int argc, char *argv[])" >  n>
                           "{" > n>
                           > r n
                           > "return 0 ;" n>
                           > "}" > n>
                           )
                       "cmain"
                       "Insert a C main statement"
                       'c++-tempo-tags)


(tempo-define-template "c++-namespace"
                       '(> "namespace " (p "Namespace:" namespace) p n>
                           "{" n> n> n>
                           "}"
                           )
                       "cnsp"
                       "Insert a C++ namespace declaration"
                       'c++-tempo-tags)

(provide 'init-c++)
