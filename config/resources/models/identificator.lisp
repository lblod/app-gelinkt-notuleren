(define-resource identificator ()
  :class (s-prefix "adms:Identifier")
  :properties `((:identificator :string ,(s-prefix "skos:notation"))) ;; TODO: should have a specific type
  :resource-base (s-url "http://data.lblod.info/id/identificatoren/")
  :features '(include-uri)
  :on-path "identificatoren")