(define-resource rol ()
  :class (s-prefix "org:Role")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/concept/functionarisRol/")
  :features '(include-uri)
  :on-path "rollen"
)