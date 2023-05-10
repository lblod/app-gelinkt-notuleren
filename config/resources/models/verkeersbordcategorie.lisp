(define-resource verkeersbordcategorie ()
  :class (s-prefix "mobiliteit:Verkeersbordcategorie")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((verkeersbordconcept :via ,(s-prefix "org:classification")
                                   :inverse t
                                   :as "verkeersbordconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordcategorieen/")
  :on-path "verkeersbordcategorieen"
  )