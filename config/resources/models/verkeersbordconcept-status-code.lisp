(define-resource verkeersbordconcept-status-code ()
  :class (s-prefix "lblodmow:VerkeersbordconceptStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((verkeersbordconcept :via ,(s-url "http://www.w3.org/2003/06/sw-vocab-status/ns#term_status")
                                   :inverse t
                                   :as "verkeersbordconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordconcept-status-codes/")
  :on-path "verkeersbordconcept-status-codes"
  )