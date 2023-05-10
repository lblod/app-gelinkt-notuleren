(define-resource rechtsgrond ()
  :class (s-prefix "eli:LegalResource")
  :properties `((:buitenwerkingtreding :date ,(s-prefix "eli:date_no_longer_in_force"))
                (:inwekingtreding :date ,(s-prefix "eli:first_date_entry_in_force"))
                (:type-document :uri-set ,(s-prefix "eli:type_document"))) ;;TODO: what about predefined lists?
  :resource-base (s-url "https://data.lblod.info/id/rechtsgronden/")
  :features '(include-uri)
  :on-path "rechtsgronden")