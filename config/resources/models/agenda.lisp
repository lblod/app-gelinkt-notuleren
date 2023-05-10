(define-resource agenda ()
  :class (s-prefix "bv:Agenda")
  :properties `(
                (:inhoud :string ,(s-prefix "prov:value"))
                (:agenda-status :string ,(s-prefix "bv:agendaStatus"))
                (:agenda-type :string ,(s-prefix "bv:agendaType"))
                (:rendered-content :string ,(s-prefix "ext:renderedContent"))
                (:deleted :boolean ,(s-prefix "ext:deleted"))
                )
  :has-one `(
             (zitting :via ,(s-prefix "bv:isAgendaVoor")
                      :as "zitting")
             (published-resource :via ,(s-prefix "ext:publishesAgenda")
                                 :inverse t
                                 :as "published-resource")
             )
  :has-many `(
              (agendapunt :via ,(s-prefix "dct:isPartOf")
                          :inverse t
                          :as "agendapunten")
              (signed-resource :via ,(s-prefix "ext:signsAgenda")
                               :inverse t
                               :as "signed-resources")
              )
  :resource-base (s-url "http://data.lblod.info/id/agendas/")
  :features '(include-uri)
  :on-path "agendas")