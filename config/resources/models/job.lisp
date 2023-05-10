(define-resource job ()
  :class (s-prefix "cogs:Job")
  :properties `((:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified"))
                (:creator :url ,(s-prefix "dct:creator")) ;;Later consider using proper relation in domain.lisp
                (:status :url ,(s-prefix "adms:status")) ;;Later consider using proper relation in domain.lisp
                (:operation :url ,(s-prefix "task:operation"))) ;;Later consider using proper relation in domain.lisp

  :has-one `((job-error :via ,(s-prefix "task:error")
                        :as "error"))

  :has-many `((task :via ,(s-prefix "dct:isPartOf")
                    :inverse t
                    :as "tasks"))

  :resource-base (s-url "http://redpencil.data.gift/id/job/")
  :features '(include-uri)
  :on-path "jobs")