(define-resource data-container ()
  :class (s-prefix "nfo:DataContainer")
  :properties `((:has-graph :url ,(s-prefix "task:hasGraph")))
  :has-many `((file :via ,(s-prefix "task:hasFile") ;;subProperty of dct:hasPart because mu-resource does not like the same predicate linked to multiple types
                    :as "files")
              (harvesting-collection :via ,(s-prefix "task:hasHarvestingCollection")
                                     :as "harvesting-collections")
              (task :via ,(s-prefix "task:resultsContainer")
                    :inverse t
                    :as "result-from-tasks")
              (task :via ,(s-prefix "task:inputContainer")
                    :inverse t
                    :as "input-from-tasks")
              )
  :resource-base (s-url "http://redpencil.data.gift/id/dataContainers/")
  :features '(include-uri)
  :on-path "data-containers")