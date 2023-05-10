(define-resource task ()
  :class (s-prefix "task:Task")
  :properties `((:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified"))
                (:status :url ,(s-prefix "adms:status")) ;;Later consider using proper relation in domain.lisp
                (:operation :url ,(s-prefix "task:operation")) ;;Later consider using proper relation in domain.lisp
                (:index :string ,(s-prefix "task:index")))

  :has-one `((job-error :via ,(s-prefix "task:error")
                    :as "error")
             (job :via ,(s-prefix "dct:isPartOf")
                    :as "job"))

  :has-many `((task :via ,(s-prefix "cogs:dependsOn")
                    :as "parent-tasks")
              (data-container :via ,(s-prefix "task:resultsContainer")
                    :as "results-containers")
              (data-container :via ,(s-prefix "task:inputContainer")
                    :as "input-containers")
              )

  :resource-base (s-url "http://redpencil.data.gift/id/task/")
  :features '(include-uri)
  :on-path "tasks")


;; Old (overwritten) version of the task model
;;
;; (define-resource task ()
;;   :class (s-prefix "ext:Task")
;;   :properties `((:title :string ,(s-prefix "ext:taskTitle"))
;;                 (:description :string ,(s-prefix "ext:taskDescription"))
;;                 (:priority :number ,(s-prefix "ext:taskPriority"))
;;                 (:click-target :string ,(s-prefix "ext:taskClickTarget")))

;;   :has-one `((task :via ,(s-prefix "ext:taskChild")
;;                   :inverse t
;;                   :as "task-parent"))

;;   :has-many `((task :via ,(s-prefix "ext:taskChild")
;;                     :as "task-childs"))

;;   :resource-base (s-url "http://data.lblod.info/id/tasks/")
;;   :features '(include-uri)
;;   :on-path "tasks")

