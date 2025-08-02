#!/usr/bin/env guile
!#

;;; demo-sqlite.scm - SQLite storage demonstration

(use-modules (ice-9 format))

;; Simulated SQLite operations (would use real SQLite bindings in production)
(define db-tables '())
(define db-data (make-hash-table))

;; Create table simulation
(define (create-table name schema)
  (set! db-tables (cons (cons name schema) db-tables))
  (hash-set! db-data name '())
  (format #t "📊 Created table: ~a~%" name))

;; Insert data simulation
(define (insert-into table data)
  (let ((current (hash-ref db-data table '())))
    (hash-set! db-data table (cons data current))
    (format #t "➕ Inserted into ~a: ~a~%" table data)))

;; Query simulation
(define (select-from table where-fn)
  (let ((data (hash-ref db-data table '())))
    (if where-fn
        (filter where-fn data)
        data)))

;; Database schema
(define (setup-database)
  (create-table 'repositories 
                '(id url name description language analyzed_at))
  (create-table 'analyses
                '(id repo_id summary quality_score created_at))
  (create-table 'metrics
                '(id repo_id metric_name value timestamp)))

;; Demo operations
(define (demo-storage-operations)
  ;; Insert repositories
  (insert-into 'repositories
               '((id . 1) (url . "https://github.com/aygp-dr/repomind") 
                 (name . "repomind") (description . "Intelligent repo analysis")
                 (language . "Scheme") (analyzed_at . "2024-01-01")))
  
  (insert-into 'repositories
               '((id . 2) (url . "https://github.com/test/project")
                 (name . "test-project") (description . "Test project")
                 (language . "Python") (analyzed_at . "2024-01-02")))
  
  ;; Insert analyses
  (insert-into 'analyses
               '((id . 1) (repo_id . 1) (summary . "Well-structured Scheme project")
                 (quality_score . 0.92) (created_at . "2024-01-01")))
  
  ;; Insert metrics
  (insert-into 'metrics
               '((id . 1) (repo_id . 1) (metric_name . "response_time")
                 (value . 1.23) (timestamp . "2024-01-01 10:00:00")))
  
  ;; Query data
  (format #t "~%📋 All repositories:~%")
  (for-each (lambda (repo)
              (format #t "  - ~a: ~a~%" 
                      (assq-ref repo 'name)
                      (assq-ref repo 'description)))
            (select-from 'repositories #f))
  
  (format #t "~%🔍 Scheme repositories:~%")
  (for-each (lambda (repo)
              (format #t "  - ~a~%" (assq-ref repo 'name)))
            (select-from 'repositories 
                        (lambda (r) (string=? (assq-ref r 'language) "Scheme")))))

(define (run-demo)
  (format #t "🧪 SQLite Storage Demonstration~%")
  (format #t "==============================~%~%")
  
  (setup-database)
  (format #t "~%Database schema created~%~%")
  
  (demo-storage-operations)
  
  (format #t "~%📈 Storage Statistics:~%")
  (format #t "  Tables: ~a~%" (length db-tables))
  (format #t "  Total records: ~a~%" 
          (apply + (map (lambda (table)
                          (length (hash-ref db-data (car table) '())))
                        db-tables)))
  
  (format #t "~%✅ SQLite storage demo completed!~%"))

(run-demo)