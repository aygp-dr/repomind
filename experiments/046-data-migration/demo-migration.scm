#!/usr/bin/env guile
!#

;;; demo-migration.scm - Database migration system demonstration

(use-modules (ice-9 format))

;; Migration registry
(define migrations
  '((1 . ((up . "CREATE TABLE repositories (id INTEGER PRIMARY KEY, url TEXT);")
          (down . "DROP TABLE repositories;")
          (description . "Create repositories table")))
    (2 . ((up . "ALTER TABLE repositories ADD COLUMN name TEXT;")
          (down . "ALTER TABLE repositories DROP COLUMN name;")
          (description . "Add name column to repositories")))
    (3 . ((up . "CREATE INDEX idx_repo_name ON repositories(name);")
          (down . "DROP INDEX idx_repo_name;")
          (description . "Add index on repository name")))
    (4 . ((up . "CREATE TABLE analyses (id INTEGER, repo_id INTEGER, summary TEXT);")
          (down . "DROP TABLE analyses;")
          (description . "Create analyses table")))
    (5 . ((up . "ALTER TABLE analyses ADD COLUMN created_at TIMESTAMP DEFAULT NOW();")
          (down . "ALTER TABLE analyses DROP COLUMN created_at;")
          (description . "Add timestamp to analyses")))))

;; Current migration version
(define current-version 0)

;; Run migration up
(define (migrate-up target-version)
  (format #t "🔼 Migrating up to version ~a~%~%" target-version)
  
  (let loop ((version (+ current-version 1)))
    (when (<= version target-version)
      (let* ((migration (assq version migrations))
             (up-sql (assq-ref (cdr migration) 'up))
             (desc (assq-ref (cdr migration) 'description)))
        
        (format #t "📝 Running migration ~a: ~a~%" version desc)
        (format #t "   SQL: ~a~%" up-sql)
        (format #t "   ✅ Applied successfully~%~%")
        
        (set! current-version version)
        (loop (+ version 1))))))

;; Run migration down
(define (migrate-down target-version)
  (format #t "🔽 Migrating down to version ~a~%~%" target-version)
  
  (let loop ((version current-version))
    (when (> version target-version)
      (let* ((migration (assq version migrations))
             (down-sql (assq-ref (cdr migration) 'down))
             (desc (assq-ref (cdr migration) 'description)))
        
        (format #t "↩️  Rolling back migration ~a: ~a~%" version desc)
        (format #t "   SQL: ~a~%" down-sql)
        (format #t "   ✅ Rolled back successfully~%~%")
        
        (set! current-version (- version 1))
        (loop (- version 1))))))

;; Show migration status
(define (show-status)
  (format #t "📊 Migration Status:~%")
  (format #t "   Current version: ~a~%~%" current-version)
  
  (format #t "   Applied migrations:~%")
  (for-each (lambda (m)
              (let ((version (car m))
                    (desc (assq-ref (cdr m) 'description)))
                (format #t "     ~a [~a] ~a~%"
                        (if (<= version current-version) "✅" "⭕")
                        version
                        desc)))
            migrations))

(define (run-demo)
  (format #t "🧪 Data Migration System Demo~%")
  (format #t "============================~%~%")
  
  ;; Show initial status
  (show-status)
  
  ;; Migrate to latest
  (format #t "~%")
  (migrate-up 5)
  
  ;; Show status after migration
  (show-status)
  
  ;; Rollback demonstration
  (format #t "~%🔄 Demonstrating rollback:~%")
  (migrate-down 3)
  
  (format #t "~%Final status:~%")
  (format #t "  Current version: ~a~%" current-version)
  
  (format #t "~%✅ Migration system demo completed!~%"))

(run-demo)