# -*- ruby-mode -*-

require 'rake'

task :default => :test

desc "Run the full test suite in MIT Scheme and Guile"
task :test => [ :mit_scheme_test, :guile_test ]

desc "Run the full test suite in MIT Scheme"
task :mit_scheme_test do 
  sh %Q{mit-scheme --batch-mode --eval "(set! load/suppress-loading-message? #t)" --load load.scm --load all-tests.scm --eval "(%exit (run-registered-tests))"}
end

desc "Run the full test suite in Guile"
task :guile_test do
  sh %Q{guile -l load.scm -l all-tests.scm -c "(exit (run-registered-tests))"}
end

desc "Run a demonstration test suite to show off failure reports"
task :demo do 
  sh %Q{mit-scheme --batch-mode --eval "(set! load/suppress-loading-message? #t)" --load load.scm --load failure-report-demo.scm --eval "(%exit 0)"}
end

desc "Run a demonstration test suite to show off failure reports in Guile"
task :guile_demo do 
  sh %Q{guile -l load.scm -l failure-report-demo.scm -c "(exit 0)"}
end

desc "Install the current version of this testing manager to /infolab/share/lib/testing"
task :install do
  sh "release --verbose --maintainer=axch@mit.edu . /infolab/share/lib/testing"
  sh "touch release-hack; release -w -v release-hack /infolab/share/lib/testing/WARNING; rm release-hack"
end
