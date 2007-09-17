# -*- ruby-mode -*-

require 'rake'

task :default => :test

desc "Run the full test suite"
task :test do 
  sh %Q{mit-scheme --batch-mode --load all-tests.scm --eval "(%exit (run-registered-tests))"}
end

desc "Run the full test suite in Guile"
task :guile_test do
  sh %Q{guile -l all-tests.scm -c "(exit (run-registered-tests))"}
end

desc "Run a demonstration test suite to show off failure reports"
task :demo do 
  sh %Q{mit-scheme --batch-mode --load failure-report-demo.scm --eval "(%exit 0)"}
end

desc "Install the current version of this testing manager to /infolab/share/lib/testing"
task :install do
  sh "release --verbose --maintainer=axch@mit.edu . /infolab/share/lib/testing"
  sh "touch release-hack; release -w -v release-hack /infolab/share/lib/testing/WARNING; rm release-hack"
end
