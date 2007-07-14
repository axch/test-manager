# -*- ruby-mode -*-

require 'rake'

task :default => :test

task :test do 
  sh %Q{mit-scheme --batch-mode --load all-tests.scm --eval "(%exit (run-registered-tests))"}
end

task :demo do 
  sh %Q{mit-scheme --batch-mode --load failure-report-demo.scm --eval "(%exit 0)"}
end

desc "Install the current version of this testing manager to /infolab/share/lib/testing"
task :install do
  sh "release --verbose --maintainer=axch@mit.edu . /infolab/share/lib/testing"
  sh "touch release-hack; release -w -v release-hack /infolab/share/lib/testing/WARNING; rm release-hack"
end
