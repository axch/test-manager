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

desc "Run a demonstration test suite to show off failure reports in MIT Scheme"
task :demo do 
  sh %Q{mit-scheme --batch-mode --eval "(set! load/suppress-loading-message? #t)" --load load.scm --load failure-report-demo.scm --eval "(%exit 0)"}
end

desc "Run a demonstration test suite to show off failure reports in Guile"
task :guile_demo do 
  sh %Q{guile -l load.scm -l failure-report-demo.scm -c "(exit 0)"}
end

install_path = "/infolab/share/lib/guile-1.8/test-manager"
desc "Describe how to Install the current version of this testing manager to #{install_path}"
task :install do
  puts "To install, go to #{install_path} and run 'svn up'"
end
