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

desc "Generate html documentation"
task :doc do
  sh "cd #{File.dirname(__FILE__)}/doc/; cat testing.pod | pod2html > testing.html"
end

task :clean do
  sh "cd #{File.dirname(__FILE__)}; find . -name '*~' | xargs rm -f; find . -name 'actions.log' | xargs rm -f; find . -name 'pod2htm*.tmp' | xargs rm -f; "
end

task :release => [:doc, :clean] do
  sh "cd #{File.dirname(__FILE__)}; " + %Q{tar --create --verbose --file ../test-manager-1.0.tar --directory .. --exclude="*.svn*" --exclude=.commitmail test-manager/}
end
