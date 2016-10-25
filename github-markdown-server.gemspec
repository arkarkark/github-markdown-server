$:.push File.expand_path('../lib', __FILE__)
require 'github-markdown-server/version'

Gem::Specification.new do |s|
  s.name        = 'github-markdown-server'
  s.version     = GithubMarkdownServer::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ['Alex K (wtwf.com)']
  s.email       = 'wtwf.com'
  s.homepage    = 'https://github.com/arkarkark/github-markdown-server'
  s.summary     = %q{Use your favorite editor to edit a markdown file, Run the server and open the file. Saving in your editor updates instantly in the browser.}
  s.description = %q{Runs a webserver to preview Github markdown with live.js updating.}
  s.license     = 'MIT'

  s.add_dependency 'github-markdown-preview', '~> 4.0'
  s.add_dependency 'github-linguist', '~> 3.3'
  s.add_dependency 'rugged', '~> 0.23'

  s.add_development_dependency 'minitest', '~> 5.4'
  s.add_development_dependency 'bundler', '~> 1.7'
  s.add_development_dependency 'rake', '~> 10.3'

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = %w(lib)
end
