module GithubMarkdownServer
  class FileNotFoundError < StandardError; end

  require 'github-markdown-server/version'
  require 'github-markdown-server/resources'
  require 'github-markdown-server/server'
end
