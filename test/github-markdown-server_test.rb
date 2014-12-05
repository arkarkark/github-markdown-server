require 'minitest/autorun'

class TestBin < Minitest::Test
  def setup
    @gms_scipt = File.join(File.dirname(__FILE__), '..', 'bin', 'github-markdown-server')
  end

  def test_bad_params
    IO.popen("bundle exec #{@gms_scipt} --nonsense 2>&1") do |io|
      assert_match(/.*invalid option: --nonsense.*/, io.read, 'invalid option: --nonsense')
    end
  end

  def test_version_ouput
    IO.popen("bundle exec #{@gms_scipt} -v") do |io|
      assert_match(GithubMarkdownServer::VERSION, io.read, '-v call should output version')
    end
  end
end
