require 'github-markdown-preview'
require 'pathname'
require 'webrick'


module GithubMarkdownServer

  ##
  # Serves files and converts markdown to github like html (with live.js as well).
  class Server
    def initialize(options = {})
      port = options[:port] || 8000
      @directory = File.expand_path(options[:directory])
      url = "http://localhost:#{port}/"

      if !File.directory?(@directory)
        url += File.basename(@directory)
        @directory = File.dirname(@directory)
      end

      server_options = {
        :Port => port,
        :DocumentRoot => @directory,
        Logger: WEBrick::Log.new("/dev/null"),
        AccessLog: [],
      }
      if options[:browser]
        server_options[:StartCallback] = Proc.new {
          system('open', url)
        }
      end
      server = WEBrick::HTTPServer.new(server_options)

      @md2html = GithubMarkdownPreview::HtmlPreview.new(nil, options)

      trap 'INT' do server.shutdown end
      server.mount_proc '/' do |req, res|
        if req.path == '/favicon.ico'
          source_file = Resources.expand_path(File.join('image','favicon.ico'))
        else
          source_file = File.join(@directory, req.path)
        end
        case req.request_method
        when 'HEAD'
          res['last-modified'] = File.mtime(source_file).to_s
        else
          livejs = "<script>\n#{IO.read(Resources.expand_path(File.join('js','live.js')))}\n</script>"

          if source_file.end_with? '.md'
            @md2html.source_file = source_file
            res.body = @md2html.update
            res.body += livejs
          elsif File.directory?(source_file)
            directory = source_file
            for index in ['index.md', 'index.html', 'index.htm']
              source_file = File.join(directory, index)
              if File.exists?(source_file)
                res.body = IO.read(source_file)
                res.body += livejs
                break
              end
            end
            if !File.exists?(source_file)
              bonus = nil
              readme = File.join(directory, '/README.md')
              if File.exists?(readme)
                @md2html.source_file = readme
                bonus = "<hr>#{@md2html.update}"
              else
                bonus = @md2html.wrap_preview('')
              end
              res.body = directory_listing(directory, req.path == '/', bonus)
            end
          else
            res.body = IO.read(source_file)
          end
        end
        res['content-type'] = mime_type(File.extname(source_file))
      end

      puts "Starting server #{url}"
      server.start
    end

    def directory_listing(dir, root, bonus)
      body = '<ul>'
      body += '<li><a href="../">..</a>' unless root
      dirs = Pathname.glob(File.join(dir, '/*/')).map do |x|
        d = x.basename.to_s
        "<li><a href=\"#{d}/\">#{d}</a></li>"
      end
      mds = Pathname.glob(File.join(dir, '*.md')).map do |x|
        md = x.basename.to_s
        "<li><a href=\"#{md}\">#{md}</a></li>"
      end

      body += dirs.join('') unless dirs.empty?
      body += mds.join('') unless mds.empty?

      body += '</ul>'
      body += "#{bonus}" if bonus
      body
    end

    def mime_type(ext)
      ext = ext.downcase.sub(/^\./, '')
      case ext
      when 'gif', 'jpg', 'png'
        "image/#{ext}"
      when 'md', 'html', 'htm'
        'text/html'
      when 'js', 'css'
        "text/#{ext}"
      when 'ico'
        'image/vnd.microsoft.icon'
      else
        'application/octet-stream'
      end
    end
  end
end
