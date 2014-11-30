require 'github-markdown-preview'
require 'pathname'
require 'tempfile'
require 'webrick'


module GithubMarkdownServer

  ##
  # Serves files and converts markdown to github like html (with live.js as well).
  class Server
    def initialize(options = {})
      port = options[:server_port] || 8000
      @directory = File.expand_path(options[:directory])
      @file_name = File.expand_path(options[:file_name])
      url = "http://localhost:#{port}/"

      if !File.directory?(@directory)
        @directory = File.dirname(@file_name)
      end

      url = url + @file_name[@directory.length + 1..-1] if @file_name != @directory

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
            res.body = md2html(source_file)
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
                bonus = md2html(readme)
              else
                bonus = emptystyles
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


    def md2html(file)
      out = Tempfile.new(File.basename(file))
      GithubMarkdownPreview::HtmlPreview.new(file, {:preview_file => out.path})
      IO.read(out.path)
    end

    def emptystyles
      file = Tempfile.new('')
      out = Tempfile.new('')
      GithubMarkdownPreview::HtmlPreview.new(file, {:preview_file => out.path}).wrap_preview('')
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
      if bonus
        sep = '<div class="readme-content">'
        body = bonus.sub!(sep, (body + sep))
      end
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
