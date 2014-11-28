# Github Markdown Server

Use your favorite editor to edit a markdown file, Run the server and open the file.
Saving in your editor updates instantly in the browser.

github-markdown-server is built on top of [github-markdown-preview](https://github.com/dmarcotte/github-markdown-preview).

## Usage

```shell
github-markdown-server README.md
```

This will start a server serving in the current directory and all child directories and open a browser (on a mac) showing README.md converted to html. If you navigate to a directory name only child directories and .md files are shown in a directory listing. If there is a README.md file in that directory it will be appended to the directory listing.

## Contrib

There is a [contrib](contrib/) directory with an emacs lisp file which will start a server and open a file for you. It'll keep track of which servers it has started and reuse an existing server if a file you want to preview is under that server's serving directory.

There is also a small python script (called `p`) which will work with files and produce paths and urls.

## Contributing

Please feel free to send me pull requests! This is my first Ruby project and it always feels like my first time when I write Emacs Lisp.
