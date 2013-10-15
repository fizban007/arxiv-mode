arXiv-mode
==========

arxiv-mode is an [Emacs](www.gnu.org/s/emacs/‎) major mode for viewing
updates on [arXiv.org](http://arxiv.org).

Currently this project is its early stage, working functions include
search for the articles posted on a given date, in a given
category. The mode will generate a list of articles where one can
navigate and read the abstracts. One can also open the url to the
article with a custom defined web browser.


Common Usage
============

To start reading abstracts, run `M-x arxiv-read`, and enter the date
and category you would like to read.

In the article list, use `n` and `p` to navigate the article list. One
can use `C-u` to prefix the navigation commands for more movement
steps. Press `a` to toggle visibility of the abstract window. Press
`u` to open the entry in a web browser. Press `q` to exit the mode.

Installation
============

Just put the directory in your filesystem and at it to your
`load-path`. Put the following into your `.emacs` file

````lisp
(require 'arxiv-mode)
````

Customization
=============

`arxiv-mode` can be customized through emacs native customization
interface. Currently only a few variables can be modified but others
will be added as code develops. These can be found under the group
`arxiv`, or just run `arxiv-customize`.

TODO
====

Currently I'm working on better paging support and
navigation. Following are the functionality that I have in mind for
some point in the future:

* Ability to download pdf to a temporary location and open it in an
  external viewer.

* Integration with Zotero so that one can put an article into
  collection directly.

* Implement a search interface to do complex arXiv searches in Emacs
  directly.