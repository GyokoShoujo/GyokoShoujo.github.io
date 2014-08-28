#lang mustache
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <meta name="author" content="Keroanne"/>
    <meta name="description" content="Gyoko Shoujo graphic novel">
    <title>Gyoko Shoujo - {{ page_title }} (page {{ page_number }})</title>
    <link href="http://fonts.googleapis.com/css?family=Crafty+Girls" rel="stylesheet" type="text/css"/>
    <link href="/static/css/gyoko.css" rel="stylesheet" type="text/css"/>
    <script src="//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
    <script src="/static/js/gyoko.js"></script>
  </head>
  <body>
    <div class="header"><a href="/"><img src="/static/images/header.png"/></a></div>
    <div class="nav-selector">
      {{# has_previous? }}
      <a href="{{ previous_uri }}">≪</a>
      |
      {{/ has_previous? }}
      Page:
      <select class="nav-select">
        {{# has_prev_chapter_uri? }}
        <option value="{{ prev_chapter_uri }}">Previous Chapter</option>
        {{/ has_prev_chapter_uri? }}
        {{# pages_list }}
        <option {{# selected_page? }}selected{{/ selected_page? }}
                value="{{ html_uri }}">{{ page_number }}</option>
        {{/ pages_list }}
        {{# has_next_chapter_uri? }}
        <option value="{{ next_chapter_uri }}">Next Chapter</option>
        {{/ has_next_chapter_uri? }}
      </select>
      <input class="nav-go" type="button" onclick="nav_click();" value="Go" />
      {{# has_next? }}
      |
      <a href="{{ next_uri }}">≫</a>
      {{/ has_next? }}
    </div>
    {{# has_next? }}<a href="{{ next_uri }}">{{/ has_next? }}
    <img class="page-image" src="{{ image_uri }}" />
    {{# has_next? }}</a>{{/ has_next? }}
    <div class="main-content">
    {{& content_markdown }}
    </div>
    <div class="pager-footer">
      {{# has_previous? }}<a href="{{ previous_uri }}">≪</a>{{/ has_previous? }}
      Page {{ page_number }}
      {{# has_next? }}<a href="{{ next_uri }}">≫</a>{{/ has_next? }}
    </div>
  </body>
  <script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-51219911-1', 'gyokoshoujo.com');
  ga('send', 'pageview');
  </script>
</html>
