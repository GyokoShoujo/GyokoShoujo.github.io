#lang mustache
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <meta name="author" content="Keroanne"/>
    <meta name="description" content="Gyoko Shoujo graphic novel">
    <title>Gyoko Shoujo - {{ page_title }} (page {{ page_number }})</title>
    <link href="http://fonts.googleapis.com/css?family=Crafty+Girls" rel="stylesheet" type="text/css"/>
    <link href="/static/css/gyoko.css" rel="stylesheet" type="text/css"/>
  </head>
  <body>
    <div class="header"><a href="/"><img src="/static/images/header.png"/></a></div>
    {{# has_next }}<a href="{{ next_uri }}">{{/ has_next }}
    <img class="page-image" src="{{ image_uri }}" />
    {{# has_next }}</a>{{/ has_next }}
    <div class="main-content">
    {{& content_markdown }}
    </div>
    <div class="pager-footer">
      {{# has_previous }}<a href="{{ previous_uri }}"> ≪ </a>{{/ has_previous }}
      Page {{ page_number }}
      {{# has_next }}<a href="{{ next_uri }}"> ≫ </a>{{/ has_next }}
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
