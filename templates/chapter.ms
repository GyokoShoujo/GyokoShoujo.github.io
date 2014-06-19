#lang mustache
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <meta name="author" content="Keroanne"/>
    <meta name="description" content="Gyoko Shoujo graphic novel">
    <title>Gyoko Shoujo - {{ chapter_title }}</title>
    <link href="http://fonts.googleapis.com/css?family=Crafty+Girls" rel="stylesheet" type="text/css"/>
    <link href="/static/css/gyoko.css" rel="stylesheet" type="text/css"/>
  </head>
  <body>
    <a href="{{ page1_uri }}">
      <img class="page-image" src="{{ cover_image_uri }}" />
    </a>
    <div class="main-content">
    {{ content_markdown }}
    </div>
    <div class="pager-footer">
      <a href="{{ page1_uri }}">{{ chapter_title }}</a>
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
