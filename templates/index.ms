#lang mustache
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <meta name="author" content="Keroanne"/>
    <meta name="description" content="Gyoko Shoujo graphic novel">
    <title>Gyoko Shoujo</title>
    <link href="http://fonts.googleapis.com/css?family=Crafty+Girls" rel="stylesheet" type="text/css"/>
    <link href="/static/css/gyoko.css" rel="stylesheet" type="text/css"/>
  </head>
  <body>
    <a href="{{ chapter1_uri }}">
      <img class="page-image" src="/static/images/Cover.png"/>
    </a>
    <div class="main-content">
    {{& content_markdown }}
    </div>
    <h2>Chapters</h2>
    <table class="chapter-links">
    {{# chapters }}
      <tr class="chapter-link">
        <td class="thumbnail">
          <a href="{{ html_uri }}">
            <img class="page-thumbnail" src="{{ cover_thumbnail_uri }}" />
          </a>
        </td>
        <td class="title">
          <a href="{{ html_uri }}">{{ number }}. {{ title }}</a>
        </td>
      </tr>
    {{/ chapters }}
    </table>
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
